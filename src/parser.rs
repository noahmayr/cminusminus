use std::{cell::RefCell, rc::Rc};

use crate::{context::*, lexer::*};
use arcstr::ArcStr;
use miette::{MietteDiagnostic, Result, SourceSpan};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeExpr {
    IntLiteral(ArcStr),
    StringLiteral(ArcStr),
    Var(ArcStr),
}
pub type SrcExpr = Src<NodeExpr>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeBuiltin {
    Exit(SrcExpr),
    Print(SrcExpr),
}
pub type SrcBuiltin = Src<NodeBuiltin>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct NodeScope {
    pub statements: Vec<SrcStmt>,
}
pub type SrcScope = Src<NodeScope>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeStmt {
    Builtin(SrcBuiltin),
    Decl { ident: ArcStr, expr: SrcExpr },
    Assign { ident: ArcStr, expr: SrcExpr },
    Scope(SrcScope),
}
pub type SrcStmt = Src<NodeStmt>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ParseTree(pub SrcScope);

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a Vec<SrcToken>,
    context: Rc<Context>,
    cursor: RefCell<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<SrcToken>, context: Rc<Context>) -> Self {
        Self {
            tokens,
            context,
            cursor: RefCell::new(0),
        }
    }

    pub fn parse(tokens: &'a Vec<SrcToken>, context: Rc<Context>) -> Result<ParseTree> {
        Self::new(tokens, context).run()
    }

    pub fn run(&self) -> Result<ParseTree> {
        let mut statements: Vec<SrcStmt> = vec![];

        while let Some(statement) = self.parse_statement()? {
            statements.push(statement)
        }
        *self.cursor.borrow_mut() = 0;
        let scope = Src::new(NodeScope { statements }, 0..(self.context.src().len()));
        Ok(ParseTree(scope))
    }

    fn parse_statement(&self) -> Result<Option<SrcStmt>> {
        if let Some(builtin) = self.parse_builtin()? {
            let end = match self.consume_or_fail(&Token::Semicolon, builtin.end())? {
                Some(semi) => semi.end(),
                None => builtin.end(),
            };
            let span = builtin.span_to(end);
            Ok(Some(Src::new(NodeStmt::Builtin(builtin), span)))
        } else if let Some(token_let) = self.try_consume(&Token::Let) {
            let Some(src_ident) = self.peek() else {
                self.context.error(Error::UnexpectedEof {
                    expected: Token::Ident("some_identifier".into()),
                    eof: self.context.eof(),
                });
                return Ok(None);
            };
            let Token::Ident(ident) = src_ident.inner() else {
                self.context.error(Error::UnexpectedEof {
                    expected: Token::Ident("placeholder".into()),
                    eof: self.context.eof(),
                });
                return Ok(None);
            };
            self.advance();
            let Some(eq) = self.consume_or_fail(&Token::Equals, src_ident.end())? else {
                return Ok(None);
            };
            let Some(expr) = self.parse_expression() else {
                self.context.error(Error::ExpressionExpected {
                    after: token_let.span_to(eq),
                });
                return Ok(None);
            };
            let end = match self.consume_or_fail(&Token::Semicolon, expr.end())? {
                Some(semi) => semi.end(),
                None => token_let.end(),
            };
            let span = token_let.span_to(end);
            Ok(Some(Src::new(
                NodeStmt::Decl {
                    ident: ident.clone(),
                    expr,
                },
                span,
            )))
        } else if let Some(Src {
            inner: Token::Ident(ident),
            span: ident_span,
        }) = self.peek()
        {
            self.advance();
            let Some(eq) = self.consume_or_fail(&Token::Equals, ident_span.offset()+ident_span.len())? else {
                return Ok(None);
            };
            let Some(expr) = self.parse_expression() else {
                self.context.error(Error::ExpressionExpected {
                    after: ident_span.span_to(eq)
                });
                return Ok(None);
            };
            let end = match self.consume_or_fail(&Token::Semicolon, expr.end())? {
                Some(semi) => semi.end(),
                None => ident_span.offset() + ident_span.len(),
            };
            Ok(Some(Src::new(
                NodeStmt::Assign {
                    ident: ident.clone(),
                    expr,
                },
                ident_span.span_to(end),
            )))
        } else if let Some(start) = self.try_consume(&Token::OpenCurly) {
            let mut statements = vec![];
            loop {
                if self.peek_matches(&Token::CloseCurly) {
                    break;
                }
                let Some(statement) = self.parse_statement()? else {
                    break;
                };
                statements.push(statement);
            }
            let Some(end) = self.consume_or_fail(
                &Token::CloseCurly,
                statements
                    .last()
                    .map(|s| s.end())
                    .unwrap_or_else(|| start.end()),
            )?
            else {
                return Ok(None);
            };
            let scope = Src::new(NodeScope { statements }, start.span_to(end));
            Ok(Some(Src::new(NodeStmt::Scope(scope), start.span_to(end))))
        } else if let Some(token) = self.peek() {
            self.context.error(Error::UnexpectedToken {
                token: token.clone(),
            });
            self.advance();
            Ok(None)
        } else {
            Ok(None)
        }
    }

    fn parse_builtin(&self) -> Result<Option<SrcBuiltin>> {
        let Some(token) = self.peek() else {
            return Ok(None);
        };
        match token.inner() {
            Token::Ident(keyword) => match keyword.as_str() {
                "exit" => {
                    self.advance();
                    let Some(prev) = self.consume_or_fail(&Token::OpenParen, token.end())? else {
                        return Ok(None);
                    };
                    let Some(expression) = self.parse_expression() else {
                        self.context.error(Error::ExpressionExpected {
                            after: token.span_to(prev),
                        });
                        return Ok(None);
                    };
                    let builtin_exit = NodeBuiltin::Exit(expression.clone());

                    let Some(close_paren) =
                        self.consume_or_fail(&Token::CloseParen, expression.end())?
                    else {
                        return Ok(None);
                    };

                    Ok(Some(Src::new(builtin_exit, token.span_to(close_paren))))
                }
                "print" => {
                    self.advance();

                    let Some(prev) = self.consume_or_fail(&Token::OpenParen, token.end())? else {
                        return Ok(None);
                    };
                    let Some(expression) = self.parse_expression() else {
                        self.context.error(Error::ExpressionExpected {
                            after: token.span_to(prev),
                        });
                        return Ok(None);
                    };

                    let Some(close_paren) =
                        self.consume_or_fail(&Token::CloseParen, expression.end())?
                    else {
                        return Ok(None);
                    };

                    let builtin_print = NodeBuiltin::Print(expression);
                    Ok(Some(Src::new(builtin_print, token.span_to(close_paren))))
                }
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    fn parse_expression(&self) -> Option<SrcExpr> {
        let token = self.peek()?;
        match token.inner() {
            Token::IntLiteral(value) => {
                self.advance();
                Some(Src::new(NodeExpr::IntLiteral(value.clone()), token))
            }
            Token::StringLiteral(value) => {
                self.advance();
                Some(Src::new(NodeExpr::StringLiteral(value.clone()), token))
            }
            Token::Ident(name) => {
                self.advance();
                Some(Src::new(NodeExpr::Var(name.clone()), token))
            }
            _ => None,
        }
    }

    fn try_consume(&self, expected: &Token) -> Option<&SrcToken> {
        let token = self.peek()?;
        if token.inner() == expected {
            self.advance();
            Some(token)
        } else {
            None
        }
    }

    fn consume_or_fail(&self, expected: &Token, at: usize) -> Result<Option<&SrcToken>> {
        match self.consume() {
            None => {
                self.context.error(Error::UnexpectedEof {
                    expected: expected.clone(),
                    eof: self.context.eof(),
                });
                Ok(None)
            }
            Some(actual) => {
                if actual.inner() != expected {
                    self.context.error(Error::ExpectedToken {
                        expected: expected.clone(),
                        actual: actual.clone(),
                        at,
                    });
                    *self.cursor.borrow_mut() -= 1;
                    return Ok(None);
                }
                Ok(Some(actual))
            }
        }
    }

    fn consume(&self) -> Option<&SrcToken> {
        let token = self.tokens.get(self.pos());
        self.advance();
        token
    }

    fn advance(&self) {
        self.advance_by(1);
    }

    fn advance_by(&self, amount: usize) {
        *self.cursor.borrow_mut() += amount;
    }

    fn peek(&self) -> Option<&SrcToken> {
        self.peek_ahead(0)
    }

    fn peek_matches(&self, token: &Token) -> bool {
        let Some(next) = self.peek() else {
            return false;
        };
        next.inner() == token
    }

    fn peek_ahead(&self, offset: usize) -> Option<&SrcToken> {
        self.tokens.get(self.pos() + offset)
    }

    fn pos(&self) -> usize {
        *self.cursor.borrow()
    }
}

#[derive(Debug)]
enum Error {
    ExpressionExpected {
        after: SourceSpan,
    },
    ExpectedToken {
        expected: Token,
        actual: SrcToken,
        at: usize,
    },
    UnexpectedToken {
        token: SrcToken,
    },
    UnexpectedEof {
        expected: Token,
        eof: usize,
    },
}

impl From<Error> for MietteDiagnostic {
    fn from(value: Error) -> Self {
        match value {
            Error::ExpressionExpected { after } => MietteDiagnostic::new("Expression expected")
                .with_code("cmm::parser::expression_expected")
                .add_label("after this", after),

            Error::ExpectedToken {
                expected,
                actual,
                at,
            } => MietteDiagnostic::new(format!("Expected token: {}", expected))
                .with_code("cmm::parser::expected_token")
                .add_label(format!("unexpected token: {}", *actual), &actual)
                .add_label(format!("expected token: {}", expected), at),
            Error::UnexpectedEof { expected, eof } => MietteDiagnostic::new("Unexpected EOF")
                .with_code("cmm::parser::unexpected_eof")
                .add_label(format!("Unexpected EOF expected: {}", expected), eof),
            Error::UnexpectedToken { token } => {
                MietteDiagnostic::new(format!("Unxpected token: {}", *token))
                    .with_code("cmm::parser::expected_token")
                    .add_label(format!("unexpected token: {}", *token), &token)
            }
        }
    }
}
