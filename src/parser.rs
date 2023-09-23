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
pub enum NodeStmt {
    Builtin(SrcBuiltin),
    Let { ident: ArcStr, expr: SrcExpr },
    Scope(Vec<SrcStmt>),
}
pub type SrcStmt = Src<NodeStmt>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Program(pub Vec<SrcStmt>);

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

    pub fn parse(tokens: &'a Vec<SrcToken>, context: Rc<Context>) -> Result<Program> {
        Self::new(tokens, context).run()
    }

    pub fn run(&self) -> Result<Program> {
        let mut statements: Vec<SrcStmt> = vec![];

        while let Some(statement) = self.parse_statement()? {
            statements.push(statement)
        }
        *self.cursor.borrow_mut() = 0;
        self.context.result(Program(statements))
    }

    fn parse_statement(&self) -> Result<Option<SrcStmt>> {
        if let Some(builtin) = self.parse_builtin()? {
            let Some(semi) = self.consume_or_fail(&Token::Semicolon)? else {
                return Ok(None);
            };
            let span = builtin.to(semi);
            Ok(Some(Src::new(NodeStmt::Builtin(builtin), span)))
        } else if let Some(token_let) = self.try_consume(&Token::Let) {
            let ident = self.peek();
            let ident = match ident {
                None => {
                    self.context.error(ParserError::UnexpectedEof {
                        expected: Token::Ident("some_identifier".into()),
                        eof: self.context.eof(),
                    });
                    return Ok(None);
                }
                Some(ident) => {
                    let Token::Ident(ident) = ident.inner() else {
self.context.error(ParserError::UnexpectedEof {
                        expected: Token::Ident("placeholder".into()),
                        eof:  self.context.eof(),
                    });
                        return Ok(None);

                    };
                    ident
                }
            };
            self.advance();
            let Some(eq) = self.consume_or_fail(&Token::Equals)? else {
                return Ok(None);
            };
            let Some(expr) = self.parse_expression() else {
                        self.context.error(ParserError::ExpressionExpected {
                    after: token_let.to(eq),
                });
                return Ok(None);
            };
            let Some(semi) = self.consume_or_fail(&Token::Semicolon)? else {
                return Ok(None);
            };
            let span = token_let.to(semi);
            Ok(Some(Src::new(
                NodeStmt::Let {
                    ident: ident.clone(),
                    expr,
                },
                span,
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
            let Some(end) = self.consume_or_fail(&Token::CloseCurly)? else {
                return Ok(None);
            };
            Ok(Some(Src::new(NodeStmt::Scope(statements), start.to(end))))
        } else if let Some(token) = self.peek() {
            self.context.error(ParserError::UnexpectedToken {
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
                    let Some(prev) = self.consume_or_fail(&Token::OpenParen)? else {
                        return Ok(None);
                    };
                    let Some(expression) = self.parse_expression() else {
                        self.context.error(ParserError::ExpressionExpected {
                            after: token.to(prev),
                        });
                        return Ok(None);
                    };
                    let builtin_exit = NodeBuiltin::Exit(expression);

                    let Some(close_paren) = self.consume_or_fail(&Token::CloseParen)? else {
                        return Ok(None);
                    };

                    Ok(Some(Src::new(builtin_exit, token.to(close_paren))))
                }
                "print" => {
                    self.advance();

                    let Some(prev) = self.consume_or_fail(&Token::OpenParen)? else {
                        return Ok(None);
                    };
                    let Some(expression) = self.parse_expression() else {
                        self.context.error(ParserError::ExpressionExpected {
                            after: token.to(prev),
                        });
                        return Ok(None);
                    };

                    let Some(close_paren) = self.consume_or_fail(&Token::CloseParen)? else {
                        return Ok(None);
                    };

                    let builtin_print = NodeBuiltin::Print(expression);
                    Ok(Some(Src::new(builtin_print, token.to(close_paren))))
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

    fn consume_or_fail(&self, expected: &Token) -> Result<Option<&SrcToken>> {
        match self.consume() {
            None => {
                self.context.error(ParserError::UnexpectedEof {
                    expected: expected.clone(),
                    eof: self.context.eof(),
                });
                Ok(None)
            }
            Some(actual) => {
                if actual.inner() != expected {
                    self.context.error(ParserError::ExpectedToken {
                        expected: expected.clone(),
                        actual: actual.clone(),
                    });
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
enum ParserError {
    ExpressionExpected { after: SourceSpan },
    ExpectedToken { expected: Token, actual: SrcToken },
    UnexpectedToken { token: SrcToken },
    UnexpectedEof { expected: Token, eof: usize },
}

impl From<ParserError> for MietteDiagnostic {
    fn from(value: ParserError) -> Self {
        match value {
            ParserError::ExpressionExpected { after } => {
                MietteDiagnostic::new("Expression expected")
                    .with_code("cmm::parser::expression_expected")
                    .add_label("after this", after)
            }

            ParserError::ExpectedToken { expected, actual } => {
                MietteDiagnostic::new(format!("Expected token: {}", expected))
                    .with_code("cmm::parser::expected_token")
                    .add_label(format!("unexpected token: {}", *actual), &actual)
                    .add_label(format!("expected token: {}", expected), &actual)
            }
            ParserError::UnexpectedEof { expected, eof } => MietteDiagnostic::new("Unexpected EOF")
                .with_code("cmm::parser::unexpected_eof")
                .add_label(format!("Unexpected EOF expected: {}", expected), eof),
            ParserError::UnexpectedToken { token } => {
                MietteDiagnostic::new(format!("Unxpected token: {}", *token))
                    .with_code("cmm::parser::expected_token")
                    .add_label(format!("unexpected token: {}", *token), &token)
            }
        }
    }
}
