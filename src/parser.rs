use std::{cell::RefCell, rc::Rc};

use crate::{context::*, lexer::*};
use arcstr::ArcStr;
use miette::{MietteDiagnostic, Result, SourceSpan};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeTerm {
    IntLiteral(ArcStr),
    StringLiteral(ArcStr),
}

pub type SrcTerm = Src<NodeTerm>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeExpr {
    Term(SrcTerm),
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
            let Some(semi) = self.try_consume(&Token::Semicolon)? else {
                return Ok(None);
            };
            let span = builtin.to(semi);
            Ok(Some(Src::new(NodeStmt::Builtin(builtin), span)))
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
                    let Some(prev) = self.try_consume(&Token::OpenParen)? else {
                        return Ok(None);
                    };
                    let Some(expression) = self.parse_expression() else {
                        self.context.error(ParserError::ExpressionExpected {
                            after: token.to(prev),
                        });
                        return Ok(None);
                    };
                    let builtin_exit = NodeBuiltin::Exit(expression);

                    let Some(close_paren) = self.try_consume(&Token::CloseParen)? else {
                        return Ok(None);
                    };

                    Ok(Some(Src::new(builtin_exit, token.to(close_paren))))
                }
                "print" => {
                    self.advance();

                    let Some(prev) = self.try_consume(&Token::OpenParen)? else {
                        return Ok(None);
                    };
                    let Some(expression) = self.parse_expression() else {
                        self.context.error(ParserError::ExpressionExpected {
                            after: token.to(prev),
                        });
                        return Ok(None);
                    };

                    let Some(close_paren) = self.try_consume(&Token::CloseParen)? else {
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
        let term = self.parse_term()?;
        Some(Src::new(NodeExpr::Term(term.clone()), term))
    }

    fn parse_term(&self) -> Option<SrcTerm> {
        let token = self.peek()?;
        match token.inner() {
            Token::IntLiteral(value) => {
                self.advance();
                Some(Src::new(NodeTerm::IntLiteral(value.clone()), token))
            }
            Token::StringLiteral(value) => {
                self.advance();
                Some(Src::new(NodeTerm::StringLiteral(value.clone()), token))
            }
            _ => None,
        }
    }

    fn try_consume(&self, expected: &Token) -> Result<Option<&SrcToken>> {
        match self.consume() {
            None => {
                self.context.error(ParserError::UnexpectedEof {
                    expected: expected.clone(),
                    eof: self.context.src().len(),
                });
                Ok(None)
            }
            Some(actual) => {
                if &**actual != expected {
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
