use std::{cell::RefCell, sync::Arc};

use crate::lexer::{Keyword, Token};
use miette::{bail, miette, LabeledSpan, Report, Result, SourceSpan};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Term {
    IntLiteral(String, SourceSpan),
    StringLiteral(String, SourceSpan),
}

impl From<Term> for SourceSpan {
    fn from(value: Term) -> Self {
        match value {
            Term::IntLiteral(_, p) => p,
            Term::StringLiteral(_, p) => p,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression {
    Term(Term, SourceSpan),
}

impl From<Expression> for SourceSpan {
    fn from(value: Expression) -> Self {
        match value {
            Expression::Term(_, s) => s,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Builtin {
    Exit(Expression, SourceSpan),
    Print(Expression, SourceSpan),
}

impl From<Builtin> for SourceSpan {
    fn from(value: Builtin) -> Self {
        match value {
            Builtin::Exit(_, s) => s,
            Builtin::Print(_, s) => s,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Statement {
    Builtin(Builtin, SourceSpan),
}
impl From<Statement> for SourceSpan {
    fn from(value: Statement) -> Self {
        match value {
            Statement::Builtin(_, s) => s,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Program(pub Vec<Statement>);

#[derive(PartialEq, Eq, Debug)]
pub struct Parser<'a> {
    tokens: &'a Vec<(Token, usize)>,
    source: Arc<str>,
    cursor: RefCell<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<(Token, usize)>, source: &str) -> Self {
        Self {
            tokens,
            source: source.into(),
            cursor: RefCell::new(0),
        }
    }

    pub fn parse(&self) -> Result<Program> {
        let mut statements: Vec<Statement> = vec![];

        while let Some(statement) = self.parse_statement()? {
            statements.push(statement)
        }
        *self.cursor.borrow_mut() = 0;
        Ok(Program(statements))
    }

    fn parse_statement(&self) -> Result<Option<Statement>> {
        if let Some(builtin) = self.parse_builtin()? {
            let (semi, semi_pos) = self.try_consume(&Token::Semicolon)?;
            let start = SourceSpan::from(builtin.clone()).offset();
            Ok(Some(Statement::Builtin(
                builtin,
                (start..(semi_pos + semi.len())).into(),
            )))
        } else if let Some(token) = self.peek() {
            bail!("Unexpected token {:?}", token)
        } else {
            Ok(None)
        }
    }

    fn parse_builtin(&self) -> Result<Option<Builtin>> {
        let Some((token, pos)) = self.peek() else {
            return Ok(None);
        };
        match token {
            Token::Keyword(keyword) => match keyword {
                Keyword::Exit => {
                    self.advance();
                    let prev = self.try_consume(&Token::OpenParen)?;
                    let expression = self.parse_expression().ok_or(self.error_at(
                        self.peek().unwrap_or(prev),
                        "expected:expression",
                        Some("Expression expected"),
                        None,
                    ))?;
                    let (close_paren, close_paren_pos) = self.try_consume(&Token::CloseParen)?;

                    let builtin_exit = Builtin::Exit(
                        expression,
                        (*pos..(close_paren_pos + close_paren.len())).into(),
                    );
                    Ok(Some(builtin_exit))
                }
                Keyword::Print => {
                    self.advance();

                    let prev = self.try_consume(&Token::OpenParen)?;
                    let expression = self.parse_expression().ok_or(self.error_at(
                        self.peek().unwrap_or(prev),
                        "expected:expression",
                        Some("Expression expected"),
                        None,
                    ))?;

                    let (close_paren, close_paren_pos) = self.try_consume(&Token::CloseParen)?;
                    let builtin_print = Builtin::Print(
                        expression,
                        (*pos..(close_paren_pos + close_paren.len())).into(),
                    );
                    Ok(Some(builtin_print))
                }
            },
            _ => Ok(None),
        }
    }

    fn parse_expression(&self) -> Option<Expression> {
        let term = self.parse_term()?;
        Some(Expression::Term(term.clone(), term.into()))
    }

    fn parse_term(&self) -> Option<Term> {
        let (token, pos) = self.peek()?;
        match token {
            Token::IntLiteral(value) => {
                self.advance();
                Some(Term::IntLiteral(value.clone(), token.span(*pos)))
            }
            Token::StringLiteral(value) => {
                self.advance();
                Some(Term::StringLiteral(value.clone(), token.span(*pos)))
            }
            _ => None,
        }
    }

    fn try_consume(&self, token: &Token) -> Result<&(Token, usize)> {
        match self.consume() {
            Some(tuple) if &tuple.0 == token => Ok(tuple),
            Some(t) => bail!(self.error_at(
                t,
                "unexpected:token".into(),
                Some(format!("Expected token '{}', got '{}'", token, t.0)),
                None
            )),
            None => bail!(self.error(
                self.pos(),
                0,
                "unexpected:eof".into(),
                Some(format!("Expected token '{}', got 'EOF'", token)),
                None
            )),
        }
    }

    fn consume(&self) -> Option<&(Token, usize)> {
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

    fn peek(&self) -> Option<&(Token, usize)> {
        self.peek_ahead(0)
    }

    fn peek_ahead(&self, offset: usize) -> Option<&(Token, usize)> {
        self.tokens.get(self.pos() + offset)
    }

    fn pos(&self) -> usize {
        *self.cursor.borrow()
    }

    fn error_at<S: Into<String>>(
        &self,
        token: &(Token, usize),
        code: S,
        label: Option<S>,
        help: Option<S>,
    ) -> Report {
        let (token, pos) = token;
        self.error(*pos, token.to_string().len(), code, label, help)
    }

    fn error<S: Into<String>>(
        &self,
        pos: usize,
        len: usize,
        code: S,
        label: Option<S>,
        help: Option<S>,
    ) -> Report {
        match help {
            Some(help) => miette!(
                // severity = Severity::Error,
                code = code,
                help = help.into(),
                labels = vec![LabeledSpan::new(label.map(Into::into), pos, len)],
                "Error parsing source code."
            ),
            None => miette!(
                // severity = Severity::Error,
                code = code,
                labels = vec![LabeledSpan::new(label.map(Into::into), pos, len)],
                "Error parsing source code."
            ),
        }
        .with_source_code(self.source.clone())
    }
}
