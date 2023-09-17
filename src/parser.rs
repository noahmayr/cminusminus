use std::cell::RefCell;

use crate::{context::Src, lexer::*};
use arcstr::ArcStr;
use miette::{bail, miette, LabeledSpan, Report, Result, SourceSpan};

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

#[derive(PartialEq, Eq, Debug)]
pub struct Parser<'a> {
    tokens: &'a Vec<SrcToken>,
    source: ArcStr,
    cursor: RefCell<usize>,
}

impl<'a> Parser<'a> {
    pub fn new<S: Into<ArcStr>>(tokens: &'a Vec<SrcToken>, source: S) -> Self {
        Self {
            tokens,
            source: source.into(),
            cursor: RefCell::new(0),
        }
    }

    pub fn parse<S: Into<ArcStr>>(tokens: &'a Vec<SrcToken>, source: S) -> Result<Program> {
        Self::new(tokens, source).run()
    }

    pub fn run(&self) -> Result<Program> {
        let mut statements: Vec<SrcStmt> = vec![];

        while let Some(statement) = self.parse_statement()? {
            statements.push(statement)
        }
        *self.cursor.borrow_mut() = 0;
        Ok(Program(statements))
    }

    fn parse_statement(&self) -> Result<Option<SrcStmt>> {
        if let Some(builtin) = self.parse_builtin()? {
            let semi = self.try_consume(&Token::Semicolon)?;
            let span = builtin.to(semi);
            Ok(Some(Src::new(NodeStmt::Builtin(builtin), span)))
        } else if let Some(token) = self.peek() {
            bail!(self.error_at(
                token,
                "unexpected:token".into(),
                Some(format!("Unexpected token ({})", **token)),
                None,
            ))
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
                    let prev = self.try_consume(&Token::OpenParen)?;
                    let expression = self.parse_expression().ok_or(self.error_at(
                        self.peek().unwrap_or(prev),
                        "expected:expression",
                        Some("Expression expected"),
                        None,
                    ))?;
                    let builtin_exit = NodeBuiltin::Exit(expression);

                    let close_paren = self.try_consume(&Token::CloseParen)?;

                    Ok(Some(Src::new(builtin_exit, token.to(close_paren))))
                }
                "print" => {
                    self.advance();

                    let prev = self.try_consume(&Token::OpenParen)?;
                    let expression = self.parse_expression().ok_or(self.error_at(
                        self.peek().unwrap_or(prev),
                        "expected:expression",
                        Some("Expression expected"),
                        None,
                    ))?;

                    let close_paren = self.try_consume(&Token::CloseParen)?;
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

    fn try_consume(&self, ttype: &Token) -> Result<&SrcToken> {
        match self.consume() {
            Some(token) if &**token == ttype => Ok(token),
            Some(t) => bail!(self.error_at(
                t,
                "unexpected:token".into(),
                Some(format!("Expected token ({}), got ({})", ttype, **t)),
                None
            )),
            None => bail!(self.error(
                (self.source.len(), 0),
                "unexpected:eof".into(),
                Some(format!("Expected token ({}), got EOF", ttype)),
                None
            )),
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

    fn error_at<S: Into<String>>(
        &self,
        token: &Src<Token>,
        code: S,
        label: Option<S>,
        help: Option<S>,
    ) -> Report {
        self.error(token, code, label, help)
    }

    fn error<S: Into<String>, SP: Into<SourceSpan>>(
        &self,
        span: SP,
        code: S,
        label: Option<S>,
        help: Option<S>,
    ) -> Report {
        match help {
            Some(help) => miette!(
                // severity = Severity::Error,
                code = code,
                help = help.into(),
                labels = vec![LabeledSpan::new_with_span(
                    label.map(Into::into),
                    span.into()
                )],
                "Error parsing source code."
            ),
            None => miette!(
                // severity = Severity::Error,
                code = code,
                labels = vec![LabeledSpan::new_with_span(
                    label.map(Into::into),
                    span.into()
                )],
                "Error parsing source code."
            ),
        }
        .with_source_code(self.source.to_string())
    }
}
