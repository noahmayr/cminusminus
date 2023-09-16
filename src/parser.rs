use crate::lexer::{Keyword, Token};
use anyhow::{anyhow, bail, Result};

#[derive(PartialEq, Eq, Debug)]
pub enum Term {
    IntLiteral(String),
    StringLiteral(String),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
    Term(Term),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Builtin {
    Exit(Expression),
    Print(Expression),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
    Builtin(Builtin),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Program(pub Vec<Statement>);

#[derive(PartialEq, Eq, Debug)]
pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut statements: Vec<Statement> = vec![];

        while let Some(statement) = self.parse_statement()? {
            statements.push(statement)
        }
        self.cursor = 0;
        Ok(Program(statements))
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>> {
        if let Some(builtin) = self.parse_builtin()? {
            self.try_consume(&Token::Semicolon)?;
            Ok(Some(Statement::Builtin(builtin)))
        } else if let Some(token) = self.peek() {
            bail!("Unexpected token {:?}", token)
        } else {
            Ok(None)
        }
    }

    fn parse_builtin(&mut self) -> Result<Option<Builtin>> {
        match self.peek() {
            Some(Token::Keyword(keyword)) => match keyword {
                Keyword::Exit => {
                    self.advance();
                    self.try_consume(&Token::OpenParen)?;
                    let expression = self
                        .parse_expression()
                        .ok_or(anyhow!("Expression expected"))?;
                    let builtin_exit = Builtin::Exit(expression);
                    self.try_consume(&Token::CloseParen)?;
                    Ok(Some(builtin_exit))
                }
                Keyword::Print => {
                    self.advance();

                    self.try_consume(&Token::OpenParen)?;

                    let expression = self
                        .parse_expression()
                        .ok_or(anyhow!("Expression expected"))?;

                    let builtin_print = Builtin::Print(expression);

                    self.try_consume(&Token::CloseParen)?;
                    Ok(Some(builtin_print))
                }
            },
            _ => Ok(None),
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        Some(Expression::Term(self.parse_term()?))
    }

    fn parse_term(&mut self) -> Option<Term> {
        match self.peek() {
            Some(Token::IntLiteral(value)) => {
                let term = Term::IntLiteral(value.clone());
                self.advance();
                Some(term)
            }
            Some(Token::StringLiteral(value)) => {
                let term = Term::StringLiteral(value.clone());
                self.advance();
                Some(term)
            }
            _ => None,
        }
    }

    fn try_consume(&mut self, token: &Token) -> Result<&Token> {
        match self.consume() {
            Some(t) if t == token => Ok(t),
            _ => bail!("Expected token '{}'", token),
        }
    }

    fn consume(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.cursor);
        self.cursor += 1;
        token
    }

    // fn peek_by(&self, amount: usize) -> Option<&[Token]> {
    //     let res = &self.tokens[self.cursor..(self.cursor + amount)];
    //     if res.len() > 0 {
    //         return Some(res);
    //     }
    //     None
    // }

    fn advance(&mut self) {
        self.advance_by(1);
    }

    fn advance_by(&mut self, amount: usize) {
        self.cursor += amount;
    }

    fn peek(&self) -> Option<&Token> {
        self.peek_ahead(0)
    }

    fn peek_ahead(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + offset)
    }
}
