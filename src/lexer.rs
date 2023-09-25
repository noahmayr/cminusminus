use arcstr::{ArcStr, Substr};
use miette::{MietteDiagnostic, Result};
use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::context::*;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    IntLiteral(ArcStr),
    StringLiteral(ArcStr),
    Ident(ArcStr),
    Let,
    OpenParen,
    CloseParen,
    Semicolon,
    Equals,
    OpenCurly,
    CloseCurly,
    Plus,
    Minus,
    Star,
    FSlash,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Let => "let".to_string(),
                Self::IntLiteral(value) => format!("int '{}'", value),
                Self::StringLiteral(value) => format!("string '{}'", value),
                Self::Ident(name) => format!("ident '{}'", name),
                Self::OpenParen => "(".to_string(),
                Self::CloseParen => ")".to_string(),
                Self::Semicolon => ";".to_string(),
                Self::Equals => "=".to_string(),
                Self::OpenCurly => "{".to_string(),
                Self::CloseCurly => "}".to_string(),
                Self::Plus => "+".to_string(),
                Self::Minus => "-".to_string(),
                Self::Star => "*".to_string(),
                Self::FSlash => "/".to_string(),
            },
        )
    }
}

pub type SrcToken = Src<Token>;

pub struct Lexer {
    context: Rc<Context>,
    cursor: RefCell<usize>,
}

impl Lexer {
    pub fn new(context: Rc<Context>) -> Self {
        Self {
            context,
            cursor: RefCell::new(0),
        }
    }

    pub fn tokenize(context: Rc<Context>) -> Result<Vec<SrcToken>> {
        Lexer::new(context).run()
    }

    pub fn run(&self) -> Result<Vec<SrcToken>> {
        let mut tokens: Vec<SrcToken> = vec![];
        while let Some((ch, pos)) = self.peek() {
            if let Some(buf) = self
                .consume_slice_after(char::is_alphabetic, |ch| ch.is_alphanumeric() || ch == '_')
            {
                let token = match buf.as_str() {
                    "let" => Token::Let,
                    ident => Token::Ident(ident.into()),
                };
                tokens.push(Src::new(token, (pos, buf.len())));
            } else if let Some(value) = self.consume_slice(|ch| ch.is_ascii_digit()) {
                tokens.push(Src::new(
                    Token::IntLiteral(value.as_str().into()),
                    (pos, value.len()),
                ));
            } else if self.consume_slice(char::is_whitespace).is_some() {
                continue;
            } else if ch == '"' {
                let mut buf = String::new();
                self.advance();
                loop {
                    let Some((ch, _)) = self.peek() else {
                        self.context.error(Error::UnexpectedEofInString {
                            start: pos,
                            eof: self.context.src().len(),
                        });
                        break;
                    };
                    match ch {
                        '"' => {
                            // buf.push(ch);
                            self.advance();
                            break;
                        }
                        '\\' => {
                            if let Some((escaped, _)) = self.peek_ahead(1) {
                                if let Some(unescaped) = match escaped {
                                    'n' => Some('\n'),
                                    't' => Some('\t'),
                                    _ => None,
                                } {
                                    buf.push(unescaped);
                                    self.advance_by(2);
                                    continue;
                                }
                            }
                            self.advance();
                            buf.push(ch);
                            if let Some(('"', _)) = self.peek() {
                                buf.push('"');
                                self.advance();
                            };
                        }
                        _ => {
                            self.advance();
                            buf.push(ch);
                        }
                    }
                }
                let span = pos..self.pos();
                tokens.push(Src::new(Token::StringLiteral(buf.into()), span))
            } else if self.peek_match("//") {
                self.advance_by(2);
                while self.peek().map_or(false, |(ch, _)| ch != '\n') {
                    self.advance();
                }
            } else if let Some(token) = match ch {
                '(' => Some(Token::OpenParen),
                ')' => Some(Token::CloseParen),
                ';' => Some(Token::Semicolon),
                '=' => Some(Token::Equals),
                '{' => Some(Token::OpenCurly),
                '}' => Some(Token::CloseCurly),
                '+' => Some(Token::Plus),
                '-' => Some(Token::Minus),
                '*' => Some(Token::Star),
                '/' => Some(Token::FSlash),
                symbol => {
                    self.context.error(Error::UnexpectedSymbol { pos, symbol });
                    self.advance();
                    continue;
                }
            } {
                tokens.push(Src::new(token, (pos, 1)));
                self.advance();
            }
        }

        *self.cursor.borrow_mut() = 0;
        Ok(tokens)
    }

    fn consume_slice_after<S, R>(&self, predicate_start: S, predicate_rest: R) -> Option<Substr>
    where
        S: Fn(char) -> bool,
        R: Fn(char) -> bool,
    {
        let mut offset = 1;
        let Some((ch, pos)) = self.peek() else {
            return None;
        };
        if !predicate_start(ch) {
            return None;
        }
        while self
            .peek_ahead(offset)
            .map_or(false, |(ch, _)| predicate_rest(ch))
        {
            offset += 1;
        }
        self.advance_by(offset);

        let result = self.context.src().substr(pos..(pos + offset));

        if !result.is_empty() {
            return Some(result);
        }
        None
    }

    fn consume_slice<P>(&self, predicate: P) -> Option<Substr>
    where
        P: Fn(char) -> bool,
    {
        self.consume_slice_after(&predicate, &predicate)
    }

    fn advance(&self) {
        self.advance_by(1);
    }

    fn advance_by(&self, amount: usize) {
        *self.cursor.borrow_mut() += amount;
    }

    fn peek(&self) -> Option<(char, usize)> {
        self.peek_ahead(0)
    }

    fn peek_match(&self, expected: &str) -> bool {
        let substr = self
            .context
            .src()
            .substr(self.pos()..(self.pos() + expected.len()));
        substr.as_str() == expected
    }

    fn peek_ahead(&self, offset: usize) -> Option<(char, usize)> {
        let pos = self.pos() + offset;
        self.context.src().chars().nth(pos).map(|char| (char, pos))
    }

    fn pos(&self) -> usize {
        *self.cursor.borrow()
    }
}

#[derive(Debug)]
enum Error {
    UnexpectedEofInString { start: usize, eof: usize },
    UnexpectedSymbol { pos: usize, symbol: char },
}

impl From<Error> for MietteDiagnostic {
    fn from(value: Error) -> Self {
        match value {
            Error::UnexpectedEofInString { start, eof } => {
                MietteDiagnostic::new("Unexpected EOF in string")
                    .with_code("cmm::lexer::unexpected_eof_in_string")
                    .add_label("start of string", start)
                    .add_label("end of file", eof - 1)
            }

            Error::UnexpectedSymbol { pos, symbol } => {
                MietteDiagnostic::new(format!("Unexpected symbol: {}", symbol))
                    .with_code("cmm::lexer::unexpected_symbol")
                    .add_label(format!("Unexpected symbol: {}", symbol), pos)
            }
        }
    }
}
