use arcstr::ArcStr;
use miette::{bail, miette, LabeledSpan, Report, Result};
use std::{cell::RefCell, fmt::Display};

use crate::context::Src;

type Str = ArcStr;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    IntLiteral(Str),
    StringLiteral(Str),
    Ident(Str),
    // Let,
    OpenParen,
    CloseParen,
    Semicolon,
    // Equals,
}

// impl Token {
//     pub fn span(&self, pos: usize) -> SourceSpan {
//         (pos, self.to_string().len()).into()
//     }
// }

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // Token::Let => "let",
                Token::IntLiteral(value) => format!("int '{}'", value),
                Token::StringLiteral(value) => format!("string '{}'", value),
                Token::Ident(name) => format!("ident '{}'", name),
                Token::OpenParen => "(".to_string(),
                Token::CloseParen => ")".to_string(),
                Token::Semicolon => ";".to_string(),
                // Token::Equals => "=",
            },
        )
    }
}

pub type SrcToken = Src<Token>;

pub struct Lexer {
    source: Str,
    cursor: RefCell<usize>,
}

impl Lexer {
    pub fn new<S: Into<Str>>(source: S) -> Self {
        Self {
            source: source.into(),
            cursor: RefCell::new(0),
        }
    }

    pub fn tokenize<S: Into<Str>>(source: S) -> Result<Vec<SrcToken>> {
        Lexer::new(source).run()
    }

    pub fn run(&self) -> Result<Vec<SrcToken>> {
        let mut tokens: Vec<SrcToken> = vec![];
        while let Some((ch, pos)) = self.peek() {
            if let Some(buf) = self.consume_slice_after(char::is_alphabetic, char::is_alphanumeric)
            {
                let token = match buf {
                    // "let" => Token::Let,
                    ident => Token::Ident(ident.into()),
                };
                tokens.push(Src::new(token, (pos, buf.len())));
            } else if let Some(value) = self.consume_slice(|ch| ch.is_ascii_digit()) {
                tokens.push(Src::new(
                    Token::IntLiteral(value.into()),
                    (pos, value.len()),
                ));
            } else if self.consume_slice(char::is_whitespace).is_some() {
                continue;
            } else if ch == '"' {
                let mut buf = "\"".to_string();
                self.advance();
                loop {
                    let Some((ch, _)) = self.peek() else {
                        bail!(self.error_at(*self.cursor.borrow(), "unexpected_eof_in_string", Some("Unexpected end of file while parsing string"), None));
                    };
                    match ch {
                        '"' => {
                            buf.push(ch);
                            self.advance();
                            break;
                        }
                        '\\' => {
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
                let span = (pos, buf.len());
                tokens.push(Src::new(Token::StringLiteral(buf.into()), span))
            } else {
                tokens.push(Src::new(
                    match ch {
                        '(' => Token::OpenParen,
                        ')' => Token::CloseParen,
                        ';' => Token::Semicolon,
                        // '=' => Token::Equals,
                        unknown => bail!(self.error_at(
                            pos,
                            "unexpected_symbol".into(),
                            Some(format!("Unexpected symbol '{}'", unknown)),
                            None
                        )),
                    },
                    (pos, 1),
                ));
                self.advance();
            }
        }
        *self.cursor.borrow_mut() = 0;
        Ok(tokens)
    }

    fn error_at<S: Into<String>>(
        &self,
        pos: usize,
        code: S,
        label: Option<S>,
        help: Option<S>,
    ) -> Report {
        self.error(pos, 0, code, label, help)
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
                "Error lexing source code."
            ),
            None => miette!(
                // severity = Severity::Error,
                code = code,
                labels = vec![LabeledSpan::new(label.map(Into::into), pos, len)],
                "Error lexing source code."
            ),
        }
        .with_source_code(self.source.to_string())
    }

    fn consume_slice_after<S, R>(&self, predicate_start: S, predicate_rest: R) -> Option<&str>
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

        let result = &self.source[pos..(pos + offset)];
        if !result.is_empty() {
            return Some(result);
        }
        None
    }

    fn consume_slice<P>(&self, predicate: P) -> Option<&str>
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

    fn peek_ahead(&self, offset: usize) -> Option<(char, usize)> {
        let pos = self.pos() + offset;
        self.source.chars().nth(pos).map(|char| (char, pos))
    }

    fn pos(&self) -> usize {
        *self.cursor.borrow()
    }
}
