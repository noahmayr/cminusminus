use std::fmt::Display;

#[derive(PartialEq, Eq, Debug)]
pub enum Keyword {
    Exit,
    Print,
}

impl TryFrom<&str> for Keyword {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "exit" => Ok(Self::Exit),
            "print" => Ok(Self::Print),
            str => Err(format!("Unknown keyword {}", str)),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    Keyword(Keyword),
    IntLiteral(String),
    StringLiteral(String),
    OpenParen,
    CloseParen,
    Semicolon,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Keyword(keyword) => match keyword {
                    Keyword::Exit => "exit",
                    Keyword::Print => "print",
                },
                Token::IntLiteral(value) => value,
                Token::StringLiteral(value) => value,
                Token::OpenParen => "(",
                Token::CloseParen => ")",
                Token::Semicolon => ";",
            },
        )
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, cursor: 0 }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        while let Some(ch) = self.peek() {
            if ch.is_alphabetic() {
                let buf = self.consume_slice(char::is_alphanumeric);

                let keyword = buf.map(Keyword::try_from);

                let token = match keyword {
                    Some(Ok(keyword)) => Token::Keyword(keyword),
                    Some(Err(err)) => panic!("{}", err),
                    None => panic!("Failed to parse keyword after peeking alphabetic char"),
                };
                tokens.push(token);
            } else if let Some(value) = self.consume_slice(|ch| ch.is_digit(10)) {
                tokens.push(Token::IntLiteral(value.to_string()));
            } else if self.consume_slice(char::is_whitespace).is_some() {
                continue;
            } else if ch == '"' {
                let mut buf = "\"".to_string();
                self.advance();
                while let Some(ch) = self.peek() {
                    match ch {
                        '"' => {
                            buf.push(ch);
                            self.advance();
                            break;
                        }
                        '\\' => {
                            self.advance();
                            let next = self.peek();

                            if next == None {
                                panic!("Unterminated string during escape");
                            } else if next == Some('"') {
                                buf.push_str("\\\"");
                                self.advance();
                            } else {
                                buf.push_str("\\");
                            }
                        }
                        _ => {
                            self.advance();
                            buf.push(ch);
                        }
                    }
                }
                tokens.push(Token::StringLiteral(buf))
            } else {
                tokens.push(match ch {
                    '(' => Token::OpenParen,
                    ')' => Token::CloseParen,
                    ';' => Token::Semicolon,
                    unsupported => panic!("Unsupported symbol '{}'", unsupported),
                });
                self.advance()
            }
        }
        self.cursor = 0;
        return tokens;
    }

    fn consume_slice<P>(&mut self, predicate: P) -> Option<&str>
    where
        P: Fn(char) -> bool,
    {
        let mut offset = 0;
        while self.peek_ahead(offset).map_or(false, |ch| predicate(ch)) {
            offset += 1;
        }
        let result = &self.source[self.cursor..(self.cursor + offset)];
        if result.len() > 0 {
            self.advance_by(offset);
            return Some(result);
        }
        None
    }

    // fn consume(&mut self) -> Option<char> {
    //     let res = self.source.chars().nth(self.cursor);
    //     if res.is_some() {
    //         self.cursor += 1;
    //     }
    //     return res;
    // }

    fn advance(&mut self) {
        self.advance_by(1);
    }

    fn advance_by(&mut self, amount: usize) {
        self.cursor += amount;
    }

    fn peek(&self) -> Option<char> {
        self.peek_ahead(0)
    }

    fn peek_ahead(&self, offset: usize) -> Option<char> {
        self.source.chars().nth(self.cursor + offset)
    }
}
