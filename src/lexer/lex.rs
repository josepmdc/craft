use crate::error;

use super::token::{Location, Token, TokenKind};

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current_idx: usize,
    line: i32,
    col: i32,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current_idx: 0,
            line: 1,
            col: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current_idx;
            self.scan_token();
        }
        self.tokens.push(Token::new(
            TokenKind::Eof,
            "",
            Location::new(self.col, self.line),
        ));
        &self.tokens
    }

    fn scan_token(&mut self) {
        match self.advance() {
            ';' => self.add_token(TokenKind::Semicolon),
            ':' => self.add_token(TokenKind::Colon),
            '.' => self.add_token(TokenKind::Dot),
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            '[' => self.add_token(TokenKind::LeftBracket),
            ']' => self.add_token(TokenKind::RightBracket),
            ',' => self.add_token(TokenKind::Comma),
            '+' => self.add_token(TokenKind::Plus),
            '-' => self.add_token(TokenKind::Minus),
            '*' => self.add_token(TokenKind::Star),
            '/' => self.add_token(TokenKind::Slash),
            '=' => match self.current() {
                '=' => {
                    self.advance();
                    self.add_token(TokenKind::EqualEqual);
                }
                _ => self.add_token(TokenKind::Equal),
            },
            '<' => match self.current() {
                '=' => {
                    self.advance();
                    self.add_token(TokenKind::LessEqual)
                }
                _ => self.add_token(TokenKind::Less),
            },
            '>' => match self.current() {
                '=' => {
                    self.advance();
                    self.add_token(TokenKind::GreaterEqual)
                }
                _ => self.add_token(TokenKind::Greater),
            },
            '!' => match self.current() {
                '=' => {
                    self.advance();
                    self.add_token(TokenKind::BangEqual)
                }
                _ => self.add_token(TokenKind::Bang),
            },
            '&' => match self.current() {
                '&' => {
                    self.advance();
                    self.add_token(TokenKind::And)
                }
                c => error::report(self.line, self.col, format!("Unexpected character {c}")),
            },
            '|' => match self.current() {
                '|' => {
                    self.advance();
                    self.add_token(TokenKind::Or)
                }
                c => error::report(self.line, self.col, format!("Unexpected character {c}")),
            },
            '"' => self.add_string(),
            ' ' | '\r' | '\t' => (),
            '\n' => self.add_new_line(),
            c => {
                if c.is_ascii_digit() {
                    self.add_number();
                } else if c.is_alphabetic() {
                    self.add_identifier();
                } else {
                    error::report(self.line, self.col, format!("Unexpected character {c}"))
                }
            }
        };
    }

    fn match_keyword(word: &str) -> Option<TokenKind> {
        match word {
            "fn" => Some(TokenKind::Fn),
            "let" => Some(TokenKind::VarDeclaration),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "while" => Some(TokenKind::While),
            "printf" => Some(TokenKind::Printf),
            "struct" => Some(TokenKind::Struct),
            "ret" => Some(TokenKind::Return),
            _ => None,
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        let value = self.source.get(self.start..self.current_idx).unwrap();
        self.tokens.push(Token::new(
            kind,
            value,
            Location {
                col: self.col - value.len() as i32,
                line: self.line,
            },
        ))
    }

    fn add_number(&mut self) {
        while self.current().is_ascii_digit() {
            self.advance();
        }

        let is_float = self.current() == '.' && self.peek().is_ascii_digit();

        if is_float {
            self.advance();
            while self.current().is_ascii_digit() {
                self.advance();
            }
        }

        let literal = self
            .source
            .get(self.start..self.current_idx)
            .expect("[Number] Could not get substring");

        self.add_token(if is_float {
            TokenKind::F64(literal.parse().expect("[Number] Could not parse number"))
        } else {
            TokenKind::I64(literal.parse().expect("[Number] Could not parse number"))
        });
    }

    fn add_string(&mut self) {
        while self.current() != '"' && !self.is_at_end() {
            if self.current() == '\n' {
                self.add_new_line();
            }
            self.advance();
        }

        if self.is_at_end() {
            error::report(self.line, self.col, "Unterminated string".to_string());
            return;
        }

        self.advance();

        let literal = self
            .source
            .get((self.start + 1)..(self.current_idx - 1))
            .expect("[String] Could not get substring")
            .to_string();

        let literal = literal.replace("\\n", "\n");

        self.add_token(TokenKind::String(literal));
    }

    fn add_identifier(&mut self) {
        while self.current().is_alphanumeric() || self.current() == '_' {
            self.advance();
        }

        let id = self
            .source
            .get(self.start..self.current_idx)
            .expect("Could not get identifier");

        self.add_token(
            Scanner::match_keyword(id).unwrap_or_else(|| TokenKind::Identifier(id.to_string())),
        );
    }

    fn add_new_line(&mut self) {
        self.line += 1;
        self.col = 0;
    }

    fn advance(&mut self) -> char {
        trace!(
            "[LEXER] Advanced from \"{}\" to \"{}\"",
            self.current(),
            self.peek(),
        );
        self.col += 1;
        self.current_idx += 1;
        self.source
            .chars()
            .nth(self.current_idx - 1)
            .expect("Unexpected end of source")
    }

    pub fn current(&self) -> char {
        self.source.chars().nth(self.current_idx).unwrap_or('\0')
    }

    fn peek(&self) -> char {
        self.source
            .chars()
            .nth(self.current_idx + 1)
            .unwrap_or('\0')
    }

    fn is_at_end(&self) -> bool {
        self.current_idx >= self.source.len()
    }
}
