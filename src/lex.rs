use std::fmt;

use crate::error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub col: i32,
    pub line: i32,
}

impl Location {
    pub fn new(col: i32, line: i32) -> Self {
        Self { col, line }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    EOF,
    Fn,
    Semicolon,
    Comma,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Number { literal: f64 },
    String { literal: String },
    Plus,
    Minus,
    Star,
    Slash,
    Identifier(String),
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    True,
    False,
    VarDeclaration,
    If,
    Else,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub loc: Location,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TokenKind::String { literal } => write!(
                f,
                "{}:{} => {:?} {:?}",
                self.loc.line, self.loc.col, self.lexeme, literal
            ),
            TokenKind::Number { literal } => write!(
                f,
                "{}:{} => {:?} {:?}",
                self.loc.line, self.loc.col, self.lexeme, literal
            ),
            _ => write!(
                f,
                "{}:{} => {:?} {:?}",
                self.loc.line, self.loc.col, self.kind, self.lexeme
            ),
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, value: &str, loc: Location) -> Self {
        Self {
            kind,
            lexeme: value.to_string(),
            loc,
        }
    }
}

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
            TokenKind::EOF,
            "",
            Location::new(self.col, self.line),
        ));
        &self.tokens
    }

    fn scan_token(&mut self) {
        match self.advance() {
            ';' => self.add_token(TokenKind::Semicolon),
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            ',' => self.add_token(TokenKind::Comma),
            '+' => self.add_token(TokenKind::Plus),
            '-' => self.add_token(TokenKind::Minus),
            '*' => self.add_token(TokenKind::Star),
            '/' => self.add_token(TokenKind::Slash),
            '=' => self.add_token(TokenKind::Equal),
            '<' => self.add_token(TokenKind::Less),
            '>' => self.add_token(TokenKind::Greater),
            '"' => self.add_string(),
            ' ' | '\r' | '\t' => (),
            '\n' => self.add_new_line(),
            c => {
                if c.is_digit(10) {
                    self.add_number();
                } else if c.is_alphabetic() {
                    self.add_identifier();
                } else {
                    error::report(self.line, self.col, format!("Unexpected character {}", c))
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
        while self.current().is_digit(10) {
            self.advance();
        }

        if self.current() == '.' && self.peek().is_digit(10) {
            self.advance();
            while self.current().is_digit(10) {
                self.advance();
            }
        }

        let literal: f64 = self
            .source
            .get(self.start..self.current_idx)
            .expect("[Number] Could not get substring")
            .parse()
            .expect("[Number] Could not parse number");

        self.add_token(TokenKind::Number { literal });
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

        self.add_token(TokenKind::String { literal });
    }

    fn add_identifier(&mut self) {
        while self.current().is_alphanumeric() {
            self.advance();
        }

        let id = self
            .source
            .get(self.start..self.current_idx)
            .expect("Could not get identifier");

        self.add_token(Scanner::match_keyword(id).unwrap_or(TokenKind::Identifier(id.to_string())));
    }

    fn add_new_line(&mut self) {
        self.line += 1;
        self.col = 0;
    }

    fn advance(&mut self) -> char {
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
