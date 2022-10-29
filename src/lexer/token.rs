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
    Eof,
    Fn,
    Semicolon,
    Colon,
    Comma,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    F64(f64),
    I64(i64),
    String(String),
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
    While,
    And,
    Or,
    Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub loc: Location,
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
