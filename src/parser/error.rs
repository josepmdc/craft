use thiserror::Error;

use crate::{error, lexer::token::Token};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Expected ')' after expression")]
    MissingRightParen(),
    #[error("Expected '(' after expression")]
    MissingLeftParen(),
    #[error("Expected '{{' after expression")]
    MissingLeftBrace(),
    #[error("Expected ';' after statment")]
    MissingSemicolon(),
    #[error("Expected ',' or ')' after expression")]
    MissingCommaOrRightParen(),
    #[error("Expected else")]
    MissingElse(),
    #[error("Expected '=' after identifier")]
    MissingEquals(),
    #[error("Expected colon after identifier {0}")]
    ExpectedColon(String),
    #[error("Expected dot after identifier {0}")]
    ExpectedDot(String),
    #[error("Expected comma after identifier {0}")]
    ExpectedComma(String),
    #[error("Expected '=' after identifier {0}")]
    ExpectedEquals(String),
    #[error("Expected ')' or ',' after argument declaration")]
    PrototypeMissingRightParenOrComma(),
    #[error("Unexpected token '{0}'")]
    UnexpectedToken(String),
    #[error("Expected '{expected}' but found '{found}'")]
    UnexpectedTokenVerbose { expected: String, found: String },
    #[error("Unexpected end of source")]
    UnexpectedEndOfSource(),
    #[error("Expected identifier")]
    ExpectedIdentifier(),
    #[error("Redefined field {0} in struct {1}")]
    RedefinedField(String, String),
    #[error("Expected a string")]
    ExpectedString(),
    #[error("Expected left bracket")]
    ExpectedLeftBracket(),
    #[error("Expected right bracket")]
    ExpectedRightBracket(),
    #[error("Expected an integer")]
    ExpectedInteger(),
    #[error("Expected ! after identifier")]
    ExpectedBang(),
}

pub fn report(token: &Token, error: ParseError) -> ParseError {
    error::report(token.loc.line, token.loc.col, error.to_string());
    error
}
