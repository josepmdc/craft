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
    #[error("Expected ')' or ',' after argument declaration")]
    PrototypeMissingRightParenOrComma(),
    #[error("Unexpected token '{0}'")]
    UnexpectedToken(String),
    #[error("Expected '{expected}' but found '{found}'")]
    UnexpectedTokenVerbose { expected: String, found: String },
    #[error("Unexpected end of source")]
    UnexpectedEndOfSource(),
    #[error("Expected identifier in prototype declaration")]
    PrototypeMissingIdentifier(),
}

pub fn report(token: &Token, error: ParseError) -> ParseError {
    error::report(token.loc.line, token.loc.col, error.to_string());
    error
}
