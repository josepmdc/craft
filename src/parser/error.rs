use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Expected ')' after expression")]
    MissingRightParen(),
    #[error("Unexpected token {0}")]
    UnexpectedToken(String),
}
