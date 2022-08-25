use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Expected ')' after expression")]
    MissingRightParen(),
    #[error("Expected '(' after expression")]
    MissingLeftParen(),
    #[error("Expected '{{' after expression")]
    MissingLeftBrace(),
    #[error("Expected ';' after expression")]
    MissingSemicolon(),
    #[error("Expected ')' or ',' after argument declaration")]
    PrototypeMissingRightParenOrComma(),
    #[error("Unexpected token '{0}'")]
    UnexpectedToken(String),
    #[error("Unexpected end of source")]
    UnexpectedEndOfSource(),
    #[error("Expected identifier in prototype declaration")]
    PrototypeMissingIdentifier(),
}
