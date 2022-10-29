use crate::{lexer::token::TokenKind, parser::error::ParseError};

use super::{stmt::Stmt, ParseResult, Parser, Variable};

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub identifier: String,
    pub fields: Vec<Variable>,
}

impl Parser {
    pub fn parse_struct(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing struct");
        self.advance()?;
        let identifier = self.consume_identifier()?;

        self.consume(TokenKind::LeftBrace, ParseError::MissingLeftBrace())?;

        let mut fields = vec![];
        while self.current().kind != TokenKind::RightBrace {
            fields.push(self.parse_field()?);
        }

        self.advance()?; // skip }

        return Ok(Stmt::Struc(Struct {
            identifier,
            fields,
        }));
    }

    fn parse_field(&mut self) -> ParseResult<Variable> {
        let name = self.consume_identifier()?;
        self.consume(TokenKind::Colon, ParseError::ExpectedColon(name.clone()))?;
        let type_id = self.consume_identifier()?;
        let type_ = self.parse_type(type_id);
        Ok(Variable { name, type_ })
    }

    fn consume_identifier(&mut self) -> ParseResult<String> {
        match self.current().kind.clone() {
            TokenKind::Identifier(id) => {
                self.advance()?;
                Ok(id)
            }
            _ => Err(ParseError::ExpectedIdentifier()),
        }
    }
}
