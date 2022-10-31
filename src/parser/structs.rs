use crate::{lexer::token::TokenKind, parser::error::ParseError};

use super::{expr::Expr, stmt::Stmt, ParseResult, Parser, Variable};

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub identifier: String,
    pub fields: Vec<Variable>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructExpr {
    pub identifier: String,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    pub identifier: String,
    pub rhs: Expr,
}

impl Parser {
    pub fn parse_struct(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing struct");
        self.advance()?;
        let identifier = self.consume_identifier()?;

        self.consume(TokenKind::LeftBrace, ParseError::MissingLeftBrace())?;

        let mut fields = vec![];
        while self.current().kind != TokenKind::RightBrace {
            fields.push(self.parse_field_definition()?);
        }

        self.advance()?; // skip }

        Ok(Stmt::Struct(Struct { identifier, fields }))
    }

    fn parse_field_definition(&mut self) -> ParseResult<Variable> {
        let identifier = self.consume_identifier()?;
        self.consume(TokenKind::Colon, ParseError::ExpectedColon(identifier.clone()))?;
        let type_id = self.consume_identifier()?;
        let type_ = self.parse_type(type_id);
        Ok(Variable { identifier, type_ })
    }

    pub fn parse_struct_expr(&mut self) -> ParseResult<Expr> {
        let identifier = self.consume_identifier()?;

        self.consume(TokenKind::LeftBrace, ParseError::MissingLeftBrace())?;

        let mut fields = vec![];
        while self.current().kind != TokenKind::RightBrace {
            fields.push(self.parse_field_expr()?);
        }

        self.advance()?; // skip }

        Ok(Expr::Struct(StructExpr { identifier, fields }))
    }

    fn parse_field_expr(&mut self) -> ParseResult<StructField> {
        let identifier = self.consume_identifier()?;
        self.consume(TokenKind::Colon, ParseError::ExpectedColon(identifier.clone()))?;
        let rhs = self.parse_expr()?;
        self.consume(TokenKind::Comma, ParseError::ExpectedComma(identifier.clone()))?;
        Ok(StructField { identifier, rhs })
    }
}
