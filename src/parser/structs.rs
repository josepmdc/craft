use std::collections::HashMap;

use crate::{lexer::token::TokenKind, parser::error::ParseError};

use super::{expr::Expr, stmt::Stmt, ParseResult, Parser, Type, Variable};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Struct {
    pub identifier: String,
    pub fields: HashMap<String, FieldMetadata>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FieldMetadata {
    pub index: u32,
    pub type_: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructExpr {
    pub identifier: String,
    pub fields: HashMap<String, Expr>,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub identifier: String,
    pub rhs: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccess {
    pub variable_id: String,
    pub field: Box<FieldAccessField>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FieldAccessField {
    PrimitiveField(String),
    StructField(Expr),
}

impl Parser {
    pub fn parse_struct(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing struct");
        self.advance()?;
        let identifier = self.consume_identifier()?;

        self.consume(TokenKind::LeftBrace, ParseError::MissingLeftBrace())?;

        let mut fields = HashMap::new();
        let mut index = 0;
        while self.current().kind != TokenKind::RightBrace {
            let field = self.parse_field_definition()?;

            if fields.contains_key(&field.identifier) {
                Err(ParseError::RedefinedField(
                    field.identifier.clone(),
                    identifier.clone(),
                ))?;
            }

            fields.insert(
                field.identifier,
                FieldMetadata {
                    index,
                    type_: field.type_,
                },
            );
            index += 1;
        }

        self.advance()?; // skip }

        Ok(Stmt::Struct(Struct { identifier, fields }))
    }

    fn parse_field_definition(&mut self) -> ParseResult<Variable> {
        trace!("Parsing field definition");
        let identifier = self.consume_identifier()?;
        self.consume(
            TokenKind::Colon,
            ParseError::ExpectedColon(identifier.clone()),
        )?;

        let type_ = match self.current().kind.clone() {
            TokenKind::LeftBracket => self.parse_array_type()?,
            TokenKind::Identifier(id) => {
                self.consume_identifier()?;
                self.parse_type(id)
            }
            _ => return Err(ParseError::UnexpectedToken(self.current().lexeme.clone())),
        };

        Ok(Variable { identifier, type_ })
    }

    pub fn parse_struct_expr(&mut self) -> ParseResult<Expr> {
        trace!("Parsing struct expr");
        let identifier = self.consume_identifier()?;

        self.consume(TokenKind::Bang, ParseError::ExpectedBang())?;
        self.consume(TokenKind::LeftBrace, ParseError::MissingLeftBrace())?;

        let mut fields = HashMap::new();
        while self.current().kind != TokenKind::RightBrace {
            let field_expr = self.parse_field_expr()?;
            if fields.contains_key(&field_expr.identifier) {
                return Err(ParseError::RedefinedField(
                    field_expr.identifier,
                    identifier,
                ));
            }
            fields.insert(field_expr.identifier, field_expr.rhs);
        }

        self.advance()?; // skip }

        Ok(Expr::Struct(StructExpr { identifier, fields }))
    }

    fn parse_field_expr(&mut self) -> ParseResult<StructField> {
        trace!("Parsing field expr");

        let identifier = self.consume_identifier()?;

        self.consume(
            TokenKind::Colon,
            ParseError::ExpectedColon(identifier.clone()),
        )?;

        let rhs = self.parse_expr()?;

        self.consume(
            TokenKind::Comma,
            ParseError::ExpectedComma(identifier.clone()),
        )?;

        let field = StructField { identifier, rhs };

        trace!("Parsed field: {:#?}", field);

        Ok(field)
    }

    pub fn parse_field_access(&mut self) -> ParseResult<Expr> {
        trace!("Parsing access to struct field");

        let struct_id = self.consume_identifier()?;

        self.consume(TokenKind::Dot, ParseError::ExpectedDot(struct_id.clone()))?;

        let field_access = Expr::FieldAccess(FieldAccess {
            variable_id: struct_id,
            field: Box::from(match self.peek().kind {
                TokenKind::Dot => FieldAccessField::StructField(self.parse_field_access()?),
                _ => FieldAccessField::PrimitiveField(self.consume_identifier()?),
            }),
        });

        trace!("Parsed access to struct field: {:#?}", field_access);
        Ok(field_access)
    }
}
