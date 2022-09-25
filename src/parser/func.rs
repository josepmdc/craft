use crate::{lex::TokenKind, parser::error::ParseError};

use super::{expr::Expr, stmt::Stmt, ParseResult, Parser};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Expr>,
    pub is_anon: bool,
}

impl Parser {
    pub fn parse_fn(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing fn");
        self.advance()?;

        let prototype = self.parse_prototype()?;

        match self.current().kind {
            TokenKind::LeftBrace => self.advance()?,
            _ => return Err(ParseError::MissingLeftBrace()),
        };

        if self.current().kind == TokenKind::RightBrace {
            return Ok(Stmt::Function(Function {
                prototype,
                body: vec![],
                is_anon: false,
            }));
        }

        let mut body = vec![];
        loop {
            body.push(self.parse_expr()?);

            if self.is_at_end() {
                return Err(ParseError::UnexpectedEndOfSource());
            }

            if self.current().kind == TokenKind::RightBrace {
                self.advance()?;
                break;
            }
        }

        let func = Stmt::Function(Function {
            prototype,
            body,
            is_anon: false,
        });

        trace!("Parsed fn: {:#?}", func);

        Ok(func)
    }

    fn parse_prototype(&mut self) -> ParseResult<Prototype> {
        trace!("Parsing prototype");
        let name = match &self.current().kind {
            TokenKind::Identifier(identifier) => identifier.clone(),
            _ => return Err(ParseError::PrototypeMissingIdentifier()),
        };

        self.advance()?;

        match self.current().kind {
            TokenKind::LeftParen => self.advance()?,
            _ => return Err(ParseError::MissingLeftParen()),
        }

        if self.current().kind == TokenKind::RightParen {
            self.advance()?;
            return Ok(Prototype { name, args: vec![] });
        }

        let args = self.parse_prototype_args()?;

        let proto = Prototype { name, args };

        trace!("Parsed prototype: {:#?}", proto);

        Ok(proto)
    }

    fn parse_prototype_args(&mut self) -> ParseResult<Vec<String>> {
        let mut args = vec![];

        loop {
            match &self.current().kind {
                TokenKind::Identifier(id) => {
                    args.push(id.clone());
                    self.advance()?;
                }
                _ => return Err(ParseError::PrototypeMissingRightParenOrComma()),
            }

            match self.current().kind {
                TokenKind::RightParen => {
                    self.advance()?;
                    break;
                }
                TokenKind::Comma => {
                    self.advance()?;
                }
                _ => {
                    return Err(ParseError::PrototypeMissingRightParenOrComma());
                }
            };
        }

        Ok(args)
    }

    pub fn parse_fn_call(&mut self, name: String) -> ParseResult<Expr> {
        trace!("Parsing fn call");
        self.advance()?; // Skip opening '('

        if let TokenKind::RightParen = self.current().kind {
            return Ok(Expr::FnCall {
                fn_name: name,
                args: vec![],
            });
        }

        let mut args = vec![];

        loop {
            args.push(self.parse_expr()?);

            match self.current().kind {
                TokenKind::Comma => self.advance()?,
                TokenKind::RightParen => {
                    self.advance()?;
                    break;
                }
                _ => {
                    debug!("Expected comma or RightParen, found: {:#?}", self.current());
                    return Err(ParseError::MissingCommaOrRightParen());
                }
            }
        }

        self.consume(TokenKind::Semicolon, ParseError::MissingSemicolon())?;

        let expr = Expr::FnCall {
            fn_name: name,
            args,
        };

        trace!("Parsed fn call: {:#?}", expr);

        Ok(expr)
    }
}
