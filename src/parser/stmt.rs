use crate::lex::{Token, TokenKind};

use super::{error::ParseError, expr::Expr, ParseResult, Parser};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Function(Function),
    Var { token: Token, initializer: Expr },
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Stmt>,
    pub is_anon: bool,
}

impl Parser {
    pub fn parse_var_declaration(&mut self) -> ParseResult<Stmt> {
        self.advance()?;
        let stmt = match &self.current().kind {
            TokenKind::Identifier(_) => {
                let identifier = self.advance()?.clone();
                self.consume(TokenKind::Equal, ParseError::MissingLeftParen())?;
                Stmt::Var {
                    token: identifier,
                    initializer: self.parse_expr()?,
                }
            }
            _ => todo!("Return error"),
        };
        self.consume(TokenKind::Semicolon, ParseError::MissingSemicolon())?;
        Ok(stmt)
    }

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
            body.push(self.parse_declaration()?);

            if self.is_at_end() {
                return Err(ParseError::UnexpectedEndOfSource());
            }

            if self.current().kind == TokenKind::RightBrace {
                self.advance()?;
                break;
            }
        }

        let func = Function {
            prototype,
            body,
            is_anon: false,
        };

        trace!("Parsed fn: {:#?}", func);

        Ok(Stmt::Function(func))
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
        };

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
                TokenKind::Comma => self.advance()?,
                _ => return Err(ParseError::PrototypeMissingRightParenOrComma()),
            };
        }

        trace!("Parsed args: {:#?}", args);

        Ok(args)
    }
}
