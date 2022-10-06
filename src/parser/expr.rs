use crate::{
    lex::{Token, TokenKind},
    parser::error::{self, ParseError},
};

use super::{stmt::Stmt, LiteralValue, ParseResult, Parser};

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal {
        value: LiteralValue,
    },
    Variable(Token),
    FnCall {
        fn_name: String,
        args: Vec<Expr>,
    },
    Conditional {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
}

impl Parser {
    pub fn parse_toplevel_expr(&mut self) -> ParseResult<Stmt> {
        match self.parse_expr() {
            Ok(expr) => Ok(Stmt::Expr(expr)),
            Err(err) => Err(err),
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        trace!("Parsing expr. Starting at {:#?}", self.current());
        let expr = self.parse_equality();
        trace!("Parsed expr: {:#?}", expr);
        expr
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        trace!("Parsing equality");
        let mut expr = self.parse_comparison()?;
        while self.match_any([TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_comparison()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed equality: {:#?}", expr);
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        trace!("Parsing comparison");
        let mut expr = self.parse_term()?;

        while self.match_any([
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_term()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed comparison: {:#?}", expr);
        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        trace!("Parsing term");
        let mut expr = self.parse_factor()?;
        while self.match_any([TokenKind::Minus, TokenKind::Plus]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_term()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed term: {:#?}", expr);
        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        trace!("Parsing factor");
        let mut expr = self.parse_unary()?;
        while self.match_any([TokenKind::Slash, TokenKind::Star]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_unary()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        trace!("Parsed factor: {:#?}", expr);
        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        trace!("Parsing unary");
        if self.match_any([TokenKind::Bang, TokenKind::Minus]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.parse_primary()?;
            let expr = Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
            });
            trace!("Parsed unary: {:#?}", expr);
            return Ok(expr);
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        trace!("Parsing primary");
        let expr = match self.current().to_owned().kind {
            TokenKind::False => {
                self.advance()?;
                Expr::Literal {
                    value: LiteralValue::Boolean(false),
                }
            }
            TokenKind::True => {
                self.advance()?;
                Expr::Literal {
                    value: LiteralValue::Boolean(true),
                }
            }
            TokenKind::String { literal } => {
                self.advance()?;
                Expr::Literal {
                    value: LiteralValue::String(literal.clone()),
                }
            }
            TokenKind::Number { literal } => {
                self.advance()?;
                Expr::Literal {
                    value: LiteralValue::Number(literal.clone()),
                }
            }
            TokenKind::LeftParen => self.parse_grouping()?,
            TokenKind::Identifier(id) => match self.peek().kind {
                TokenKind::LeftParen => {
                    self.advance()?;
                    self.parse_fn_call(id)?
                }
                _ => {
                    let var = self.current().clone();
                    self.advance()?;
                    Expr::Variable(var)
                }
            },
            TokenKind::If => self.parse_if()?,
            _ => {
                let token = self.current();
                return Err(error::report(
                    token,
                    ParseError::UnexpectedToken(token.lexeme.clone()),
                ));
            }
        };

        trace!("Parsed primary: {:#?}", expr);
        Ok(expr)
    }

    pub fn parse_if(&mut self) -> ParseResult<Expr> {
        trace!("Parsing if");
        self.advance()?; // skip 'if' token

        let cond = self.parse_expr()?;
        self.consume(TokenKind::LeftBrace, ParseError::MissingLeftBrace())?;
        let then = self.parse_expr()?;
        self.consume(TokenKind::RightBrace, ParseError::MissingRightBrace())?;

        self.consume(TokenKind::Else, ParseError::MissingElse())?;
        self.consume(TokenKind::LeftBrace, ParseError::MissingLeftBrace())?;
        let else_ = self.parse_expr()?;
        self.consume(TokenKind::RightBrace, ParseError::MissingRightBrace())?;

        let conditional = Expr::Conditional {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(else_),
        };
        trace!("Parsed if expr {:#?}", conditional);
        Ok(conditional)
    }

    fn parse_grouping(&mut self) -> ParseResult<Expr> {
        trace!("Parsing grouping");
        self.advance()?;
        let expr = self.parse_expr()?;
        self.consume(TokenKind::RightParen, ParseError::MissingRightParen())?;
        trace!("Parsed grouping: {:#?}", expr);
        Ok(expr)
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
            };
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