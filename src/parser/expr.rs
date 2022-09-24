use crate::{
    lex::{Token, TokenKind},
    parser::error::ParseError,
};

use super::{
    parser::*,
    stmt::{Function, Prototype, Stmt},
};

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
    Literal { value: LiteralValue },
    Variable(Token),
    FnCall { fn_name: String, args: Vec<Expr> },
}

impl Parser {
    pub fn parse_toplevel_expr(&mut self) -> ParseResult<Stmt> {
        match self.parse_expr() {
            Ok(expr) => Ok(Stmt::Function(Function {
                prototype: Prototype {
                    name: ANONYMOUS_FUNCTION_NAME.to_string(),
                    args: vec![],
                },
                body: vec![expr],
                is_anon: true,
            })),
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
                },
            },
            _ => {
                let token = self.current();
                return Err(
                    self.report_error(token, ParseError::UnexpectedToken(token.lexeme.clone()))
                );
            }
        };

        trace!("Parsed primary: {:#?}", expr);
        Ok(expr)
    }

    fn parse_grouping(&mut self) -> ParseResult<Expr> {
        trace!("Parsing grouping");
        self.advance()?;
        let expr = self.parse_expr()?;
        self.consume(TokenKind::RightParen, ParseError::MissingRightParen())?;
        trace!("Parsed grouping: {:#?}", expr);
        Ok(expr)
    }
}
