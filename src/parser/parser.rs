use crate::{
    error,
    lex::{Token, TokenType},
};

use super::error::ParseError;

type ParseResult<T> = Result<T, ParseError>;

const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Boolean(bool),
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal { value: LiteralValue },
    Grouping { expression: Box<Expr> },
}

#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
    pub is_op: bool,
    pub prec: usize,
}

#[derive(Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Option<Expr>,
    pub is_anon: bool,
}

pub struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
}

impl Parser {
    /*
        expression     → equality ;
        equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        term           → factor ( ( "-" | "+" ) factor )* ;
        factor         → unary ( ( "/" | "*" ) unary )* ;
        unary          → ( "!" | "-" ) unary
                       | primary ;
        primary        → NUMBER | STRING | "true" | "false" | "nil"
                       | "(" expression ")" ;
    */

    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current_index: 0,
        }
    }

    pub fn parse(&mut self) -> ParseResult<Function> {
        self.toplevel_expr()
    }

    fn toplevel_expr(&mut self) -> ParseResult<Function> {
        match self.expression() {
            Ok(expr) => Ok(Function {
                prototype: Prototype {
                    name: ANONYMOUS_FUNCTION_NAME.to_string(),
                    args: vec![],
                    is_op: false,
                    prec: 0,
                },
                body: Some(expr),
                is_anon: true,
            }),

            Err(err) => Err(err),
        }
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?;
        while self.match_any([TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.comparison()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.term()?;

        while self.match_any([
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.term()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.factor()?;
        while self.match_any([TokenType::Minus, TokenType::Plus]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.term()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.unary()?;
        while self.match_any([TokenType::Slash, TokenType::Star]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.unary()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        if self.match_any([TokenType::Bang, TokenType::Minus]) {
            let operator = self.current().clone();
            self.advance()?;
            let right = self.primary()?;
            return Ok(Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
            }));
        }
        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        let expr = match &self.current().type_ {
            TokenType::False => Expr::Literal {
                value: LiteralValue::Boolean(false),
            },
            TokenType::True => Expr::Literal {
                value: LiteralValue::Boolean(true),
            },
            TokenType::String { literal } => Expr::Literal {
                value: LiteralValue::String(literal.clone()),
            },
            TokenType::Number { literal } => Expr::Literal {
                value: LiteralValue::Number(literal.clone()),
            },
            TokenType::LeftParen => {
                self.advance()?;
                let expr = self.expression()?;
                self.assert_current_type(TokenType::RightParen, ParseError::MissingRightParen())?;
                Expr::Grouping {
                    expression: Box::new(expr),
                }
            }
            _ => {
                let token = self.current();
                return Err(
                    self.report_error(token, ParseError::UnexpectedToken(token.lexeme.clone()))
                );
            }
        };
        self.advance()?;
        Ok(expr)
    }

    fn match_any<const L: usize>(&self, types: [TokenType; L]) -> bool {
        types.iter().any(|t| self.check_current_type(t.clone()))
    }

    fn check_current_type(&self, type_: TokenType) -> bool {
        !self.is_at_end() && type_ == self.current().type_
    }

    fn assert_current_type(&mut self, type_: TokenType, error: ParseError) -> ParseResult<()> {
        match self.check_current_type(type_) {
            true => Ok(()),
            false => Err(self.report_error(self.current(), error)),
        }
    }

    fn advance(&mut self) -> ParseResult<()> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEndOfSource());
        }
        self.current_index += 1;
        Ok(())
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current_index - 1).unwrap()
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.current_index).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.current().type_ == TokenType::EOF
    }

    fn report_error(&self, token: &Token, error: ParseError) -> ParseError {
        error::report(token.loc.line, token.loc.col, error.to_string());
        error
    }

    // TODO Use sync when there's an error
    fn sync(&mut self) -> ParseResult<()> {
        self.advance()?;

        while !self.is_at_end() {
            if self.previous().type_ == TokenType::Semicolon {
                return Ok(());
            }

            match self.current().type_ {
                TokenType::Fn => return Ok(()),
                _ => self.advance()?,
            };
        }
        Ok(())
    }
}
