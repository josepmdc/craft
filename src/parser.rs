use crate::lex::{Token, TokenType};

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Boolean(bool),
    Null,
    Number(f64),
    String(String),
}

enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal {
        value: LiteralValue,
    },
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

// TODO re-enable warnings
#[allow(dead_code)]
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

    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn expresion(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while self.match_any(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.process_next_token().clone();
            let right = self.comparison();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self.match_any(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.process_next_token().clone();
            let right = self.term();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while self.match_any(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.process_next_token().clone();
            let right = self.term();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while self.match_any(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.process_next_token().clone();
            let right = self.unary();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if self.match_any(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.process_next_token().clone();
            let right = self.primary();
            return Expr::Unary {
                operator,
                right: Box::new(right),
            };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        let expr = match &self.peek().type_ {
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
            TokenType::LeftParen => todo!("Check for closing paren, else error"),
            _ => todo!("Return error"),
        };
        self.process_next_token();
        expr
    }

    fn match_any(&self, types: &[TokenType]) -> bool {
        types
            .iter()
            .any(|t| !self.is_at_end() && *t == self.peek().type_)
    }

    fn process_next_token(&mut self) -> &Token {
        self.current += 1;
        self.get_previous_token()
    }

    fn get_current_token(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn get_previous_token(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.peek().type_ == TokenType::EOF
    }
}
