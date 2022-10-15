pub mod error;
pub mod expr;
pub mod stmt;

use crate::lex::{Token, TokenKind};

use self::{
    error::ParseError,
    stmt::{Function, Stmt},
};

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Boolean(bool),
    Number(f64),
    String(String),
}

pub struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current_index: 0,
        }
    }

    pub fn parse(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut statments: Vec<Stmt> = Vec::new();
        statments.push(Stmt::Function(Function {
            prototype: stmt::Prototype {
                name: "print".to_string(),
                args: vec!["x".to_string()],
            },
            body: vec![],
            return_expr: None,
            is_anon: false,
            is_builtin: true,
        }));
        while !self.is_at_end() {
            statments.push(self.parse_declaration()?);
        }
        Ok(statments)
    }

    fn parse_declaration(&mut self) -> ParseResult<Stmt> {
        trace!("Parsing declaration. Current: {:#?}", self.current());
        let stmt = match self.current().kind {
            TokenKind::Fn => self.parse_fn()?,
            _ => self.parse_toplevel_expr()?,
        };
        Ok(stmt)
    }

    fn match_any<const L: usize>(&self, types: [TokenKind; L]) -> bool {
        types.iter().any(|t| self.current_is(t.clone()))
    }

    fn current_is(&self, kind: TokenKind) -> bool {
        !self.is_at_end() && kind == self.current().kind
    }

    fn consume(&mut self, kind: TokenKind, error: ParseError) -> ParseResult<&Token> {
        trace!("Consuming {:#?}", kind);
        match self.current_is(kind) {
            true => {
                self.advance()?;
                Ok(self.previous())
            }
            false => Err(error::report(self.current(), error)),
        }
    }

    fn advance(&mut self) -> ParseResult<&Token> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEndOfSource());
        }

        trace!(
            "Advanced from \"{}\" to \"{}\"",
            self.current().lexeme,
            self.tokens[self.current_index + 1].lexeme,
        );

        self.current_index += 1;
        Ok(self.previous())
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.current_index).unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current_index - 1).unwrap()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current_index + 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::EOF
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lex::{Location, Scanner, Token, TokenKind},
        parser::{
            expr::{BinaryExpr, Expr},
            stmt::{Function, Prototype, Stmt},
            LiteralValue,
        },
    };

    use super::Parser;

    #[test]
    fn parse_arithmetic() {
        let src = "2 + 2 * 3 / 2".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Stmt::Expr(Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Literal {
                value: LiteralValue::Number(2.0),
            }),
            operator: Token {
                kind: TokenKind::Plus,
                lexeme: "+".to_string(),
                loc: Location { col: 3, line: 1 },
            },
            right: Box::new(Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Binary(BinaryExpr {
                    left: Box::new(Expr::Literal {
                        value: LiteralValue::Number(2.0),
                    }),
                    operator: Token {
                        kind: TokenKind::Star,
                        lexeme: "*".to_string(),
                        loc: Location { col: 7, line: 1 },
                    },
                    right: Box::new(Expr::Literal {
                        value: LiteralValue::Number(3.0),
                    }),
                })),
                operator: Token {
                    kind: TokenKind::Slash,
                    lexeme: "/".to_string(),
                    loc: Location { col: 11, line: 1 },
                },
                right: Box::new(Expr::Literal {
                    value: LiteralValue::Number(2.0),
                }),
            })),
        }));

        assert_eq!(actual_ast.len(), 1);
        assert_eq!(expected_ast, actual_ast[0]);
    }

    #[test]
    fn parse_parenthesized_arithmetic() {
        let src = "(2 + 2) * 3 / 2".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Stmt::Expr(Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Binary(BinaryExpr {
                    left: Box::new(Expr::Literal {
                        value: LiteralValue::Number(2.0),
                    }),
                    operator: Token {
                        kind: TokenKind::Plus,
                        lexeme: "+".to_string(),
                        loc: Location { col: 4, line: 1 },
                    },
                    right: Box::new(Expr::Literal {
                        value: LiteralValue::Number(2.0),
                    }),
                })),
                operator: Token {
                    kind: TokenKind::Star,
                    lexeme: "*".to_string(),
                    loc: Location { col: 9, line: 1 },
                },
                right: Box::new(Expr::Literal {
                    value: LiteralValue::Number(3.0),
                }),
            })),
            operator: Token {
                kind: TokenKind::Slash,
                lexeme: "/".to_string(),
                loc: Location { col: 13, line: 1 },
            },
            right: Box::new(Expr::Literal {
                value: LiteralValue::Number(2.0),
            }),
        }));

        assert_eq!(actual_ast.len(), 1);
        assert_eq!(expected_ast, actual_ast[0]);
    }

    #[test]
    fn parse_function_definition() {
        let src = r#"
            fn main(a, b) {
                2 + 2 
            }
        "#
        .to_string();

        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Function {
            prototype: Prototype {
                name: "main".to_string(),
                args: vec!["a".to_string(), "b".to_string()],
            },
            body: vec![],
            return_expr: Some(Expr::Binary(BinaryExpr {
                left: Box::new(Expr::Literal {
                    value: LiteralValue::Number(2.0),
                }),
                operator: Token {
                    kind: TokenKind::Plus,
                    lexeme: "+".to_string(),
                    loc: Location { col: 18, line: 3 },
                },
                right: Box::new(Expr::Literal {
                    value: LiteralValue::Number(2.0),
                }),
            })),
            is_anon: false,
            is_builtin: false,
        };

        let func = match actual_ast[0].clone() {
            Stmt::Function(func) => func,
            _ => panic!("Expected a function"),
        };

        assert_eq!(expected_ast, func);
    }

    #[test]
    fn parse_function_call() {
        let src = "fn main() { some_fn(a, b); }".to_string();
        let mut scanner = Scanner::new(src.clone());
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens.to_vec());
        let actual_ast = parser.parse().unwrap();
        trace!("AST for {}: \n{:#?}", src, actual_ast);

        let expected_ast = Stmt::Function(Function {
            prototype: Prototype {
                name: "main".to_string(),
                args: vec![],
            },
            body: vec![Stmt::Expr(Expr::FnCall {
                fn_name: "some_fn".to_string(),
                args: vec![
                    Expr::Variable("a".to_string()),
                    Expr::Variable("b".to_string()),
                ],
            })],
            return_expr: None,
            is_anon: false,
            is_builtin: false,
        });

        assert_eq!(actual_ast.len(), 1);
        assert_eq!(expected_ast, actual_ast[0]);
    }
}
