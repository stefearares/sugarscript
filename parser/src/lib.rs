use std::fmt::Display;

use lexer::{lex, token::Token};

type Result<T> = std::result::Result<T, ParserErr>;

pub fn parse(input: &str) -> Result<Module> {
    Parser::new(lex(input)).parse_module()
}

/// Parses an input string into an expression but may **panic** when encountering
/// a statement.
pub fn parse_expr(input: &str) -> Result<Expr> {
    Parser::new(lex(input)).parse_expr()
}

pub use ast::*;
mod ast;

#[derive(Debug)]
pub enum ParserErr {
    ExpectedIdent(Token),
    ExpectedRParen(Token),
    UnexpectedToken(Token),
    ExpectedAssignment(Token),
    ExpectedLiteral(Token),
    NoMatchArm,
}

impl Display for ParserErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserErr::ExpectedIdent(token) => write!(f, "Expected identifier found: {token:#?}"),
            ParserErr::ExpectedRParen(token) => {
                write!(f, "Expected closing parenthesis found: {token:#?}")
            }
            ParserErr::UnexpectedToken(token) => write!(f, "Unexpected token: {token:#?}"),
            ParserErr::ExpectedAssignment(token) => {
                write!(f, "Expected an `=` but found: {token:#?}")
            }
            ParserErr::ExpectedLiteral(token) => {
                write!(f, "Expected a literal but found: {token:#?}")
            }
            ParserErr::NoMatchArm => {
                write!(f, "Match needs atleast one match arm but none found")
            }
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn current_token(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or(&Token::Eof)
    }
    fn next_token(&mut self) {
        self.position += 1;
    }

    pub fn parse_module(&mut self) -> Result<Module> {
        let mut module = Module { statements: vec![] };

        loop {
            match self.current_token() {
                Token::Eof => break,
                Token::Newline(_) => {
                    self.next_token();
                    continue;
                }
                _ => module.statements.push(self.parse_statement()?),
            }
        }
        Ok(module)
    }

    fn parse_statement(&mut self) -> Result<Stmt> {
        match self.current_token() {
            Token::Let => {
                self.next_token();
                self.parse_let()
            }
            _ => Ok(Stmt::Expression(self.parse_expr()?)),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.current_token() {
            Token::Function => self.parse_function(),
            Token::Match => self.parse_match(),
            _ => self.parse_add_sub(),
        }
    }

    fn parse_let(&mut self) -> Result<Stmt> {
        let identifier = match self.current_token() {
            Token::Ident(name) => Ok(Identifier::Identifier(name.clone())),
            any => Err(ParserErr::ExpectedIdent(any.clone())),
        }?;
        self.next_token();

        match self.current_token() {
            Token::Assign => {}
            any => return Err(ParserErr::ExpectedAssignment(any.clone())),
        };
        self.next_token();

        let expr = self.parse_expr()?;

        Ok(Stmt::Let(LetStatement { identifier, expr }))
    }

    fn parse_pattern(&mut self) -> Result<Pattern> {
        match self.parse_literal() {
            Ok(val) => Ok(Pattern::Literal(val)),
            Err(_) => match self.current_token() {
                Token::Ident(name) => Ok(Pattern::Ident(Identifier::Identifier(name.clone()))),
                any => Err(ParserErr::UnexpectedToken(any.clone())),
            },
        }
    }

    fn parse_match(&mut self) -> Result<Expr> {
        self.expect_token(Token::Match)?;
        let expr = self.parse_expr()?;
        self.expect_token(Token::Colon)?;
        let mut arms = Vec::new();
        loop {
            match self.current_token() {
                Token::Eof => break,
                Token::Newline(_) => {
                    self.next_token();
                    continue;
                }
                Token::Pipe => {
                    self.next_token();
                    let pattern = self.parse_pattern()?;
                    self.next_token();
                    self.expect_token(Token::Arrow)?;
                    let expression = self.parse_expr()?;
                    arms.push((pattern, expression));
                }
                any => return Err(ParserErr::UnexpectedToken(any.clone())),
            }
        }
        match !arms.is_empty() {
            true => Ok(Expr::Match(Match {
                initial_expr: Box::new(expr),
                arms,
            })),
            false => Err(ParserErr::NoMatchArm),
        }
    }

    fn parse_literal(&mut self) -> Result<Literal> {
        match self.current_token() {
            Token::String(inner) => Ok(Literal::String(inner.to_string())),
            Token::Int(inner) => Ok(Literal::Int(inner.parse().unwrap())),
            any => Err(ParserErr::ExpectedLiteral(any.clone())),
        }
    }

    fn parse_function(&mut self) -> Result<Expr> {
        self.expect_token(Token::Function)?;

        let params = self.parse_parameters()?;

        self.expect_token(Token::Arrow)?;

        let body = self.parse_expr()?;

        Ok(Expr::FunctionDecl(FnDecl {
            params,
            body: Box::new(body),
        }))
    }

    fn parse_parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut params = Vec::new();

        while *self.current_token() != Token::Arrow {
            let param_name = match self.current_token() {
                Token::Ident(name) => name.clone(),
                _ => return Err(ParserErr::ExpectedIdent(self.current_token().clone())),
            };
            self.next_token();

            params.push(Identifier::Identifier(param_name));

            if self.current_token() == &Token::Comma {
                self.next_token();
            } else if matches!(self.current_token(), Token::Arrow) {
                break;
            } else {
                return Err(ParserErr::UnexpectedToken(self.current_token().clone()));
            }
        }

        Ok(params)
    }

    fn parse_add_sub(&mut self) -> Result<Expr> {
        let mut expr = self.parse_mult_div()?;
        loop {
            match self.current_token() {
                Token::Newline(_) => {
                    self.next_token();
                    continue;
                }
                Token::Plus => {
                    self.next_token();
                    let right = self.parse_mult_div()?;
                    expr = Expr::new_add(expr, right);
                }
                Token::Minus => {
                    self.next_token();
                    let right = self.parse_mult_div()?;
                    expr = Expr::new_substract(expr, right);
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn expect_token(&mut self, expected: Token) -> Result<()> {
        if &expected == self.current_token() {
            self.next_token();
            Ok(())
        } else {
            Err(ParserErr::UnexpectedToken(self.current_token().clone()))
        }
    }

    /// Goes through the integers and parenthesis in the expression
    fn parse_mult_div(&mut self) -> Result<Expr> {
        let mut expr = self.parse_primary()?;
        loop {
            match self.current_token() {
                Token::Newline(_) => {
                    self.next_token();
                    continue;
                }
                Token::Asterisk => {
                    self.next_token();
                    let right = self.parse_primary()?;
                    expr = Expr::new_multiply(expr, right);
                }
                Token::Slash => {
                    self.next_token();
                    let right = self.parse_primary()?;
                    expr = Expr::new_divide(expr, right);
                }
                _ => break, // No more multiplication or division operations
            }
        }
        Ok(expr)
    }

    fn parse_identifier(&mut self) -> Result<Expr> {
        let name = match self.current_token() {
            Token::Ident(name) => Ok(name),
            any => Err(ParserErr::UnexpectedToken(any.clone())),
        }?
        .clone();

        self.next_token();

        if self.current_token() == &Token::Lparen {
            self.parse_function_call(Expr::Ident(Identifier::Identifier(name)).into())
        } else {
            Ok(Expr::Ident(Identifier::Identifier(name)))
        }
    }

    fn parse_function_call(&mut self, callee: Box<Expr>) -> Result<Expr> {
        self.expect_token(Token::Lparen)?;

        let mut arguments = Vec::new();
        if self.current_token() != &Token::Rparen {
            loop {
                arguments.push(self.parse_expr()?);
                match self.current_token() {
                    Token::Comma => self.next_token(),
                    Token::Rparen => break,
                    any => {
                        println!("test");
                        return Err(ParserErr::UnexpectedToken(any.clone()));
                    }
                }
            }
        }
        self.expect_token(Token::Rparen)?;

        Ok(Expr::FunctionCall(FnCall { callee, arguments }))
    }

    // TODO(rares): add parse_literal function

    ///  Parses Intergers and Parentheses.
    fn parse_primary(&mut self) -> Result<Expr> {
        while let &Token::Newline(_) = self.current_token() {
            self.next_token();
        }
        match self.current_token() {
            Token::Int(value) => {
                // FIXME: parse into proper value after adding support for more
                //        types in `Expr::Literal`
                let val = value.parse().unwrap();
                self.next_token();
                Ok(Expr::new_literal(val))
            }

            // If a parethesis is found then parse the upcomming expression
            Token::Lparen => {
                self.next_token();
                let mut expr = self.parse_expr()?;

                // Verify if there's a closing parenthesis.
                if !matches!(self.current_token(), Token::Rparen) {
                    return Err(ParserErr::ExpectedRParen(self.current_token().clone()));
                }

                self.next_token();

                while let Expr::FunctionDecl(_) | Expr::FunctionCall(_) | Expr::Ident(_) = expr {
                    if self.current_token() != &Token::Lparen {
                        break;
                    }
                    expr = self.parse_function_call(expr.into())?;
                }

                Ok(expr)
            }

            Token::Ident(_) => self.parse_identifier(),

            _ => Err(ParserErr::UnexpectedToken(self.current_token().clone())),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::ast::Literal;
    use crate::Identifier::Identifier;

    use super::*;

    #[test]
    fn parse_addition() {
        let expected = Expr::Add(Add {
            left: Box::new(Expr::Literal(Literal::Int(1))),
            right: Box::new(Expr::Literal(Literal::Int(2))),
        });
        assert_eq!(expected, Parser::new(lex("1 + 2")).parse_add_sub().unwrap());
    }

    #[test]
    fn parse_multiplication() {
        let expected = Expr::Multiply(Multiply {
            left: Box::new(Expr::Literal(Literal::Int(1))),
            right: Box::new(Expr::Literal(Literal::Int(2))),
        });
        assert_eq!(
            expected,
            Parser::new(lex("1 * 2")).parse_mult_div().unwrap()
        );
    }

    #[test]
    fn parse_nested() {
        // (1 + 2) * 3
        let expected = Expr::Multiply(Multiply {
            left: Box::new(Expr::Add(Add {
                left: Box::new(Expr::Literal(Literal::Int(1))),
                right: Box::new(Expr::Literal(Literal::Int(2))),
            })),
            right: Box::new(Expr::Literal(Literal::Int(3))),
        });
        assert_eq!(
            expected,
            Parser::new(lex("(1 + 2) * 3")).parse_mult_div().unwrap()
        );
    }
    #[test]
    fn parse_match() {
        let tokens = vec![
            Token::Match,
            Token::Ident("x".into()),
            Token::Colon,
            Token::Pipe,
            Token::Int("0".to_string()),
            Token::Arrow,
            Token::Int("1".to_string()),
        ];
        let mut parser = Parser::new(tokens.clone());
        let result = parser.parse_match();
        assert_eq!(
            result.unwrap(),
            Expr::Match(Match {
                initial_expr: Box::new(Expr::Ident("x".into())),
                arms: vec![(
                    Pattern::Literal(Literal::Int(0)),
                    Expr::Literal(Literal::Int(1))
                )],
            })
        );
    }
    #[test]
    fn parse_simple_function() {
        let tokens = vec![
            Token::Function,
            Token::Ident("x".into()),
            Token::Comma,
            Token::Ident("y".into()),
            Token::Arrow,
            Token::Ident("x".into()),
            Token::Plus,
            Token::Ident("y".into()),
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse_function();

        assert_eq!(
            result.unwrap(),
            Expr::FunctionDecl(FnDecl {
                params: vec![Identifier("x".into()), Identifier("y".into())],
                body: Box::new(Expr::Add(Add {
                    left: Box::new(Expr::Ident(Identifier("x".into()))),
                    right: Box::new(Expr::Ident(Identifier("y".into()))),
                })),
            })
        );
    }

    #[test]
    fn parse_simple_function_call() {
        let tokens = vec![
            Token::Ident("add".into()),
            Token::Lparen,
            Token::Int("1".into()),
            Token::Comma,
            Token::Int("2".into()),
            Token::Rparen,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse_expr();

        assert_eq!(
            result.unwrap(),
            Expr::FunctionCall(FnCall {
                callee: Expr::Ident("add".into()).into(),
                arguments: vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                ],
            })
        );
    }

    // 1 + (2 + 1) * 3
    #[test]
    fn test_parsing() {
        let tokens = vec![
            Token::Int("1".into()),
            Token::Plus,
            Token::Lparen,
            Token::Int("2".into()),
            Token::Plus,
            Token::Int("1".into()),
            Token::Rparen,
            Token::Asterisk,
            Token::Int("3".into()),
        ];

        let result = Parser::new(tokens).parse_add_sub();

        assert_eq!(
            result.unwrap(),
            Expr::new_add(
                Expr::new_literal(1),
                Expr::new_multiply(
                    Expr::new_add(Expr::new_literal(2), Expr::new_literal(1)),
                    Expr::new_literal(3)
                )
            )
        );
    }

    // (1 + 2) * 2
    #[test]
    fn test_add_paren_mult() {
        let tokens = vec![
            Token::Lparen,
            Token::Int("1".into()),
            Token::Plus,
            Token::Int("2".into()),
            Token::Rparen,
            Token::Asterisk,
            Token::Int("2".into()),
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse_expr();

        assert_eq!(
            result.unwrap(),
            Expr::new_multiply(
                Expr::new_add(Expr::new_literal(1), Expr::new_literal(2)),
                Expr::new_literal(2)
            )
        );
    }

    #[test]
    fn test_parse_multiline_binary_operator() {
        let input = [
            (
                Expr::Add(Add {
                    left: Box::new(Expr::Literal(Literal::Int(1))),
                    right: Box::new(Expr::Literal(Literal::Int(2))),
                }),
                r"1 +
2",
            ),
            (
                Expr::Add(Add {
                    left: Box::new(Expr::Literal(Literal::Int(1))),
                    right: Box::new(Expr::Literal(Literal::Int(2))),
                }),
                r"1
            +
            2",
            ),
            (
                Expr::Subtract(Subtract {
                    left: Box::new(Expr::Literal(Literal::Int(1))),
                    right: Box::new(Expr::Literal(Literal::Int(2))),
                }),
                r"1
            -
            2",
            ),
            (
                Expr::Multiply(Multiply {
                    left: Box::new(Expr::Literal(Literal::Int(1))),
                    right: Box::new(Expr::Literal(Literal::Int(2))),
                }),
                r"1
            *
            2",
            ),
            (
                Expr::Divide(Divide {
                    left: Box::new(Expr::Literal(Literal::Int(1))),
                    right: Box::new(Expr::Literal(Literal::Int(2))),
                }),
                r"1
            /
            2",
            ),
        ];

        for (expected, actual) in input {
            assert_eq!(expected, parse_expr(dbg!(actual)).unwrap());
        }
    }

    #[test]
    fn test_parse_new_line() {
        let input = r"let x = 10
        x";

        let expected = Module::from([
            Stmt::Let(LetStatement {
                identifier: "x".into(),
                expr: Expr::new_literal(10),
            }),
            Stmt::Expression(Expr::Ident("x".into())),
        ]);

        assert_eq!(expected, parse(input).unwrap());
    }

    #[test]
    fn test_parse_module() {
        let input = "1 + 2
        3 * 3
        ";

        let expected = Module::from([
            Stmt::new_expr(Expr::new_add(Expr::new_literal(1), Expr::new_literal(2))),
            Stmt::new_expr(Expr::new_multiply(
                Expr::new_literal(3),
                Expr::new_literal(3),
            )),
        ]);

        assert_eq!(expected, parse(input).unwrap());
    }

    #[test]
    fn test_parse_anonymous_fn_call() {
        let input = "(fn x -> x)(10)";

        let expected = Expr::FunctionCall(FnCall {
            callee: Expr::FunctionDecl(FnDecl {
                params: vec!["x".into()],
                body: Expr::new_ident("x").into(),
            })
            .into(),
            arguments: vec![Expr::new_literal(10)],
        });

        assert_eq!(expected, parse_expr(input).unwrap());
    }

    #[test]
    fn test_parse_lambda_calculus_true() {
        let input = "(fn x -> fn y -> x)(1)(2)";

        let expected = Expr::FunctionCall(FnCall {
            callee: Expr::FunctionCall(FnCall {
                callee: Expr::FunctionDecl(FnDecl {
                    params: vec!["x".into()],
                    body: Expr::FunctionDecl(FnDecl {
                        params: vec!["y".into()],
                        body: Expr::new_ident("x").into(),
                    })
                    .into(),
                })
                .into(),
                arguments: vec![Expr::new_literal(1)],
            })
            .into(),
            arguments: vec![Expr::new_literal(2)],
        });

        let result = parse_expr(input).unwrap();
        assert_eq!(expected, result, "\nexpected: {expected}\nresult: {result}");
    }
}
