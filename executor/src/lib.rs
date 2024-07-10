use std::fmt::Display;

use env::Environment;
use parser::{
    Add, Divide, Expr, FnCall, FnDecl, Identifier, If, LetStatement, Literal, Match, Module,
    Multiply, Pattern, Stmt, Subtract,
};

pub mod env;
pub use object::*;
mod object;

type Result<T> = std::result::Result<T, EvalErr>;

#[derive(Debug, PartialEq, Eq)]
pub enum EvalErr {
    UnknownIdent(String),
    AlreadyDefined(String),
    ExpectedFunction(Object),
    WrongArity(usize, usize),
    NoMatchFound,
}

impl Display for EvalErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalErr::UnknownIdent(name) => write!(f, "unknown identifier: {name}"),
            EvalErr::AlreadyDefined(name) => write!(f, "identifier is already defined: {name}"),
            EvalErr::ExpectedFunction(obj) => write!(f, "expected a function but found: {obj}"),
            EvalErr::WrongArity(expected, actual) => {
                write!(f, "wrong arity, expected: {expected} but found: {actual}")
            }
            EvalErr::NoMatchFound => write!(f, "cannot find mathcing arm."),
        }
    }
}

pub fn eval(ast: Module, env: &mut Environment) -> Result<Vec<Object>> {
    let mut results = vec![];

    for stmt in ast.statements {
        results.push(eval_statement(stmt, env)?);
    }
    Ok(results)
}

pub fn eval_statement(stmt: Stmt, env: &mut Environment) -> Result<Object> {
    match stmt {
        Stmt::Expression(expr) => eval_expr(expr, env),
        Stmt::Let(val) => exec_let(val, env),
    }
}

fn eval_expr(ast: Expr, env: &Environment) -> Result<Object> {
    match ast {
        Expr::Add(add) => eval_add(add, env),
        Expr::Subtract(sub) => eval_subtract(sub, env),
        Expr::Multiply(mult) => eval_multiply(mult, env),
        Expr::Divide(div) => eval_divide(div, env),
        Expr::Literal(Literal::Int(val)) => Ok(Object::Integer(val)),
        Expr::Literal(Literal::String(val)) => Ok(Object::String(val)),

        Expr::Match(match_expr) => eval_match(match_expr, env),

        Expr::Ident(Identifier::Identifier(name)) => eval_identifier(name, env),
        Expr::Ident(Identifier::Builtin(_name)) => todo!(),

        Expr::Let(_let_expr) => todo!(),

        Expr::If(If {
            condition,
            then,
            r#else,
        }) => {
            if condition {
                eval_expr(*then, env)
            } else {
                eval_expr(*r#else, env)
            }
        }
        Expr::FunctionDecl(fn_decl @ FnDecl { .. }) => Ok(eval_function_decl(fn_decl, env.clone())),
        Expr::FunctionCall(fn_call @ FnCall { .. }) => eval_function_call(fn_call, env),
        Expr::Pattern(pattern) => eval_pattern(pattern, env),
    }
}

fn exec_let(
    LetStatement { identifier, expr }: LetStatement,
    env: &mut Environment,
) -> Result<Object> {
    match identifier {
        Identifier::Identifier(name) => match env.store.get(&name) {
            None => {
                env.store.insert(name, eval_expr(expr, env)?);
                Ok(Object::Unit)
            }
            Some(_) => Err(EvalErr::AlreadyDefined(name)),
        },
        Identifier::Builtin(val) => Err(EvalErr::AlreadyDefined(val)),
    }
}

fn eval_function_call(FnCall { callee, arguments }: FnCall, env: &Environment) -> Result<Object> {
    let mut env = env.clone();

    // eval callee
    let (params, body, fn_env) = match eval_expr(*callee, &env)? {
        Object::Function(params, body, fn_env) => (params, body, fn_env),
        any => {
            return Err(EvalErr::ExpectedFunction(any));
        }
    };

    if params.len() != arguments.len() {
        return Err(EvalErr::WrongArity(params.len(), arguments.len()));
    }

    // eval args
    let mut evaluated_args = Vec::with_capacity(arguments.len());
    for arg in arguments {
        evaluated_args.push(eval_expr(arg, &env)?);
    }

    // extend env with fn_env
    env.store.extend(fn_env.store);

    for (arg, param) in evaluated_args.into_iter().zip(params) {
        let Identifier::Identifier(param) = param else {
            todo!("builtin functions aren't implemented yet");
        };

        env.store.insert(param, arg);
    }
    eval_expr(body, &env)
}

fn eval_identifier(name: String, env: &Environment) -> Result<Object> {
    env.store
        .get(&name)
        .cloned()
        .ok_or(EvalErr::UnknownIdent(name))
}

fn eval_pattern(pattern: Pattern, env: &Environment) -> Result<Object> {
    match pattern {
        Pattern::Literal(literal) => eval_literal(literal),
        Pattern::Ident(Identifier::Identifier(name)) => eval_identifier(name, env),
        Pattern::Ident(Identifier::Builtin(name)) => Err(EvalErr::AlreadyDefined(name)),
    }
}

// Added so it can evaluate pure literals
fn eval_literal(ast: Literal) -> Result<Object> {
    match ast {
        Literal::Int(val) => Ok(Object::Integer(val)),
        Literal::String(val) => Ok(Object::String(val)),
        // Handle other literal types as needed
    }
}

fn eval_match(Match { initial_expr, arms }: Match, env: &Environment) -> Result<Object> {
    let initial_val = eval_expr(*initial_expr.clone(), env)?;

    for (pattern, expr) in arms {
        match pattern {
            Pattern::Ident(ident) => {
                return eval_function_call(
                    FnCall {
                        callee: Expr::FunctionDecl(FnDecl {
                            params: vec![ident],
                            body: expr.into(),
                        })
                        .into(),
                        arguments: vec![*initial_expr],
                    },
                    env,
                )
            }
            Pattern::Literal(literal) => {
                if eval_literal(literal)? == initial_val {
                    return eval_expr(expr, env);
                }
            }
        }
    }

    Err(EvalErr::NoMatchFound)
}

fn eval_function_decl(FnDecl { params, body: expr }: FnDecl, env: Environment) -> Object {
    Object::Function(params, *expr, env)
}

fn eval_add(Add { left, right }: Add, env: &Environment) -> Result<Object> {
    Ok(Object::Integer(
        eval_expr(*left, env)?.unwrap() + eval_expr(*right, env)?.unwrap(),
    ))
}
fn eval_subtract(Subtract { left, right }: Subtract, env: &Environment) -> Result<Object> {
    Ok(Object::Integer(
        eval_expr(*left, env)?.unwrap() - eval_expr(*right, env)?.unwrap(),
    ))
}

fn eval_multiply(Multiply { left, right }: Multiply, env: &Environment) -> Result<Object> {
    Ok(Object::Integer(
        eval_expr(*left, env)?.unwrap() * eval_expr(*right, env)?.unwrap(),
    ))
}

fn eval_divide(Divide { left, right }: Divide, env: &Environment) -> Result<Object> {
    Ok(Object::Integer(
        eval_expr(*left, env)?.unwrap() / eval_expr(*right, env)?.unwrap(),
    ))
}
#[cfg(test)]
mod tests {
    use parser::parse_expr;

    use super::*;
    use std::fmt::Debug;

    fn apply_tests<Expected, Actual, F>(tests: &[(Expected, Actual)], func: F)
    where
        Expected: PartialEq + Debug,
        Actual: PartialEq + Debug,
        F: Fn(&Actual, &Environment) -> Expected,
    {
        for (expected, actual) in tests {
            let env = Environment::new();
            assert_eq!(*expected, func(actual, &env));
        }
    }

    #[test]
    fn match_test() {
        let match_input1 = "match 1:
                            | 1 -> 4
                            | 2 -> 5";
        let match_input2 = "match 4:
                            | 3 -> 56
                            | 1 -> 3
                            | 4 -> 2";
        let match_input3 = "match 1:
                            | 1 -> 2
                            | 2 -> 3
                            | 1 -> 4
                            | 1 -> 4
                            | 1 -> 4";

        let _expected_expr = Expr::Match(Match {
            initial_expr: Box::new(Expr::Literal(Literal::Int(1))),
            arms: vec![
                (
                    Pattern::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::String("Matched".to_string())),
                ),
                (
                    Pattern::Literal(Literal::Int(2)),
                    Expr::Literal(Literal::String("Not Matched".to_string())),
                ),
            ],
        });

        let tests = [(4, match_input1), (2, match_input2), (2, match_input3)];

        apply_tests(&tests, |actual, env| {
            eval_expr(parse_expr(actual).unwrap(), env)
                .unwrap()
                .unwrap()
        });
    }

    #[test]
    fn test_match_any() {
        let input = "match 42:
                        | 1 -> 1
                        | meaning -> meaning";

        apply_tests(&[(42, input)], |actual, env| {
            eval_expr(parse_expr(actual).unwrap(), env)
                .unwrap()
                .unwrap()
        })
    }

    #[test]
    fn addition() {
        let tests = [(3, "1 + 2"), (3, "2 + 1"), (12, "10 + 2")];

        apply_tests(&tests, |actual, env| {
            eval_expr(parse_expr(actual).unwrap(), env)
                .unwrap()
                .unwrap()
        })
    }

    #[test]
    fn multiplication() {
        let tests = [(2, "1 * 2"), (2, "2 * 1"), (20, "10 * 2")];

        apply_tests(&tests, |actual, env| {
            eval_expr(parse_expr(actual).unwrap(), env)
                .unwrap()
                .unwrap()
        })
    }

    #[test]
    fn test_eval_integer() {
        let tests = [(1, "1"), (200, "200"), (42, "42")];

        apply_tests(&tests, |actual, env| {
            eval_expr(parse_expr(actual).unwrap(), env)
                .unwrap()
                .unwrap()
        })
    }

    #[test]
    fn test_exec_let() {
        let mut env = Environment::new();
        assert!(exec_let(
            LetStatement::new("ten".into(), Expr::Literal(Literal::Int(10))),
            &mut env
        )
        .is_ok());
        assert!(env
            .store
            .get("ten")
            .is_some_and(|val| *val == Object::Integer(10)));
    }

    #[test]
    fn test_function_decl() {
        let input = FnDecl {
            params: vec![Identifier::Identifier("x".into())],
            body: Expr::new_multiply(
                Expr::Ident(Identifier::Identifier("x".into())),
                Expr::Ident(Identifier::Identifier("x".into())),
            )
            .into(),
        };

        let env = Environment::default();

        assert_eq!(
            Object::Function(
                vec![Identifier::Identifier("x".into())],
                Expr::new_multiply(
                    Expr::Ident(Identifier::Identifier("x".into())),
                    Expr::Ident(Identifier::Identifier("x".into())),
                ),
                env.clone()
            ),
            eval_function_decl(input, env)
        )
    }

    #[test]
    fn test_function_call() {
        //| let x = 10
        //| x(2)

        let input = [
            Stmt::Let(LetStatement {
                identifier: "x".into(),
                expr: Expr::Literal(Literal::Int(10)),
            }),
            Stmt::Expression(Expr::FunctionCall(FnCall {
                callee: Box::new(Expr::Ident("x".into())),
                arguments: vec![Expr::new_literal(2)],
            })),
        ]
        .into();

        assert!(eval(input, &mut Environment::default())
            .is_err_and(|e| e == EvalErr::ExpectedFunction(Object::Integer(10))));
    }

    #[test]
    fn test_function_call_with_ident_defined() {
        //| let x = 10
        //|
        //| let add = fn x ->
        //|     x + x
        //|
        //| add(3) # => 6
        let input = Module {
            statements: vec![
                Stmt::Let(LetStatement {
                    identifier: "x".into(),
                    expr: Expr::Literal(Literal::Int(10)),
                }),
                Stmt::Let(LetStatement {
                    identifier: "add".into(),
                    expr: Expr::FunctionDecl(FnDecl {
                        params: vec!["x".into()],
                        body: Box::new(Expr::Add(Add {
                            left: Box::new(Expr::Ident("x".into())),
                            right: Box::new(Expr::Ident("x".into())),
                        })),
                    }),
                }),
                Stmt::Expression(Expr::FunctionCall(FnCall {
                    callee: Box::new(Expr::Ident("add".into())),
                    arguments: vec![Expr::Literal(Literal::Int(3))],
                })),
            ],
        };

        let expected = vec![Object::Unit, Object::Unit, Object::Integer(6)];

        assert_eq!(expected, eval(input, &mut Environment::default()).unwrap());
    }

    #[test]
    fn test_function_call_with_closure() {
        //| let true = fn x -> fn y -> x
        //| true(1)(2)

        let input = Module {
            statements: vec![
                Stmt::new_let(
                    "true",
                    Expr::FunctionDecl(FnDecl {
                        params: vec!["x".into()],
                        body: Box::new(Expr::FunctionDecl(FnDecl {
                            params: vec!["y".into()],
                            body: Expr::new_ident("x").into(),
                        })),
                    }),
                ),
                Stmt::new_expr(Expr::FunctionCall(FnCall {
                    callee: Box::new(Expr::FunctionCall(FnCall {
                        callee: Box::new(Expr::Ident("true".into())),
                        arguments: vec![Expr::new_literal(1)],
                    })),
                    arguments: vec![Expr::new_literal(2)],
                })),
            ],
        };

        assert_eq!(
            vec![Object::Unit, Object::Integer(1)],
            eval(input, &mut Environment::default()).unwrap()
        )
    }
}
