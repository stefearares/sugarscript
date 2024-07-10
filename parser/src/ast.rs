use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Literal(Literal),
    Add(Add),
    Subtract(Subtract),
    Multiply(Multiply),
    Divide(Divide),
    Ident(Identifier),
    Pattern(Pattern),

    If(If),
    Match(Match),
    FunctionDecl(FnDecl),
    FunctionCall(FnCall),
    Let(Let),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Literal(Literal),
    Ident(Identifier),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Int(i32),
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub identifier: Identifier,
    pub expression: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub condition: bool,
    pub then: Box<Expr>,
    pub r#else: Box<Expr>,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Match {
    pub initial_expr: Box<Expr>,
    pub arms: Vec<(Pattern, Expr)>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnDecl {
    pub params: Vec<Identifier>,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnCall {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Add {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Multiply {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Divide {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Subtract {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl Expr {
    pub fn new_literal(value: i32) -> Self {
        Expr::Literal(Literal::Int(value))
    }

    pub fn new_add(left: Expr, right: Expr) -> Self {
        Expr::Add(Add {
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    pub fn new_multiply(left: Expr, right: Expr) -> Self {
        Expr::Multiply(Multiply {
            left: Box::new(left),
            right: Box::new(right),
        })
    }
    pub fn new_divide(left: Expr, right: Expr) -> Self {
        Expr::Divide(Divide {
            left: Box::new(left),
            right: Box::new(right),
        })
    }
    pub fn new_substract(left: Expr, right: Expr) -> Self {
        Expr::Subtract(Subtract {
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    pub fn new_ident(name: &str) -> Self {
        Expr::Ident(name.into())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub statements: Vec<Stmt>,
}

impl<const N: usize> From<[Stmt; N]> for Module {
    fn from(stmts: [Stmt; N]) -> Self {
        Module {
            statements: stmts.into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expression(Expr),
    Let(LetStatement),
}
impl Stmt {
    pub fn new_expr(expr: Expr) -> Stmt {
        Stmt::Expression(expr)
    }

    pub fn new_let(ident: &str, expr: Expr) -> Stmt {
        Stmt::Let(LetStatement {
            identifier: ident.into(),
            expr,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub identifier: Identifier,
    pub expr: Expr,
}

impl LetStatement {
    pub fn new(identifier: String, expr: Expr) -> Self {
        Self {
            identifier: Identifier::Identifier(identifier),
            expr,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Identifier {
    Identifier(String),
    Builtin(String),
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        // TODO: handle builtin
        Identifier::Identifier(value.to_string())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Identifier(name) => write!(f, "{name}"),
            Identifier::Builtin(_) => todo!(),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(val) => write!(f, "{val}"),
            Literal::String(val) => write!(f, "\"{val}\""),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Literal(val) => write!(f, "{val}"),
            Pattern::Ident(val) => write!(f, "{val}"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{l}"),
            Expr::Add(Add { left, right }) => write!(f, "{left} + {right}"),
            Expr::Subtract(Subtract { left, right }) => write!(f, "{left} - {right}"),
            Expr::Multiply(Multiply { left, right }) => write!(f, "{left} * {right}"),
            Expr::Divide(Divide { left, right }) => write!(f, "{left} / {right}"),
            Expr::Ident(ident) => write!(f, "{ident}"),
            Expr::Pattern(pattern) => write!(f, "{pattern}"),
            Expr::If(_) => todo!(),
            Expr::Match(Match { initial_expr, arms }) => {
                let arms_string = arms.iter().fold("".to_string(), |acc, (literal, expr)| {
                    acc + &format!("\t| {literal} -> {expr}\n")
                });

                write!(f, "match {initial_expr}:\n{arms_string}")
            }
            Expr::FunctionDecl(FnDecl { params, body }) => {
                write!(
                    f,
                    "(fn {} -> {body})",
                    params
                        .iter()
                        .fold("".to_string(), |acc, param| acc + &format!("{param}"))
                )
            }
            Expr::FunctionCall(FnCall { callee, arguments }) => {
                let args_string = arguments
                    .iter()
                    .fold("".to_string(), |acc, arg| acc + &format!("{arg}"));
                write!(f, "{callee}({args_string})")
            }
            Expr::Let(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {}
