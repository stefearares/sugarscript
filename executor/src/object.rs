use std::fmt::Display;

use parser::{Expr, Identifier};

use super::env::Environment;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i32),
    String(String),
    Function(Vec<Identifier>, Expr, Environment),
    Unit,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{val}"),
            Object::String(val) => write!(f, "{val:#?}"),
            Object::Function(params, expr, env) => {
                write!(f, "fn({params:#?}) -> {expr:#?} \nenv: {env:#?}")
            }
            Object::Unit => write!(f, "Unit"),
        }
    }
}

impl From<i32> for Object {
    fn from(value: i32) -> Self {
        Object::Integer(value)
    }
}

impl Object {
    // #[deprecated]
    pub fn unwrap(self) -> i32 {
        match self {
            Object::Integer(integer) => integer,
            _ => todo!(),
        }
    }
}
