use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;

pub type MutEnv = Rc<RefCell<Environment>>;

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn new_mut() -> MutEnv {
        Rc::new(RefCell::new(Self::new()))
    }

    pub fn add() {}
}
