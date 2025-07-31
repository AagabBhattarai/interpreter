use crate::error::EvalError;
use crate::evaluate::Referenceable;
use crate::native_function::clock_native;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type ReferenceTable = HashMap<String, Referenceable>;
pub type Pointer = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub struct Environment {
    pub environment: ReferenceTable,
    pub parent_environment: Option<Pointer>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            environment: ReferenceTable::new(),
            parent_environment: None,
        }
    }

    pub fn create_child(parent: Pointer) -> Self {
        Self {
            environment: ReferenceTable::new(),
            parent_environment: Some(parent),
        }
    }

    pub fn register_native_functions(&mut self) {
        const NATIVE_FUNCTIONS: &[(
            &str,
            fn(Vec<Referenceable>) -> Result<Referenceable, EvalError>,
        )] = &[("clock", clock_native)];
        for (name, func) in NATIVE_FUNCTIONS.iter() {
            self.environment.insert(
                name.to_string(),
                Referenceable::Native(*func, name.to_string()),
            );
        }
    }
    pub fn define(&mut self, name: &str, value: Referenceable) {
        self.environment.insert(name.to_string(), value);
    }

    pub fn lookup(&self, name: &str) -> Option<Referenceable> {
        if let Some(value) = self.environment.get(name) {
            return Some(value.clone());
        }
        if let Some(parent) = &self.parent_environment {
            return parent.borrow().lookup(name);
        }
        None
    }

    pub fn lookup_at(&self, name: &str, depth: usize) -> Referenceable {
        if depth == 0 {
            self.environment.get(name).unwrap().clone()
        } else {
            self.parent_environment
                .as_ref()
                .expect("Resolver and environment depth mismatch")
                .borrow()
                .lookup_at(name, depth - 1)
        }
    }
    pub fn set(&mut self, name: &str, value: Referenceable, kind: &str) -> Result<(), String> {
        if self.environment.contains_key(name) {
            self.environment.insert(name.to_string(), value);
            return Ok(());
        }
        if let Some(parent) = &self.parent_environment {
            return parent.borrow_mut().set(name, value, kind);
        }

        Err(format!("Undefined {} {}", kind, name))
    }
    pub fn set_at(&mut self, name: &str, value: Referenceable, depth: usize, kind: &str) {
        if depth == 0 {
            self.environment.insert(name.to_string(), value);
        } else {
            self.parent_environment
                .as_mut()
                .expect("Resolver and environment depth mismatch to set var")
                .borrow_mut()
                .set_at(name, value, depth - 1, kind)
        }
    }
}
