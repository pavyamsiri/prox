use prox_interner::Symbol;

use crate::value::Value;
use core::default;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// A shared environment.
#[derive(Debug, Clone)]
pub struct SharedEnvironment {
    /// The environment.
    inner: Arc<Mutex<Environment>>,
}

/// An environment.
#[derive(Debug, Clone)]
struct Environment {
    /// The values in the environment.
    values: HashMap<Symbol, Value>,
    /// The parent environment.
    parent: Option<SharedEnvironment>,
}

impl default::Default for SharedEnvironment {
    fn default() -> Self {
        let globals = HashMap::new();
        Self {
            inner: Arc::new(Mutex::new(Environment {
                values: globals,
                parent: None,
            })),
        }
    }
}

impl Environment {
    /// Declare that a variable exists but is not yet defined.
    pub fn declare(&mut self, name: Symbol, value: Value) {
        self.values.insert(name, value);
    }

    pub fn access(&self, name: &Symbol) -> Option<Value> {
        if let Some(value) = self.values.get(name) {
            Some(value.clone())
        } else if let Some(parent) = self.parent.clone() {
            parent.access(name)
        } else {
            None
        }
    }
}

impl SharedEnvironment {
    /// Declare that a variable exists but is not yet defined.
    pub fn declare(&mut self, name: Symbol, value: Value) {
        let mut inner = self.inner.lock().unwrap();
        inner.declare(name, value)
    }

    /// Create a new scope.
    pub fn new_scope(&self) -> Self {
        Self {
            inner: Arc::new(Mutex::new(Environment {
                values: HashMap::new(),
                parent: Some(self.clone()),
            })),
        }
    }

    pub fn access(&self, name: &Symbol) -> Option<Value> {
        let inner = self.inner.lock().unwrap();
        inner.access(name)
    }

    pub fn access_global(&self, name: &Symbol) -> Option<Value> {
        let mut current = self.clone();
        loop {
            let parent = {
                let inner = current.inner.lock().unwrap();
                inner.parent.clone()
            };
            match parent {
                Some(parent) => {
                    current = parent;
                }
                None => break,
            }
        }
        current.access(name)
    }

    pub fn access_at(&self, name: &Symbol, distance: usize) -> Option<Value> {
        let mut current = self.clone();
        for _ in 0..distance {
            let parent = {
                let inner = current.inner.lock().unwrap();
                inner.parent.clone()
            };
            current = parent?;
        }
        current.access(name)
    }
}
