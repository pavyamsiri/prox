extern crate alloc;

use crate::native::{Clock, NativeFunction as _};
use crate::value::Value;
use alloc::rc::Rc;
use core::cell::RefCell;
use core::default;
use core::fmt;
use prox_interner::{Interner, Symbol};
use std::collections::HashMap;
use std::collections::hash_map::Entry;

/// Invalid assignment error.
#[derive(Debug)]
pub struct AssignmentError;

/// A shared environment.
#[derive(Debug, Clone)]
pub struct SharedEnvironment {
    /// The environment.
    inner: Rc<RefCell<Environment>>,
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
            inner: Rc::new(RefCell::new(Environment {
                values: globals,
                parent: None,
            })),
        }
    }
}

impl Environment {
    /// Declare a variable in the current scope.
    fn declare(&mut self, name: Symbol, value: Value) {
        self.values.insert(name, value);
    }

    /// Access a variable by symbol, returning its value if it has been declared.
    /// The variable accessed is the variable with the same symbol that is closest to the current scope.
    fn access(&self, name: Symbol) -> Option<Value> {
        #[expect(clippy::option_if_let_else, reason = "clearer to write it this way.")]
        if let Some(value) = self.values.get(&name) {
            Some(value.clone())
        } else if let Some(parent) = self.parent.clone() {
            parent.access(name)
        } else {
            None
        }
    }

    /// Assign a value to a variable by its symbol.
    /// The variable assigned is the variable with the same symbol that is closest to the current scope.
    ///
    /// # Errors
    /// This will fail if the variable has not been declared.
    fn assign(&mut self, name: Symbol, value: Value) -> Result<(), AssignmentError> {
        if let Entry::Occupied(mut entry) = self.values.entry(name) {
            entry.insert(value);
            Ok(())
        } else if let Some(mut parent) = self.parent.clone() {
            parent.assign(name, value)
        } else {
            Err(AssignmentError)
        }
    }
}

impl SharedEnvironment {
    /// Create a new shared environment with included builtins given an interner.
    pub fn with_interner(interner: &mut Interner) -> Self {
        let mut globals = HashMap::new();
        let clock = Clock;
        let clock_symbol = interner.intern(clock.name());
        globals.insert(clock_symbol, Value::NativeFunction(Rc::new(clock)));

        Self {
            inner: Rc::new(RefCell::new(Environment {
                values: globals,
                parent: None,
            })),
        }
    }

    /// Declare that a variable exists but is not yet defined.
    pub fn declare(&mut self, name: Symbol, value: Value) {
        let mut inner = self.inner.borrow_mut();
        inner.declare(name, value);
    }

    /// Create a new scope.
    #[must_use]
    pub fn new_scope(&self) -> Self {
        Self {
            inner: Rc::new(RefCell::new(Environment {
                values: HashMap::new(),
                parent: Some(self.clone()),
            })),
        }
    }

    /// Access a symbol.
    #[must_use]
    pub fn access(&self, name: Symbol) -> Option<Value> {
        let inner = self.inner.borrow();
        inner.access(name)
    }

    /// Access a global symbol.
    #[must_use]
    pub fn access_global(&self, name: Symbol) -> Option<Value> {
        let mut current = self.clone();
        loop {
            let parent = {
                let inner = current.inner.borrow();
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

    /// Access a symbol at the given depth.
    #[must_use]
    pub fn access_at(&self, name: Symbol, distance: usize) -> Option<Value> {
        let mut current = self.clone();
        for _ in 0..distance {
            let parent = {
                let inner = current.inner.borrow();
                inner.parent.clone()
            };
            current = parent?;
        }
        current.access(name)
    }

    /// Assign a value to a variable by its symbol.
    /// The variable assigned is the variable with the same symbol that is closest to the current scope.
    ///
    /// # Errors
    /// This will fail if the variable has not been declared.
    pub fn assign(&mut self, name: Symbol, value: Value) -> Result<(), AssignmentError> {
        let mut inner = self.inner.borrow_mut();
        inner.assign(name, value)
    }

    /// Assign a value to a variable by its symbol in the global scope.
    ///
    /// # Errors
    /// This will fail if the variable has not been declared.
    pub fn assign_global(&self, name: Symbol, value: Value) -> Result<(), AssignmentError> {
        let mut current = self.clone();
        loop {
            let parent = {
                let inner = current.inner.borrow();
                inner.parent.clone()
            };
            match parent {
                Some(parent) => {
                    current = parent;
                }
                None => break,
            }
        }
        current.assign(name, value)
    }

    /// Assign a value to a symbol at the given depth.
    ///
    /// # Errors
    /// This will fail if the variable has not been declared.
    pub fn assign_at(
        &mut self,
        name: Symbol,
        value: Value,
        distance: usize,
    ) -> Result<(), AssignmentError> {
        let mut current = self.clone();
        for _ in 0..distance {
            let parent = {
                let inner = current.inner.borrow();
                inner.parent.clone()
            };
            current = parent.ok_or(AssignmentError)?;
        }
        current.assign(name, value)
    }
}

impl fmt::Display for SharedEnvironment {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Collect environments from global to local
        fn collect_environments(env: &Environment, environments: &mut Vec<Environment>) {
            environments.push(env.clone());

            if let Some(ref parent) = env.parent {
                let parent_inner = parent.inner.borrow();
                collect_environments(&parent_inner, environments);
            }
        }

        // Collect all environments
        let inner = self.inner.borrow();
        let mut environments = Vec::new();
        collect_environments(&inner, &mut environments);

        // Print environments from global to local
        for (depth, env) in environments.into_iter().rev().enumerate() {
            if depth == 0 {
                // Global scope
                writeln!(f, "Global Scope:")?;
                #[expect(clippy::iter_over_hash_type, reason = "meant for debugging.")]
                for (name, value) in env.values.iter() {
                    writeln!(f, "  {name:?}: {value}")?;
                }
            } else {
                // Local scopes
                writeln!(f, "{:indent$}Local Scope:", "", indent = depth * 2 + 1)?;
                #[expect(clippy::iter_over_hash_type, reason = "meant for debugging.")]
                for (name, value) in env.values.iter() {
                    writeln!(
                        f,
                        "{:indent$}  {:?}: {}",
                        "",
                        name,
                        value,
                        indent = depth * 2 + 1
                    )?;
                }
            }
        }

        Ok(())
    }
}
