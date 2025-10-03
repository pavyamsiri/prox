use crate::environment::SharedEnvironment;
use core::convert;
use core::fmt;
use prox_interner::Symbol;
use prox_parser::ast::NodeIndex;
use std::sync::Arc;

/// A value.
#[derive(Debug, Clone)]
pub enum Value {
    /// Numbers.
    Number(f64),
    /// Nil.
    Nil,
    /// Booleans.
    Bool(bool),
    /// Strings.
    String(Arc<str>),
    /// Functions.
    Function(Arc<Function>),
}

impl Value {
    /// Return whether the value is truthy.
    #[must_use]
    pub fn truthy(&self) -> bool {
        match self {
            Value::Number(_) => todo!(),
            Value::Nil => todo!(),
            Value::Bool(val) => *val,
            Value::String(_) => todo!(),
            Value::Function(function) => todo!(),
        }
    }

    /// Evaluate less than.
    pub fn less_than(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs < rhs,
            _ => panic!("invalid operands"),
        }
    }

    /// Evaluate subtract.
    pub fn sub(&self, other: &Self) -> f64 {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs - rhs,
            _ => panic!("invalid operands"),
        }
    }

    /// Evaluate add.
    pub fn add(&self, other: &Self) -> f64 {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs + rhs,
            _ => panic!("invalid operands"),
        }
    }
}

impl convert::From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

impl convert::From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl convert::From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(value) => {
                write!(f, "{value}")
            }
            _ => {
                write!(f, "{self:?}")
            }
        }
    }
}

/// The type of function.
#[derive(Debug, Clone)]
pub enum FunctionKind {
    /// A normal function.
    Normal,
    /// A constructor.
    Constructor,
}

#[derive(Debug, Clone)]
pub struct Function {
    /// The name of the function.
    pub(crate) name: Symbol,
    /// The parameters.
    pub(crate) parameters: Vec<Symbol>,
    /// The function body.
    pub(crate) body: NodeIndex,
    /// The closure.
    pub(crate) closure: SharedEnvironment,
    /// The function type.
    pub(crate) kind: FunctionKind,
}
