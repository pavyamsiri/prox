extern crate alloc;

use crate::environment::SharedEnvironment;
use crate::error::RuntimeErrorKind;
use crate::native::NativeFunction;
use alloc::rc::Rc;
use core::cell::RefCell;
use core::convert;
use core::fmt;
use prox_interner::Interner;
use prox_interner::Symbol;
use prox_parser::ast::NodeIndex;
use std::collections::HashMap;

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
    String(Rc<str>),
    /// Functions.
    Function(Rc<Function>),
    /// Native functions.
    NativeFunction(Rc<dyn NativeFunction>),
    /// Classes.
    Class(Rc<Class>),
    /// Instances.
    Instance(Rc<Instance>),
}

impl Value {
    /// Resolve a value's symbols.
    pub const fn resolve<'value>(
        &'value self,
        interner: &'value Interner,
    ) -> ResolvedValue<'value> {
        ResolvedValue {
            inner: self,
            interner,
        }
    }

    /// Return whether the value is truthy.
    #[must_use]
    pub const fn truthy(&self) -> bool {
        match *self {
            Value::Nil => false,
            Value::Bool(val) => val,
            _ => true,
        }
    }

    /// Evaluate subtract.
    pub fn sub(&self, other: &Self) -> Result<Value, RuntimeErrorKind> {
        match (self, other) {
            (&Value::Number(lhs), &Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate divide.
    pub fn div(&self, other: &Self) -> Result<Value, RuntimeErrorKind> {
        match (self, other) {
            (&Value::Number(lhs), &Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate multiply.
    pub fn mul(&self, other: &Self) -> Result<Value, RuntimeErrorKind> {
        match (self, other) {
            (&Value::Number(lhs), &Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate add.
    pub fn add(&self, other: &Self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
            (Value::String(lhs), Value::String(rhs)) => {
                let mut buffer = lhs.to_string();
                buffer.push_str(rhs);
                Ok(Value::String(buffer.into()))
            }
            _ => Err(RuntimeErrorKind::InvalidAddOperands),
        }
    }

    /// Evaluate equality.
    pub fn eq(&self, other: &Self) -> bool {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Function(lhs), Value::Function(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::NativeFunction(lhs), Value::NativeFunction(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Class(lhs), Value::Class(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Instance(lhs), Value::Instance(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }

    /// Evaluate less than.
    pub fn less_than(&self, other: &Self) -> Result<Value, RuntimeErrorKind> {
        match (self, other) {
            (&Value::Number(lhs), &Value::Number(rhs)) => Ok(Value::Bool(lhs < rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate less than or equal.
    pub fn less_than_or_equal(&self, other: &Self) -> Result<Value, RuntimeErrorKind> {
        match (self, other) {
            (&Value::Number(lhs), &Value::Number(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate greater than.
    pub fn greater_than(&self, other: &Self) -> Result<Value, RuntimeErrorKind> {
        match (self, other) {
            (&Value::Number(lhs), &Value::Number(rhs)) => Ok(Value::Bool(lhs > rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate greater than or equal.
    pub fn greater_than_or_equal(&self, other: &Self) -> Result<Value, RuntimeErrorKind> {
        match (self, other) {
            (&Value::Number(lhs), &Value::Number(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate numeric negation.
    pub fn neg(&self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match self {
            Value::Number(val) => Ok(Value::Number(-val)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperand),
        }
    }

    /// Evaluate boolean not.
    pub const fn not(&self) -> bool {
        !self.truthy()
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
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::Number(value) => {
                write!(f, "{value}")
            }
            Value::Bool(value) => {
                write!(f, "{value}")
            }
            Value::String(ref value) => {
                write!(f, "{value}")
            }
            Value::NativeFunction(ref func) => {
                write!(f, "<native fn {}>", func.name())
            }
            Value::Function(ref func) => {
                write!(f, "<fn {:?}>", func.name)
            }
            Value::Nil => {
                write!(f, "nil")
            }
            Value::Class(ref class) => write!(f, "<class {:?}>", class.name),
            Value::Instance(ref instance) => write!(f, "<object {:?}>", instance.class.name),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedValue<'value> {
    /// The value to print.
    inner: &'value Value,
    /// The interner of the symbols.
    interner: &'value Interner,
}

impl fmt::Display for ResolvedValue<'_> {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self.inner {
            Value::Class(ref class) => {
                let name = self.interner.resolve(class.name).ok_or(fmt::Error)?;
                write!(f, "{name}")
            }
            Value::Instance(ref instance) => {
                let name = self
                    .interner
                    .resolve(instance.class.name)
                    .ok_or(fmt::Error)?;
                write!(f, "<object {name}>")
            }
            Value::Function(ref func) => {
                let name = self.interner.resolve(func.name).ok_or(fmt::Error)?;
                write!(f, "<fn {name}>")
            }
            ref value => {
                write!(f, "{value}")
            }
        }
    }
}

/// The type of function.
#[derive(Debug, Clone)]
pub(crate) enum FunctionKind {
    /// A normal function.
    Normal,
    /// A constructor.
    Constructor,
}

#[derive(Clone)]
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

impl Function {
    pub(crate) fn bind(&self, this_sym: Symbol, value: Rc<Instance>) -> Function {
        let mut bound_function = self.clone();
        let mut new_closure = bound_function.closure.new_scope();
        new_closure.declare(this_sym, Value::Instance(value));
        bound_function.closure = new_closure;
        bound_function
    }
}

impl fmt::Debug for Function {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.name)
            .field("parameters", &self.parameters)
            .field("body", &self.body)
            .field("kind", &self.kind)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    /// The name of the class.
    pub(crate) name: Symbol,
    /// The methods.
    pub(crate) methods: Vec<Rc<Function>>,
    /// The super class.
    #[expect(
        clippy::struct_field_names,
        reason = "can't remove class without using `r#super`."
    )]
    pub(crate) super_class: Option<Rc<Class>>,
}

impl Class {
    pub(crate) fn find_method(&self, name: Symbol) -> Option<&Function> {
        if let Some(method) = self.methods.iter().find(|meth| meth.name == name) {
            return Some(method);
        }

        let super_class = self.super_class.as_ref()?;
        super_class.find_method(name)
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    /// The class.
    pub(crate) class: Rc<Class>,
    /// The fields.
    pub(crate) fields: Rc<RefCell<HashMap<Symbol, Value>>>,
}

impl Instance {
    pub fn new(class: Rc<Class>) -> Self {
        Self {
            class,
            fields: Rc::new(RefCell::new(HashMap::new())),
        }
    }
}
