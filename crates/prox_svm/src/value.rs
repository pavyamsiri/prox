pub mod native;

use crate::{
    error::RuntimeErrorKind,
    gc::{ArenaIndex, ValueAllocator as GcContext},
};
use compact_str::CompactString;
use core::fmt;
use native::NativeFunction;
use prox_bytecode::{StackSlot, chunk::ChunkId, pool::ConstantInterner};
use prox_interner::Symbol;
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
    String(ArenaIndex<CompactString>),
    /// Closures.
    Closure(ArenaIndex<Closure>),
    /// Methods.
    Method(ArenaIndex<Method>),
    /// Native functions.
    Native(ArenaIndex<Box<dyn NativeFunction>>),
    /// Upvalues.
    Upvalue(ArenaIndex<Upvalue>),
    /// Classes.
    Class(ArenaIndex<Class>),
    /// Classes.
    Instance(ArenaIndex<Instance>),
}

impl Value {
    /// Execute add.
    pub(crate) fn add(
        allocator: &mut GcContext,
        lhs: &Self,
        rhs: &Self,
    ) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
            (Value::String(lhs), Value::String(rhs)) => {
                let lhs = allocator.resolve_string(*lhs).unwrap();
                let rhs = allocator.resolve_string(*rhs).unwrap();
                let mut concat = CompactString::new(lhs);
                concat.push_str(rhs);
                Ok(Value::String(allocator.make_string_from_owned(concat)))
            }
            _ => Err(RuntimeErrorKind::InvalidAddOperands),
        }
    }

    /// Execute subtraction.
    pub(crate) fn sub(&self, rhs: &Self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Execute multiplication.
    pub(crate) fn mul(&self, rhs: &Self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Execute division.
    pub(crate) fn div(&self, rhs: &Self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate equality.
    pub(crate) fn eq(allocator: &GcContext, lhs: &Self, rhs: &Self) -> bool {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => {
                let lhs = allocator.resolve_string(*lhs);
                let rhs = allocator.resolve_string(*rhs);
                lhs == rhs
            }
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs == rhs,
            (Value::Native(lhs), Value::Native(rhs)) => lhs == rhs,
            (Value::Class(lhs), Value::Class(rhs)) => lhs == rhs,
            (Value::Instance(lhs), Value::Instance(rhs)) => lhs == rhs,
            (Value::Method(lhs), Value::Method(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    /// Evaluate less than.
    pub(crate) fn less_than(&self, rhs: &Self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs < rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate less than or equal.
    pub(crate) fn less_than_or_equal(&self, rhs: &Self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate greater than.
    pub(crate) fn greater_than(&self, rhs: &Self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs > rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
        }
    }

    /// Evaluate greater than or equal.
    pub(crate) fn greater_than_or_equal(&self, rhs: &Self) -> Result<Value, RuntimeErrorKind> {
        #[expect(
            clippy::pattern_type_mismatch,
            reason = "impossible to satisfy along with `needless_borrowed_reference`."
        )]
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            _ => Err(RuntimeErrorKind::NonArithmeticOperands),
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
}

impl Value {
    pub(crate) const fn resolve<'value>(
        &'value self,
        allocator: &'value GcContext,
        interner: &'value ConstantInterner,
    ) -> ResolvedValue<'value> {
        ResolvedValue {
            inner: self,
            allocator,
            interner,
        }
    }
}

pub trait Trace {
    fn mark(&self, context: &mut GcContext);
}

impl Trace for Value {
    fn mark(&self, context: &mut GcContext) {
        match *self {
            Value::String(string) => {
                context.mark_string(string);
            }
            Value::Closure(closure) => {
                context.mark_closure(closure);
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    /// The name of the function.
    pub(crate) name: Symbol,
    /// The chunk ID of the function body.
    pub(crate) chunk: ChunkId,
    /// The arity of the function.
    pub(crate) arity: u8,
    /// The upvalues.
    pub(crate) upvalues: Vec<ArenaIndex<Upvalue>>,
}

impl Closure {
    pub(crate) const fn resolve<'value>(
        &'value self,
        allocator: &'value GcContext,
        interner: &'value ConstantInterner,
    ) -> ResolvedClosure<'value> {
        ResolvedClosure {
            inner: self,
            allocator,
            interner,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub(crate) closure: ArenaIndex<Closure>,
    pub(crate) receiver: ArenaIndex<Instance>,
}

#[derive(Debug, Clone)]
pub enum Upvalue {
    Open {
        slot: StackSlot,
        next: Option<ArenaIndex<Upvalue>>,
    },
    Closed {
        value: Value,
    },
}

impl Upvalue {
    pub(crate) const fn resolve<'value>(
        &'value self,
        allocator: &'value GcContext,
        interner: &'value ConstantInterner,
    ) -> ResolvedUpvalue<'value> {
        ResolvedUpvalue {
            inner: self,
            allocator,
            interner,
        }
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
            Value::Nil => {
                write!(f, "nil")
            }
            Value::String(ref value) => {
                write!(f, "{value:?}")
            }
            Value::Closure(ref closure) => {
                write!(f, "<fn {closure:?}>")
            }
            Value::Native(ref native) => {
                write!(f, "<native fn {native:?}>")
            }
            Value::Upvalue(ref upvalue) => write!(f, "<upvalue {upvalue:?}>"),
            Value::Class(ref class) => write!(f, "<class {class:?}>"),
            Value::Instance(ref instance) => write!(f, "<instance {instance:?}>"),
            Value::Method(ref method) => write!(f, "<fn {method:?}>"),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedValue<'value> {
    /// The value to print.
    inner: &'value Value,
    /// The allocator to use to dereference references.
    allocator: &'value GcContext,
    /// The constant interner.
    interner: &'value ConstantInterner,
}

impl fmt::Display for ResolvedValue<'_> {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self.inner {
            Value::Closure(handle) => {
                let closure = self.allocator.resolve_closure(handle).ok_or(fmt::Error)?;
                let name = self
                    .interner
                    .resolve_string(closure.name)
                    .ok_or(fmt::Error)?;
                write!(f, "<fn {name}>")
            }
            Value::Method(handle) => {
                let method = self.allocator.resolve_method(handle).ok_or(fmt::Error)?;
                let closure = self
                    .allocator
                    .resolve_closure(method.closure)
                    .ok_or(fmt::Error)?;
                let name = self
                    .interner
                    .resolve_string(closure.name)
                    .ok_or(fmt::Error)?;
                write!(f, "<fn {name}>")
            }
            Value::Native(handle) => {
                let native = self.allocator.resolve_native(handle).ok_or(fmt::Error)?;
                write!(f, "<native fn {}>", native.name())
            }
            Value::String(handle) => {
                let string = self.allocator.resolve_string(handle).ok_or(fmt::Error)?;
                write!(f, "{string}")
            }
            Value::Class(handle) => {
                let class = self.allocator.resolve_class(handle).ok_or(fmt::Error)?;
                let name = self.interner.resolve_string(class.name).ok_or(fmt::Error)?;
                write!(f, "{name}")
            }
            Value::Instance(handle) => {
                let instance = self.allocator.resolve_instance(handle).ok_or(fmt::Error)?;
                let class = self
                    .allocator
                    .resolve_class(instance.class)
                    .ok_or(fmt::Error)?;
                let name = self.interner.resolve_string(class.name).ok_or(fmt::Error)?;
                write!(f, "<object {name}>")
            }
            Value::Upvalue(handle) => {
                let upvalue = self.allocator.resolve_upvalue(handle).ok_or(fmt::Error)?;
                match *upvalue {
                    Upvalue::Open { slot, next } => match next {
                        Some(next_index) => {
                            write!(
                                f,
                                "<open upvalue ${}, next = {next_index:?}>",
                                slot.to_usize()
                            )
                        }
                        None => {
                            write!(f, "<open upvalue ${}>", slot.to_usize())
                        }
                    },
                    Upvalue::Closed { ref value } => {
                        write!(
                            f,
                            "<closed upvalue {}>",
                            value.resolve(self.allocator, self.interner)
                        )
                    }
                }
            }
            ref value => {
                write!(f, "{value}")
            }
        }
    }
}

#[derive(Debug)]
pub struct ResolvedClosure<'value> {
    /// The value to print.
    inner: &'value Closure,
    /// The allocator to use to dereference references.
    allocator: &'value GcContext,
    /// The constant interner.
    interner: &'value ConstantInterner,
}

impl fmt::Display for ResolvedClosure<'_> {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Closure")
            .field(
                "name",
                &self
                    .interner
                    .resolve_string(self.inner.name)
                    .ok_or(fmt::Error)?,
            )
            .field("chunk", &self.inner.chunk)
            .field("arity", &self.inner.arity)
            .field(
                "upvalues",
                &self
                    .inner
                    .upvalues
                    .iter()
                    .filter_map(|handle| self.allocator.resolve_upvalue(*handle))
                    .map(|val| val.resolve(self.allocator, self.interner))
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

pub struct ResolvedUpvalue<'value> {
    /// The value to print.
    inner: &'value Upvalue,
    /// The allocator to use to dereference references.
    allocator: &'value GcContext,
    /// The constant interner.
    interner: &'value ConstantInterner,
}

impl fmt::Debug for ResolvedUpvalue<'_> {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self.inner {
            Upvalue::Open { slot, next } => f
                .debug_struct("Upvalue::Open")
                .field("slot", &slot.to_usize())
                .field("next", &next)
                .finish(),
            Upvalue::Closed { ref value } => f
                .debug_struct("Upvalue::Closed")
                .field(
                    "value",
                    &format_args!("{}", value.resolve(self.allocator, self.interner)),
                )
                .finish(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    /// The name of the class.
    pub(crate) name: Symbol,
    /// The class' methods.
    pub(crate) methods: HashMap<Symbol, ArenaIndex<Closure>>,
}

#[derive(Debug, Clone)]
pub struct Instance {
    /// The instance's class.
    pub(crate) class: ArenaIndex<Class>,
    pub(crate) fields: HashMap<Symbol, Value>,
}
