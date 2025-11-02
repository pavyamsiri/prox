extern crate alloc;

/// The native function interface and builtins.
pub(crate) mod native;

use crate::{
    error::RuntimeErrorKind,
    gc::{AllocatorError, ArenaIndex, MarkColor, ObjectAllocator},
};
use alloc::rc::Rc;
use compact_str::CompactString;
use core::fmt;
use native::NativeFunction;
use prox_bytecode::{StackSlot, chunk::ChunkId, pool::ConstantInterner};
use prox_interner::Symbol;
use std::collections::HashMap;

/// A value.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Value {
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
    Native(ArenaIndex<Rc<dyn NativeFunction>>),
    /// Upvalues.
    Upvalue(ArenaIndex<Upvalue>),
    /// Classes.
    Class(ArenaIndex<Class>),
    /// Classes.
    Instance(ArenaIndex<Instance>),
}

impl Value {
    /// Execute add.
    #[inline]
    pub(crate) fn add(
        allocator: &mut ObjectAllocator,
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
    #[inline]
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
    #[inline]
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
    #[inline]
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
    #[inline]
    pub(crate) fn eq(allocator: &ObjectAllocator, lhs: &Self, rhs: &Self) -> bool {
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
    #[inline]
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
    #[inline]
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
    #[inline]
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
    #[inline]
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
    #[inline]
    #[must_use]
    pub(crate) const fn truthy(&self) -> bool {
        match *self {
            Value::Nil => false,
            Value::Bool(val) => val,
            _ => true,
        }
    }

    /// Evaluate numeric negation.
    #[inline]
    pub(crate) fn neg(&self) -> Result<Value, RuntimeErrorKind> {
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
        allocator: &'value ObjectAllocator,
        interner: &'value ConstantInterner,
    ) -> ResolvedValue<'value> {
        ResolvedValue {
            inner: self,
            allocator,
            interner,
        }
    }
}

pub(crate) trait Trace {
    /// Mark itself as being visited.
    fn mark(&self, context: &mut ObjectAllocator) -> Result<(), AllocatorError>;
    /// Blacken itself and mark its references.
    fn blacken(&self, context: &mut ObjectAllocator) -> Result<(), AllocatorError>;
}

impl Trace for Value {
    fn mark(&self, context: &mut ObjectAllocator) -> Result<(), AllocatorError> {
        match *self {
            Value::String(index) => {
                context.mark(index.into(), MarkColor::Grey)?;
            }
            Value::Closure(index) => {
                context.mark(index.into(), MarkColor::Grey)?;
            }
            Value::Method(index) => {
                context.mark(index.into(), MarkColor::Grey)?;
            }
            Value::Native(index) => {
                context.mark(index.into(), MarkColor::Grey)?;
            }
            Value::Upvalue(index) => {
                context.mark(index.into(), MarkColor::Grey)?;
            }
            Value::Class(index) => {
                context.mark(index.into(), MarkColor::Grey)?;
            }
            Value::Instance(index) => {
                context.mark(index.into(), MarkColor::Grey)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn blacken(&self, context: &mut ObjectAllocator) -> Result<(), AllocatorError> {
        match *self {
            Value::String(index) => {
                context.mark(index.into(), MarkColor::Black)?;
            }
            Value::Closure(index) => {
                context.mark(index.into(), MarkColor::Black)?;
                let upvalues = {
                    let closure = context.resolve_closure(index)?;

                    closure.upvalues.clone()
                };

                for upvalue in upvalues {
                    Value::Upvalue(upvalue).mark(context)?;
                }
            }
            Value::Method(index) => {
                context.mark(index.into(), MarkColor::Black)?;
                let (closure, receiver) = {
                    let method = context.resolve_method(index)?;

                    (method.closure, method.receiver)
                };

                Value::Closure(closure).mark(context)?;
                Value::Instance(receiver).mark(context)?;
            }
            Value::Native(index) => {
                context.mark(index.into(), MarkColor::Black)?;
            }
            Value::Upvalue(index) => {
                context.mark(index.into(), MarkColor::Black)?;
                let value = {
                    let upvalue = context.resolve_upvalue(index)?;

                    match *upvalue {
                        Upvalue::Closed { value } => value,
                        Upvalue::Open { .. } => return Ok(()),
                    }
                };
                value.mark(context)?;
            }
            Value::Class(index) => {
                context.mark(index.into(), MarkColor::Black)?;
                let methods: Vec<_> = {
                    let class = context.resolve_class(index)?;

                    class.methods.values().copied().collect()
                };

                for method in methods {
                    Value::Closure(method).mark(context)?;
                }
            }
            Value::Instance(index) => {
                context.mark(index.into(), MarkColor::Black)?;
                let (class, fields) = {
                    let instance = context.resolve_instance(index)?;

                    (
                        instance.class,
                        instance.fields.values().copied().collect::<Vec<_>>(),
                    )
                };

                Value::Class(class).mark(context)?;
                for field in fields {
                    field.mark(context)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}

/// A closure.
#[derive(Debug, Clone)]
pub(crate) struct Closure {
    /// The name of the function.
    pub(crate) name: Symbol,
    /// The chunk ID of the function body.
    pub(crate) chunk: ChunkId,
    /// The arity of the function.
    pub(crate) arity: u8,
    /// The upvalues.
    pub(crate) upvalues: Vec<ArenaIndex<Upvalue>>,
}

/// A class.
#[derive(Debug, Clone)]
pub(crate) struct Class {
    /// The name of the class.
    pub(crate) name: Symbol,
    /// The class' methods.
    pub(crate) methods: HashMap<Symbol, ArenaIndex<Closure>>,
}

/// An instance.
#[derive(Debug, Clone)]
pub(crate) struct Instance {
    /// The instance's class.
    pub(crate) class: ArenaIndex<Class>,
    /// The instance's fields.
    pub(crate) fields: HashMap<Symbol, Value>,
}

/// A bound method.
#[derive(Debug, Clone)]
pub(crate) struct Method {
    /// The closure.
    pub(crate) closure: ArenaIndex<Closure>,
    /// The bound instance.
    pub(crate) receiver: ArenaIndex<Instance>,
}

/// An upvalue.
#[derive(Debug, Clone)]
pub(crate) enum Upvalue {
    /// An upvalue that is still open i.e. on the stack.
    Open {
        /// The stack slot.
        slot: StackSlot,
        /// The next open upvalue.
        next: Option<ArenaIndex<Upvalue>>,
    },
    /// An upvalue that is closed i.e. on the heap.
    Closed {
        /// The closed value.
        value: Value,
    },
}

impl Closure {
    /// Resolve a closure for display formatting purposes.
    pub(crate) const fn resolve<'value>(
        &'value self,
        allocator: &'value ObjectAllocator,
        interner: &'value ConstantInterner,
    ) -> ResolvedClosure<'value> {
        ResolvedClosure {
            inner: self,
            allocator,
            interner,
        }
    }
}

impl Upvalue {
    /// Resolve an upvalue for display formatting purposes.
    pub(crate) const fn resolve<'value>(
        &'value self,
        allocator: &'value ObjectAllocator,
        interner: &'value ConstantInterner,
    ) -> ResolvedUpvalue<'value> {
        ResolvedUpvalue {
            inner: self,
            allocator,
            interner,
        }
    }
}

#[derive(Debug)]
pub(crate) struct ResolvedValue<'value> {
    /// The value to print.
    inner: &'value Value,
    /// The allocator to use to dereference references.
    allocator: &'value ObjectAllocator,
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
                let closure = self
                    .allocator
                    .resolve_closure(handle)
                    .map_err(|_err| fmt::Error)?;
                let name = self
                    .interner
                    .resolve_string(closure.name)
                    .ok_or(fmt::Error)?;
                write!(f, "<fn {name}>")
            }
            Value::Method(handle) => {
                let method = self
                    .allocator
                    .resolve_method(handle)
                    .map_err(|_err| fmt::Error)?;
                let closure = self
                    .allocator
                    .resolve_closure(method.closure)
                    .map_err(|_err| fmt::Error)?;
                let name = self
                    .interner
                    .resolve_string(closure.name)
                    .ok_or(fmt::Error)?;
                write!(f, "<fn {name}>")
            }
            Value::Native(handle) => {
                let native = self
                    .allocator
                    .resolve_native(handle)
                    .map_err(|_err| fmt::Error)?;
                write!(f, "<native fn {}>", native.name())
            }
            Value::String(handle) => {
                let string = self
                    .allocator
                    .resolve_string(handle)
                    .map_err(|_err| fmt::Error)?;
                write!(f, "{string}")
            }
            Value::Class(handle) => {
                let class = self
                    .allocator
                    .resolve_class(handle)
                    .map_err(|_err| fmt::Error)?;
                let name = self.interner.resolve_string(class.name).ok_or(fmt::Error)?;
                write!(f, "{name}")
            }
            Value::Instance(handle) => {
                let instance = self
                    .allocator
                    .resolve_instance(handle)
                    .map_err(|_err| fmt::Error)?;
                let class = self
                    .allocator
                    .resolve_class(instance.class)
                    .map_err(|_err| fmt::Error)?;
                let name = self.interner.resolve_string(class.name).ok_or(fmt::Error)?;
                write!(f, "<object {name}>")
            }
            Value::Upvalue(handle) => {
                let upvalue = self
                    .allocator
                    .resolve_upvalue(handle)
                    .map_err(|_err| fmt::Error)?;
                let upvalue = upvalue.resolve(self.allocator, self.interner);
                write!(f, "{upvalue:?}")
            }
            Value::Number(value) => write!(f, "{value}"),
            Value::Bool(value) => write!(f, "{value}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

/// A resolved closure for the purposes of display formatting.
#[derive(Debug)]
pub(crate) struct ResolvedClosure<'value> {
    /// The value to print.
    inner: &'value Closure,
    /// The allocator to use to dereference references.
    allocator: &'value ObjectAllocator,
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
                    .filter_map(|handle| self.allocator.resolve_upvalue(*handle).ok())
                    .map(|val| val.resolve(self.allocator, self.interner))
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

/// A resolved upvalue for the purposes of display formatting.
pub(crate) struct ResolvedUpvalue<'value> {
    /// The value to print.
    inner: &'value Upvalue,
    /// The allocator to use to dereference references.
    allocator: &'value ObjectAllocator,
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
