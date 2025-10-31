//! Errors encountered during runtime.

use crate::gc::AllocatorError;
use crate::gc::Generation;
use core::convert;
use core::fmt;
use prox_interner::{Interner, Symbol};
use prox_span::Span;

/// A runtime error.
#[derive(Debug)]
pub struct RuntimeError {
    /// The type of runtime error.
    pub kind: RuntimeErrorKind,
    /// The offending span.
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum RuntimeErrorKind {
    /// Attempted to access a symbol that hasn't been declared.
    InvalidAccess(Symbol),
    /// Attempted to use an arithmetic unary operator on a non-arithmetic value.
    NonArithmeticOperand,
    /// Attempted to use an arithmetic binary operator on non-arithmetic values.
    NonArithmeticOperands,
    /// Attempted to use the add binary operator on values that can't be added together.
    InvalidAddOperands,
    /// Attempted to call a value that can't be called.
    InvalidCallee,
    /// Attempted to call a callable with the wrong number of arguments.
    InvalidArgumentCount {
        /// The expected number of arguments.
        expected: u8,
        /// The actual number of arguments.
        actual: u8,
    },
    /// Attempted to apply class inheritance to a non-class value..
    InvalidSubClass,
    /// Attempted to subclass an identifier that doesn't correspond to a class.
    InvalidSuperClass,
    /// Attempted to use `.field` syntax on a value that isn't an instance.
    InvalidGet,
    /// Attempted to use `.field = value` syntax on a value that isn't an instance.
    InvalidSet,
    /// Attempted to access a field that is not defined on its instance.
    UndefinedField {
        /// The symbol of the field.
        name: Symbol,
    },
    // These errors can only occur if the VM or compiler is misimplemented.
    /// Segfault.
    Segfault,
    /// Not a instance.
    InvalidInstance,
    /// Call frame base is not a closure.
    InvalidCallFrame,
    /// Open upvalue link list contains closed upvalues.
    InvalidOpenUpvalue,
    /// Attempted to resolve a constant using an invalid symbol.
    InvalidSymbol {
        /// The invalid symbol.
        symbol: u32,
        /// The name of the interner.
        name: &'static str,
    },
    /// Attempted to dereference an arena index that is invalid; index is out of bounds.
    OutOfBoundsDereference {
        /// The out of bounds index.
        index: usize,
        /// The name of the arena.
        name: &'static str,
    },
    /// Attempted to dereference an arena index that is invalid; entry is free.
    FreeDereference {
        /// The index to the free entry.
        index: usize,
        /// The name of the arena.
        name: &'static str,
    },
    /// Attempted to dereference an arena index that is invalid; index's generation is out of date.
    WrongGenerationDereference {
        /// The entry's generation.
        expected: Generation,
        /// The handle's generation.
        actual: Generation,
        /// The name of the arena.
        name: &'static str,
    },
    /// Attempted to dereference a chunk but got nothing..
    InvalidChunkDereference,
    /// Attempted to attach a non-method value to a class.
    InvalidMethodAttach,
    /// Attempted to attach a method to a non-class. value.
    InvalidClassAttach,
    /// Popping from an empty stack.
    EmptyStack,
    /// Popping from an empty call stack.
    EmptyCallStack,
    /// Failed IO operation.
    Io,
}

impl RuntimeError {
    /// Format a runtime error as a single line into the given buffer.
    ///
    /// # Errors
    /// This function will error if it can not write into the buffer.
    pub fn format(
        &self,
        buffer: &mut impl fmt::Write,
        interner: &Interner,
    ) -> Result<(), fmt::Error> {
        match self.kind {
            RuntimeErrorKind::InvalidAccess(symbol) => {
                let invalid_name = interner.resolve(symbol).ok_or(fmt::Error)?;
                write!(buffer, "Undefined variable '{invalid_name}'.")
            }
            RuntimeErrorKind::NonArithmeticOperand => {
                write!(buffer, "Operand must be a number.")
            }
            RuntimeErrorKind::NonArithmeticOperands => {
                write!(buffer, "Operands must be numbers.")
            }
            RuntimeErrorKind::InvalidAddOperands => {
                write!(buffer, "Operands must be two numbers or two strings.")
            }
            RuntimeErrorKind::InvalidCallee => {
                write!(buffer, "Can only call functions and classes.")
            }
            RuntimeErrorKind::InvalidArgumentCount { expected, actual } => {
                write!(buffer, "Expected {expected} arguments but got {actual}.")
            }
            RuntimeErrorKind::InvalidSubClass => write!(buffer, "Subclass be a class."),
            RuntimeErrorKind::InvalidSuperClass => write!(buffer, "Superclass must be a class."),
            RuntimeErrorKind::InvalidGet => write!(buffer, "Only instances have properties."),
            RuntimeErrorKind::InvalidSet => write!(buffer, "Only instances have fields."),
            RuntimeErrorKind::UndefinedField { name } => {
                let name = interner.resolve(name).ok_or(fmt::Error)?;
                write!(buffer, "Undefined property '{name}'.")
            }
            RuntimeErrorKind::Segfault => {
                write!(buffer, "Segfault!")
            }
            RuntimeErrorKind::InvalidInstance => write!(buffer, "Invalid instance."),
            RuntimeErrorKind::InvalidMethodAttach => write!(buffer, "Invalid method to attach."),
            RuntimeErrorKind::InvalidClassAttach => write!(buffer, "Invalid class to attach to."),
            RuntimeErrorKind::InvalidCallFrame => write!(buffer, "Invalid call frame."),
            RuntimeErrorKind::InvalidOpenUpvalue => write!(buffer, "Invalid open upvalue."),
            RuntimeErrorKind::InvalidChunkDereference => {
                write!(buffer, "Invalid chunk dereference.")
            }
            RuntimeErrorKind::EmptyStack => write!(buffer, "Empty stack."),
            RuntimeErrorKind::EmptyCallStack => write!(buffer, "Empty call stack."),
            RuntimeErrorKind::Io => write!(buffer, "Failed IO operation."),
            RuntimeErrorKind::OutOfBoundsDereference { index, name } => {
                write!(
                    buffer,
                    "{name}: Index {index} is an out of bounds dereference."
                )
            }
            RuntimeErrorKind::FreeDereference { index, name } => {
                write!(
                    buffer,
                    "{name}: Index {index} is a dereference of a freed entry."
                )
            }
            RuntimeErrorKind::WrongGenerationDereference {
                expected,
                actual,
                name,
            } => {
                write!(
                    buffer,
                    "{name}: Dereference with outdated handle. Expected {} but got {}.",
                    expected.raw(),
                    actual.raw(),
                )
            }
            RuntimeErrorKind::InvalidSymbol { symbol, name } => {
                write!(buffer, "{name}: Failed to resolve the symbol {symbol}.")
            }
        }
    }
}

impl convert::From<AllocatorError> for RuntimeErrorKind {
    fn from(value: AllocatorError) -> Self {
        match value {
            AllocatorError::OutOfBounds { index, name } => {
                RuntimeErrorKind::OutOfBoundsDereference { index, name }
            }
            AllocatorError::WrongGeneration {
                expected,
                actual,
                name,
            } => RuntimeErrorKind::WrongGenerationDereference {
                expected,
                actual,
                name,
            },
            AllocatorError::Free { index, name } => {
                RuntimeErrorKind::FreeDereference { index, name }
            }
        }
    }
}
