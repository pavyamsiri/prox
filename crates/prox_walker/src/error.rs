//! Runtime errors.
use core::fmt;

use prox_interner::{Interner, Symbol};
use prox_parser::Span;

/// A runtime error.
#[derive(Debug)]
pub struct RuntimeError {
    /// The type of runtime error.
    pub kind: RuntimeErrorKind,
    /// The offending span.
    pub span: Span,
}

/// The type of runtime error.
#[derive(Debug)]
pub enum RuntimeErrorKind {
    /// Attempted to access a symbol that hasn't been declared.
    InvalidAccess,
    /// Attempted to assign a value to a symbol that hasn't been declared.
    InvalidAssign,
    /// Attempted to call a value that can't be called.
    InvalidCallee,
    /// Attempted to call a callable with the wrong number of arguments.
    InvalidArgumentCount {
        /// The expected number of arguments.
        expected: u8,
        /// The actual number of arguments.
        actual: usize,
    },
    /// Visited a non-statement node when a statement node was expected.
    InvalidStatement,
    /// Visited a non-expression node when an expression node was expected.
    InvalidExpr,
    /// Attempted to use `.field` syntax on a value that isn't an instance.
    InvalidGet,
    /// Attempted to use `.field = value` syntax on a value that isn't an instance.
    InvalidSet,
    /// Attempted to subclass an identifier that doesn't correspond to a class.
    InvalidSuperClass,
    /// Attempted to use an arithmetic unary operator on a non-arithmetic value.
    NonArithmeticOperand,
    /// Attempted to use an arithmetic binary operator on non-arithmetic values.
    NonArithmeticOperands,
    /// Attempted to use the add binary operator on values that can't be added together.
    InvalidAddOperands,
    /// Attempted to access a field that is not defined on its instance.
    UndefinedField {
        /// The symbol of the field.
        name: Symbol,
    },
    /// Failed to resolve a symbol to its string.
    InvalidSymbol,
    /// Error when writing to IO.
    Io,

    // These errors should never happen assuming parsing has already
    // caught all non-runtime checkable errors.
    /// Encountered a null node.
    NullNode,
    /// Returning from a non function scope.
    ReturnFromNonFunction,
    /// `super` is not a class.
    NonClassSuper,
    /// `this` is not a class.
    NonInstanceThis,
}

impl RuntimeError {
    /// Format a runtime error as a single line into the given buffer.
    ///
    /// # Errors
    /// This function will error if it can not write into the buffer.
    pub fn format(
        &self,
        source: &str,
        buffer: &mut impl fmt::Write,
        interner: &Interner,
    ) -> Result<(), fmt::Error> {
        let lexeme = &source[self.span.range()];
        match self.kind {
            RuntimeErrorKind::InvalidAccess | RuntimeErrorKind::InvalidAssign => {
                write!(buffer, "Undefined variable '{lexeme}'.")
            }
            RuntimeErrorKind::InvalidCallee => {
                write!(buffer, "Can only call functions and classes.")
            }
            RuntimeErrorKind::InvalidArgumentCount { expected, actual } => {
                write!(buffer, "Expected {expected} arguments but got {actual}.")
            }
            RuntimeErrorKind::InvalidGet => write!(buffer, "Only instances have properties."),
            RuntimeErrorKind::InvalidSet => write!(buffer, "Only instances have fields."),
            RuntimeErrorKind::InvalidSuperClass => write!(buffer, "Superclass must be a class."),
            RuntimeErrorKind::NonArithmeticOperand => {
                write!(buffer, "Operand must be a number.")
            }
            RuntimeErrorKind::NonArithmeticOperands => {
                write!(buffer, "Operands must be numbers.")
            }
            RuntimeErrorKind::InvalidAddOperands => {
                write!(buffer, "Operands must be two numbers or two strings.")
            }
            RuntimeErrorKind::UndefinedField { name } => {
                let name = interner.resolve(name).ok_or(fmt::Error)?;
                write!(buffer, "Undefined property '{name}'.")
            }
            RuntimeErrorKind::Io => write!(buffer, "Encountered IO error."),
            RuntimeErrorKind::NonInstanceThis => write!(buffer, "`this` was not an instance."),
            RuntimeErrorKind::NonClassSuper => write!(buffer, "`super` was not a class."),
            RuntimeErrorKind::ReturnFromNonFunction => {
                write!(buffer, "Returning from a non-function scope.")
            }
            RuntimeErrorKind::NullNode => write!(buffer, "Visited a null node."),
            RuntimeErrorKind::InvalidStatement => write!(buffer, "Visited an invalid statement."),
            RuntimeErrorKind::InvalidExpr => write!(buffer, "Visited an invalid expression."),
            RuntimeErrorKind::InvalidSymbol => {
                write!(buffer, "Interned symbol could not be resolved.")
            }
        }
    }
}
