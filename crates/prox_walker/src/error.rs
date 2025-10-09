//! Runtime errors.
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
    InvalidAccess,
    InvalidCallee,
    InvalidArgumentCount,
    InvalidStatement,
    InvalidExpr,
    /// Error when writing to IO.
    Io,
    // These errors should never happen assuming parsing has already
    // caught all non-runtime checkable errors.
    /// Encountered a null node.
    NullNode,
    /// Returning from a non function scope.
    ReturnFromNonFunction,
}
