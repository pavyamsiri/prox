enum TreeKind {
    /// An expression atom/literal.
    ExprAtom,
    /// An expression group i.e. `(expr)`.
    ExprGroup,
    /// A prefixed/unary expression i.e. `!expr`.
    ExprPrefix,
    /// An infix/binary expression i.e. `expr + expr`.
    ExprInfix,
    /// An infix/binary assignment expression i.e. `expr = expr`.
    ExprInfixAssignment,
    /// An infix/binary short circuitable expression i.e. `expr and expr`.
    ExprInfixShortCircuit,
    /// An expression call `expr(params)`.
    ExprCall,
    /// An expression get `expr.field`.
    ExprGet,
    /// An expression set `expr.field = value`.
    ExprSet,

    /// The topmost level structure.
    Program,

    // Statements

    // Error trees.
    /// The parser has encountered an error.
    Error,
}
