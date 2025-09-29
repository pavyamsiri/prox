pub mod typed_trees;

use core::fmt;
use prox_lexer::token::Token;
use prox_lexer::{Lexer, SourceCode};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TreeKind {
    /// A atom/literal.
    ExprThis,
    /// An identifier.
    ExprIdent,
    /// Literal true.
    ExprTrue,
    /// Literal false.
    ExprFalse,
    /// Literal nil.
    ExprNil,
    /// String literal.
    ExprStringLiteral,
    /// Numeric literal.
    ExprNumericLiteral,
    /// An super method call i.e. `super.method`.
    ExprSuperCall,
    /// An expression group i.e. `(expr)`.
    ExprGroup,

    /// An infix/binary assignment expression i.e. `expr = expr`.
    ExprBinaryAssignment,
    /// An expression call `expr(params)`.
    ExprCall,
    /// An expression get `expr.field`.
    ExprGet,
    /// An expression set `expr.field = value`.
    ExprSet,

    // Unary expressions.
    /// An expression prefixed with `!`, `!expr`.
    ExprUnaryBang,
    /// An expression prefixed with `-`, `-expr`.
    ExprUnaryMinus,

    // Binary expressions.
    /// A binary expression with `+`, `left + right`.
    ExprBinaryPlus,
    /// A binary expression with `-`, `left - right`.
    ExprBinaryMinus,
    /// A binary expression with `*`, `left * right`.
    ExprBinaryStar,
    /// A binary expression with `/`, `left / right`.
    ExprBinarySlash,
    /// A binary expression with `<`, `left < right`.
    ExprBinaryLess,
    /// A binary expression with `<=`, `left <= right`.
    ExprBinaryLessEqual,
    /// A binary expression with `>`, `left > right`.
    ExprBinaryGreater,
    /// A binary expression with `>=`, `left >= right`.
    ExprBinaryGreaterEqual,
    /// A binary expression with `==`, `left == right`.
    ExprBinaryEqualEqual,
    /// A binary expression with `!=`, `left != right`.
    ExprBinaryBangEqual,
    /// A binary expression with `and`, `left and right`.
    ExprBinaryAnd,
    /// A binary expression with `or`, `left or right`.
    ExprBinaryOr,

    /// The topmost level structure.
    Program,

    // Statements
    /// A function declaration.
    StmtFnDecl,
    /// A variable declaration.
    StmtVarDecl,
    /// A class declaration.
    StmtClassDecl,
    /// A method declaration.
    StmtMethodDecl,
    /// A block.
    StmtBlock,
    /// A return statement.
    StmtReturn,
    /// An expression statement.
    StmtExpr,
    /// A print statement.
    StmtPrint,
    /// An if statement.
    StmtIf,
    /// A while statement.
    StmtWhile,
    /// A for statement.
    StmtFor,

    // Miscellaneous
    /// A list of parameters in a function declaration.
    ParamList,
    /// A parameter in a parameter list.
    Param,
    /// A variable declaration's initializer.
    VarDeclInitializer,
    /// A list of arguments in a function call.
    ArgList,
    /// An argument in an argument list.
    Arg,

    // Error trees.
    /// The parser has encountered an error.
    Error,
}

/// A parse tree.
#[derive(Debug)]
pub struct Cst {
    /// The type of parse tree.
    pub tag: TreeKind,
    /// The children of the tree.
    pub children: Vec<Node>,
}

/// A node in a parse tree.
#[derive(Debug)]
pub enum Node {
    /// A leaf node.
    Token(Token),
    /// A sub-tree.
    Tree(Cst),
}

impl Node {
    /// Return the underlying token if the node is a token.
    pub fn token(&self) -> Option<&Token> {
        match *self {
            Node::Token(ref token) => Some(token),
            Node::Tree(_) => None,
        }
    }

    /// Return the underlying tree if the node is a tree.
    pub fn tree(&self) -> Option<&Cst> {
        match *self {
            Node::Token(_) => None,
            Node::Tree(ref tree) => Some(tree),
        }
    }
}

impl Cst {
    /// Dump the CST.
    ///
    /// # Panics
    /// If parsed tree does not correspond to the given the source lookup.
    ///
    /// # Errors
    /// Can error if the given buffer becomes full.
    pub fn dump(
        &self,
        lookup: &SourceCode<'_>,
        buffer: &mut impl fmt::Write,
        level: usize,
        skip_trivia: bool,
    ) -> Result<(), fmt::Error> {
        let indent = " ".repeat(level);
        writeln!(buffer, "{indent}{:?}", self.tag)?;

        for child in &self.children {
            match *child {
                Node::Token(token) => {
                    if skip_trivia && token.is_whitespace() {
                        continue;
                    }
                    let lexeme =
                        Lexer::lexeme(lookup, &token).expect("can't handle when token is invalid.");
                    writeln!(buffer, "{indent} '{lexeme}'")?;
                }
                Node::Tree(ref tree) => tree.dump(lookup, buffer, level + 1, skip_trivia)?,
            }
        }
        Ok(())
    }
}

impl TreeKind {
    /// Return the name of the tree type.
    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            TreeKind::ExprThis => "a keyword this",
            TreeKind::ExprTrue => "a keyword true",
            TreeKind::ExprFalse => "a keyword false",
            TreeKind::ExprNil => "a keyword nil",
            TreeKind::ExprStringLiteral => "a string literal",
            TreeKind::ExprNumericLiteral => "a numeric literal",
            TreeKind::ExprIdent => "an identifier",
            TreeKind::ExprSuperCall => "a super call",
            TreeKind::ExprGroup => "an expression group",
            TreeKind::ExprBinaryAssignment => "an assignment expression",
            TreeKind::ExprCall => "an expression call",
            TreeKind::ExprGet => "a field access",
            TreeKind::ExprSet => "a field set",
            TreeKind::Program => "the program",
            TreeKind::StmtFnDecl => "a function declaration",
            TreeKind::StmtVarDecl => "a variable declaration",
            TreeKind::StmtClassDecl => "a class declaration",
            TreeKind::StmtMethodDecl => "a method declaration",
            TreeKind::StmtBlock => "a block statement",
            TreeKind::StmtReturn => "a return statement",
            TreeKind::StmtExpr => "an expression statement",
            TreeKind::StmtPrint => "a print statement",
            TreeKind::StmtIf => "an if statement",
            TreeKind::StmtWhile => "a while statement",
            TreeKind::StmtFor => "a for statement",
            TreeKind::ParamList => "a parameter list",
            TreeKind::Param => "a parameter",
            TreeKind::VarDeclInitializer => "a variable declaration initializer",
            TreeKind::ArgList => "an argument list",
            TreeKind::Arg => "an argument",
            TreeKind::Error => "an error",
            TreeKind::ExprUnaryBang => "a unary boolean negation",
            TreeKind::ExprUnaryMinus => "a unary numeric negation",
            TreeKind::ExprBinaryPlus => "a binary addition",
            TreeKind::ExprBinaryMinus => "a binary subtraction",
            TreeKind::ExprBinaryStar => "a binary multiplication",
            TreeKind::ExprBinarySlash => "a binary division",
            TreeKind::ExprBinaryLess => "a binary less than",
            TreeKind::ExprBinaryLessEqual => "a binary less than or equal",
            TreeKind::ExprBinaryGreater => "a binary greater than",
            TreeKind::ExprBinaryGreaterEqual => "a binary greater than or equal",
            TreeKind::ExprBinaryEqualEqual => "a binary equality",
            TreeKind::ExprBinaryBangEqual => "a binary inequality",
            TreeKind::ExprBinaryAnd => "a binary and",
            TreeKind::ExprBinaryOr => "a binary or",
        }
    }
}
