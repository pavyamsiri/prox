use core::fmt;
use core::fmt::Write as _;
use prox_lexer::token::Token;
use prox_lexer::{Lexer, SourceLookup};

#[derive(Debug, Clone, Copy)]
pub enum TreeKind {
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

#[derive(Debug)]
pub struct Tree {
    pub tag: TreeKind,
    pub children: Vec<Node>,
}

#[derive(Debug)]
pub enum Node {
    Token(Token),
    Tree(Tree),
}

impl Tree {
    pub fn dump<'src>(
        &self,
        lookup: &SourceLookup<'src>,
        buffer: &mut impl fmt::Write,
        level: usize,
        skip_trivia: bool,
    ) -> Result<(), fmt::Error> {
        let indent = " ".repeat(level);
        writeln!(buffer, "{indent}{:?}", self.tag)?;

        for child in &self.children {
            match *child {
                Node::Token(token) => {
                    if token.is_whitespace() {
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
