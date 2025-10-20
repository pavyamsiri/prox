use super::{Cst, Node, TreeKind};
use prox_lexer::token::{Token, TokenKind};
use prox_span::Span;

/// Unary operations.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Boolean not.
    Not,
    /// Numeric negation.
    Neg,
}

/// Binary operations.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// Multiplication.
    Mul,
    /// Division.
    Div,
    /// Addition.
    Add,
    /// Subtraction.
    Sub,
    /// Less than.
    Lt,
    /// Less than or equal.
    Le,
    /// Greater than.
    Gt,
    /// Greater than or equal.
    Ge,
    /// Not equal.
    Ne,
    /// Equal.
    Eq,
    /// And.
    And,
    /// Or.
    Or,
}

pub trait CstNode<'node> {
    /// Cast a node to a typed node if possible.
    fn ref_cast(node: &'node Cst) -> Option<Self>
    where
        Self: Sized;
    /// Return a iterator over the non-trivial tokens.
    fn non_trivia_tokens(&self) -> impl Iterator<Item = &Token>;
    /// The expected tree type.
    fn expected_type() -> TreeKind;
    /// The tree type name.
    fn type_name(&self) -> &'static str;
}

/// Interface for binary expression nodes.
pub trait UnaryNode<'tree> {
    /// Return the unary expression's operator.
    fn op(&self) -> UnaryOp;
    /// Return the token of the operator.
    fn op_token(&self) -> Option<&Span>;
    /// Return the unary expression's operand.
    fn operand(&self) -> Option<Expr<'tree>>;
}

/// Interface for binary expression nodes.
pub trait BinaryNode<'tree> {
    /// Return the binary expression's operator.
    fn op(&self) -> BinaryOp;
    /// Return the binary expression's operands.
    fn operands(&self) -> Option<(Expr<'tree>, Expr<'tree>)>;
}

macro_rules! generate_token_cast {
    ($name:ident, $tag:pat) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name(Span);

        impl $name {
            #[must_use]
            pub fn token_cast(token: &Token) -> Option<Self> {
                matches!(token.tag, $tag).then_some(Self(token.span))
            }

            #[must_use]
            pub const fn span(&self) -> &Span {
                &self.0
            }
        }
    };
    ($name:ident, $tag:pat, $tree_tag:pat) => {
        generate_token_cast!($name, $tag);

        impl $name {
            #[must_use]
            pub fn ref_cast(node: &Cst) -> Option<Self> {
                matches!(node.tag, $tree_tag)
                    .then(|| {
                        node.children
                            .iter()
                            .filter_map(|node| node.token())
                            .find_map(|tok| Self::token_cast(tok))
                    })
                    .flatten()
            }
        }
    };
}

macro_rules! generate_node_ref_cast {
    ($name:ident, $tag:expr) => {
        #[derive(Debug)]
        pub struct $name<'node>(&'node Cst);

        impl<'node> CstNode<'node> for $name<'node> {
            fn ref_cast(node: &'node Cst) -> Option<Self> {
                (node.tag == $tag).then_some(Self(node))
            }

            fn non_trivia_tokens(&self) -> impl Iterator<Item = &Token> {
                self.0.children.iter().filter_map(|child| child.token())
            }

            fn expected_type() -> TreeKind {
                $tag
            }

            fn type_name(&self) -> &'static str {
                stringify!($name)
            }
        }
    };
}

macro_rules! generate_get_token {
    ($fn_name:ident, $tag:pat) => {
        #[must_use]
        pub fn $fn_name(&self) -> Option<&Span> {
            self.0.children.iter().find_map(|child| {
                child
                    .token()
                    .and_then(|tok| matches!(tok.tag, $tag).then_some(&tok.span))
            })
        }
    };
}

macro_rules! generate_get_span {
    ($first:pat, $last:pat) => {
        #[must_use]
        pub fn get_span(&self) -> Option<Span> {
            self.0
                .children
                .iter()
                .try_fold((None, None), |(first, last), child| {
                    let first = first.or_else(|| {
                        if let Node::Token(Token { tag: $first, span }) = *child {
                            Some(span)
                        } else {
                            None
                        }
                    });
                    let last = if let Node::Token(Token { tag: $last, span }) = *child {
                        Some(span)
                    } else {
                        last
                    };
                    Some((first, last))
                })
                .and_then(|(first, last)| match (first, last) {
                    (Some(fir), Some(las)) => Some(fir.merge(las)),
                    _ => None,
                })
        }
    };
}

macro_rules! generate_binary_expr_helpers {
    ($name:ident, $tok:pat, $op:expr) => {
        impl<'tree> BinaryNode<'tree> for $name<'tree> {
            fn op(&self) -> BinaryOp {
                $op
            }

            fn operands(&self) -> Option<(Expr<'tree>, Expr<'tree>)> {
                let mut lhs: Option<Expr<'tree>> = None;
                let mut rhs: Option<Expr<'tree>> = None;

                let mut is_left = true;
                for child in &self.0.children {
                    match *child {
                        Node::Token(Token { tag: $tok, .. }) => {
                            is_left = false;
                        }
                        Node::Tree(ref node) => {
                            let Some(expr) = Expr::ref_cast(node) else {
                                continue;
                            };
                            if is_left {
                                lhs = Some(expr);
                            } else {
                                rhs = Some(expr);
                            }
                        }
                        _ => {}
                    }
                }

                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => Some((lhs, rhs)),
                    _ => None,
                }
            }
        }
    };
}

macro_rules! generate_unary_expr_helpers {
    ($name:ident, $tok:pat, $op:expr) => {
        impl<'tree> UnaryNode<'tree> for $name<'tree> {
            fn op(&self) -> UnaryOp {
                $op
            }

            fn op_token(&self) -> Option<&Span> {
                self.0.children.iter().find_map(|child| {
                    child
                        .token()
                        .and_then(|tok| matches!(tok.tag, $tok).then_some(&tok.span))
                })
            }

            fn operand(&self) -> Option<Expr<'tree>> {
                self.0
                    .children
                    .iter()
                    .filter_map(|child| child.tree())
                    .find_map(Expr::ref_cast)
            }
        }
    };
}

generate_node_ref_cast!(Program, TreeKind::Program);

// Declarations
generate_node_ref_cast!(FnDecl, TreeKind::StmtFnDecl);
generate_node_ref_cast!(VarDecl, TreeKind::StmtVarDecl);
generate_node_ref_cast!(ClassDecl, TreeKind::StmtClassDecl);
generate_node_ref_cast!(MethodDecl, TreeKind::StmtMethodDecl);
// Statements
generate_node_ref_cast!(Block, TreeKind::StmtBlock);
generate_node_ref_cast!(If, TreeKind::StmtIf);
generate_node_ref_cast!(ExprStmt, TreeKind::StmtExpr);
generate_node_ref_cast!(For, TreeKind::StmtFor);
generate_node_ref_cast!(Print, TreeKind::StmtPrint);
generate_node_ref_cast!(Return, TreeKind::StmtReturn);
generate_node_ref_cast!(While, TreeKind::StmtWhile);
// Miscellaneous
generate_node_ref_cast!(ParamList, TreeKind::ParamList);
generate_node_ref_cast!(Param, TreeKind::Param);
generate_node_ref_cast!(ArgList, TreeKind::ArgList);
generate_node_ref_cast!(Arg, TreeKind::Arg);
generate_node_ref_cast!(VarDeclInitializer, TreeKind::VarDeclInitializer);
generate_node_ref_cast!(ForStmtInitializer, TreeKind::ForStmtInitializer);
generate_node_ref_cast!(ForStmtCondition, TreeKind::ForStmtCondition);
generate_node_ref_cast!(ForStmtIncrement, TreeKind::ForStmtIncrement);
generate_node_ref_cast!(WhileStmtCondition, TreeKind::WhileStmtCondition);
generate_node_ref_cast!(SuperClass, TreeKind::SuperClass);
// Expressions
generate_node_ref_cast!(ExprMul, TreeKind::ExprBinaryStar);
generate_node_ref_cast!(ExprDiv, TreeKind::ExprBinarySlash);
generate_node_ref_cast!(ExprAdd, TreeKind::ExprBinaryPlus);
generate_node_ref_cast!(ExprSub, TreeKind::ExprBinaryMinus);
generate_node_ref_cast!(ExprLt, TreeKind::ExprBinaryLess);
generate_node_ref_cast!(ExprLe, TreeKind::ExprBinaryLessEqual);
generate_node_ref_cast!(ExprGt, TreeKind::ExprBinaryGreater);
generate_node_ref_cast!(ExprGe, TreeKind::ExprBinaryGreaterEqual);
generate_node_ref_cast!(ExprEq, TreeKind::ExprBinaryEqualEqual);
generate_node_ref_cast!(ExprNe, TreeKind::ExprBinaryBangEqual);
generate_node_ref_cast!(ExprAnd, TreeKind::ExprBinaryAnd);
generate_node_ref_cast!(ExprOr, TreeKind::ExprBinaryOr);
// Unary
generate_node_ref_cast!(ExprNot, TreeKind::ExprUnaryBang);
generate_node_ref_cast!(ExprNeg, TreeKind::ExprUnaryMinus);
// Misc
generate_node_ref_cast!(ExprGet, TreeKind::ExprGet);
generate_node_ref_cast!(ExprSet, TreeKind::ExprSet);
generate_node_ref_cast!(ExprAssignment, TreeKind::ExprBinaryAssignment);
generate_node_ref_cast!(ExprSuperMethod, TreeKind::ExprSuperMethod);
generate_node_ref_cast!(ExprGroup, TreeKind::ExprGroup);

generate_binary_expr_helpers!(ExprMul, TokenKind::Star, BinaryOp::Mul);
generate_binary_expr_helpers!(ExprDiv, TokenKind::Slash, BinaryOp::Div);
generate_binary_expr_helpers!(ExprAdd, TokenKind::Plus, BinaryOp::Add);
generate_binary_expr_helpers!(ExprSub, TokenKind::Minus, BinaryOp::Sub);
generate_binary_expr_helpers!(ExprLt, TokenKind::LessThan, BinaryOp::Lt);
generate_binary_expr_helpers!(ExprLe, TokenKind::LessThanEqual, BinaryOp::Le);
generate_binary_expr_helpers!(ExprGt, TokenKind::GreaterThan, BinaryOp::Gt);
generate_binary_expr_helpers!(ExprGe, TokenKind::GreaterThanEqual, BinaryOp::Ge);
generate_binary_expr_helpers!(ExprEq, TokenKind::EqualEqual, BinaryOp::Eq);
generate_binary_expr_helpers!(ExprNe, TokenKind::BangEqual, BinaryOp::Ne);
generate_binary_expr_helpers!(ExprAnd, TokenKind::KeywordAnd, BinaryOp::And);
generate_binary_expr_helpers!(ExprOr, TokenKind::KeywordOr, BinaryOp::Or);

generate_unary_expr_helpers!(ExprNot, TokenKind::Bang, UnaryOp::Not);
generate_unary_expr_helpers!(ExprNeg, TokenKind::Minus, UnaryOp::Neg);

generate_node_ref_cast!(ExprCall, TreeKind::ExprCall);

// Tokens
generate_token_cast!(Nil, TokenKind::KeywordNil, TreeKind::ExprNil);
generate_token_cast!(True, TokenKind::KeywordTrue, TreeKind::ExprTrue);
generate_token_cast!(False, TokenKind::KeywordFalse, TreeKind::ExprFalse);
generate_token_cast!(Ident, TokenKind::Ident, TreeKind::ExprIdent);
generate_token_cast!(
    NumericLiteral,
    TokenKind::NumericLiteral,
    TreeKind::ExprNumericLiteral
);
generate_token_cast!(
    StringLiteral,
    TokenKind::StringLiteral,
    TreeKind::ExprStringLiteral
);
generate_token_cast!(ExprThis, TokenKind::KeywordThis, TreeKind::ExprThis);

impl<'tree> Program<'tree> {
    pub fn declarations_or_statements(self) -> impl Iterator<Item = DeclarationOrStatement<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .filter_map(DeclarationOrStatement::ref_cast)
    }
}

#[derive(Debug)]
pub enum Declaration<'tree> {
    Var(VarDecl<'tree>),
    Class(ClassDecl<'tree>),
    Fn(FnDecl<'tree>),
}

impl<'node> Declaration<'node> {
    #[must_use]
    pub fn ref_cast(node: &'node Cst) -> Option<Self> {
        match node.tag {
            TreeKind::StmtVarDecl => Some(Self::Var(VarDecl::ref_cast(node)?)),
            TreeKind::StmtClassDecl => Some(Self::Class(ClassDecl::ref_cast(node)?)),
            TreeKind::StmtFnDecl => Some(Self::Fn(FnDecl::ref_cast(node)?)),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Statement<'tree> {
    If(If<'tree>),
    Block(Block<'tree>),
    ExprStmt(ExprStmt<'tree>),
    For(For<'tree>),
    While(While<'tree>),
    Print(Print<'tree>),
    Return(Return<'tree>),
}

impl<'node> Statement<'node> {
    #[must_use]
    pub fn ref_cast(node: &'node Cst) -> Option<Self> {
        match node.tag {
            TreeKind::StmtIf => Some(Self::If(If::ref_cast(node)?)),
            TreeKind::StmtBlock => Some(Self::Block(Block::ref_cast(node)?)),
            TreeKind::StmtExpr => Some(Self::ExprStmt(ExprStmt::ref_cast(node)?)),
            TreeKind::StmtFor => Some(Self::For(For::ref_cast(node)?)),
            TreeKind::StmtWhile => Some(Self::While(While::ref_cast(node)?)),
            TreeKind::StmtPrint => Some(Self::Print(Print::ref_cast(node)?)),
            TreeKind::StmtReturn => Some(Self::Return(Return::ref_cast(node)?)),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum DeclarationOrStatement<'tree> {
    // Declarations.
    Var(VarDecl<'tree>),
    Class(ClassDecl<'tree>),
    Fn(FnDecl<'tree>),

    // Statements.
    If(If<'tree>),
    Block(Block<'tree>),
    ExprStmt(ExprStmt<'tree>),
    For(For<'tree>),
    While(While<'tree>),
    Print(Print<'tree>),
    Return(Return<'tree>),
}

impl<'node> DeclarationOrStatement<'node> {
    /// Return the type name.
    #[must_use]
    pub fn type_name(&self) -> &'static str {
        match *self {
            DeclarationOrStatement::Var(ref decl) => decl.type_name(),
            DeclarationOrStatement::Class(ref decl) => decl.type_name(),
            DeclarationOrStatement::Fn(ref decl) => decl.type_name(),
            DeclarationOrStatement::If(ref stmt) => stmt.type_name(),
            DeclarationOrStatement::Block(ref stmt) => stmt.type_name(),
            DeclarationOrStatement::ExprStmt(ref stmt) => stmt.type_name(),
            DeclarationOrStatement::For(ref stmt) => stmt.type_name(),
            DeclarationOrStatement::While(ref stmt) => stmt.type_name(),
            DeclarationOrStatement::Print(ref stmt) => stmt.type_name(),
            DeclarationOrStatement::Return(ref stmt) => stmt.type_name(),
        }
    }

    #[must_use]
    pub fn ref_cast(node: &'node Cst) -> Option<Self> {
        match node.tag {
            TreeKind::StmtVarDecl => Some(Self::Var(VarDecl::ref_cast(node)?)),
            TreeKind::StmtClassDecl => Some(Self::Class(ClassDecl::ref_cast(node)?)),
            TreeKind::StmtFnDecl => Some(Self::Fn(FnDecl::ref_cast(node)?)),
            TreeKind::StmtIf => Some(Self::If(If::ref_cast(node)?)),
            TreeKind::StmtBlock => Some(Self::Block(Block::ref_cast(node)?)),
            TreeKind::StmtExpr => Some(Self::ExprStmt(ExprStmt::ref_cast(node)?)),
            TreeKind::StmtFor => Some(Self::For(For::ref_cast(node)?)),
            TreeKind::StmtWhile => Some(Self::While(While::ref_cast(node)?)),
            TreeKind::StmtPrint => Some(Self::Print(Print::ref_cast(node)?)),
            TreeKind::StmtReturn => Some(Self::Return(Return::ref_cast(node)?)),
            _ => None,
        }
    }
}

impl<'tree> ClassDecl<'tree> {
    /// Return the name of the class if it is there.
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        self.0.children.iter().find_map(|node| match *node {
            Node::Token(Token {
                tag: TokenKind::Ident,
                ref span,
            }) => Some(Ident(*span)),
            _ => None,
        })
    }

    /// Return the super class if it exists.
    pub fn super_class(&self) -> Option<SuperClass<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(SuperClass::ref_cast)
    }

    /// Return the methods if they exist.
    pub fn methods(&self) -> impl Iterator<Item = MethodDecl<'tree>> + use<'tree> {
        self.0.children.iter().filter_map(|child| {
            let tree = child.tree()?;
            MethodDecl::ref_cast(tree)
        })
    }

    generate_get_span!(TokenKind::KeywordClass, TokenKind::RightBrace);
}

impl SuperClass<'_> {
    /// Return the name if it exists.
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        self.0.children.iter().find_map(|child| {
            let tok = child.token()?;
            Ident::token_cast(tok)
        })
    }
}
impl<'node> FnDecl<'node> {
    /// Return the name of the function if it is there.
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        self.0.children.iter().find_map(|node| match *node {
            Node::Token(Token {
                tag: TokenKind::Ident,
                ref span,
            }) => Some(Ident(*span)),
            _ => None,
        })
    }

    /// Return an iterator over the parameters of the function.
    pub fn param_list(&self) -> impl Iterator<Item = Param<'node>> + use<'node> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(ParamList::ref_cast))
            .into_iter()
            .flat_map(|list| list.parameters())
    }

    /// Return the body of the function if it is there.
    #[must_use]
    pub fn body(&self) -> Option<Block<'node>> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(Block::ref_cast))
    }

    generate_get_token!(fn_keyword, TokenKind::KeywordFun);
}

impl<'node> MethodDecl<'node> {
    /// Return the name of the method if it is there.
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        self.0.children.iter().find_map(|node| match *node {
            Node::Token(Token {
                tag: TokenKind::Ident,
                ref span,
            }) => Some(Ident(*span)),
            _ => None,
        })
    }

    /// Return an iterator over the parameters of the method.
    pub fn param_list(&self) -> impl Iterator<Item = Param<'node>> + use<'node> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(ParamList::ref_cast))
            .into_iter()
            .flat_map(|list| list.parameters())
    }

    /// Return the body of the method if it is there.
    #[must_use]
    pub fn body(&self) -> Option<Block<'node>> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(Block::ref_cast))
    }
}

impl<'node> ParamList<'node> {
    pub fn parameters(&self) -> impl Iterator<Item = Param<'node>> + use<'node> {
        self.0.children.iter().filter_map(|child| {
            let tree = child.tree()?;
            Param::ref_cast(tree)
        })
    }
}

impl Param<'_> {
    #[must_use]
    pub fn name(&self) -> Option<Ident> {
        self.0.children.iter().find_map(|child| {
            let tok = child.token()?;
            Ident::token_cast(tok)
        })
    }
}

impl<'node> Block<'node> {
    /// Return an iterator over the declarations of the block.
    pub fn declarations_or_statements(
        &self,
    ) -> impl Iterator<Item = DeclarationOrStatement<'node>> + use<'node> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .filter_map(DeclarationOrStatement::ref_cast)
    }

    generate_get_token!(right_brace, TokenKind::RightBrace);
    generate_get_span!(TokenKind::LeftBrace, TokenKind::RightBrace);
}

impl<'tree> If<'tree> {
    /// Return the condition if it exists.
    pub fn condition(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    /// Return the then clause and else clause if they exist.
    #[must_use]
    pub fn clauses(&self) -> (Option<Statement<'tree>>, Option<Statement<'tree>>) {
        let mut then_clause: Option<Statement<'tree>> = None;
        let mut else_clause: Option<Statement<'tree>> = None;

        let mut is_then = true;
        for child in &self.0.children {
            match *child {
                Node::Token(Token {
                    tag: TokenKind::KeywordElse,
                    ..
                }) => {
                    is_then = false;
                }
                Node::Token(_) => {}
                Node::Tree(ref node) => {
                    let Some(expr) = Statement::ref_cast(node) else {
                        continue;
                    };
                    if is_then {
                        then_clause = Some(expr);
                    } else {
                        else_clause = Some(expr);
                    }
                }
            }
        }
        (then_clause, else_clause)
    }

    generate_get_token!(if_keyword, TokenKind::KeywordIf);
}

impl<'tree> Return<'tree> {
    /// Return the return value if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    generate_get_span!(TokenKind::KeywordReturn, TokenKind::Semicolon);
}

impl<'tree> ExprStmt<'tree> {
    /// Return the expresssion value if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    /// Return the span of the statement.
    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        let value_span = self.value()?.get_span()?;
        let semicolon = self.semicolon()?;

        Some(value_span.merge(*semicolon))
    }

    generate_get_token!(semicolon, TokenKind::Semicolon);
}

impl<'tree> Print<'tree> {
    /// Return the return value if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    generate_get_span!(TokenKind::KeywordPrint, TokenKind::Semicolon);
}

impl<'tree> For<'tree> {
    /// Return the initializer if it exists.
    pub fn initializer(&self) -> Option<ForStmtInitializer<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(ForStmtInitializer::ref_cast)
    }

    /// Return the condition if it exists.
    pub fn condition(&self) -> Option<ForStmtCondition<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(ForStmtCondition::ref_cast)
    }

    /// Return the increment if it exists.
    pub fn increment(&self) -> Option<ForStmtIncrement<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(ForStmtIncrement::ref_cast)
    }

    /// Return the body of the for statement if it is there.
    #[must_use]
    pub fn body(&self) -> Option<Statement<'tree>> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(Statement::ref_cast))
    }

    generate_get_token!(for_keyword, TokenKind::KeywordFor);
}

impl<'tree> ForStmtInitializer<'tree> {
    /// Return the variable declaration if it exists.
    #[must_use]
    pub fn decl(&self) -> Option<VarDecl<'tree>> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(VarDecl::ref_cast))
    }

    /// Return the expression statement if it exists.
    #[must_use]
    pub fn expr_stmt(&self) -> Option<ExprStmt<'tree>> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(ExprStmt::ref_cast))
    }
}

impl<'tree> ForStmtCondition<'tree> {
    /// Return the condition statement if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }
}

impl<'tree> ForStmtIncrement<'tree> {
    /// Return the increment statement if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }
}

impl<'tree> While<'tree> {
    /// Return the condition if it exists.
    pub fn condition(&self) -> Option<WhileStmtCondition<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(WhileStmtCondition::ref_cast)
    }

    /// Return the body of the while statement if it is there.
    #[must_use]
    pub fn body(&self) -> Option<Statement<'tree>> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(Statement::ref_cast))
    }

    generate_get_token!(while_keyword, TokenKind::KeywordWhile);
}

impl<'tree> WhileStmtCondition<'tree> {
    /// Return the condition statement if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }
}

impl<'tree> VarDecl<'tree> {
    /// Return the name of the variable if it exists.
    pub fn name(&self) -> Option<Ident> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.token())
            .find_map(Ident::token_cast)
    }

    /// Return the initializer if it exists.
    pub fn initializer(&self) -> Option<VarDeclInitializer<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(VarDeclInitializer::ref_cast)
    }

    generate_get_span!(TokenKind::KeywordVar, TokenKind::Semicolon);
}

impl<'tree> VarDeclInitializer<'tree> {
    /// Return the value if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }
}

impl<'tree> ExprCall<'tree> {
    /// Return the callee if it exists.
    pub fn callee(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    /// Return the argument list if it exists.
    #[must_use]
    pub fn arg_list(&self) -> Option<ArgList<'tree>> {
        self.0
            .children
            .iter()
            .find_map(|child| child.tree().and_then(ArgList::ref_cast))
    }
}

impl<'tree> ArgList<'tree> {
    pub fn args(&self) -> impl Iterator<Item = Arg<'tree>> + use<'tree> {
        self.0.children.iter().filter_map(|child| {
            let tree = child.tree()?;
            Arg::ref_cast(tree)
        })
    }

    generate_get_span!(TokenKind::LeftParenthesis, TokenKind::RightParenthesis);
}

impl<'tree> Arg<'tree> {
    /// Return the argument expression if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }
}

impl<'tree> ExprGet<'tree> {
    /// Return the object being accessed if it exists.
    pub fn object(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    /// Return the name of the field if it exists.
    #[must_use]
    pub fn field(&self) -> Option<Ident> {
        self.0.children.iter().rev().find_map(|child| {
            let tok = child.token()?;
            Ident::token_cast(tok)
        })
    }
}

impl<'tree> ExprSet<'tree> {
    /// Return the object being set if it exists.
    pub fn object(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    /// Return the value to set the field to if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .rev()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    /// Return the name of the field if it exists.
    #[must_use]
    pub fn field(&self) -> Option<Ident> {
        self.0.children.iter().find_map(|child| {
            let tok = child.token()?;
            Ident::token_cast(tok)
        })
    }
}

impl<'tree> ExprAssignment<'tree> {
    /// Return the destination being assigned to if it exists.
    pub fn name(&self) -> Option<Ident> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Ident::ref_cast)
    }

    /// Return the value to set the destination to if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .rev()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }
}

impl ExprSuperMethod<'_> {
    generate_get_token!(super_keyword, TokenKind::KeywordSuper);
    generate_get_token!(name, TokenKind::Ident);
}

impl<'tree> ExprGroup<'tree> {
    /// Return the inner expression if it exists.
    pub fn value(&self) -> Option<Expr<'tree>> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.tree())
            .find_map(Expr::ref_cast)
    }

    generate_get_span!(TokenKind::LeftParenthesis, TokenKind::RightParenthesis);
}

#[derive(Debug)]
pub enum Expr<'tree> {
    /// A nil value.
    Nil(Nil),
    /// A true value.
    True(True),
    /// A false value.
    False(False),
    /// An identifier.
    Ident(Ident),
    /// A numeric literal.
    NumericLiteral(NumericLiteral),
    /// A string literal.
    StringLiteral(StringLiteral),
    /// Boolean not.
    Not(ExprNot<'tree>),
    /// Or.
    Neg(ExprNeg<'tree>),
    /// Multiplication.
    Mul(ExprMul<'tree>),
    /// Division.
    Div(ExprDiv<'tree>),
    /// Addition.
    Add(ExprAdd<'tree>),
    /// Subtraction.
    Sub(ExprSub<'tree>),
    /// Less than.
    Lt(ExprLt<'tree>),
    /// Less than or equal.
    Le(ExprLe<'tree>),
    /// Greater than.
    Gt(ExprGt<'tree>),
    /// Greater than or equal.
    Ge(ExprGe<'tree>),
    /// Not equal.
    Ne(ExprNe<'tree>),
    /// Equal.
    Eq(ExprEq<'tree>),
    /// And.
    And(ExprAnd<'tree>),
    /// Or.
    Or(ExprOr<'tree>),
    /// Call.
    Call(ExprCall<'tree>),
    /// Get super class' method.
    SuperMethod(ExprSuperMethod<'tree>),
    /// Get.
    Get(ExprGet<'tree>),
    /// Set.
    Set(ExprSet<'tree>),
    /// Group.
    Group(ExprGroup<'tree>),
    /// Assignment.
    Assignment(ExprAssignment<'tree>),
    /// The `this` keyword.
    This(ExprThis),
}

impl<'node> Expr<'node> {
    #[must_use]
    pub fn type_name(&self) -> &'static str {
        match *self {
            Expr::Ident(_) => "ExprIdent",
            Expr::NumericLiteral(_) => "ExprNumericLiteral",
            Expr::StringLiteral(_) => "ExprStringLiteral",
            Expr::This(_) => "ExprThis",
            Expr::Mul(ref expr) => expr.type_name(),
            Expr::Div(ref expr) => expr.type_name(),
            Expr::Add(ref expr) => expr.type_name(),
            Expr::Sub(ref expr) => expr.type_name(),
            Expr::Lt(ref expr) => expr.type_name(),
            Expr::Le(ref expr) => expr.type_name(),
            Expr::Gt(ref expr) => expr.type_name(),
            Expr::Ge(ref expr) => expr.type_name(),
            Expr::Ne(ref expr) => expr.type_name(),
            Expr::Eq(ref expr) => expr.type_name(),
            Expr::And(ref expr) => expr.type_name(),
            Expr::Or(ref expr) => expr.type_name(),
            Expr::Call(ref expr) => expr.type_name(),
            Expr::Get(ref expr) => expr.type_name(),
            Expr::Set(ref expr) => expr.type_name(),
            Expr::Assignment(ref expr) => expr.type_name(),
            Expr::Not(ref expr) => expr.type_name(),
            Expr::Neg(ref expr) => expr.type_name(),
            Expr::SuperMethod(ref expr) => expr.type_name(),
            Expr::Nil(_) => "ExprNil",
            Expr::True(_) => "ExprTrue",
            Expr::False(_) => "ExprFalse",
            Expr::Group(_) => "ExprGroup",
        }
    }

    #[must_use]
    pub fn ref_cast(node: &'node Cst) -> Option<Self> {
        match node.tag {
            TreeKind::ExprBinaryStar => Some(Self::Mul(ExprMul::ref_cast(node)?)),
            TreeKind::ExprBinarySlash => Some(Self::Div(ExprDiv::ref_cast(node)?)),
            TreeKind::ExprBinaryPlus => Some(Self::Add(ExprAdd::ref_cast(node)?)),
            TreeKind::ExprBinaryMinus => Some(Self::Sub(ExprSub::ref_cast(node)?)),
            TreeKind::ExprBinaryLess => Some(Self::Lt(ExprLt::ref_cast(node)?)),
            TreeKind::ExprBinaryLessEqual => Some(Self::Le(ExprLe::ref_cast(node)?)),
            TreeKind::ExprBinaryGreater => Some(Self::Gt(ExprGt::ref_cast(node)?)),
            TreeKind::ExprBinaryGreaterEqual => Some(Self::Ge(ExprGe::ref_cast(node)?)),
            TreeKind::ExprBinaryEqualEqual => Some(Self::Eq(ExprEq::ref_cast(node)?)),
            TreeKind::ExprBinaryBangEqual => Some(Self::Ne(ExprNe::ref_cast(node)?)),
            TreeKind::ExprBinaryAnd => Some(Self::And(ExprAnd::ref_cast(node)?)),
            TreeKind::ExprBinaryOr => Some(Self::Or(ExprOr::ref_cast(node)?)),
            TreeKind::ExprUnaryBang => Some(Self::Not(ExprNot::ref_cast(node)?)),
            TreeKind::ExprUnaryMinus => Some(Self::Neg(ExprNeg::ref_cast(node)?)),
            TreeKind::ExprCall => Some(Self::Call(ExprCall::ref_cast(node)?)),
            TreeKind::ExprSuperMethod => Some(Self::SuperMethod(ExprSuperMethod::ref_cast(node)?)),
            TreeKind::ExprGet => Some(Self::Get(ExprGet::ref_cast(node)?)),
            TreeKind::ExprSet => Some(Self::Set(ExprSet::ref_cast(node)?)),
            TreeKind::ExprBinaryAssignment => {
                Some(Self::Assignment(ExprAssignment::ref_cast(node)?))
            }
            TreeKind::ExprNil => Some(Self::Nil(Nil::ref_cast(node)?)),
            TreeKind::ExprTrue => Some(Self::True(True::ref_cast(node)?)),
            TreeKind::ExprFalse => Some(Self::False(False::ref_cast(node)?)),
            TreeKind::ExprIdent => Some(Self::Ident(Ident::ref_cast(node)?)),
            TreeKind::ExprThis => Some(Self::This(ExprThis::ref_cast(node)?)),
            TreeKind::ExprGroup => Some(Self::Group(ExprGroup::ref_cast(node)?)),
            TreeKind::ExprNumericLiteral => {
                Some(Self::NumericLiteral(NumericLiteral::ref_cast(node)?))
            }
            TreeKind::ExprStringLiteral => {
                Some(Self::StringLiteral(StringLiteral::ref_cast(node)?))
            }
            _ => None,
        }
    }

    /// Return the span of the expression.
    #[expect(
        clippy::cognitive_complexity,
        reason = "makes more sense to put all get_span code together."
    )]
    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        macro_rules! handle_unary {
            ($node:expr) => {{
                let op = $node.op_token()?;
                let operand = $node.operand()?;
                Some(op.merge(operand.get_span()?))
            }};
        }

        macro_rules! handle_binary {
            ($node:expr) => {{
                let (lhs, rhs) = $node.operands()?;
                Some(lhs.get_span()?.merge(rhs.get_span()?))
            }};
        }

        match *self {
            Expr::Nil(ref token) => Some(*token.span()),
            Expr::True(ref token) => Some(*token.span()),
            Expr::False(ref token) => Some(*token.span()),
            Expr::Ident(ref token) => Some(*token.span()),
            Expr::NumericLiteral(ref token) => Some(*token.span()),
            Expr::StringLiteral(ref token) => Some(*token.span()),
            Expr::This(ref token) => Some(*token.span()),
            Expr::Mul(ref expr) => handle_binary!(expr),
            Expr::Div(ref expr) => handle_binary!(expr),
            Expr::Add(ref expr) => handle_binary!(expr),
            Expr::Sub(ref expr) => handle_binary!(expr),
            Expr::Lt(ref expr) => handle_binary!(expr),
            Expr::Le(ref expr) => handle_binary!(expr),
            Expr::Gt(ref expr) => handle_binary!(expr),
            Expr::Ge(ref expr) => handle_binary!(expr),
            Expr::Ne(ref expr) => handle_binary!(expr),
            Expr::Eq(ref expr) => handle_binary!(expr),
            Expr::And(ref expr) => handle_binary!(expr),
            Expr::Or(ref expr) => handle_binary!(expr),
            Expr::Not(ref expr) => handle_unary!(expr),
            Expr::Neg(ref expr) => handle_unary!(expr),
            Expr::Call(ref expr) => {
                let lhs = expr.callee()?;
                let rhs = expr.arg_list()?;
                Some(lhs.get_span()?.merge(rhs.get_span()?))
            }
            Expr::SuperMethod(ref expr) => {
                let lhs = expr.super_keyword()?;
                let rhs = expr.name()?;
                Some(lhs.merge(*rhs))
            }
            Expr::Get(ref expr) => {
                let lhs = expr.object()?;
                let rhs = expr.field()?;
                Some(lhs.get_span()?.merge(*rhs.span()))
            }
            Expr::Set(ref expr) => {
                let lhs = expr.object()?;
                let rhs = expr.value()?;
                Some(lhs.get_span()?.merge(rhs.get_span()?))
            }
            Expr::Assignment(ref expr) => {
                let lhs = expr.name()?;
                let rhs = expr.value()?;
                Some(lhs.span().merge(rhs.get_span()?))
            }
            Expr::Group(ref expr) => expr.get_span(),
        }
    }
}
