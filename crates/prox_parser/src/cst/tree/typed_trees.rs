use super::{Cst, Node, TreeKind};
use prox_lexer::{
    span::Span,
    token::{Token, TokenKind},
};

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
        #[derive(Debug)]
        pub struct $name(Span);

        impl $name {
            #[must_use]
            pub fn ref_cast(token: &Token) -> Option<Self> {
                matches!(token.tag, $tag).then_some(Self(token.span))
            }

            #[must_use]
            pub const fn span(&self) -> &Span {
                &self.0
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

generate_node_ref_cast!(Program, TreeKind::Program);

// Declarations
generate_node_ref_cast!(FnDecl, TreeKind::StmtFnDecl);
generate_node_ref_cast!(VarDecl, TreeKind::StmtVarDecl);
generate_node_ref_cast!(ClassDecl, TreeKind::StmtClassDecl);
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

generate_node_ref_cast!(ExprCall, TreeKind::ExprCall);

// Tokens
generate_token_cast!(Ident, TokenKind::Ident);
generate_token_cast!(NumericLiteral, TokenKind::NumericLiteral);
generate_token_cast!(StringLiteral, TokenKind::StringLiteral);

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
            Ident::ref_cast(tok)
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

impl<'tree> VarDecl<'tree> {
    /// Return the name of the variable if it exists.
    pub fn name(&self) -> Option<Ident> {
        self.0
            .children
            .iter()
            .filter_map(|child| child.token())
            .find_map(Ident::ref_cast)
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
#[derive(Debug)]
pub enum Expr<'tree> {
    /// An identifier.
    Ident(Ident),
    /// A numeric literal.
    NumericLiteral(NumericLiteral),
    /// A string literal.
    StringLiteral(StringLiteral),
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
}

impl<'node> Expr<'node> {
    #[must_use]
    pub fn ref_cast(node: &'node Cst) -> Option<Self> {
        macro_rules! get_atom {
            ($name:expr) => {
                node.children
                    .iter()
                    .filter_map(|node| node.token())
                    .find_map(|tok| $name(tok))?
            };
        }
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
            TreeKind::ExprCall => Some(Self::Call(ExprCall::ref_cast(node)?)),
            TreeKind::ExprIdent => Some(Self::Ident(get_atom!(Ident::ref_cast))),
            TreeKind::ExprNumericLiteral => {
                Some(Self::NumericLiteral(get_atom!(NumericLiteral::ref_cast)))
            }
            TreeKind::ExprStringLiteral => {
                Some(Self::StringLiteral(get_atom!(StringLiteral::ref_cast)))
            }
            _ => None,
        }
    }
}
