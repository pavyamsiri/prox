use core::num::NonZeroU32;
use std::hash;

use prox_interner::{Interner, Symbol};
use prox_lexer::span::Span;

/// Index to AST node wrapping `u32`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NodeIndex(NonZeroU32);

impl NodeIndex {
    /// Create a new node index.
    /// The value `0` is reserved for the null node.
    pub fn new(index: u32) -> Option<Self> {
        NonZeroU32::new(index).map(NodeIndex)
    }

    /// Convert back into a raw index.
    pub const fn raw(self) -> u32 {
        self.0.get()
    }

    /// Convert into a `usize`.
    pub const fn as_usize(self) -> usize {
        self.0.get() as usize
    }
}

/// Index to extra data wrapping `u32`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ExtraIndex(u32);

impl ExtraIndex {
    /// Create a new extra data index.
    pub const fn new(index: u32) -> Self {
        Self(index)
    }

    /// Convert back into a raw index.
    pub const fn raw(self) -> u32 {
        self.0
    }

    /// Convert into a `usize`.
    pub const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

struct NodeData {
    lhs: u32,
    rhs: u32,
    token_or_symbol: u32,
}

impl NodeData {
    const EMPTY: Self = NodeData {
        lhs: 0,
        rhs: 0,
        token_or_symbol: 0,
    };
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NodeTag {
    /// The topmost level structure.
    Program,

    // Declarations.
    /// A function declaration.
    /// `lhs` = extra data index.
    /// `rhs` = extra data length.
    /// `token` = name of function (symbol).
    /// `extra_data` = (is\_method, block, pcount, p1, p2, ..., pn)
    /// `method_flag` = 0 means function and 1 means method.
    StmtFnDecl,
    /// A variable declaration.
    /// `lhs` = rvalue or null.
    /// `rhs` = null.
    /// `token` = name of variable (symbol).
    StmtVarDecl,
    /// A class declaration.
    /// `lhs` = extra data index.
    /// `rhs` = extra data length.
    /// `token` = name of class (symbol).
    /// `extra_data` = (fcount, f1, f2, ..., fn, superclass?)
    /// `superclass` = name of superclass (symbol).
    /// `superclass` is optional and its presence can be deduced from the extra data length.
    StmtClassDecl,
    /// A block.
    /// `lhs` = extra data index.
    /// `rhs` = extra data length.
    /// `token` = null.
    /// `extra_data` = (stmt1, stmt2, .., stmtn)
    Block,
    /// An if statement.
    /// `lhs` = extra data index.
    /// `rhs` = condition.
    /// `token` = then.
    /// `extra_data` = (has\_else, else?)
    /// `has_else` = 0 means no else and 1 means has else.
    /// `else` is the else body.
    If,
    /// A return statement.
    /// `lhs` = value or null if no return value.
    /// `rhs` = null.
    /// `token` = null.
    Return,
    /// A print statement.
    /// `lhs` = value.
    /// `rhs` = null.
    /// `token` = null.
    Print,
    /// A for statement.
    /// `lhs` = extra data index.
    /// `rhs` = extra data length.
    /// `token` = body.
    /// `extra_data` = (
    ///    has\_initializer, initializer?
    ///    has\_condition, condition?,
    ///    has\_increment, increment?
    ///  )
    For,
    /// A while statement.
    /// `lhs` = condition.
    /// `rhs` = body.
    /// `token` = null.
    While,
    /// An expression statement.
    /// `lhs` = value.
    /// `rhs` = null.
    /// `token` = null.
    ExprStmt,

    // Expressions.
    /// Unary expression.
    /// `lhs` = unary op enum.
    /// `rhs` = value.
    /// `token` = null.
    Unary,
    /// Binary expression.
    /// `lhs` = lhs expression.
    /// `rhs` = rhs expression.
    /// `token` = binary op enum.
    Binary,
    /// An expression call.
    /// `lhs` = extra data index.
    /// `rhs` = extra data length.
    /// `token` = callee.
    /// `extra_data` = (arg1, arg2, ..., argn)
    Call,
    /// An expression group.
    /// `lhs` = inner expression.
    /// `rhs` = null.
    /// `token` = null.
    Group,
    /// An assignment expression.
    /// `lhs` = value.
    /// `rhs` = null.
    /// `token` = name of variable (symbol).
    Assignment,
    /// A super method.
    /// `lhs` = null.
    /// `rhs` = null.
    /// `token` = name of method (symbol).
    SuperMethod,
    /// A field access.
    /// `lhs` = object.
    /// `rhs` = null.
    /// `token` = name of field (symbol).
    FieldGet,
    /// Set a field.
    /// `lhs` = object.
    /// `rhs` = value.
    /// `token` = name of field (symbol).
    FieldSet,

    // Literals
    /// Identifier.
    /// `lhs` = null.
    /// `rhs` = null.
    /// `token` = intern symbol.
    Ident,
    /// Number as double precision floating point.
    /// `lhs` = low byte.
    /// `rhs` = high byte.
    /// `token` = null.
    Number,
    /// Literal boolean.
    /// `lhs` = 0 (false) or 1 (true).
    /// `rhs` = null.
    /// `token` = null.
    Boolean,
    /// Literal nil.
    /// `lhs` = null.
    /// `rhs` = null.
    /// `token` = null.
    Nil,

    /// Error node.
    Error,
}

/// Binary operations.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOp {
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

impl BinaryOp {
    /// Convert to raw byte.
    const fn raw(self) -> u8 {
        self as u8
    }
}

/// Unary operations.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnaryOp {
    /// Numeric negation.
    Neg,
    /// Boolean negation.
    Not,
}

impl UnaryOp {
    /// Convert to raw byte.
    const fn raw(self) -> u8 {
        self as u8
    }
}

struct Ast {
    /// List of nodes.
    nodes: NodeList,
    /// Extra data for variable length data for nodes.
    /// This is indexed by certain node types which require extra data.
    /// The exact layout can differ for each node type.
    extra_data: Vec<u32>,
    /// String interner.
    strings: Interner,
    /// Source spans parallel to each node.
    spans: Vec<Span>,
    /// Root note index.
    root: NodeIndex,
}

/// A struct-of-arrays representation of the AST as a list of nodes.
struct NodeList {
    /// The node type (1 byte each).
    tags: Vec<NodeTag>,
    /// The node data (12 bytes each).
    data: Vec<NodeData>,
}

impl NodeList {
    /// Create a new node list.
    fn new() -> Self {
        // Reserve index 0 as the null node.
        // Reserve index 1 as the program/root node.
        Self {
            tags: vec![NodeTag::Error],
            data: vec![NodeData::EMPTY],
        }
    }

    /// Add a node.
    fn add(&mut self, tag: NodeTag, data: NodeData) -> NodeIndex {
        let index = self.tags.len().try_into().expect("node index overflowed.");
        let node_index = NodeIndex::new(index).expect("node index overflowed to 0.");
        self.tags.push(tag);
        self.data.push(data);
        node_index
    }
}

impl Ast {
    /// Create a new empty AST
    pub fn new() -> Self {
        let mut nodes = NodeList::new();
        let root = nodes.add(NodeTag::Program, NodeData::EMPTY);
        let empty_span = Span {
            start: 0,
            length: 0,
        };

        Self {
            nodes,
            extra_data: Vec::with_capacity(1024),
            spans: vec![empty_span, empty_span],
            strings: Interner::with_hasher(hash::DefaultHasher::new()),
            root,
        }
    }

    /// Add a node and return its index.
    /// `tag` is the node type.
    /// `data` is the node-specific data.
    /// `span` is the node's span in source.
    fn add_node(&mut self, tag: NodeTag, data: NodeData, span: Span) -> NodeIndex {
        self.spans.push(span);
        self.nodes.add(tag, data)
    }

    /// Add extra data given index slice and return the starting index into `extra_data` array.
    fn add_extra_data(&mut self, data: &[u32]) -> (ExtraIndex, u32) {
        self.add_extra_data_iter(data.iter().copied())
    }

    /// Add extra data given index iter and return the starting index into `extra_data` array.
    fn add_extra_data_iter(&mut self, data: impl Iterator<Item = u32>) -> (ExtraIndex, u32) {
        let index = self
            .extra_data
            .len()
            .try_into()
            .expect("extra index overflowed.");
        let extra_index = ExtraIndex(index);
        let end: u32 = self
            .extra_data
            .len()
            .try_into()
            .expect("extra index overflowed.");
        self.extra_data.extend(data);

        let length = end - index;
        (extra_index, length)
    }

    /// Get the node's span.
    fn span(&self, index: NodeIndex) -> Span {
        self.spans[index.as_usize()]
    }
}

struct AstBuilder {
    ast: Ast,
}

impl AstBuilder {
    pub fn new() -> Self {
        Self { ast: Ast::new() }
    }
}

// Builder functions.
impl AstBuilder {
    /// Add a binary expression.
    pub fn build_binary(
        &mut self,
        op: BinaryOp,
        left: NodeIndex,
        right: NodeIndex,
        span: Span,
    ) -> NodeIndex {
        let data = NodeData {
            lhs: left.raw(),
            rhs: right.raw(),
            token_or_symbol: u32::from(op.raw()),
        };
        self.ast.add_node(NodeTag::Binary, data, span)
    }

    /// Add a unary expression.
    pub fn build_unary(&mut self, op: UnaryOp, value: NodeIndex, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: u32::from(op.raw()),
            rhs: value.raw(),
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::Unary, data, span)
    }

    /// Add a function declaration.
    pub fn build_function(
        &mut self,
        name: Symbol,
        params: &[NodeIndex],
        block: NodeIndex,
        is_method: bool,
        span: Span,
    ) -> NodeIndex {
        let mut extra_data = vec![
            u32::from(is_method),
            block.raw(),
            params
                .len()
                .try_into()
                .expect("can't have more than 255 parameters."),
        ];
        extra_data.extend(params.iter().map(|param| param.raw()));

        let (lhs, length) = self.ast.add_extra_data(&extra_data);
        let data = NodeData {
            lhs: lhs.raw(),
            rhs: length,
            token_or_symbol: name.raw(),
        };
        self.ast.add_node(NodeTag::StmtFnDecl, data, span)
    }

    /// Add a class declaration.
    pub fn build_class(
        &mut self,
        name: Symbol,
        super_class: Option<Symbol>,
        methods: &[NodeIndex],
        span: Span,
    ) -> NodeIndex {
        let mut extra_data = vec![methods.len().try_into().expect("too many methods.")];
        extra_data.extend(methods.iter().map(|param| param.raw()));
        if let Some(super_class) = super_class {
            extra_data.push(super_class.raw());
        }

        let (lhs, length) = self.ast.add_extra_data(&extra_data);

        let data = NodeData {
            lhs: lhs.raw(),
            rhs: length,
            token_or_symbol: name.raw(),
        };
        self.ast.add_node(NodeTag::StmtClassDecl, data, span)
    }

    /// Add a variable declaration.
    pub fn build_var_decl(
        &mut self,
        name: Symbol,
        value: Option<NodeIndex>,
        span: Span,
    ) -> NodeIndex {
        let data = NodeData {
            lhs: value.map_or(0, NodeIndex::raw),
            rhs: 0,
            token_or_symbol: name.raw(),
        };
        self.ast.add_node(NodeTag::StmtVarDecl, data, span)
    }

    /// Add call.
    pub fn build_call(&mut self, callee: NodeIndex, args: &[NodeIndex], span: Span) -> NodeIndex {
        let (lhs, length) = self
            .ast
            .add_extra_data_iter(args.iter().map(|arg| arg.raw()));

        let data = NodeData {
            lhs: lhs.raw(),
            rhs: length,
            token_or_symbol: callee.raw(),
        };
        self.ast.add_node(NodeTag::Call, data, span)
    }

    /// Add identifier.
    pub fn build_identifier(&mut self, name: Symbol, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: 0,
            rhs: 0,
            token_or_symbol: name.raw(),
        };
        self.ast.add_node(NodeTag::Ident, data, span)
    }

    /// Add number.
    pub fn build_number(&mut self, value: f64, span: Span) -> NodeIndex {
        let value = value.to_bits();
        let lo = (value & 0xFFFF_FFFF) as u32;
        let hi = (value >> 32) as u32;
        let data = NodeData {
            lhs: lo,
            rhs: hi,
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::Number, data, span)
    }

    /// Add boolean.
    pub fn build_boolean(&mut self, value: bool, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: u32::from(value),
            rhs: 0,
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::Boolean, data, span)
    }

    /// Add nil.
    pub fn build_nil(&mut self, span: Span) -> NodeIndex {
        self.ast.add_node(NodeTag::Nil, NodeData::EMPTY, span)
    }

    /// Add group.
    pub fn build_group(&mut self, inner: NodeIndex, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: inner.raw(),
            rhs: 0,
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::Group, data, span)
    }

    /// Add block.
    pub fn build_block(&mut self, statements: &[NodeIndex], span: Span) -> NodeIndex {
        let (lhs, length) = self
            .ast
            .add_extra_data_iter(statements.iter().map(|stmt| stmt.raw()));
        let data = NodeData {
            lhs: lhs.raw(),
            rhs: length,
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::Block, data, span)
    }

    /// Add if statement.
    pub fn build_if(
        &mut self,
        condition: NodeIndex,
        then: NodeIndex,
        else_statement: Option<NodeIndex>,
        span: Span,
    ) -> NodeIndex {
        let mut extra_data = vec![];
        if let Some(else_statement) = else_statement {
            extra_data.push(u32::from(true));
            extra_data.push(else_statement.raw());
        } else {
            extra_data.push(u32::from(false));
        }

        let (lhs, _) = self.ast.add_extra_data(&extra_data);

        let data = NodeData {
            lhs: lhs.raw(),
            rhs: condition.raw(),
            token_or_symbol: then.raw(),
        };
        self.ast.add_node(NodeTag::Block, data, span)
    }

    /// Add a return statement.
    pub fn build_return(&mut self, value: Option<NodeIndex>, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: value.map_or(0, NodeIndex::raw),
            rhs: 0,
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::Return, data, span)
    }

    /// Add a print statement.
    pub fn build_print(&mut self, value: NodeIndex, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: value.raw(),
            rhs: 0,
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::Print, data, span)
    }

    /// Add an assignment expression.
    pub fn build_assignment(&mut self, name: Symbol, value: NodeIndex, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: value.raw(),
            rhs: 0,
            token_or_symbol: name.raw(),
        };
        self.ast.add_node(NodeTag::Assignment, data, span)
    }

    /// Add super method.
    pub fn build_super_method(&mut self, name: Symbol, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: 0,
            rhs: 0,
            token_or_symbol: name.raw(),
        };
        self.ast.add_node(NodeTag::SuperMethod, data, span)
    }

    /// Add field access.
    pub fn build_field_get(&mut self, object: NodeIndex, field: Symbol, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: object.raw(),
            rhs: 0,
            token_or_symbol: field.raw(),
        };
        self.ast.add_node(NodeTag::FieldGet, data, span)
    }

    /// Add field set.
    pub fn build_field_set(
        &mut self,
        object: NodeIndex,
        field: Symbol,
        value: NodeIndex,
        span: Span,
    ) -> NodeIndex {
        let data = NodeData {
            lhs: object.raw(),
            rhs: value.raw(),
            token_or_symbol: field.raw(),
        };
        self.ast.add_node(NodeTag::FieldSet, data, span)
    }

    /// Add for statement.
    pub fn build_for(
        &mut self,
        initializer: Option<NodeIndex>,
        condition: Option<NodeIndex>,
        increment: Option<NodeIndex>,
        body: NodeIndex,
        span: Span,
    ) -> NodeIndex {
        let mut extra_data = vec![];
        // Initializer
        if let Some(initializer) = initializer {
            extra_data.push(u32::from(true));
            extra_data.push(initializer.raw());
        } else {
            extra_data.push(u32::from(false));
        }
        // Condition
        if let Some(condition) = condition {
            extra_data.push(u32::from(true));
            extra_data.push(condition.raw());
        } else {
            extra_data.push(u32::from(false));
        }
        // Increment
        if let Some(increment) = increment {
            extra_data.push(u32::from(true));
            extra_data.push(increment.raw());
        } else {
            extra_data.push(u32::from(false));
        }

        let (lhs, length) = self.ast.add_extra_data(&extra_data);

        let data = NodeData {
            lhs: lhs.raw(),
            rhs: length,
            token_or_symbol: body.raw(),
        };
        self.ast.add_node(NodeTag::For, data, span)
    }

    /// Add a while statement.
    pub fn build_while(&mut self, condition: NodeIndex, body: NodeIndex, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: condition.raw(),
            rhs: body.raw(),
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::While, data, span)
    }

    /// Add an expression statement.
    pub fn build_expression_statement(&mut self, value: NodeIndex, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: value.raw(),
            rhs: 0,
            token_or_symbol: 0,
        };
        self.ast.add_node(NodeTag::ExprStmt, data, span)
    }
}
