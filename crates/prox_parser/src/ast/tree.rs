use core::convert;
use core::default;
use core::fmt;
use core::iter;
use core::num::NonZeroU32;
use prox_interner::{Interner, Symbol};
use prox_lexer::span::Span;
use std::hash;

/// Index to AST node wrapping `u32`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeIndex(NonZeroU32);

impl NodeIndex {
    /// Create a new node index.
    /// The value `0` is reserved for the null node.
    pub fn new(index: u32) -> Option<Self> {
        NonZeroU32::new(index).map(NodeIndex)
    }

    /// Convert back into a raw index.
    #[must_use]
    pub const fn raw(self) -> u32 {
        self.0.get()
    }

    /// Convert into a `usize`.
    #[must_use]
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

#[derive(Debug, Clone, Copy)]
pub struct NodeData {
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

    /// Get left node if present
    #[must_use]
    pub fn left(&self) -> Option<NodeIndex> {
        NodeIndex::new(self.lhs)
    }

    /// Get right node if present
    #[must_use]
    pub fn right(&self) -> Option<NodeIndex> {
        NodeIndex::new(self.rhs)
    }

    /// Get the stored number assuming this node is a `Number` tag.
    #[must_use]
    pub fn number(&self) -> f64 {
        let lo = u64::from(self.lhs);
        let hi = u64::from(self.rhs);
        let bits = (hi << 32) | lo;
        f64::from_bits(bits)
    }

    /// Get the symbol.
    #[must_use]
    pub fn symbol(&self) -> Symbol {
        Symbol::from(self.token_or_symbol)
    }

    /// Get the token or symbol value.
    #[must_use]
    pub const fn middle(&self) -> u32 {
        self.token_or_symbol
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeTag {
    /// The topmost level structure.
    /// `lhs` = extra data index.
    /// `rhs` = extra data length.
    /// `token` = null.
    /// `extra_data` = (s1, s2, ..., sn)
    /// `si` = `i`-th statement or declaration.
    Program,

    // Declarations.
    /// A function declaration.
    /// `lhs` = extra data index.
    /// `rhs` = extra data length.
    /// `token` = name of function (symbol).
    /// `extra_data` = (is\_method, block, p1, p2, ..., pn)
    /// `method_flag` = 0 means function and 1 means method.
    /// `pi` = name of parameter `i` (symbol).
    StmtFnDecl,
    /// A variable declaration.
    /// `lhs` = rvalue?.
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
    /// `lhs` = value?.
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
pub enum BinaryOp {
    /// Multiplication.
    Mul = 2,
    /// Division.
    Div = 3,
    /// Addition.
    Add = 0,
    /// Subtraction.
    Sub = 1,
    /// Less than.
    Lt = 4,
    /// Less than or equal.
    Le = 5,
    /// Greater than.
    Gt = 6,
    /// Greater than or equal.
    Ge = 7,
    /// Not equal.
    Ne = 9,
    /// Equal.
    Eq = 8,
    /// And.
    And = 10,
    /// Or.
    Or = 11,
}

impl BinaryOp {
    /// Convert to raw byte.
    const fn raw(self) -> u8 {
        self as u8
    }
}

impl convert::TryFrom<u32> for BinaryOp {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Add),
            1 => Ok(Self::Sub),
            2 => Ok(Self::Mul),
            3 => Ok(Self::Div),
            4 => Ok(Self::Lt),
            5 => Ok(Self::Le),
            6 => Ok(Self::Gt),
            7 => Ok(Self::Ge),
            8 => Ok(Self::Eq),
            9 => Ok(Self::Ne),
            10 => Ok(Self::And),
            11 => Ok(Self::Or),
            _ => Err(()),
        }
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

pub struct Ast {
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

impl fmt::Debug for Ast {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ast")
            .field("nodes", &self.nodes)
            .field("extra_data", &self.extra_data)
            .field("spans", &self.spans)
            .field("root", &self.root)
            .finish_non_exhaustive()
    }
}

/// A struct-of-arrays representation of the AST as a list of nodes.
#[derive(Debug)]
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

    /// Return the node tag.
    pub fn tag(&self, index: NodeIndex) -> NodeTag {
        self.tags[index.as_usize()]
    }

    /// Return the data tag.
    pub fn data(&self, index: NodeIndex) -> NodeData {
        self.data[index.as_usize()]
    }

    /// Update a node.
    pub fn update(&mut self, index: NodeIndex, data: NodeData) {
        self.data[index.as_usize()] = data;
    }
}

impl default::Default for Ast {
    fn default() -> Self {
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
            strings: Interner::with_hasher(hash::RandomState::new()),
            root,
        }
    }
}

impl Ast {
    /// Create a new empty AST.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new AST with a pre-existing interner.
    #[must_use]
    pub fn with_interner(interner: Interner) -> Self {
        Self {
            strings: interner,
            ..Default::default()
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
        self.extra_data.extend(data);

        let end: u32 = self
            .extra_data
            .len()
            .try_into()
            .expect("extra index overflowed.");

        let length = end - index;
        (extra_index, length)
    }

    /// Get the node's span.
    #[must_use]
    pub fn span(&self, index: NodeIndex) -> Span {
        self.spans[index.as_usize()]
    }

    /// Get the root.
    #[must_use]
    pub const fn root(&self) -> NodeIndex {
        self.root
    }

    /// Get the node data.
    #[must_use]
    pub fn data(&self, index: NodeIndex) -> NodeData {
        self.nodes.data(index)
    }

    /// Get the node tag.
    #[must_use]
    pub fn tag(&self, index: NodeIndex) -> NodeTag {
        self.nodes.tag(index)
    }

    /// Resolve a symbol into a string.
    #[must_use]
    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.strings.resolve(symbol)
    }
}

#[derive(Default)]
pub struct AstBuilder {
    ast: Ast,
}

impl AstBuilder {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new builder with a pre-existing interner.
    #[must_use]
    pub fn with_interner(interner: Interner) -> Self {
        Self {
            ast: Ast::with_interner(interner),
        }
    }

    #[must_use]
    pub fn finish(mut self, statements: &[NodeIndex]) -> Ast {
        let (lhs, length) = self
            .ast
            .add_extra_data_iter(statements.iter().map(|stmt| stmt.raw()));
        self.ast.nodes.update(
            self.ast.root,
            NodeData {
                lhs: lhs.raw(),
                rhs: length,
                token_or_symbol: 0,
            },
        );
        self.ast
    }

    /// Intern a string.
    pub fn intern(&mut self, text: &str) -> Symbol {
        self.ast.strings.intern(text)
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
        params: &[Symbol],
        block: NodeIndex,
        is_method: bool,
        span: Span,
    ) -> NodeIndex {
        let mut extra_data = vec![u32::from(is_method), block.raw()];
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
        let bits = value.to_bits().to_le();
        let lo = (bits & 0xFFFF_FFFF) as u32;
        let hi = (bits >> 32) as u32;
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
        self.ast.add_node(NodeTag::If, data, span)
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

impl Ast {
    pub fn traverse(&self) {
        self.visit_node(self.root);
    }

    /// Visit node.
    fn visit_node(&self, index: NodeIndex) {
        let data = self.nodes.data(index);
        let tag = self.nodes.tag(index);

        println!("At {tag:?} with data = {data:#?}");

        // Return an iterator based on node type
        match tag {
            NodeTag::Program => {
                println!("At program root.");
                let extra_data = self.extra_data(index);
                for stmt_index in extra_data {
                    println!("Visiting {stmt_index}...");
                    self.visit_node(NodeIndex::new(stmt_index).expect("encountered null node."));
                }
            }
            NodeTag::StmtFnDecl => {
                let mut extra_data = self.extra_data(index);
                let is_method = extra_data.next().unwrap();
                println!("At function declaration. Method = {}", is_method == 1);
                let block = extra_data.next().unwrap();
                for parameter in extra_data {
                    let name = self.strings.resolve(Symbol::from(parameter)).unwrap();
                    println!("\tParameter = {name:?}");
                }
                self.visit_node(NodeIndex::new(block).unwrap());
            }
            NodeTag::Block => {
                let extra_data: Vec<_> = self.extra_data(index).collect();
                println!("Visiting {extra_data:?}...");
                for stmt_index in extra_data {
                    self.visit_node(NodeIndex::new(stmt_index).expect("encountered null node."));
                }
            }
            NodeTag::If => {
                println!("If");
                let condition = NodeIndex::new(data.rhs).unwrap();
                let then = NodeIndex::new(data.token_or_symbol).unwrap();
                println!("CONDITION");
                self.visit_node(condition);
                println!("THEN");
                self.visit_node(then);
                if let Some(else_block) = self.extra_data(index).next() {
                    println!("ELSE");
                    self.visit_node(NodeIndex::new(else_block).unwrap());
                }
            }
            NodeTag::Binary => {
                let op = BinaryOp::try_from(data.token_or_symbol).unwrap();
                println!("Binary: lhs {op:?} rhs");
                self.visit_node(NodeIndex::new(data.lhs).unwrap());
                self.visit_node(NodeIndex::new(data.rhs).unwrap());
            }
            NodeTag::Ident => {
                let name = self
                    .strings
                    .resolve(Symbol::from(data.token_or_symbol))
                    .unwrap();
                println!("Identifier = {name}");
            }
            NodeTag::Number => {
                let value = data.number();
                println!("Number = {value}");
            }
            NodeTag::Return => {
                println!("RETURNING");
                if let Some(value) = NodeIndex::new(data.lhs) {
                    println!("with value");
                    self.visit_node(value);
                }
            }
            NodeTag::Print => {
                println!("PRINTING");
                let value = NodeIndex::new(data.lhs).unwrap();
                self.visit_node(value);
            }
            NodeTag::Call => {
                println!("CALLING");
                let callee = NodeIndex::new(data.token_or_symbol).unwrap();
                self.visit_node(callee);
                let extra_data = self.extra_data(index);
                for arg in extra_data {
                    let arg_index = NodeIndex::new(arg).unwrap();
                    self.visit_node(arg_index);
                }
            }
            NodeTag::StmtVarDecl => {
                let name = self
                    .strings
                    .resolve(Symbol::from(data.token_or_symbol))
                    .unwrap();
                println!("DECLARING {name}");
                if let Some(rvalue) = NodeIndex::new(data.lhs) {
                    println!("rvalue");
                    self.visit_node(rvalue);
                }
            }
            _ => {
                todo!()
            }
        }
    }
}

pub struct ExtraDataIterator<'data> {
    data: &'data [u32],
    start: usize,
    end: usize,
}

impl iter::Iterator for ExtraDataIterator<'_> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        (self.start < self.end).then(|| {
            let index = self.start;
            self.start += 1;
            self.data[index]
        })
    }
}

impl Ast {
    /// Return the slice over extra data assuming the node has start in `lhs` and length in `rhs`.
    #[must_use]
    pub fn extra_data_slice(&self, index: NodeIndex) -> &[u32] {
        let data = self.data(index);
        let start = data.lhs as usize;
        let length = data.rhs as usize;
        &self.extra_data[start..(start + length)]
    }

    /// Return a function declaration's extra data. This function does no checking and assumes that the node tag
    /// is `StmtFnDecl`.
    #[must_use]
    pub fn fn_decl_extra(&self, index: NodeIndex) -> (bool, u32, &[u32]) {
        let extra_data = self.extra_data_slice(index);
        let is_method = extra_data[0] == 1;
        let block = extra_data[1];
        let parameters = &extra_data[2..];
        (is_method, block, parameters)
    }

    /// Return an if statement's extra data. This function does no checking and assumes that the node tag
    /// is `If`.
    #[must_use]
    pub fn if_extra(&self, index: NodeIndex) -> Option<u32> {
        let data = self.data(index);
        let start = data.lhs as usize;
        let has_else = self.extra_data[start] == 1;
        has_else.then(|| self.extra_data[start + 1])
    }

    #[must_use]
    pub fn extra_data(&self, index: NodeIndex) -> ExtraDataIterator<'_> {
        let data = self.nodes.data(index);
        let tag = self.nodes.tag(index);
        println!("Extra data = {:?}", self.extra_data);

        match tag {
            NodeTag::Program | NodeTag::StmtFnDecl | NodeTag::Block | NodeTag::Call => {
                let start = data.lhs as usize;
                let length = data.rhs as usize;
                ExtraDataIterator {
                    data: &self.extra_data,
                    start,
                    end: start + length,
                }
            }
            NodeTag::StmtVarDecl => todo!(),
            NodeTag::StmtClassDecl => todo!(),
            NodeTag::If => {
                let start = data.lhs as usize;
                let has_else = self.extra_data[start] == 1;
                if has_else {
                    ExtraDataIterator {
                        data: &self.extra_data,
                        start: start + 1,
                        end: start + 2,
                    }
                } else {
                    ExtraDataIterator {
                        data: &self.extra_data,
                        start: 0,
                        end: 0,
                    }
                }
            }
            NodeTag::Return => todo!(),
            NodeTag::Print => todo!(),
            NodeTag::For => todo!(),
            NodeTag::While => todo!(),
            NodeTag::ExprStmt => todo!(),
            NodeTag::Unary => todo!(),
            NodeTag::Binary => todo!(),
            NodeTag::Group => todo!(),
            NodeTag::Assignment => todo!(),
            NodeTag::SuperMethod => todo!(),
            NodeTag::FieldGet => todo!(),
            NodeTag::FieldSet => todo!(),
            NodeTag::Ident => todo!(),
            NodeTag::Number => todo!(),
            NodeTag::Boolean => todo!(),
            NodeTag::Nil => todo!(),
            NodeTag::Error => todo!(),
        }
    }
}
