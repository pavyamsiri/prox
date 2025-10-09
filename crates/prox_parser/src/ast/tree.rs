use core::convert;
use core::default;
use core::fmt;
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
    /// Convert back into a raw index.
    const fn raw(self) -> u32 {
        self.0
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

    /// Get the stored boolean assuming this node is a `Boolean` tag.
    #[must_use]
    pub const fn bool(&self) -> bool {
        self.lhs == 1
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
    /// `superclass` = index to superclass which must be `Ident`.
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
    /// `lhs` = value.
    /// `rhs` = null.
    /// `token` = unary op enum.
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
    /// String literals.
    /// `lhs` = null.
    /// `rhs` = null.
    /// `token` = intern symbol.
    StringLiteral,
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

    /// Return the string representation.
    const fn as_str(self) -> &'static str {
        match self {
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::Ne => "!=",
            BinaryOp::Eq => "==",
            BinaryOp::And => "and",
            BinaryOp::Or => "or",
        }
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
pub enum UnaryOp {
    /// Numeric negation.
    Neg = 0,
    /// Boolean negation.
    Not = 1,
}

impl UnaryOp {
    /// Convert to raw byte.
    const fn raw(self) -> u8 {
        self as u8
    }

    /// Return the string representation.
    const fn as_str(self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
        }
    }
}

impl convert::TryFrom<u32> for UnaryOp {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Neg),
            1 => Ok(Self::Not),
            _ => Err(()),
        }
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
    /// Super keyword.
    kw_super: Symbol,
    /// This keyword.
    kw_this: Symbol,
    /// The symbol for `init`; the name for constructors.
    init_sym: Symbol,
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
    fn tag(&self, index: NodeIndex) -> NodeTag {
        self.tags[index.as_usize()]
    }

    /// Return the data tag.
    fn data(&self, index: NodeIndex) -> NodeData {
        self.data[index.as_usize()]
    }

    /// Update a node.
    fn update(&mut self, index: NodeIndex, data: NodeData) {
        self.data[index.as_usize()] = data;
    }
}

impl default::Default for Ast {
    fn default() -> Self {
        let interner = Interner::with_hasher(hash::RandomState::new());
        Self::with_interner(interner)
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
    pub fn with_interner(mut interner: Interner) -> Self {
        let mut nodes = NodeList::new();
        let root = nodes.add(NodeTag::Program, NodeData::EMPTY);
        let empty_span = Span {
            start: 0,
            length: 0,
        };

        let kw_super = interner.intern("super");
        let kw_this = interner.intern("this");
        let init_sym = interner.intern("init");

        Self {
            nodes,
            extra_data: Vec::with_capacity(1024),
            spans: vec![empty_span, empty_span],
            strings: interner,
            root,
            kw_super,
            kw_this,
            init_sym,
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

    /// Return a reference to the interner.
    #[must_use]
    pub const fn get_interner(&self) -> &Interner {
        &self.strings
    }

    /// Return a mutable reference to the interner.
    #[must_use]
    pub const fn get_interner_mut(&mut self) -> &mut Interner {
        &mut self.strings
    }

    /// Return the symbol representing the super keyword.
    #[must_use]
    pub const fn kw_super(&self) -> Symbol {
        self.kw_super
    }

    /// Return the symbol representing the super keyword.
    #[must_use]
    pub const fn kw_this(&self) -> Symbol {
        self.kw_this
    }

    /// Return the symbol representing the init constructor name.
    #[must_use]
    pub const fn init_sym(&self) -> Symbol {
        self.init_sym
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
            lhs: value.raw(),
            rhs: 0,
            token_or_symbol: u32::from(op.raw()),
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
    ///
    /// # Panics
    /// This function will panic if given 2^32 or more methods.
    pub fn build_class(
        &mut self,
        name: Symbol,
        super_class: Option<NodeIndex>,
        methods: &[NodeIndex],
        span: Span,
    ) -> NodeIndex {
        let num_methods = methods.len().try_into().expect("too many methods");
        let mut extra_data = vec![num_methods];
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

    /// Add string.
    pub fn build_string(&mut self, name: Symbol, span: Span) -> NodeIndex {
        let data = NodeData {
            lhs: 0,
            rhs: 0,
            token_or_symbol: name.raw(),
        };
        self.ast.add_node(NodeTag::StringLiteral, data, span)
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

// Extra data accessors.
impl Ast {
    /// Return the slice over extra data assuming the node has start in `lhs` and length in `rhs`.
    #[must_use]
    pub fn extra_data_slice(&self, index: NodeIndex) -> &[u32] {
        let data = self.data(index);
        let start = data.lhs as usize;
        let length = data.rhs as usize;
        &self.extra_data[start..(start + length)]
    }

    /// Return a class declaration's extra data. This function does no checking and assumes that the node tag
    /// is `StmtClassDecl`.
    ///
    /// # Panics
    /// This function will panic if the number of methods (1st element of extra data)
    /// is not equal to the length of the extra data array minus 1
    /// (the number of methods + the method count) or the length of the extra data array
    /// minus 2 (the number of methods + the method count + the super class).
    #[must_use]
    pub fn class_decl_extra(&self, index: NodeIndex) -> (&[u32], Option<NodeIndex>) {
        let extra_data = self.extra_data_slice(index);
        let length = extra_data.len();
        let num_methods = extra_data[0] as usize;
        assert!(
            ((num_methods + 1) == length) || (num_methods + 2 == length),
            "extra data holds methods + count + optionally a super class."
        );
        let super_class = (num_methods + 2 == length)
            .then(|| NodeIndex::new(extra_data[num_methods + 1]).expect("malformed AST."));

        (&extra_data[1..=num_methods], super_class)
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

    /// Return a for statements's extra data. This function does no checking and assumes that the node tag
    /// is `For`.
    ///
    /// # Panics
    /// This function expects that the extra data pointed to by the data will have at least
    /// the three flags, `has_initializer`, `has_condition` and `has_increment`.
    #[must_use]
    pub fn for_extra(&self, index: NodeIndex) -> (Option<u32>, Option<u32>, Option<u32>) {
        let mut extra_data = self.extra_data_slice(index).iter();
        assert!(
            extra_data.len() >= 3,
            "extra data should have at least the three flags."
        );

        let has_initializer = *extra_data.next().unwrap() == 1;
        let initializer = has_initializer.then(|| {
            let initializer = extra_data.next().unwrap();
            *initializer
        });

        let has_condition = *extra_data.next().unwrap() == 1;
        let condition = has_condition.then(|| {
            let condition = extra_data.next().unwrap();
            *condition
        });

        let has_increment = *extra_data.next().unwrap() == 1;
        let increment = has_increment.then(|| {
            let increment = extra_data.next().unwrap();
            *increment
        });

        (initializer, condition, increment)
    }
}

// Format an AST node.
impl Ast {
    /// Format an AST node into the given buffer.
    ///
    /// # Errors
    /// If any of the child AST nodes are malformed this function fails.
    /// The buffer may be partially written to still.
    pub fn dump(&self, buffer: &mut impl fmt::Write, index: NodeIndex) -> Result<(), fmt::Error> {
        self.dump_impl(buffer, index, 0)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "formatting code is going to be long."
    )]
    fn dump_impl(
        &self,
        buffer: &mut impl fmt::Write,
        index: NodeIndex,
        depth: u8,
    ) -> Result<(), fmt::Error> {
        let tag = self.tag(index);
        let data = self.data(index);

        let indent = " ".repeat(2 * depth as usize);
        let next_depth = depth.saturating_add(1);

        match tag {
            NodeTag::Program => {
                let statements = self.extra_data_slice(index);
                writeln!(buffer, "{indent}Program:")?;
                for stmt in statements {
                    self.dump_impl(buffer, NodeIndex::new(*stmt).ok_or(fmt::Error)?, next_depth)?;
                }
            }
            NodeTag::StmtFnDecl => {
                let next_indent = " ".repeat(2 * next_depth as usize);
                let second_indent = " ".repeat(2 * (next_depth.saturating_add(1)) as usize);
                let third_indent = " ".repeat(2 * (next_depth.saturating_add(2)) as usize);
                let name = {
                    let sym = data.symbol();
                    self.strings.resolve(sym).ok_or(fmt::Error)?
                };
                let (is_method, body, parameters) = self.fn_decl_extra(index);
                let func_or_meth = if is_method { "meth" } else { "func" };

                writeln!(buffer, "{indent}FnDecl [{name}/{func_or_meth}]:")?;
                writeln!(buffer, "{next_indent}ParamList:")?;

                tracing::debug!("BEFORE PARAM @ decl with index {index:?} ");

                if !parameters.is_empty() {
                    for (param_index, param) in parameters.iter().enumerate() {
                        let param_name = self.resolve((*param).into()).ok_or(fmt::Error)?;
                        writeln!(buffer, "{second_indent}Param{param_index}:")?;
                        writeln!(buffer, "{third_indent}\"{param_name}\"")?;
                    }
                }
                writeln!(buffer, "{next_indent}Body:")?;
                self.dump_impl(
                    buffer,
                    NodeIndex::new(body).ok_or(fmt::Error)?,
                    next_depth.saturating_add(1),
                )?;
            }
            NodeTag::Call => {
                let callee = NodeIndex::new(data.middle()).ok_or(fmt::Error)?;
                let args = self.extra_data_slice(index);

                let next_indent = " ".repeat(2 * next_depth as usize);
                let second_indent = " ".repeat(2 * (next_depth.saturating_add(1)) as usize);
                writeln!(buffer, "{indent}ExprCall:")?;
                writeln!(buffer, "{next_indent}Callee: ")?;
                self.dump_impl(buffer, callee, next_depth.saturating_add(1))?;

                if !args.is_empty() {
                    writeln!(buffer, "{next_indent}Args:")?;
                    for (arg_index, arg) in args.iter().enumerate() {
                        writeln!(buffer, "{second_indent}Arg{arg_index}:")?;
                        self.dump_impl(
                            buffer,
                            NodeIndex::new(*arg).ok_or(fmt::Error)?,
                            next_depth.saturating_add(2),
                        )?;
                    }
                }
            }
            NodeTag::StmtVarDecl => {
                let name = {
                    let sym = data.symbol();
                    self.strings.resolve(sym).ok_or(fmt::Error)?
                };
                writeln!(buffer, "{indent}VarDecl [{name}]:")?;

                if let Some(value) = data.left() {
                    self.dump_impl(buffer, value, next_depth)?;
                }
            }
            NodeTag::StmtClassDecl => {
                let next_indent = " ".repeat(2 * next_depth as usize);
                let second_indent = " ".repeat(2 * (next_depth.saturating_add(1)) as usize);
                let name = {
                    let sym = data.symbol();
                    self.strings.resolve(sym).ok_or(fmt::Error)?
                };
                let (methods, super_class) = self.class_decl_extra(index);
                if let Some(super_class) = super_class {
                    let super_class_name = self
                        .strings
                        .resolve(self.data(super_class).symbol())
                        .ok_or(fmt::Error)?;
                    writeln!(buffer, "{indent}ClassDecl [{name} < {super_class_name}]:")?;
                } else {
                    writeln!(buffer, "{indent}ClassDecl [{name}]:")?;
                }

                writeln!(buffer, "{next_indent}Methods:")?;
                for (method_index, method) in methods.iter().enumerate() {
                    writeln!(buffer, "{second_indent}Method{method_index}:")?;
                    self.dump_impl(
                        buffer,
                        NodeIndex::new(*method).ok_or(fmt::Error)?,
                        next_depth.saturating_add(2),
                    )?;
                }
            }
            NodeTag::Block => {
                let statements = self.extra_data_slice(index);
                writeln!(buffer, "{indent}Block:")?;
                for stmt in statements {
                    self.dump_impl(buffer, NodeIndex::new(*stmt).ok_or(fmt::Error)?, next_depth)?;
                }
            }
            NodeTag::If => {
                let next_indent = " ".repeat(2 * next_depth as usize);

                let condition = data.right().ok_or(fmt::Error)?;
                let then_clause = NodeIndex::new(data.middle()).ok_or(fmt::Error)?;

                writeln!(buffer, "{indent}If:")?;
                writeln!(buffer, "{next_indent}Condition:")?;
                self.dump_impl(buffer, condition, next_depth.saturating_add(1))?;

                writeln!(buffer, "{next_indent}Then:")?;
                self.dump_impl(buffer, then_clause, next_depth.saturating_add(1))?;

                if let Some(else_clause) = self.if_extra(index) {
                    writeln!(buffer, "{next_indent}Else:")?;
                    self.dump_impl(
                        buffer,
                        NodeIndex::new(else_clause).ok_or(fmt::Error)?,
                        next_depth.saturating_add(1),
                    )?;
                }
            }
            NodeTag::For => {
                let next_indent = " ".repeat(2 * next_depth as usize);

                let (initializer, condition, increment) = self.for_extra(index);
                let body = NodeIndex::new(data.middle()).ok_or(fmt::Error)?;

                writeln!(buffer, "{indent}For:")?;

                if let Some(initializer) = initializer {
                    writeln!(buffer, "{next_indent}Initializer:")?;
                    self.dump_impl(
                        buffer,
                        NodeIndex::new(initializer).ok_or(fmt::Error)?,
                        next_depth.saturating_add(1),
                    )?;
                }

                if let Some(condition) = condition {
                    writeln!(buffer, "{next_indent}Condition:")?;
                    self.dump_impl(
                        buffer,
                        NodeIndex::new(condition).ok_or(fmt::Error)?,
                        next_depth.saturating_add(1),
                    )?;
                }

                if let Some(increment) = increment {
                    writeln!(buffer, "{next_indent}Increment:")?;
                    self.dump_impl(
                        buffer,
                        NodeIndex::new(increment).ok_or(fmt::Error)?,
                        next_depth.saturating_add(1),
                    )?;
                }

                writeln!(buffer, "{next_indent}Body:")?;
                self.dump_impl(buffer, body, next_depth.saturating_add(1))?;
            }
            NodeTag::While => {
                let next_indent = " ".repeat(2 * next_depth as usize);

                let condition = data.left().ok_or(fmt::Error)?;
                let body = data.right().ok_or(fmt::Error)?;

                writeln!(buffer, "{indent}While:")?;
                writeln!(buffer, "{next_indent}Condition:")?;
                self.dump_impl(buffer, condition, next_depth.saturating_add(1))?;

                writeln!(buffer, "{next_indent}Body:")?;
                self.dump_impl(buffer, body, next_depth.saturating_add(1))?;
            }
            NodeTag::Return => {
                writeln!(buffer, "{indent}Return:")?;
                if let Some(lhs) = data.left() {
                    self.dump_impl(buffer, lhs, next_depth)?;
                }
            }
            NodeTag::Print => {
                writeln!(buffer, "{indent}Print:")?;
                let lhs = data.left().ok_or(fmt::Error)?;
                self.dump_impl(buffer, lhs, next_depth)?;
            }
            NodeTag::ExprStmt => {
                writeln!(buffer, "{indent}ExprStmt:")?;
                let lhs = data.left().ok_or(fmt::Error)?;
                self.dump_impl(buffer, lhs, next_depth)?;
            }
            NodeTag::Unary => {
                let op = UnaryOp::try_from(data.token_or_symbol)
                    .map_err(|()| fmt::Error)?
                    .as_str();
                let value = data.left().ok_or(fmt::Error)?;

                writeln!(buffer, "ExprUnary [{op}]:")?;
                self.dump_impl(buffer, value, next_depth)?;
            }
            NodeTag::Binary => {
                let op = BinaryOp::try_from(data.token_or_symbol)
                    .map_err(|()| fmt::Error)?
                    .as_str();
                let (Some(lhs), Some(rhs)) = (data.left(), data.right()) else {
                    return Err(fmt::Error);
                };

                let next_indent = " ".repeat(2 * next_depth as usize);

                writeln!(buffer, "{indent}ExprBinary [{op}]:")?;

                writeln!(buffer, "{next_indent}LHS:")?;
                self.dump_impl(buffer, lhs, next_depth.saturating_add(1))?;

                writeln!(buffer, "{next_indent}RHS:")?;
                self.dump_impl(buffer, rhs, next_depth.saturating_add(1))?;
            }
            NodeTag::Group => {
                let value = data.left().ok_or(fmt::Error)?;
                writeln!(buffer, "{indent}Group:")?;
                self.dump_impl(buffer, value, next_depth)?;
            }
            NodeTag::Assignment => {
                let value = data.left().ok_or(fmt::Error)?;
                let name = {
                    let sym = data.symbol();
                    self.strings.resolve(sym).ok_or(fmt::Error)?
                };

                writeln!(buffer, "{indent}ExprAssign [{name}]:")?;
                self.dump_impl(buffer, value, next_depth)?;
            }
            NodeTag::FieldGet => {
                let next_indent = " ".repeat(2 * next_depth as usize);
                let second_indent = " ".repeat(2 * next_depth.saturating_add(1) as usize);
                let object = data.left().ok_or(fmt::Error)?;
                let field = {
                    let sym = data.symbol();
                    self.strings.resolve(sym).ok_or(fmt::Error)?
                };

                writeln!(buffer, "{indent}FieldGet:")?;
                writeln!(buffer, "{next_indent}Instance:")?;
                self.dump_impl(buffer, object, next_depth.saturating_add(1))?;
                writeln!(buffer, "{next_indent}Field:")?;
                writeln!(buffer, "{second_indent}{field}")?;
            }
            NodeTag::FieldSet => {
                let next_indent = " ".repeat(2 * next_depth as usize);
                let second_indent = " ".repeat(2 * next_depth.saturating_add(1) as usize);
                let object = data.left().ok_or(fmt::Error)?;
                let value = data.right().ok_or(fmt::Error)?;
                let field = {
                    let sym = data.symbol();
                    self.strings.resolve(sym).ok_or(fmt::Error)?
                };

                writeln!(buffer, "{indent}FieldSet:")?;
                writeln!(buffer, "{next_indent}Instance:")?;
                self.dump_impl(buffer, object, next_depth.saturating_add(1))?;
                writeln!(buffer, "{next_indent}Field:")?;
                writeln!(buffer, "{second_indent}{field}")?;
                writeln!(buffer, "{next_indent}Value:")?;
                self.dump_impl(buffer, value, next_depth.saturating_add(1))?;
            }
            NodeTag::SuperMethod => {
                let next_indent = " ".repeat(2 * next_depth as usize);
                let name = {
                    let sym = data.symbol();
                    self.strings.resolve(sym).ok_or(fmt::Error)?
                };

                writeln!(buffer, "{indent}SuperMethod:")?;
                writeln!(buffer, "{next_indent}{name}")?;
            }
            NodeTag::Ident => {
                let lexeme = self.strings.resolve(data.symbol()).ok_or(fmt::Error)?;
                writeln!(buffer, "{indent}{lexeme}")?;
            }
            NodeTag::StringLiteral => {
                let lexeme = self.strings.resolve(data.symbol()).ok_or(fmt::Error)?;
                writeln!(buffer, "{indent}{lexeme:?}")?;
            }
            NodeTag::Number => {
                let number = data.number();
                writeln!(buffer, "{indent}{number}")?;
            }
            NodeTag::Boolean => {
                let flag = data.lhs == 1;
                writeln!(buffer, "{indent}{flag}")?;
            }
            NodeTag::Nil => writeln!(buffer, "{indent}nil")?,
            NodeTag::Error => writeln!(buffer, "{indent}error")?,
        }

        Ok(())
    }
}
