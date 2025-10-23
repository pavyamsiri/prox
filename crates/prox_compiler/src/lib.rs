use core::fmt;
use mitsein::vec1::Vec1;
use prox_bytecode::InstructionOffset;
use prox_bytecode::Opcode;
use prox_bytecode::OpcodeEmitter;
use prox_bytecode::StackSlot;
use prox_bytecode::UpvalueIndex;
use prox_bytecode::chunk::Chunk;
use prox_bytecode::chunk::ChunkId;
use prox_bytecode::class::Class;
use prox_bytecode::function::Closure;
use prox_bytecode::function::Upvalue;
use prox_bytecode::is_jump_opcode;
use prox_bytecode::pool::ConstantInterner;
use prox_interner::ConstantIndex;
use prox_interner::{Interner, Symbol};
use prox_lexer::SourceCode;
use prox_parser::ast::Ast;
use prox_parser::ast::BinaryOp;
use prox_parser::ast::NodeIndex;
use prox_parser::ast::NodeTag;
use prox_parser::ast::UnaryOp;
use prox_span::Span;

macro_rules! unwrap_node {
    ($node:expr => $span:expr) => {
        $node.ok_or(CompilationError {
            kind: CompilationErrorKind::NullNode,
            span: $span,
        })
    };
}

macro_rules! convert_node {
    ($index:expr => $span:expr) => {
        NodeIndex::new($index).ok_or(CompilationError {
            kind: CompilationErrorKind::NullNode,
            span: $span,
        })
    };
}

macro_rules! map_node {
    ($index:expr => $span:expr) => {
        if let Some(node) = $index {
            Some(convert_node!(node => $span)?)
        } else {
            None
        }
    };
}

#[expect(dead_code, reason = "not formatting errors right now.")]
pub struct CompilationError {
    kind: CompilationErrorKind,
    span: Span,
}

enum CompilationErrorKind {
    /// Encountered a null node.
    NullNode,
    /// Patching a jump using an instruction index that is out of stream.
    OutOfBoundsJumpPatch,
    /// Patching a non-jump instruction.
    InvalidJumpPatch,
    /// Allocated too many chunks.
    TooManyChunks,
}

pub struct ChunkBuilder {
    name: Symbol,
    stream: Vec<u8>,
    starts: Vec<usize>,
    spans: Vec<Span>,
}

pub struct ChunkPoolBuilder {
    root: ChunkBuilder,
    functions: Vec<ChunkBuilder>,
}

pub struct ChunkPool {
    root: Chunk,
    functions: Box<[Chunk]>,
}

impl ChunkBuilder {
    const fn new(name: Symbol) -> Self {
        Self {
            name,
            stream: Vec::new(),
            starts: Vec::new(),
            spans: Vec::new(),
        }
    }

    fn finish(self) -> Chunk {
        Chunk {
            name: self.name,
            stream: self.stream.into_boxed_slice(),
            starts: self.starts.into_boxed_slice(),
            spans: self.spans.into_boxed_slice(),
        }
    }

    fn emit_opcode(&mut self, opcode: Opcode, span: Span) -> usize {
        let label = self.get_label();
        self.starts.push(label);
        self.spans.push(span);
        opcode.encode(self);
        label
    }

    const fn get_label(&self) -> usize {
        self.stream.len()
    }

    fn patch_jump(&mut self, start: usize, span: Span) -> Result<(), CompilationError> {
        let current_label = self.get_label();
        if (start + 5) >= current_label {
            return Err(CompilationError {
                kind: CompilationErrorKind::OutOfBoundsJumpPatch,
                span,
            });
        }

        let offset = i32::try_from(current_label - start)
            .expect("`current_label` was already checked to be greater than `start`.");
        let opcode = self.stream[start];

        if !is_jump_opcode(opcode) {
            return Err(CompilationError {
                kind: CompilationErrorKind::InvalidJumpPatch,
                span,
            });
        }

        let [first, second, third, fourth] = offset.to_le_bytes();

        self.stream[start + 1] = first;
        self.stream[start + 2] = second;
        self.stream[start + 3] = third;
        self.stream[start + 4] = fourth;

        Ok(())
    }
}

impl OpcodeEmitter for ChunkBuilder {
    fn emit_u8(&mut self, value: u8) {
        self.stream.push(value);
    }

    fn emit_u32(&mut self, value: u32) {
        for byte in value.to_le_bytes() {
            self.stream.push(byte);
        }
    }

    fn emit_i32(&mut self, value: i32) {
        for byte in value.to_le_bytes() {
            self.stream.push(byte);
        }
    }
}

impl ChunkPoolBuilder {
    const fn new(name: Symbol) -> Self {
        Self {
            root: ChunkBuilder::new(name),
            functions: Vec::new(),
        }
    }

    fn finish(self) -> ChunkPool {
        ChunkPool {
            root: self.root.finish(),
            functions: self
                .functions
                .into_iter()
                .map(ChunkBuilder::finish)
                .collect(),
        }
    }

    fn get(&self, index: ChunkId, span: Span) -> Result<&ChunkBuilder, CompilationError> {
        match index.to_usize() {
            0 => Ok(&self.root),
            index => self.functions.get(index - 1).ok_or(CompilationError {
                kind: CompilationErrorKind::NullNode,
                span,
            }),
        }
    }

    fn get_mut(
        &mut self,
        index: ChunkId,
        span: Span,
    ) -> Result<&mut ChunkBuilder, CompilationError> {
        match index.to_usize() {
            0 => Ok(&mut self.root),
            index => self.functions.get_mut(index - 1).ok_or(CompilationError {
                kind: CompilationErrorKind::NullNode,
                span,
            }),
        }
    }

    /// Allocate a new chunk given its name.
    fn allocate(&mut self, name: Symbol, span: Span) -> Result<ChunkId, CompilationError> {
        self.functions.push(ChunkBuilder::new(name));
        ChunkId::try_from(self.functions.len()).map_err(|_err| CompilationError {
            kind: CompilationErrorKind::TooManyChunks,
            span,
        })
    }
}

pub struct Compilation {
    pool: ChunkPool,
    resolver: ConstantInterner,
}

impl Compilation {
    /// Disassemble the compilation.
    /// # Errors
    /// This function will error if it can not write into the buffer or if the chunk pool is malformed.
    pub fn disassemble(
        &self,
        buffer: &mut impl fmt::Write,
        source: &SourceCode<'_>,
    ) -> Result<(), fmt::Error> {
        self.pool.root.disassemble(buffer, source, &self.resolver)?;

        for chunk in self.pool.functions.iter() {
            chunk.disassemble(buffer, source, &self.resolver)?;
        }

        Ok(())
    }
}

struct Local {
    name: Symbol,
    depth: usize,
    is_captured: bool,
}

enum FunctionKind {
    Function,
    Method,
}

struct CompilableClosure<'params> {
    name: Symbol,
    kind: FunctionKind,
    parameters: &'params [u32],
    body: NodeIndex,
    span: Span,
}

struct CompilerContext {
    locals: Vec1<Local>,
    depth: usize,
    upvalues: Vec<Upvalue>,
}

impl CompilerContext {
    fn new(kind: &FunctionKind, interner: &mut ConstantInterner) -> Self {
        let base_local = match *kind {
            FunctionKind::Function => interner.intern(""),
            FunctionKind::Method => interner.intern("this"),
        };
        Self {
            locals: Vec1::from_one(Local {
                name: base_local,
                depth: 0,
                is_captured: false,
            }),
            upvalues: Vec::new(),
            depth: 0,
        }
    }

    const fn begin_scope(&mut self) {
        self.depth += 1;
    }

    fn end_scope(&mut self, chunk: &mut ChunkBuilder, span: Span) {
        let old_depth = self.depth;
        self.depth -= 1;

        for local in self
            .locals
            .iter()
            .rev()
            .take_while(|local| local.depth >= old_depth)
        {
            if local.is_captured {
                chunk.emit_opcode(Opcode::CloseUpvalue, span);
            } else {
                chunk.emit_opcode(Opcode::Pop, span);
            }
        }
    }

    fn add_local(&mut self, name: Symbol) -> StackSlot {
        self.locals.push(Local {
            name,
            depth: self.depth,
            is_captured: false,
        });
        StackSlot::try_from(usize::from(self.locals.len()) - 1).unwrap()
    }

    fn add_upvalue(&mut self, upvalue: Upvalue) -> u32 {
        if let Some(index) = self.upvalues.iter().position(|&val| val == upvalue) {
            return u32::try_from(index).expect("not supporting upvalue indices beyond 2^32.");
        }
        self.upvalues.push(upvalue);
        u32::try_from(self.upvalues.len() - 1).expect("not supporting upvalue indices beyond 2^32.")
    }

    fn resolve_local(&self, name: Symbol) -> Option<StackSlot> {
        for (index, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                return Some(
                    index
                        .try_into()
                        .expect("not supporting stack sizes beyond 2^32 values."),
                );
            }
        }
        None
    }

    fn assign_variable(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        name: Symbol,
        span: Span,
    ) -> Result<(), CompilationError> {
        if self.depth > 0 {
            tracing::debug!("Assigning {name:?} to local depth {}", self.depth);
            self.add_local(name);
        }
        // Global scope
        else {
            tracing::debug!("Assigning {name:?} to global");
            pool.get_mut(current_chunk, span)?
                .emit_opcode(Opcode::SetGlobal(name), span);
        }
        Ok(())
    }
}

struct Compiler {
    contexts: Vec1<CompilerContext>,
}

impl Compiler {
    fn begin_context(&mut self, kind: &FunctionKind, interner: &mut ConstantInterner) {
        self.contexts.push(CompilerContext::new(kind, interner));
    }

    fn end_context(&mut self) -> Option<CompilerContext> {
        self.contexts.pop_if_many().or_none()
    }

    fn get_context_mut(&mut self, index: usize) -> Option<&mut CompilerContext> {
        self.contexts.get_mut(index)
    }

    fn current_context(&self) -> &CompilerContext {
        self.contexts.last()
    }

    fn current_context_mut(&mut self) -> &mut CompilerContext {
        self.contexts.last_mut()
    }

    /// Begin a new scope.
    fn begin_scope(&mut self) {
        self.current_context_mut().begin_scope();
    }

    /// End scope.
    fn end_scope(&mut self, chunk: &mut ChunkBuilder, span: Span) {
        self.current_context_mut().end_scope(chunk, span);
    }

    /// Add a local variable.
    fn add_local(&mut self, name: Symbol) {
        self.current_context_mut().add_local(name);
    }

    fn assign_variable(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        name: Symbol,
        span: Span,
    ) -> Result<(), CompilationError> {
        self.current_context_mut()
            .assign_variable(pool, current_chunk, name, span)
    }

    fn resolve_local(&self, name: Symbol) -> Option<StackSlot> {
        self.current_context().resolve_local(name)
    }

    /// Resolve an upvalue.
    fn resolve_upvalue(&mut self, name: Symbol) -> Option<UpvalueIndex> {
        // Go from innermost scope to outermost scope.
        let num_contexts = usize::from(self.contexts.len());
        let sentinel = num_contexts - 1;
        for context_index in (0..sentinel).rev() {
            if let Some(slot) = self
                .get_context_mut(context_index)
                .expect("must be in bounds")
                .resolve_local(name)
            {
                // Mark local as captured.
                self.get_context_mut(context_index)
                    .expect("must be in bounds")
                    .locals
                    .get_mut(slot.to_usize())
                    .expect("stack slot is guaranteed in bounds.")
                    .is_captured = true;

                // Immediate child context has local upvalue.
                let mut upvalue = self
                    .contexts
                    .get_mut(context_index + 1)
                    .expect("must be in bounds.")
                    .add_upvalue(Upvalue::Local(slot));
                // Mark descendants as having non-local upvalues
                for child_index in (context_index + 2)..num_contexts {
                    upvalue = self
                        .contexts
                        .get_mut(child_index)
                        .expect("must be in bounds")
                        .add_upvalue(Upvalue::Upvalue(UpvalueIndex::from(upvalue)));
                }
                return Some(UpvalueIndex::from(upvalue));
            }
        }
        None
    }
}

impl Compiler {
    fn new(interner: &mut ConstantInterner) -> Self {
        Self {
            contexts: Vec1::from_one(CompilerContext::new(&FunctionKind::Function, interner)),
        }
    }

    fn compile(
        mut self,
        mut pool: ChunkPoolBuilder,
        ast: &Ast,
        mut interner: ConstantInterner,
    ) -> Result<Compilation, CompilationError> {
        self.compile_stmt(&mut pool, ChunkId::from(0), ast, &mut interner, ast.root())?;

        Ok(Compilation {
            pool: pool.finish(),
            resolver: interner,
        })
    }

    fn compile_stmt(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let tag = ast.tag(index);

        match tag {
            NodeTag::Program => self.compile_program(pool, current_chunk, ast, interner, index),
            NodeTag::StmtVarDecl => {
                self.compile_var_decl(pool, current_chunk, ast, interner, index)
            }
            NodeTag::StmtFnDecl => self.compile_fn_decl(pool, current_chunk, ast, interner, index),
            NodeTag::StmtClassDecl => {
                self.compile_class_decl(pool, current_chunk, ast, interner, index)
            }
            NodeTag::Block => self.compile_block(pool, current_chunk, ast, interner, index),
            NodeTag::If => self.compile_if(pool, current_chunk, ast, interner, index),
            NodeTag::Return => self.compile_return(pool, current_chunk, ast, interner, index),
            NodeTag::Print => self.compile_print(pool, current_chunk, ast, interner, index),
            NodeTag::For => self.compile_for(pool, current_chunk, ast, interner, index),
            NodeTag::While => self.compile_while(pool, current_chunk, ast, interner, index),
            NodeTag::ExprStmt => self.compile_expr_stmt(pool, current_chunk, ast, interner, index),
            NodeTag::Unary => self.compile_unary_expr(pool, current_chunk, ast, interner, index),
            NodeTag::Binary => self.compile_binary_expr(pool, current_chunk, ast, interner, index),
            NodeTag::Call => self.compile_call(pool, current_chunk, ast, interner, index),
            NodeTag::Group => self.compile_group(pool, current_chunk, ast, interner, index),
            NodeTag::Assignment => {
                self.compile_assignment(pool, current_chunk, ast, interner, index)
            }
            NodeTag::SuperMethod => {
                self.compile_get_super(pool, current_chunk, ast, interner, index)
            }
            NodeTag::FieldGet => Self::compile_property_get(pool, current_chunk, ast, index),
            NodeTag::FieldSet => Self::compile_property_set(pool, current_chunk, ast, index),
            NodeTag::Ident => self.compile_identifier(pool, current_chunk, ast, index),
            NodeTag::StringLiteral => Self::compile_string(pool, current_chunk, ast, index),
            NodeTag::Number => Self::compile_number(pool, current_chunk, ast, interner, index),
            NodeTag::Boolean => Self::compile_bool(pool, current_chunk, ast, index),
            NodeTag::Nil => Self::compile_nil(pool, current_chunk, ast, index),
            NodeTag::Error => Err(CompilationError {
                kind: CompilationErrorKind::NullNode,
                span: ast.span(index),
            }),
        }
    }

    /// Compile a program node.
    fn compile_program(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let extra_data = ast.extra_data_slice(index);
        let span = ast.span(index);
        for stmt_index in extra_data.iter().copied() {
            self.compile_stmt(
                pool,
                current_chunk,
                ast,
                interner,
                convert_node!(stmt_index => span)?,
            )?;
        }
        Ok(())
    }

    /// Compile a block node.
    fn compile_block(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        self.begin_scope();
        let extra_data = ast.extra_data_slice(index);
        let span = ast.span(index);
        for stmt_index in extra_data.iter().copied() {
            self.compile_stmt(
                pool,
                current_chunk,
                ast,
                interner,
                convert_node!(stmt_index => span)?,
            )?;
        }

        let chunk = pool.get_mut(current_chunk, span)?;
        self.end_scope(chunk, span);
        Ok(())
    }

    /// Compile an if statement.
    fn compile_if(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        const DEFAULT_OFFSET: InstructionOffset = InstructionOffset::from_i32(0xBEEF);
        let data = ast.data(index);
        let span = ast.span(index);
        let condition = unwrap_node!(data.right() => span)?;
        let then_clause = convert_node!(data.middle() => span)?;
        let else_clause = map_node!(ast.if_extra(index) => span);

        self.compile_stmt(pool, current_chunk, ast, interner, condition)?;

        // Emit unpatched jump to else clause or end
        let jump_to_else = {
            let current_chunk = pool.get_mut(current_chunk, span)?;
            let jump_to_else = current_chunk.emit_opcode(Opcode::JumpIfFalse(DEFAULT_OFFSET), span);
            current_chunk.emit_opcode(Opcode::Pop, span);
            jump_to_else
        };

        // Then clause
        self.compile_stmt(pool, current_chunk, ast, interner, then_clause)?;

        // Emit unpatched jump to end
        let jump_to_end = {
            let current_chunk = pool.get_mut(current_chunk, span)?;
            let jump_to_end = current_chunk.emit_opcode(Opcode::JumpIfFalse(DEFAULT_OFFSET), span);

            // Patch jump to else
            current_chunk.patch_jump(jump_to_else, span)?;
            current_chunk.emit_opcode(Opcode::Pop, span);
            jump_to_end
        };

        // Handle else clause
        if let Some(else_clause) = else_clause {
            self.compile_stmt(pool, current_chunk, ast, interner, else_clause)?;
        }

        pool.get_mut(current_chunk, span)?
            .patch_jump(jump_to_end, span)?;

        Ok(())
    }

    /// Compile an expression statement.
    fn compile_expr_stmt(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let value_node = unwrap_node!(ast.data(index).left() => span)?;
        self.compile_stmt(pool, current_chunk, ast, interner, value_node)?;

        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::Pop, span);

        Ok(())
    }

    /// Compile an expression group.
    fn compile_group(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let value_node = unwrap_node!(ast.data(index).left() => span)?;
        self.compile_stmt(pool, current_chunk, ast, interner, value_node)?;

        Ok(())
    }

    /// Compile a for statement.
    fn compile_for(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        const DEFAULT_OFFSET: InstructionOffset = InstructionOffset::from_i32(-0x4262);
        let data = ast.data(index);
        let span = ast.span(index);
        let (initializer, condition, increment) = ast.for_extra(index);
        let body = convert_node!(data.middle() => span)?;

        let initializer = map_node!(initializer => span);
        let condition = map_node!(condition => span);
        let increment = map_node!(increment => span);

        self.begin_scope();

        if let Some(initializer) = initializer {
            self.compile_stmt(pool, current_chunk, ast, interner, initializer)?;
        }

        let start_label = pool.get(current_chunk, span)?.get_label();

        // The instruction index to the jump to leave the for loop.
        let exit_jump = if let Some(condition) = condition {
            self.compile_stmt(pool, current_chunk, ast, interner, condition)?;
            let current_chunk = pool.get_mut(current_chunk, span)?;
            let exit_jump = current_chunk.emit_opcode(Opcode::JumpIfFalse(DEFAULT_OFFSET), span);
            current_chunk.emit_opcode(Opcode::Pop, span);
            Some(exit_jump)
        } else {
            None
        };

        // Loop body
        self.compile_stmt(pool, current_chunk, ast, interner, body)?;

        // Increment
        if let Some(increment) = increment {
            self.compile_stmt(pool, current_chunk, ast, interner, increment)?;
            pool.get_mut(current_chunk, span)?
                .emit_opcode(Opcode::Pop, span);
        }

        {
            // Loop back to condition
            let current_chunk = pool.get_mut(current_chunk, span)?;
            let offset = -i32::try_from(current_chunk.get_label() - start_label)
                .expect("the label grows monotonically so its guaranteed to be larger than start.");
            current_chunk.emit_opcode(Opcode::Jump(offset.into()), span);

            // Patch exit jump
            if let Some(exit_jump) = exit_jump {
                current_chunk.patch_jump(exit_jump, span)?;
            }
        }

        let current_chunk = pool.get_mut(current_chunk, span)?;
        self.end_scope(current_chunk, span);
        Ok(())
    }

    /// Compile a while statement.
    fn compile_while(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        const DEFAULT_OFFSET: InstructionOffset = InstructionOffset::from_i32(-0x5740);
        let span = ast.span(index);
        let condition_index = unwrap_node!(ast.data(index).left() => span)?;
        let body_index = unwrap_node!(ast.data(index).right() => span)?;

        // Condition
        let condition_label = pool.get(current_chunk, span)?.get_label();
        self.compile_stmt(pool, current_chunk, ast, interner, condition_index)?;

        let exit_jump = {
            let current_chunk = pool.get_mut(current_chunk, span)?;
            let exit_jump = current_chunk.emit_opcode(Opcode::JumpIfFalse(DEFAULT_OFFSET), span);
            current_chunk.emit_opcode(Opcode::Pop, span);
            exit_jump
        };

        // Body
        self.compile_stmt(pool, current_chunk, ast, interner, body_index)?;
        // Jump back to condition check
        let current_chunk = pool.get_mut(current_chunk, span)?;
        let offset = -i32::try_from(current_chunk.get_label() - condition_label)
            .expect("the label grows monotonically so its guaranteed to be larger than start.");
        current_chunk.emit_opcode(Opcode::Jump(offset.into()), span);

        // Patch jump
        current_chunk.patch_jump(exit_jump, span)?;
        current_chunk.emit_opcode(Opcode::Pop, span);

        Ok(())
    }

    /// Compile a return statement.
    fn compile_return(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let value_index = ast.data(index).left();

        if let Some(value_index) = value_index {
            self.compile_stmt(pool, current_chunk, ast, interner, value_index)?;
        } else {
            pool.get_mut(current_chunk, span)?
                .emit_opcode(Opcode::LoadNil, span);
        }

        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::Print, span);

        Ok(())
    }

    /// Compile a print node.
    fn compile_print(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let value_node = unwrap_node!(ast.data(index).left() => span)?;
        self.compile_stmt(pool, current_chunk, ast, interner, value_node)?;

        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::Print, span);

        Ok(())
    }

    /// Compile a string literal.
    fn compile_string(
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let symbol = ast.data(index).symbol();

        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::LoadString(symbol), span);

        Ok(())
    }

    /// Compile a number.
    fn compile_number(
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let value = ast.data(index).number();
        let number_index = interner.add_number(value);

        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::LoadNumber(number_index), span);

        Ok(())
    }

    /// Compile a boolean.
    fn compile_bool(
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let value = ast.data(index).bool();
        let opcode = if value {
            Opcode::LoadTrue
        } else {
            Opcode::LoadFalse
        };
        pool.get_mut(current_chunk, span)?.emit_opcode(opcode, span);

        Ok(())
    }

    /// Compile nil.
    fn compile_nil(
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::LoadNil, span);

        Ok(())
    }

    /// Compile a unary expression.
    fn compile_unary_expr(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let value = unwrap_node!(data.left() => span)?;
        macro_rules! handle_unary {
            ($opcode:expr) => {{
                self.compile_stmt(pool, current_chunk, ast, interner, value)?;
                let chunk = pool.get_mut(current_chunk, span)?;
                chunk.emit_opcode($opcode, span);
            }};
        }

        let op = UnaryOp::try_from(data.middle()).expect("unary op is invalid.");
        match op {
            UnaryOp::Neg => handle_unary!(Opcode::Neg),
            UnaryOp::Not => handle_unary!(Opcode::Not),
        }

        Ok(())
    }

    /// Compile a binary expression.
    fn compile_binary_expr(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let lhs = unwrap_node!(data.left() => span)?;
        let rhs = unwrap_node!(data.right() => span)?;
        macro_rules! handle_binary {
            ($opcode:expr) => {{
                self.compile_stmt(pool, current_chunk, ast, interner, lhs)?;
                self.compile_stmt(pool, current_chunk, ast, interner, rhs)?;
                let chunk = pool.get_mut(current_chunk, span)?;
                chunk.emit_opcode($opcode, span);
            }};
        }

        let op = BinaryOp::try_from(data.middle()).expect("binary op is invalid.");
        match op {
            BinaryOp::Mul => handle_binary!(Opcode::Mul),
            BinaryOp::Div => handle_binary!(Opcode::Div),
            BinaryOp::Add => handle_binary!(Opcode::Add),
            BinaryOp::Sub => handle_binary!(Opcode::Sub),
            BinaryOp::Lt => handle_binary!(Opcode::Lt),
            BinaryOp::Le => handle_binary!(Opcode::Le),
            BinaryOp::Gt => handle_binary!(Opcode::Gt),
            BinaryOp::Ge => handle_binary!(Opcode::Ge),
            BinaryOp::Ne => handle_binary!(Opcode::Ne),
            BinaryOp::Eq => handle_binary!(Opcode::Eq),
            BinaryOp::Or => {
                const DEFAULT_OFFSET: InstructionOffset = InstructionOffset::from_i32(-0x1211);
                self.compile_stmt(pool, current_chunk, ast, interner, lhs)?;
                // If false then skip to rhs evaluation.
                let jump_to_rhs = {
                    let current_chunk = pool.get_mut(current_chunk, span)?;
                    current_chunk.emit_opcode(Opcode::JumpIfFalse(DEFAULT_OFFSET), span)
                };
                pool.get_mut(current_chunk, span)?
                    .emit_opcode(Opcode::Pop, span);
                // Otherwise unconditional jump to end.
                let jump_to_end = {
                    let current_chunk = pool.get_mut(current_chunk, span)?;
                    current_chunk.emit_opcode(Opcode::Jump(DEFAULT_OFFSET), span)
                };
                pool.get_mut(current_chunk, span)?
                    .patch_jump(jump_to_rhs, span)?;
                self.compile_stmt(pool, current_chunk, ast, interner, rhs)?;
                pool.get_mut(current_chunk, span)?
                    .emit_opcode(Opcode::Pop, span);
                pool.get_mut(current_chunk, span)?
                    .patch_jump(jump_to_end, span)?;
            }
            BinaryOp::And => {
                const DEFAULT_OFFSET: InstructionOffset = InstructionOffset::from_i32(-0x018AB);
                self.compile_stmt(pool, current_chunk, ast, interner, lhs)?;
                // If true then skip to rhs evaluation.
                let jump_to_rhs = {
                    let current_chunk = pool.get_mut(current_chunk, span)?;
                    current_chunk.emit_opcode(Opcode::JumpIfFalse(DEFAULT_OFFSET), span)
                };
                // Otherwise unconditional jump to end.
                let jump_to_end = {
                    let current_chunk = pool.get_mut(current_chunk, span)?;
                    current_chunk.emit_opcode(Opcode::Jump(DEFAULT_OFFSET), span)
                };
                pool.get_mut(current_chunk, span)?
                    .patch_jump(jump_to_rhs, span)?;
                pool.get_mut(current_chunk, span)?
                    .emit_opcode(Opcode::Pop, span);
                self.compile_stmt(pool, current_chunk, ast, interner, rhs)?;
                pool.get_mut(current_chunk, span)?
                    .emit_opcode(Opcode::Pop, span);
                pool.get_mut(current_chunk, span)?
                    .patch_jump(jump_to_end, span)?;
            }
        }

        Ok(())
    }

    /// Compile a call.
    fn compile_call(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let callee = convert_node!(ast.data(index).middle() => span)?;
        let args = ast.extra_data_slice(index);
        let arity = u8::try_from(args.len()).expect("can't have more than 255 arguments");

        self.compile_stmt(pool, current_chunk, ast, interner, callee)?;

        for arg in args.iter().copied() {
            let arg = convert_node!(arg => span)?;
            self.compile_stmt(pool, current_chunk, ast, interner, arg)?;
        }

        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::Call(arity), span);

        Ok(())
    }

    /// Compile a closure.
    fn compile_closure(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        ast: &Ast,
        interner: &mut ConstantInterner,
        closure: &CompilableClosure,
    ) -> Result<ConstantIndex<Closure>, CompilationError> {
        let sub_chunk = pool.allocate(closure.name, closure.span)?;

        self.begin_context(&closure.kind, interner);
        self.begin_scope();

        // Define parameters
        for parameter in closure.parameters.iter().copied() {
            let param_symbol = Symbol::from(parameter);
            self.add_local(param_symbol);
        }

        self.compile_stmt(pool, sub_chunk, ast, interner, closure.body)?;
        let context = self.end_context().expect("popping a context we pushed on.");

        let func = Closure {
            name: closure.name,
            chunk: sub_chunk,
            arity: u8::try_from(closure.parameters.len())
                .expect("can't have more than 255 parameters."),
            upvalues: context.upvalues,
        };

        Ok(interner.add_closure(func))
    }

    /// Compile a function declaration.
    fn compile_fn_decl(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let fn_name = ast.data(index).symbol();
        let (is_method, block_index, parameters) = ast.fn_decl_extra(index);
        assert!(!is_method, "should not be a method.");
        let block_index = convert_node!(block_index => span)?;

        let closure_index = self.compile_closure(
            pool,
            ast,
            interner,
            &CompilableClosure {
                name: fn_name,
                kind: FunctionKind::Function,
                parameters,
                body: block_index,
                span,
            },
        )?;
        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::LoadClosure(closure_index), span);

        self.assign_variable(pool, current_chunk, fn_name, span)?;

        Ok(())
    }

    /// Compile a class declaration.
    fn compile_class_decl(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let span = ast.span(index);
        let class_name = ast.data(index).symbol();
        let (methods, super_class) = ast.class_decl_extra(index);

        let class = Class { name: class_name };
        let class_index = interner.add_class(class);
        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::LoadClass(class_index), span);
        self.assign_variable(pool, current_chunk, class_name, span)?;

        if let Some(super_class) = super_class {
            let super_class_name = ast.data(super_class).symbol();
            self.push_identifier(pool, current_chunk, super_class_name, span)?;

            self.begin_scope();
            self.add_local(ast.kw_super());
            self.push_identifier(pool, current_chunk, class_name, span)?;
            pool.get_mut(current_chunk, span)?
                .emit_opcode(Opcode::Inherit, span);
        }

        self.push_identifier(pool, current_chunk, class_name, span)?;

        for method in methods {
            let method_node = convert_node!(*method => span)?;
            let method_name = ast.data(method_node).symbol();
            let method_span = ast.span(method_node);
            let (is_method, block, parameters) = ast.fn_decl_extra(method_node);
            assert!(is_method, "should be a method.");
            let method_index = self.compile_closure(
                pool,
                ast,
                interner,
                &CompilableClosure {
                    name: method_name,
                    kind: FunctionKind::Method,
                    parameters,
                    body: convert_node!(block => method_span)?,
                    span: method_span,
                },
            )?;

            pool.get_mut(current_chunk, span)?
                .emit_opcode(Opcode::LoadClosure(method_index), span);

            pool.get_mut(current_chunk, span)?
                .emit_opcode(Opcode::AttachMethod, span);
        }

        if super_class.is_some() {
            self.end_scope(pool.get_mut(current_chunk, span)?, span);
        }

        Ok(())
    }

    /// Compile a variable declaration.
    fn compile_var_decl(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let name = data.symbol();

        if let Some(value_index) = data.left() {
            self.compile_stmt(pool, current_chunk, ast, interner, value_index)?;
        } else {
            pool.get_mut(current_chunk, span)?
                .emit_opcode(Opcode::LoadNil, span);
        }

        // Local scope
        self.assign_variable(pool, current_chunk, name, span)?;
        Ok(())
    }

    /// Compile get super.
    fn compile_get_super(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let name = data.symbol();

        self.push_identifier(pool, current_chunk, interner.intern("this"), span)?;
        self.push_identifier(pool, current_chunk, interner.intern("super"), span)?;
        pool.get_mut(current_chunk, span)?
            .emit_opcode(Opcode::GetSuper(name), span);

        Ok(())
    }

    /// Compile a property get.
    fn compile_property_get(
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let name = data.symbol();

        let chunk = pool.get_mut(current_chunk, span)?;
        chunk.emit_opcode(Opcode::GetProperty(name), span);
        Ok(())
    }

    /// Compile a property set.
    fn compile_property_set(
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let name = data.symbol();

        let chunk = pool.get_mut(current_chunk, span)?;
        chunk.emit_opcode(Opcode::SetProperty(name), span);
        Ok(())
    }

    /// Compile an assignment operation.
    fn compile_assignment(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        interner: &mut ConstantInterner,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let name = data.symbol();

        let value_index = unwrap_node!(data.left() => span)?;
        self.compile_stmt(pool, current_chunk, ast, interner, value_index)?;

        let chunk = pool.get_mut(current_chunk, span)?;
        if let Some(slot) = self.resolve_local(name) {
            chunk.emit_opcode(Opcode::SetLocal(slot), span);
        } else if let Some(upvalue_index) = self.resolve_upvalue(name) {
            chunk.emit_opcode(Opcode::SetUpvalue(upvalue_index), span);
        } else {
            chunk.emit_opcode(Opcode::SetGlobal(name), span);
        }

        Ok(())
    }

    /// Compile an identifier.
    fn compile_identifier(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        ast: &Ast,
        index: NodeIndex,
    ) -> Result<(), CompilationError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let name = data.symbol();

        self.push_identifier(pool, current_chunk, name, span)?;

        Ok(())
    }

    /// Push a variable onto the stack.
    fn push_identifier(
        &mut self,
        pool: &mut ChunkPoolBuilder,
        current_chunk: ChunkId,
        name: Symbol,
        span: Span,
    ) -> Result<(), CompilationError> {
        let chunk = pool.get_mut(current_chunk, span)?;
        if let Some(slot) = self.resolve_local(name) {
            chunk.emit_opcode(Opcode::GetLocal(slot), span);
        } else if let Some(upvalue_index) = self.resolve_upvalue(name) {
            chunk.emit_opcode(Opcode::GetUpvalue(upvalue_index), span);
        } else {
            chunk.emit_opcode(Opcode::GetGlobal(name), span);
        }
        Ok(())
    }
}

impl Compiler {}

/// Compile an AST into bytecode.
///
/// # Errors
/// This will error if there are any issues during compilation like a malformed AST.
pub fn compile(ast: &Ast, interner: Interner) -> Result<Compilation, CompilationError> {
    let mut interner = ConstantInterner::with_interner(interner);

    let root = ast.root();
    let program_symbol = interner.intern("program");
    let pool = ChunkPoolBuilder::new(program_symbol);
    tracing::debug!("Compiling root {root:?}...");
    Compiler::new(&mut interner).compile(pool, ast, interner)
}
