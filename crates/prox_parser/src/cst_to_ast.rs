use crate::{
    ast::{Ast, AstBuilder, BinaryOp as AstBinaryOp, NodeIndex, UnaryOp as AstUnaryOp},
    cst::tree::{
        Cst,
        typed_trees::{
            BinaryNode as _, BinaryOp as CstBinaryOp, Block, ClassDecl, CstNode as _,
            DeclarationOrStatement, Expr, ExprAssignment, ExprCall, ExprGet, ExprGroup, ExprSet,
            ExprStmt, ExprSuperMethod, ExprThis, FnDecl, For, Ident, If, Nil, NumericLiteral,
            Print, Program, Return, Statement, StringLiteral, UnaryNode as _,
            UnaryOp as CstUnaryOp, VarDecl, While,
        },
    },
};
use core::default;
use core::fmt;
use prox_interner::{Interner, Symbol};
use prox_lexer::{SourceCode, span::Span};

#[derive(Debug)]
pub struct ConversionError {
    kind: ConversionErrorKind,
    source: Option<Box<ConversionError>>,
}

impl fmt::Display for ConversionError {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ConversionErrorKind::Failure { context } => writeln!(f, "{context}"),
            ConversionErrorKind::InvalidSpan { context, span } => {
                writeln!(f, "invalid span {context} @ {}..{}", span.start, span.end())
            }
            ConversionErrorKind::Missing { context } => writeln!(f, "missing {context}"),
        }?;

        if let Some(ref source) = self.source {
            write!(f, "\t is caused by {source}")?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum ConversionErrorKind {
    Failure { context: &'static str },
    InvalidSpan { context: &'static str, span: Span },
    Missing { context: &'static str },
}

trait OptionExt<T> {
    fn missing(self, context: &'static str) -> Result<T, ConversionError>;
    fn invalid_span(self, context: &'static str, span: Span) -> Result<T, ConversionError>;
}

impl<T> OptionExt<T> for Option<T> {
    fn missing(self, context: &'static str) -> Result<T, ConversionError> {
        self.ok_or(ConversionError {
            kind: ConversionErrorKind::Missing { context },
            source: None,
        })
    }

    fn invalid_span(self, context: &'static str, span: Span) -> Result<T, ConversionError> {
        self.ok_or(ConversionError {
            kind: ConversionErrorKind::InvalidSpan { context, span },
            source: None,
        })
    }
}

trait ResultExt<T> {
    fn context(self, context: &'static str) -> Result<T, ConversionError>;
}

impl<T> ResultExt<T> for Result<T, ConversionError> {
    fn context(self, context: &'static str) -> Result<T, ConversionError> {
        let Err(err) = self else { return self };

        Err(ConversionError {
            kind: ConversionErrorKind::Failure { context },
            source: Some(Box::new(err)),
        })
    }
}

type NodeConversionResult = Result<(NodeIndex, Span), ConversionError>;

pub struct CstToAstConverter {
    builder: AstBuilder,
}

impl default::Default for CstToAstConverter {
    fn default() -> Self {
        Self::new()
    }
}

impl CstToAstConverter {
    #[must_use]
    pub fn new() -> Self {
        Self {
            builder: AstBuilder::new(),
        }
    }

    /// Create a new converter with a pre-existing interner.
    #[must_use]
    pub fn with_interner(interner: Interner) -> Self {
        tracing::info!("Building AST with interner {interner}");
        Self {
            builder: AstBuilder::with_interner(interner),
        }
    }

    /// Converts a CST program root node into an AST.
    ///
    /// # Errors
    /// The conversion expects a correctly parsed CST and will error if there are any missing nodes.
    pub fn convert(mut self, source: &SourceCode<'_>, root: &Cst) -> Result<Ast, ConversionError> {
        let program = Program::ref_cast(root).missing("program")?;
        let mut statements = Vec::new();
        for decl_or_stmt in program.declarations_or_statements() {
            let (node, _) = self.convert_declaration_or_statement(source, &decl_or_stmt)?;
            statements.push(node);
        }

        tracing::debug!("Finishing conversion...");
        Ok(self.builder.finish(&statements))
    }

    /// Convert a variable declaration.
    fn convert_var_decl(
        &mut self,
        source: &SourceCode<'_>,
        decl: &VarDecl,
    ) -> NodeConversionResult {
        let span = decl.get_span().missing("vardecl.span")?;
        let (symbol, _) = self.get_ident(source, decl.name().as_ref(), "vardecl.name")?;

        let value = if let Some(init) = decl.initializer() {
            let value = init.value().missing("vardecl.initializer.value")?;
            let (value, _) = self
                .convert_expression(source, &value)
                .context("vardecl.initializer")?;
            Some(value)
        } else {
            None
        };

        let node = self.builder.build_var_decl(symbol, value, span);
        Ok((node, span))
    }

    fn convert_class_decl(
        &mut self,
        source: &SourceCode<'_>,
        decl: &ClassDecl,
    ) -> NodeConversionResult {
        let span = decl.get_span().missing("classdecl.span")?;
        let (name_sym, _) = self.get_ident(source, decl.name().as_ref(), "classdecl.name")?;

        let super_class = if let Some(super_class) = decl.super_class() {
            let (super_class_sym, super_class_span) = self.get_ident(
                source,
                super_class.name().as_ref(),
                "classdecl.superclass.name",
            )?;
            Some(
                self.builder
                    .build_identifier(super_class_sym, super_class_span),
            )
        } else {
            None
        };

        let mut methods = Vec::new();
        for method in decl.methods() {
            let (method_name, _) =
                self.get_ident(source, method.name().as_ref(), "classdecl.method.name")?;
            let params: Vec<_> = method
                .param_list()
                .filter_map(|param| param.name())
                .filter_map(|param| source.get_lexeme(param.span()))
                .map(|param| self.builder.intern(param))
                .collect();
            let block = method.body().missing("classdecl.method.body")?;
            let (block, _) = self
                .convert_block(source, &block)
                .context("classdecl.method.body")?;
            methods.push(
                self.builder
                    .build_function(method_name, &params, block, true, span),
            );
        }

        let class_node = self
            .builder
            .build_class(name_sym, super_class, &methods, span);

        Ok((class_node, span))
    }

    fn convert_fn_decl(&mut self, source: &SourceCode<'_>, decl: &FnDecl) -> NodeConversionResult {
        let fn_keyword = decl.fn_keyword().missing("fndecl.fn_keyword")?;
        let (name_sym, _) = self.get_ident(source, decl.name().as_ref(), "fndecl.name")?;

        let parameters: Vec<_> = decl
            .param_list()
            .map(|param| param.name())
            .map(|param| self.get_ident(source, param.as_ref(), "fndecl.param"))
            .map(|param| param.map(|param| param.0))
            .collect::<Result<Vec<_>, _>>()?;

        let body = decl.body().missing("fndecl.body")?;
        let (block, block_span) = self.convert_block(source, &body).context("fndecl.body")?;
        let span = fn_keyword.merge(block_span);

        let fn_node = self
            .builder
            .build_function(name_sym, &parameters, block, false, span);

        Ok((fn_node, span))
    }

    fn convert_block(&mut self, source: &SourceCode<'_>, block: &Block) -> NodeConversionResult {
        let span = block.get_span().missing("block.span")?;
        let statements: Vec<_> = block
            .declarations_or_statements()
            .map(|decl_or_stmt| {
                self.convert_declaration_or_statement(source, &decl_or_stmt)
                    .context("block.statement")
                    .map(|(node, _)| node)
            })
            .collect::<Result<Vec<NodeIndex>, ConversionError>>()?;

        let block_node = self.builder.build_block(&statements, span);

        Ok((block_node, span))
    }

    fn convert_if(&mut self, source: &SourceCode<'_>, stmt: &If) -> NodeConversionResult {
        let if_keyword = stmt.if_keyword().missing("if.if_keyword")?;
        let condition = stmt.condition().missing("if.condition")?;
        let (condition_node, _) = self
            .convert_expression(source, &condition)
            .context("if.condition")?;
        let (then_clause, else_clause) = stmt.clauses();
        let then_clause = then_clause.missing("if.then")?;
        let (then_node, then_span) = self
            .convert_statement(source, &then_clause)
            .context("if.then")?;
        let else_node = if let Some(else_clause) = else_clause {
            Some(
                self.convert_statement(source, &else_clause)
                    .context("if.else")?,
            )
        } else {
            None
        };

        if let Some((else_node, else_span)) = else_node {
            let span = if_keyword.merge(else_span);
            let if_node = self
                .builder
                .build_if(condition_node, then_node, Some(else_node), span);
            Ok((if_node, span))
        } else {
            let span = if_keyword.merge(then_span);
            let if_node = self.builder.build_if(condition_node, then_node, None, span);
            Ok((if_node, span))
        }
    }

    /// Convert a for statement.
    fn convert_for(&mut self, source: &SourceCode<'_>, stmt: &For) -> NodeConversionResult {
        let initializer = if let Some(initializer) = stmt.initializer() {
            if let Some(decl) = initializer.decl() {
                Some(
                    self.convert_var_decl(source, &decl)
                        .context("for.initializer.var_decl")?
                        .0,
                )
            } else if let Some(expr_stmt) = initializer.expr_stmt() {
                Some(
                    self.convert_expr_stmt(source, &expr_stmt)
                        .context("for.initializer.expr_stmt")?
                        .0,
                )
            } else {
                return Err(ConversionError {
                    kind: ConversionErrorKind::Missing {
                        context: "for.initializer",
                    },
                    source: None,
                });
            }
        } else {
            None
        };

        let condition = if let Some(condition) = stmt.condition() {
            let condition = condition.value().missing("for.condition")?;
            Some(self.convert_expression(source, &condition)?.0)
        } else {
            None
        };

        let increment = if let Some(increment) = stmt.increment() {
            let increment = increment.value().missing("for.increment")?;
            Some(self.convert_expression(source, &increment)?.0)
        } else {
            None
        };

        let body = stmt.body().missing("for.body")?;
        let (body, body_span) = self.convert_statement(source, &body).context("for.body")?;

        let span = {
            let for_keyword = stmt.for_keyword().missing("for.for_keyword")?;
            for_keyword.merge(body_span)
        };

        let for_node = self
            .builder
            .build_for(initializer, condition, increment, body, span);

        Ok((for_node, span))
    }

    /// Convert a while statement.
    fn convert_while(&mut self, source: &SourceCode<'_>, stmt: &While) -> NodeConversionResult {
        let condition = stmt
            .condition()
            .missing("while.condition")?
            .value()
            .missing("while.condition.value")?;

        let (condition_node, _) = self
            .convert_expression(source, &condition)
            .context("while.condition")?;

        let body = stmt.body().missing("while.body")?;
        let (body_node, body_span) = self
            .convert_statement(source, &body)
            .context("while.body")?;

        let while_keyword = stmt.while_keyword().missing("while.while_keyword")?;
        let span = while_keyword.merge(body_span);
        let while_node = self.builder.build_while(condition_node, body_node, span);

        Ok((while_node, span))
    }

    /// Convert an expression statement.
    fn convert_expr_stmt(
        &mut self,
        source: &SourceCode<'_>,
        stmt: &ExprStmt,
    ) -> NodeConversionResult {
        let span = stmt.get_span().missing("expr_stmt.span")?;
        let expr_value = stmt.value().missing("expr_stmt.value")?;
        let value = self
            .convert_expression(source, &expr_value)
            .context("expr_stmt.value")?;
        let expr_node = self.builder.build_expression_statement(value.0, span);

        Ok((expr_node, span))
    }

    fn convert_print(&mut self, source: &SourceCode<'_>, stmt: &Print) -> NodeConversionResult {
        let span = stmt.get_span().missing("print.span")?;
        let value = stmt.value().missing("print.value")?;

        let (value, _) = self
            .convert_expression(source, &value)
            .context("print.value")?;
        let print_node = self.builder.build_print(value, span);

        Ok((print_node, span))
    }

    fn convert_return(&mut self, source: &SourceCode<'_>, stmt: &Return) -> NodeConversionResult {
        let span = stmt.get_span().missing("return.span")?;

        let value = if let Some(value) = stmt.value() {
            Some(
                self.convert_expression(source, &value)
                    .context("return.value")?
                    .0,
            )
        } else {
            None
        };

        let return_node = self.builder.build_return(value, span);
        Ok((return_node, span))
    }

    fn convert_declaration_or_statement(
        &mut self,
        source: &SourceCode<'_>,
        decl_or_stmt: &DeclarationOrStatement,
    ) -> NodeConversionResult {
        tracing::debug!("Converting {}", decl_or_stmt.type_name());
        match *decl_or_stmt {
            DeclarationOrStatement::Var(ref decl) => self.convert_var_decl(source, decl),
            DeclarationOrStatement::Class(ref decl) => self.convert_class_decl(source, decl),
            DeclarationOrStatement::Fn(ref decl) => self.convert_fn_decl(source, decl),
            DeclarationOrStatement::If(ref stmt) => self.convert_if(source, stmt),
            DeclarationOrStatement::Block(ref stmt) => self.convert_block(source, stmt),
            DeclarationOrStatement::ExprStmt(ref stmt) => self.convert_expr_stmt(source, stmt),
            DeclarationOrStatement::For(ref stmt) => self.convert_for(source, stmt),
            DeclarationOrStatement::While(ref stmt) => self.convert_while(source, stmt),
            DeclarationOrStatement::Print(ref stmt) => self.convert_print(source, stmt),
            DeclarationOrStatement::Return(ref stmt) => self.convert_return(source, stmt),
        }
    }

    fn convert_statement(
        &mut self,
        source: &SourceCode<'_>,
        stmt: &Statement,
    ) -> NodeConversionResult {
        match *stmt {
            Statement::If(ref stmt) => self.convert_if(source, stmt),
            Statement::Block(ref block) => self.convert_block(source, block),
            Statement::ExprStmt(ref stmt) => self.convert_expr_stmt(source, stmt),
            Statement::For(ref stmt) => self.convert_for(source, stmt),
            Statement::While(ref stmt) => self.convert_while(source, stmt),
            Statement::Print(ref stmt) => self.convert_print(source, stmt),
            Statement::Return(ref stmt) => self.convert_return(source, stmt),
        }
    }

    fn convert_expression(&mut self, source: &SourceCode<'_>, expr: &Expr) -> NodeConversionResult {
        macro_rules! handle_unary {
            ($node:expr, $context:literal) => {{
                let operand = $node.operand().missing($context)?;
                self.convert_unary(
                    source,
                    $node.op(),
                    $node.op_token().missing($context)?,
                    &operand,
                )
            }};
        }

        macro_rules! handle_binary {
            ($node:expr, $context:literal) => {{
                let (lhs, rhs) = $node.operands().missing($context)?;
                self.convert_binary(source, $node.op(), &lhs, &rhs)
            }};
        }

        match *expr {
            Expr::Ident(ref ident) => self.convert_ident(source, ident),
            Expr::NumericLiteral(ref literal) => self.convert_numeric_literal(source, literal),
            Expr::StringLiteral(ref literal) => self.convert_string_literal(source, literal),
            Expr::Lt(ref expr) => handle_binary!(expr, "expr.lt.operands"),
            Expr::Mul(ref expr) => handle_binary!(expr, "expr.mul.operands"),
            Expr::Div(ref expr) => handle_binary!(expr, "expr.div.operands"),
            Expr::Add(ref expr) => handle_binary!(expr, "expr.add.operands"),
            Expr::Sub(ref expr) => handle_binary!(expr, "expr.sub.operands"),
            Expr::Le(ref expr) => handle_binary!(expr, "expr.le.operands"),
            Expr::Gt(ref expr) => handle_binary!(expr, "expr.gt.operands"),
            Expr::Ge(ref expr) => handle_binary!(expr, "expr.ge.operands"),
            Expr::Ne(ref expr) => handle_binary!(expr, "expr.ne.operands"),
            Expr::Eq(ref expr) => handle_binary!(expr, "expr.eq.operands"),
            Expr::And(ref expr) => handle_binary!(expr, "expr.and.operands"),
            Expr::Or(ref expr) => handle_binary!(expr, "expr.or.operands"),
            Expr::Not(ref expr) => handle_unary!(expr, "expr.not.operand"),
            Expr::Neg(ref expr) => handle_unary!(expr, "expr.neg.operand"),
            Expr::Call(ref call) => self.convert_call(source, call),
            Expr::Get(ref get) => self.convert_get(source, get),
            Expr::Set(ref set) => self.convert_set(source, set),
            Expr::Assignment(ref assign) => self.convert_assignment(source, assign),
            Expr::Group(ref expr) => self.convert_group(source, expr),
            Expr::This(ref this) => Ok(self.convert_this(this)),
            Expr::Nil(ref literal) => Ok(self.convert_nil(literal)),
            Expr::True(ref literal) => {
                let span = *literal.span();
                Ok(self.convert_bool(true, span))
            }
            Expr::False(ref literal) => {
                let span = *literal.span();
                Ok(self.convert_bool(false, span))
            }
            Expr::SuperMethod(ref expr) => self.convert_super_method(source, expr),
        }
    }

    fn convert_ident(&mut self, source: &SourceCode<'_>, ident: &Ident) -> NodeConversionResult {
        let span = *ident.span();
        let (symbol, _) = self.get_ident(source, Some(ident), "expr.ident")?;
        let node = self.builder.build_identifier(symbol, span);
        Ok((node, span))
    }

    fn convert_bool(&mut self, value: bool, span: Span) -> (NodeIndex, Span) {
        let node = self.builder.build_boolean(value, span);
        (node, span)
    }

    fn convert_nil(&mut self, literal: &Nil) -> (NodeIndex, Span) {
        let span = *literal.span();
        let node = self.builder.build_nil(span);
        (node, span)
    }

    fn convert_this(&mut self, this: &ExprThis) -> (NodeIndex, Span) {
        let span = *this.span();
        let lexeme = "this";

        let symbol = self.builder.intern(lexeme);
        let node = self.builder.build_identifier(symbol, span);
        (node, span)
    }

    fn convert_numeric_literal(
        &mut self,
        source: &SourceCode<'_>,
        literal: &NumericLiteral,
    ) -> NodeConversionResult {
        let span = *literal.span();
        let lexeme = source
            .get_lexeme(&span)
            .invalid_span("expr.numeric", span)?;

        let value = lexeme
            .parse::<f64>()
            .expect("numeric literal tokens should always be parseable to f64");

        let node = self.builder.build_number(value, span);
        Ok((node, span))
    }

    /// Convert a string literal.
    fn convert_string_literal(
        &mut self,
        source: &SourceCode<'_>,
        literal: &StringLiteral,
    ) -> NodeConversionResult {
        let span = *literal.span();
        let lexeme = source
            .get_lexeme(&Span {
                start: span.start + 1,
                length: span.length - 2,
            })
            .invalid_span("expr.string", span)?;

        let symbol = self.builder.intern(lexeme);
        let string_node = self.builder.build_string(symbol, span);
        Ok((string_node, span))
    }

    /// Convert a unary expression.
    fn convert_unary(
        &mut self,
        source: &SourceCode<'_>,
        op: CstUnaryOp,
        op_token: &Span,
        operand: &Expr,
    ) -> NodeConversionResult {
        let (operand, operand_span) = self
            .convert_expression(source, operand)
            .context("expr.unary.operand")?;
        let span = op_token.merge(operand_span);

        let node = self
            .builder
            .build_unary(convert_unary_op(op), operand, span);
        Ok((node, span))
    }

    /// Convert a binary expression.
    fn convert_binary(
        &mut self,
        source: &SourceCode<'_>,
        op: CstBinaryOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> NodeConversionResult {
        let left = self
            .convert_expression(source, lhs)
            .context("expr.binary.lhs")?;
        let right = self
            .convert_expression(source, rhs)
            .context("expr.binary.rhs")?;
        let span = left.1.merge(right.1);

        let node = self
            .builder
            .build_binary(convert_binary_op(op), left.0, right.0, span);
        Ok((node, span))
    }

    fn convert_call(&mut self, source: &SourceCode<'_>, call: &ExprCall) -> NodeConversionResult {
        let callee = call.callee().missing("expr.call.callee")?;
        let callee = self
            .convert_expression(source, &callee)
            .context("expr.call.callee")?;

        let arg_list = call.arg_list().missing("expr.call.arg_list")?;
        let list_span = arg_list.get_span().missing("expr.call.arg_list.span")?;
        let span = callee.1.merge(list_span);

        let arguments: Vec<_> = arg_list
            .args()
            .map(|arg| arg.value().missing("expr.call.arg"))
            .flat_map(|arg| {
                arg.map(|value| {
                    self.convert_expression(source, &value)
                        .context("expr.call.arg")
                })
            })
            .map(|arg| arg.map(|(node, _)| node))
            .collect::<Result<Vec<_>, _>>()?;

        let node = self.builder.build_call(callee.0, &arguments, span);
        Ok((node, span))
    }

    fn convert_super_method(
        &mut self,
        source: &SourceCode<'_>,
        super_method: &ExprSuperMethod,
    ) -> NodeConversionResult {
        let super_span = *super_method.super_keyword().missing("expr.super.span")?;
        let name = super_method.name().missing("expr.super.method")?;
        let (sym, name_span) = self.get_ident_with_span(source, *name, "expr.super.method")?;

        let span = super_span.merge(name_span);

        let set_node = self.builder.build_super_method(sym, super_span);

        Ok((set_node, span))
    }

    fn convert_get(&mut self, source: &SourceCode<'_>, get: &ExprGet) -> NodeConversionResult {
        let object = get.object().missing("expr.get.object")?;
        let (object, object_span) = self
            .convert_expression(source, &object)
            .context("expr.get.object")?;

        let (field_sym, field_span) =
            self.get_ident(source, get.field().as_ref(), "expr.get.field")?;

        let span = object_span.merge(field_span);

        let get_node = self.builder.build_field_get(object, field_sym, span);

        Ok((get_node, span))
    }

    fn convert_set(&mut self, source: &SourceCode<'_>, set: &ExprSet) -> NodeConversionResult {
        let object = set.object().missing("expr.set.object")?;
        let (object, object_span) = self
            .convert_expression(source, &object)
            .context("expr.set.object")?;

        let (field_sym, _) = self.get_ident(source, set.field().as_ref(), "expr.set.field")?;

        let value = set.value().missing("expr.set.value")?;
        let (value, value_span) = self
            .convert_expression(source, &value)
            .context("expr.set.value")?;

        let span = object_span.merge(value_span);

        let set_node = self.builder.build_field_set(object, field_sym, value, span);

        Ok((set_node, span))
    }

    fn convert_assignment(
        &mut self,
        source: &SourceCode<'_>,
        assignment: &ExprAssignment,
    ) -> NodeConversionResult {
        let (name_sym, name_span) =
            self.get_ident(source, assignment.name().as_ref(), "expr.assignment.name")?;

        let value = assignment.value().missing("expr.assignment.value")?;
        let (value, value_span) = self
            .convert_expression(source, &value)
            .context("expr.assignment.value")?;

        let span = name_span.merge(value_span);

        let set_node = self.builder.build_assignment(name_sym, value, name_span);

        Ok((set_node, span))
    }

    fn convert_group(
        &mut self,
        source: &SourceCode<'_>,
        group: &ExprGroup,
    ) -> NodeConversionResult {
        let value = group.value().missing("expr.group.value")?;
        let (value, _) = self
            .convert_expression(source, &value)
            .context("expr.group.value")?;
        let span = group.get_span().missing("expr.group.span")?;

        let group_node = self.builder.build_group(value, span);
        Ok((group_node, span))
    }

    fn get_ident(
        &mut self,
        source: &SourceCode<'_>,
        ident: Option<&Ident>,
        context: &'static str,
    ) -> Result<(Symbol, Span), ConversionError> {
        let ident = ident.missing(context)?;
        let span = *ident.span();
        let name = source.get_lexeme(&span).invalid_span(context, span)?;
        Ok((self.builder.intern(name), span))
    }

    fn get_ident_with_span(
        &mut self,
        source: &SourceCode<'_>,
        span: Span,
        context: &'static str,
    ) -> Result<(Symbol, Span), ConversionError> {
        let name = source.get_lexeme(&span).invalid_span(context, span)?;
        Ok((self.builder.intern(name), span))
    }
}

/// Convert from a CST unary op to an AST unary op.
const fn convert_unary_op(op: CstUnaryOp) -> AstUnaryOp {
    match op {
        CstUnaryOp::Not => AstUnaryOp::Not,
        CstUnaryOp::Neg => AstUnaryOp::Neg,
    }
}
/// Convert from a CST binary op to an AST binary op.
const fn convert_binary_op(op: CstBinaryOp) -> AstBinaryOp {
    match op {
        CstBinaryOp::Mul => AstBinaryOp::Mul,
        CstBinaryOp::Div => AstBinaryOp::Div,
        CstBinaryOp::Add => AstBinaryOp::Add,
        CstBinaryOp::Sub => AstBinaryOp::Sub,
        CstBinaryOp::Lt => AstBinaryOp::Lt,
        CstBinaryOp::Le => AstBinaryOp::Le,
        CstBinaryOp::Gt => AstBinaryOp::Gt,
        CstBinaryOp::Ge => AstBinaryOp::Ge,
        CstBinaryOp::Ne => AstBinaryOp::Ne,
        CstBinaryOp::Eq => AstBinaryOp::Eq,
        CstBinaryOp::And => AstBinaryOp::And,
        CstBinaryOp::Or => AstBinaryOp::Or,
    }
}
