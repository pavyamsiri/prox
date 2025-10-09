use crate::cst::ParseError;
use crate::cst::ParseResult;
use crate::cst::tree::typed_trees::BinaryNode;
use crate::cst::tree::typed_trees::Block;
use crate::cst::tree::typed_trees::ClassDecl;
use crate::cst::tree::typed_trees::CstNode;
use crate::cst::tree::typed_trees::DeclarationOrStatement;
use crate::cst::tree::typed_trees::Expr;
use crate::cst::tree::typed_trees::ExprCall;
use crate::cst::tree::typed_trees::ExprStmt;
use crate::cst::tree::typed_trees::FnDecl;
use crate::cst::tree::typed_trees::For;
use crate::cst::tree::typed_trees::Ident as CstIdent;
use crate::cst::tree::typed_trees::If;
use crate::cst::tree::typed_trees::Param;
use crate::cst::tree::typed_trees::Print;
use crate::cst::tree::typed_trees::Program;
use crate::cst::tree::typed_trees::Return;
use crate::cst::tree::typed_trees::Statement;
use crate::cst::tree::typed_trees::VarDecl;
use crate::cst::tree::typed_trees::While;
use crate::cst::tree::{Cst, TreeKind};
use core::default;
use prox_interner::{Interner, Symbol};
use prox_lexer::SourceCode;
use prox_lexer::span::Span;
use std::collections::HashMap;
use std::hash::RandomState;

/// A map from an identifer to its resolution depth.
pub type ResolutionMap = HashMap<ResolvedIdent, usize>;

/// An identifier.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ResolvedIdent {
    /// The intern symbol.
    pub symbol: Symbol,
    /// Its span in the text.
    pub span: Span,
}

/// An error encountered during parsing and resolution.
#[derive(Debug)]
pub enum ResolutionError {
    /// A parser error.
    Parser(ParseError),
    /// Encountered the wrong node.
    InvalidNode {
        /// The span of the node.
        span: Span,
        /// The actual node type.
        actual: TreeKind,
        /// The expected node type.
        expected: TreeKind,
    },
    /// Shadowing a local.
    ShadowLocal {
        /// Previously declared variable.
        old: ResolvedIdent,
        /// Newly declared variable.
        new: ResolvedIdent,
        /// The encompassing span.
        span: Span,
    },
    /// A variable declaration is referencing itself in its initializer.
    SelfReferentialInitializer {
        /// The variable being declared.
        destination: ResolvedIdent,
        /// The variable being referenced again in its initializer.
        reference: ResolvedIdent,
        /// The span of the declaration.
        span: Span,
    },
    /// A class is inheriting from itself.
    SelfReferentialInheritance {
        /// The class being declared.
        destination: ResolvedIdent,
        /// The class being referenced again as its own super class.
        reference: ResolvedIdent,
        /// The span of the declaration.
        span: Span,
    },
    /// Returning from a non function scope.
    NonFunctionReturn {
        /// The span of the return statement.
        span: Span,
    },
    /// Returning a value in a constructor.
    ReturnValueInConstructor {
        /// The span of the return statement.
        span: Span,
        /// The span of the constructor statement.
        constructor: Span,
    },
}

/// A resolved CST.
pub struct ResolvedCst<'src> {
    /// The root CST node.
    pub root: Cst,
    /// The source code.
    pub source: SourceCode<'src>,
    /// Variable resolutions.
    pub resolution: ResolutionMap,
    /// Interned strings.
    pub interner: Interner,
}

/// The type of function the resolver is currently in.
#[derive(Debug, Clone, Copy, Default)]
enum FunctionEnvironment {
    /// The resolver is not in a function.
    #[default]
    None,
    /// The resolver is in a free function.
    Function,
    /// The resolver is in a method.
    Method,
    /// The resolver is in a constructor.
    Constructor,
}

/// The type of class the resolver is currently in.
#[derive(Debug, Clone, Copy, Default)]
enum ClassEnvironment {
    /// The resolver is not in a class.
    #[default]
    None,
    /// The resolver is in a class.
    Class,
    /// The resolver is in a sub-class.
    SubClass,
}

/// A variable resolution.
#[derive(Debug, Clone, Copy)]
enum Resolution {
    /// A variable is declared to exist but not yet defined.
    Declared {
        /// The variable's symbol and it span.
        identifier: ResolvedIdent,
        /// The span of its declaration.
        span: Span,
    },
    /// A variable is now defined.
    Defined {
        /// The variable's symbol and it span.
        identifier: ResolvedIdent,
        /// The variable's symbol and it span.
        span: Span,
    },
}

impl Resolution {
    /// Get the symbol of resolution.
    const fn get_identifier(&self) -> ResolvedIdent {
        match *self {
            Resolution::Declared { identifier, .. } | Resolution::Defined { identifier, .. } => {
                identifier
            }
        }
    }

    /// Get the span of resolution.
    const fn get_span(&self) -> Span {
        match *self {
            Resolution::Declared { span, .. } | Resolution::Defined { span, .. } => span,
        }
    }
}

/// A program resolver.
pub struct Resolver {
    /// The resolutions.
    resolution: HashMap<ResolvedIdent, usize>,
    /// The function environment,
    function: FunctionEnvironment,
    /// The class environment.
    class: ClassEnvironment,
    /// The scopes.
    scopes: Vec<HashMap<Symbol, Resolution>>,
    /// Resolution and parser errors.
    errors: Vec<ResolutionError>,
    /// Interned strings.
    interner: Interner,
}

macro_rules! early_return {
    ($option:expr) => {
        match $option {
            Some(val) => val,
            None => {
                return;
            }
        }
    };
}

impl default::Default for Resolver {
    fn default() -> Self {
        Self {
            resolution: HashMap::new(),
            function: FunctionEnvironment::None,
            class: ClassEnvironment::None,
            scopes: Vec::new(),
            errors: Vec::new(),
            interner: Interner::with_hasher(RandomState::new()),
        }
    }
}

// Helpers.
impl Resolver {
    /// Convert an identifier from the CST into a resolver identifier.
    fn convert_ident(&mut self, source: &SourceCode, ident: &CstIdent) -> Option<ResolvedIdent> {
        let span = ident.span();
        let lexeme = source.get_lexeme(span)?;
        let symbol = self.interner.intern(lexeme);
        Some(ResolvedIdent {
            symbol,
            span: *span,
        })
    }

    /// Create a resolver identifier from a given string.
    fn create_ident(&mut self, lexeme: &'static str, span: Span) -> ResolvedIdent {
        let symbol = self.interner.intern(lexeme);
        ResolvedIdent { symbol, span }
    }

    /// Attempt to cast to the given type and then return it. If it could not be cast then
    /// report an error.
    fn expect_node<'node, T: CstNode<'node>>(&mut self, node: &'node Cst) -> Option<T> {
        if let Some(node) = T::ref_cast(node) {
            Some(node)
        } else {
            self.errors.push(ResolutionError::InvalidNode {
                span: Span {
                    start: 0,
                    length: 1,
                },
                actual: node.tag,
                expected: T::expected_type(),
            });
            None
        }
    }

    /// Declare that a variable exists given its name and the span of the whole declaration.
    fn declare(&mut self, ident: &ResolvedIdent, span: Span) {
        let Some(inner_scope) = self.scopes.last_mut() else {
            return;
        };

        if let Some(resolution) = inner_scope.get(&ident.symbol) {
            let old_span = resolution.get_span();
            if inner_scope.contains_key(&ident.symbol) {
                let encompassing = old_span.merge(span);
                self.errors.push(ResolutionError::ShadowLocal {
                    span: encompassing,
                    old: resolution.get_identifier(),
                    new: *ident,
                });
                return;
            }
        }

        inner_scope.insert(
            ident.symbol,
            Resolution::Declared {
                identifier: *ident,
                span,
            },
        );
    }

    /// Define a variable given its name and the span of the whole declaration.
    fn define(&mut self, ident: &ResolvedIdent, span: Span) {
        let Some(inner_scope) = self.scopes.last_mut() else {
            return;
        };
        inner_scope.insert(
            ident.symbol,
            Resolution::Defined {
                identifier: *ident,
                span,
            },
        );
    }

    /// Get the resolution of an identifier.
    fn get_resolution(&self, ident: &ResolvedIdent) -> Option<&Resolution> {
        let inner_scope = self.scopes.last()?;
        inner_scope.get(&ident.symbol)
    }

    /// Begin a new scope.
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// End scope.
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

impl Resolver {
    /// Resolve a parsed CST.
    ///
    /// # Errors
    /// If the resolver encounters any parser or resolution errors it will return a vector of all errors
    /// it has found.
    pub fn resolve(
        mut self,
        program: ParseResult<'_>,
    ) -> Result<ResolvedCst<'_>, Vec<ResolutionError>> {
        self.errors
            .extend(program.errors.into_iter().map(ResolutionError::Parser));
        if let Some(prog) = self.expect_node::<Program>(&program.root) {
            self.resolve_program(&program.source, prog);
        }

        if self.errors.is_empty() {
            Ok(ResolvedCst {
                root: program.root,
                resolution: self.resolution,
                interner: self.interner,
                source: program.source,
            })
        } else {
            Err(self.errors)
        }
    }

    fn resolve_program(&mut self, source: &SourceCode, program: Program) {
        let outer = source.get_span();
        for stmt in program.declarations_or_statements() {
            self.resolve_decl_or_stmt(source, stmt, &outer);
        }
    }

    /// Resolve either a declaration or a statement.
    fn resolve_decl_or_stmt(
        &mut self,
        source: &SourceCode,
        decl_or_stmt: DeclarationOrStatement,
        outer: &Span,
    ) {
        match decl_or_stmt {
            DeclarationOrStatement::Var(decl) => self.resolve_var_decl(source, &decl, outer),
            DeclarationOrStatement::Class(decl) => self.resolve_class_decl(source, &decl, outer),
            DeclarationOrStatement::Fn(decl) => self.resolve_fn_decl(source, &decl, outer),
            DeclarationOrStatement::If(stmt) => self.resolve_if(source, &stmt, outer),
            DeclarationOrStatement::Block(stmt) => self.resolve_block(source, &stmt, outer),
            DeclarationOrStatement::ExprStmt(stmt) => self.resolve_expr_stmt(source, &stmt, outer),
            DeclarationOrStatement::For(stmt) => self.resolve_for_stmt(source, &stmt, outer),
            DeclarationOrStatement::While(stmt) => self.resolve_while_stmt(source, &stmt, outer),
            DeclarationOrStatement::Print(stmt) => self.resolve_print(source, &stmt, outer),
            DeclarationOrStatement::Return(stmt) => self.resolve_return(source, &stmt, outer),
        }
    }

    /// Resolve either a declaration or a statement.
    fn resolve_stmt(&mut self, source: &SourceCode, stmt: Statement, outer: &Span) {
        match stmt {
            Statement::If(stmt) => self.resolve_if(source, &stmt, outer),
            Statement::Block(stmt) => self.resolve_block(source, &stmt, outer),
            Statement::ExprStmt(stmt) => self.resolve_expr_stmt(source, &stmt, outer),
            Statement::For(stmt) => self.resolve_for_stmt(source, &stmt, outer),
            Statement::While(stmt) => self.resolve_while_stmt(source, &stmt, outer),
            Statement::Print(stmt) => self.resolve_print(source, &stmt, outer),
            Statement::Return(stmt) => self.resolve_return(source, &stmt, outer),
        }
    }

    /// Resolve a variable declaration.
    fn resolve_var_decl(&mut self, source: &SourceCode, decl: &VarDecl, outer: &Span) {
        let name = early_return!(decl.name());
        let name = early_return!(self.convert_ident(source, &name));
        let span = early_return!(decl.get_span());
        self.declare(&name, span);

        if let Some(init) = decl.initializer() {
            let value = early_return!(init.value());
            self.resolve_expr(source, outer, &value);
        }
        self.define(&name, span);
    }

    /// Resolve a class declaration.
    fn resolve_class_decl(&mut self, source: &SourceCode, decl: &ClassDecl, outer: &Span) {
        let enclosing = self.class;
        self.class = ClassEnvironment::Class;

        let name = early_return!(decl.name());
        let class_name = early_return!(self.convert_ident(source, &name));

        let span = early_return!(decl.get_span());

        self.declare(&class_name, span);
        self.define(&class_name, span);

        if let Some(super_class) = decl.super_class() {
            let super_class = early_return!(super_class.name());
            let super_class = early_return!(self.convert_ident(source, &super_class));

            if super_class.symbol == class_name.symbol {
                self.errors
                    .push(ResolutionError::SelfReferentialInheritance {
                        destination: class_name,
                        reference: super_class,
                        span,
                    });
            }

            self.class = ClassEnvironment::SubClass;
            self.resolve_variable(&super_class);

            // Declare `super` variable.
            let super_ident = self.create_ident("super", span);
            self.begin_scope();
            self.declare(&super_ident, span);
            self.define(&super_ident, span);
        }

        // Declare `this` variable.
        self.begin_scope();
        let this_ident = self.create_ident("this", span);
        self.declare(&this_ident, span);
        self.define(&this_ident, span);

        let init_ident = self.create_ident("init", span);
        for method_decl in decl.methods() {
            let method_ident = early_return!(method_decl.name());
            let method_name = early_return!(self.convert_ident(source, &method_ident));
            let function_type = if method_name.symbol == init_ident.symbol {
                FunctionEnvironment::Constructor
            } else {
                FunctionEnvironment::Method
            };
            let body = early_return!(method_decl.body());
            let right_brace = early_return!(body.right_brace());

            let method_span = method_ident.span().merge(*right_brace);

            self.resolve_function(
                source,
                &method_span,
                method_decl.param_list(),
                &body,
                function_type,
            );
        }
        self.end_scope();

        // Remove extra super class scope
        if decl.super_class().is_some() {
            self.end_scope();
        }

        self.class = enclosing;
        self.resolve_variable(&class_name);
    }

    /// Resolve a function declaration.
    fn resolve_fn_decl(&mut self, source: &SourceCode, decl: &FnDecl, outer: &Span) {
        let fn_keyword = early_return!(decl.fn_keyword());
        let body = early_return!(decl.body());
        let right_brace = early_return!(body.right_brace());
        let name_ident = early_return!(decl.name());

        let span = fn_keyword.merge(*right_brace);

        let ident = early_return!(self.convert_ident(source, &name_ident));
        self.declare(&ident, span);
        self.define(&ident, span);

        self.resolve_function(
            source,
            &span,
            decl.param_list(),
            &body,
            FunctionEnvironment::Function,
        );
    }

    /// Resolve a function/method/constructor declaration.
    fn resolve_function<'node>(
        &mut self,
        source: &SourceCode,
        outer: &Span,
        param_list: impl Iterator<Item = Param<'node>>,
        body: &Block,
        environment: FunctionEnvironment,
    ) {
        let enclosing = self.function;
        self.function = environment;
        self.begin_scope();
        for param in param_list {
            let param_name = early_return!(param.name());
            let param_span = *param_name.span();
            let param_ident = early_return!(self.convert_ident(source, &param_name));
            self.declare(&param_ident, param_span);
            self.define(&param_ident, param_span);
        }
        for decl_or_stmt in body.declarations_or_statements() {
            self.resolve_decl_or_stmt(source, decl_or_stmt, outer);
        }
        self.end_scope();
        self.function = enclosing;
    }

    /// Resolve an if statement.
    fn resolve_if(&mut self, source: &SourceCode, stmt: &If, outer: &Span) {
        let condition = early_return!(stmt.condition());
        self.resolve_expr(source, outer, &condition);

        let (then_clause, else_clause) = stmt.clauses();
        let then_clause = early_return!(then_clause);

        self.resolve_stmt(source, then_clause, outer);
        if let Some(else_clause) = else_clause {
            self.resolve_stmt(source, else_clause, outer);
        }
    }

    /// Resolve a block statement.
    fn resolve_block(&mut self, source: &SourceCode, stmt: &Block, outer: &Span) {
        self.begin_scope();

        for decl_or_stmt in stmt.declarations_or_statements() {
            self.resolve_decl_or_stmt(source, decl_or_stmt, outer);
        }

        self.end_scope();
    }

    /// Resolve a for statement.
    fn resolve_for_stmt(&mut self, source: &SourceCode, stmt: &For, outer: &Span) {
        self.begin_scope();

        if let Some(init) = stmt.initializer() {
            if let Some(decl) = init.decl() {
                self.resolve_var_decl(source, &decl, outer);
            } else if let Some(expr_stmt) = init.expr_stmt() {
                self.resolve_expr_stmt(source, &expr_stmt, outer);
            }
        }

        if let Some(condition) = stmt.condition() {
            let condition = early_return!(condition.value());
            self.resolve_expr(source, outer, &condition);
        }

        if let Some(increment) = stmt.increment() {
            let increment = early_return!(increment.value());
            self.resolve_expr(source, outer, &increment);
        }

        let body = early_return!(stmt.body());
        self.resolve_stmt(source, body, outer);
        self.end_scope();
    }

    /// Resolve a while statement.
    fn resolve_while_stmt(&mut self, source: &SourceCode, stmt: &While, outer: &Span) {
        let condition = early_return!(stmt.condition());
        let condition_expr = early_return!(condition.value());
        self.resolve_expr(source, outer, &condition_expr);

        let body = early_return!(stmt.body());
        self.resolve_stmt(source, body, outer);
    }

    /// Resolve an expression statement.
    fn resolve_expr_stmt(&mut self, source: &SourceCode, stmt: &ExprStmt, outer: &Span) {
        let value = early_return!(stmt.value());
        self.resolve_expr(source, outer, &value);
    }

    /// Resolve a print statement.
    fn resolve_print(&mut self, source: &SourceCode, stmt: &Print, outer: &Span) {
        let value = early_return!(stmt.value());
        self.resolve_expr(source, outer, &value);
    }

    /// Resolve a return statement.
    fn resolve_return(&mut self, source: &SourceCode, stmt: &Return, outer: &Span) {
        let span = early_return!(stmt.get_span());
        match self.function {
            FunctionEnvironment::None => {
                self.errors
                    .push(ResolutionError::NonFunctionReturn { span });
            }
            FunctionEnvironment::Constructor if stmt.value().is_some() => {
                self.errors.push(ResolutionError::ReturnValueInConstructor {
                    span,
                    constructor: *outer,
                });
            }
            _ => {
                let value = early_return!(stmt.value());
                self.resolve_expr(source, outer, &value);
            }
        }
    }

    /// Resolve an expression.
    fn resolve_expr(&mut self, source: &SourceCode, outer: &Span, expr: &Expr) {
        match *expr {
            Expr::NumericLiteral(_) | Expr::StringLiteral(_) => {}
            Expr::Ident(ref ident) => {
                let ident = early_return!(self.convert_ident(source, ident));
                self.resolve_variable_expression(&ident);
            }

            Expr::Mul(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Div(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Add(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Sub(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Lt(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Le(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Gt(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Ge(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Ne(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::Eq(ref expr) => {
                self.resolve_binary_expr(source, outer, expr);
            }
            Expr::And(ref expr) => self.resolve_binary_expr(source, outer, expr),
            Expr::Or(ref expr) => self.resolve_binary_expr(source, outer, expr),
            Expr::Call(ref call) => self.resolve_call(source, outer, call),
        }
    }

    /// Resolve a binary expression.
    fn resolve_binary_expr<'tree>(
        &mut self,
        source: &SourceCode,
        outer: &Span,
        expr: &impl BinaryNode<'tree>,
    ) {
        let (lhs, rhs) = early_return!(expr.operands());
        self.resolve_expr(source, outer, &lhs);
        self.resolve_expr(source, outer, &rhs);
    }

    /// Resolve a call.
    fn resolve_call(&mut self, source: &SourceCode, outer: &Span, call: &ExprCall) {
        let callee = early_return!(call.callee());
        self.resolve_expr(source, outer, &callee);
        let arg_list = early_return!(call.arg_list());
        for arg in arg_list.args() {
            let arg_expr = early_return!(arg.value());
            self.resolve_expr(source, outer, &arg_expr);
        }
    }

    /// Resolve an identifier.
    fn resolve_variable_expression(&mut self, ident: &ResolvedIdent) {
        // Can't access variable when it is declared but not defined
        if let Some(&Resolution::Declared {
            identifier: destination,
            span,
        }) = self.get_resolution(ident)
        {
            self.errors
                .push(ResolutionError::SelfReferentialInitializer {
                    destination,
                    reference: *ident,
                    span,
                });
        } else {
            self.resolve_variable(ident);
        }
    }

    /// Resolve a variable.
    fn resolve_variable(&mut self, ident: &ResolvedIdent) {
        let total_depth = self.scopes.len();
        for (depth, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(&Resolution::Defined { .. }) = scope.get(&ident.symbol) {
                self.resolution.insert(*ident, total_depth - 1 - depth);
                break;
            }
        }
    }
}
