extern crate alloc;

use crate::{
    environment::SharedEnvironment,
    error::{RuntimeError, RuntimeErrorKind},
    io::IoContext,
    native::NativeFunction,
    value::{Class, Function, FunctionKind, Instance, Value},
};
use alloc::rc::Rc;
use core::convert;
use core::fmt;
use core::ops;
use prox_interner::{Interner, Symbol};
use prox_parser::{
    ast::{Ast, BinaryOp, NodeIndex, NodeTag, UnaryOp},
    resolver::{ResolutionMap, ResolvedIdent},
};
use prox_span::Span;

macro_rules! unwrap_node {
    ($node:expr => $span:expr) => {
        $node.ok_or(RuntimeError {
            kind: RuntimeErrorKind::NullNode,
            span: $span,
        })
    };
}

macro_rules! convert_node {
    ($index:expr => $span:expr) => {
        NodeIndex::new($index).ok_or(RuntimeError {
            kind: RuntimeErrorKind::NullNode,
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

/// A resolved AST.
pub struct ResolvedAst {
    /// An AST.
    pub ast: Ast,
    /// The interner.
    pub interner: Interner,
    /// The variable resolutions.
    pub resolution: ResolutionMap,
}

impl ops::Deref for ResolvedAst {
    type Target = Ast;

    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

/// The control flow after the execution of a statement.
#[derive(Debug)]
enum ControlFlow {
    /// Continue to next statement.
    Normal,
    /// Return a value out from a function scope.
    Return(Value),
}

/// The tree walk interpreter.
pub struct Interpreter;

impl Interpreter {
    /// Interpret a program given as a resolved AST and an environment.
    ///
    /// # Errors
    /// This function will error at the first runtime error it encounters.
    pub fn run(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
    ) -> Result<(), RuntimeError> {
        // The root node.
        let root = ast.root();
        tracing::debug!("Evaluating root {root:?}...");

        match self.visit_stmt(context, environment, ast, root) {
            Ok(ControlFlow::Normal) => Ok(()),
            Ok(ControlFlow::Return(_)) => Err(RuntimeError {
                kind: RuntimeErrorKind::ReturnFromNonFunction,
                span: ast.span(ast.root()),
            }),
            Err(err) => Err(err),
        }
    }

    /// Visit a statement.
    ///
    /// This will error if the given node index is an expression.
    fn visit_stmt(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let tag = ast.tag(index);
        let mut buffer = String::new();
        ast.dump(&mut buffer, &ast.interner, index).unwrap();
        tracing::debug!("Interpreting {buffer}");
        match tag {
            NodeTag::Program => self.visit_program(context, environment, ast, index),
            NodeTag::Block => self.visit_block(context, environment, ast, index),
            NodeTag::StmtFnDecl => Self::visit_fn_decl(environment, ast, index),
            NodeTag::StmtVarDecl => self.visit_var_decl(context, environment, ast, index),
            NodeTag::StmtClassDecl => self.visit_class_decl(context, environment, ast, index),
            NodeTag::If => self.visit_if(context, environment, ast, index),
            NodeTag::Return => self.visit_return(context, environment, ast, index),
            NodeTag::Print => self.visit_print(context, environment, ast, index),
            NodeTag::For => self.visit_for(context, environment, ast, index),
            NodeTag::While => self.visit_while(context, environment, ast, index),
            NodeTag::ExprStmt => self.visit_expr_stmt(context, environment, ast, index),
            NodeTag::Ident
            | NodeTag::Number
            | NodeTag::Nil
            | NodeTag::Group
            | NodeTag::SuperMethod
            | NodeTag::Error
            | NodeTag::FieldGet
            | NodeTag::FieldSet
            | NodeTag::Boolean
            | NodeTag::Unary
            | NodeTag::Assignment
            | NodeTag::Binary
            | NodeTag::StringLiteral
            | NodeTag::Call => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidStatement,
                span: ast.span(index),
            }),
        }
    }

    /// Visit program.
    fn visit_program(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let extra_data = ast.extra_data_slice(index);
        let span = ast.span(index);
        for stmt_index in extra_data.iter().copied() {
            self.visit_stmt(
                context,
                environment,
                ast,
                convert_node!(stmt_index => span)?,
            )?;
        }
        Ok(ControlFlow::Normal)
    }

    /// Visit block.
    fn visit_block(
        &mut self,
        context: &mut impl IoContext,
        environment: &SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let mut environment = environment.new_scope();
        let span = ast.span(index);
        let extra_data = ast.extra_data_slice(index);
        for stmt_index in extra_data.iter().copied() {
            let ControlFlow::Return(value) = self.visit_stmt(
                context,
                &mut environment,
                ast,
                convert_node!(stmt_index => span)?,
            )?
            else {
                continue;
            };
            return Ok(ControlFlow::Return(value));
        }
        Ok(ControlFlow::Normal)
    }

    /// Visit function declaration.
    fn visit_fn_decl(
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let name = data.symbol();
        let (_is_method, block, parameters) = ast.fn_decl_extra(index);
        let function = Function {
            name,
            parameters: parameters.iter().copied().map(Symbol::from).collect(),
            body: convert_node!(block => span)?,
            closure: environment.new_scope(),
            kind: FunctionKind::Normal,
        };

        environment.declare(name, Value::Function(Rc::new(function)));

        Ok(ControlFlow::Normal)
    }

    /// Visit a variable declaration.
    fn visit_var_decl(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let data = ast.data(index);
        let name = data.symbol();
        let value = if let Some(value_index) = data.left() {
            self.visit_expr(context, environment, ast, value_index)?
        } else {
            Value::Nil
        };
        environment.declare(name, value);
        Ok(ControlFlow::Normal)
    }

    /// Visit a class declaration.
    fn visit_class_decl(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        // Either enum
        enum BorrowedOrOwned<'env> {
            Borrowed(&'env mut SharedEnvironment),
            Owned(SharedEnvironment),
        }

        impl BorrowedOrOwned<'_> {
            fn new_scope(&self) -> SharedEnvironment {
                match *self {
                    BorrowedOrOwned::Borrowed(ref environment) => environment.new_scope(),
                    BorrowedOrOwned::Owned(ref environment) => environment.new_scope(),
                }
            }
        }

        let data = ast.data(index);
        let span = ast.span(index);
        let class_name = data.symbol();

        let (method_indices, super_class) = ast.class_decl_extra(index);

        let super_class = if let Some(super_class) = super_class {
            let super_class = self.visit_expr(context, environment, ast, super_class)?;
            let Value::Class(super_class) = super_class else {
                return Err(RuntimeError {
                    kind: RuntimeErrorKind::InvalidSuperClass,
                    span,
                });
            };
            Some(super_class)
        } else {
            None
        };

        environment.declare(class_name, Value::Nil);

        // Add `super` if it has a super class
        let inner_environment = match super_class {
            Some(ref super_class) => {
                let mut new_environment = environment.new_scope();
                new_environment.declare(ast.kw_super(), Value::Class(Rc::clone(super_class)));
                BorrowedOrOwned::Owned(new_environment)
            }
            None => BorrowedOrOwned::Borrowed(environment),
        };

        // Create methods
        let mut methods = Vec::new();
        for method_index in method_indices {
            let method_index = NodeIndex::new(*method_index).ok_or(RuntimeError {
                kind: RuntimeErrorKind::NullNode,
                span,
            })?;
            let method_data = ast.data(method_index);
            let method_name = method_data.symbol();

            let (is_method, block, parameters) = ast.fn_decl_extra(method_index);
            assert!(is_method, "should be a method.");
            let block = NodeIndex::new(block).ok_or(RuntimeError {
                kind: RuntimeErrorKind::NullNode,
                span,
            })?;

            let kind = if method_name == ast.init_sym() {
                FunctionKind::Constructor
            } else {
                FunctionKind::Normal
            };

            let method = Function {
                name: method_name,
                parameters: parameters
                    .iter()
                    .map(|param| Symbol::from(*param))
                    .collect(),
                body: block,
                closure: inner_environment.new_scope(),
                kind,
            };
            methods.push(Rc::new(method));
        }

        let class = Class {
            name: class_name,
            methods,
            super_class,
        };

        environment
            .assign(class_name, Value::Class(Rc::new(class)))
            .map_err(|_err| RuntimeError {
                kind: RuntimeErrorKind::InvalidAssign,
                span,
            })?;

        Ok(ControlFlow::Normal)
    }

    /// Visit for statement.
    fn visit_for(
        &mut self,
        context: &mut impl IoContext,
        environment: &SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let mut environment = environment.new_scope();
        let (initializer, condition, increment) = ast.for_extra(index);
        let span = ast.span(index);
        let body = convert_node!(ast.data(index).middle() => span)?;

        if let Some(initializer) = initializer {
            let initializer = convert_node!(initializer => span)?;
            self.visit_stmt(context, &mut environment, ast, initializer)?;
        }

        let condition = map_node!(condition => span);
        let increment = map_node!(increment => span);

        loop {
            let flag = if let Some(condition) = condition {
                self.visit_expr(context, &mut environment, ast, condition)?
                    .truthy()
            } else {
                true
            };

            if !flag {
                break;
            }

            match self.visit_stmt(context, &mut environment, ast, body)? {
                ControlFlow::Normal => {}
                con @ ControlFlow::Return(_) => {
                    return Ok(con);
                }
            }

            // Increment
            if let Some(increment) = increment {
                self.visit_expr(context, &mut environment, ast, increment)?;
            }
        }

        Ok(ControlFlow::Normal)
    }

    /// Visit while statement.
    fn visit_while(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let condition = unwrap_node!(data.left() => span)?;
        let body = unwrap_node!(data.right() => span)?;

        while self
            .visit_expr(context, environment, ast, condition)?
            .truthy()
        {
            match self.visit_stmt(context, environment, ast, body)? {
                ControlFlow::Normal => {}
                con @ ControlFlow::Return(_) => return Ok(con),
            }
        }

        Ok(ControlFlow::Normal)
    }

    /// Visit expression statement.
    fn visit_expr_stmt(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let span = ast.span(index);
        let value_node = unwrap_node!(ast.data(index).left() => span)?;
        let _ = self.visit_expr(context, environment, ast, value_node)?;
        Ok(ControlFlow::Normal)
    }

    /// Visit print.
    fn visit_print(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let span = ast.span(index);
        let value_node = unwrap_node!(ast.data(index).left() => span)?;
        let value = self.visit_expr(context, environment, ast, value_node)?;

        writeln!(context, "{}", value.resolve(&ast.interner)).map_err(|fmt::Error| {
            RuntimeError {
                kind: RuntimeErrorKind::Io,
                span,
            }
        })?;
        Ok(ControlFlow::Normal)
    }

    /// Visit return.
    fn visit_return(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let Some(value_node) = ast.data(index).left() else {
            return Ok(ControlFlow::Return(Value::Nil));
        };

        let value = self.visit_expr(context, environment, ast, value_node)?;
        Ok(ControlFlow::Return(value))
    }

    /// Visit if.
    fn visit_if(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let condition = unwrap_node!(data.right() => span)?;

        let flag = self.visit_expr(context, environment, ast, condition)?;

        if flag.truthy() {
            let then_clause = convert_node!(data.middle() => span)?;
            self.visit_stmt(context, environment, ast, then_clause)
        } else if let Some(else_clause) = ast.if_extra(index) {
            let else_clause = convert_node!(else_clause => span)?;
            self.visit_stmt(context, environment, ast, else_clause)
        } else {
            Ok(ControlFlow::Normal)
        }
    }
}

// Expressions.
impl Interpreter {
    /// Visit an expression.
    fn visit_expr(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let tag = ast.tag(index);
        tracing::debug!("Visiting {tag:?} expression");
        match tag {
            NodeTag::Unary => self.visit_unary(context, environment, ast, index),
            NodeTag::Binary => self.visit_binary(context, environment, ast, index),
            NodeTag::Call => self.visit_call(context, environment, ast, index),
            NodeTag::Group => self.visit_group(context, environment, ast, index),
            NodeTag::Assignment => self.visit_assignment(context, environment, ast, index),
            NodeTag::SuperMethod => Self::visit_super_method(environment, ast, index),
            NodeTag::FieldGet => self.visit_get(context, environment, ast, index),
            NodeTag::FieldSet => self.visit_set(context, environment, ast, index),
            NodeTag::Ident => Self::visit_ident(environment, ast, index),
            NodeTag::StringLiteral => Ok(Self::visit_string_literal(ast, index)),
            NodeTag::Number => Ok(Value::Number(ast.data(index).number())),
            NodeTag::Boolean => Ok(Value::Bool(ast.data(index).bool())),
            NodeTag::Nil => Ok(Value::Nil),
            NodeTag::Error => Err(RuntimeError {
                kind: RuntimeErrorKind::NullNode,
                span: ast.span(index),
            }),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidExpr,
                span: ast.span(index),
            }),
        }
    }

    /// Visit a group.
    fn visit_group(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let span = ast.span(index);
        let inner = unwrap_node!(ast.data(index).left() => span)?;
        self.visit_expr(context, environment, ast, inner)
    }

    /// Visit an assignment.
    fn visit_assignment(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let value_index = data.left().ok_or(RuntimeError {
            kind: RuntimeErrorKind::NullNode,
            span,
        })?;
        let value = self.visit_expr(context, environment, ast, value_index)?;

        let name = data.symbol();

        assign_variable(environment, ast, name, span, value.clone())?;

        Ok(value)
    }

    /// Visit a super method get.
    fn visit_super_method(
        environment: &SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let data = ast.data(index);
        let super_span = ast.span(index);
        let method_name = data.symbol();

        let (super_value, depth) = read_local_symbol(environment, ast, ast.kw_super(), super_span)?;

        let Value::Class(super_class) = super_value else {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NonClassSuper,
                span: super_span,
            });
        };

        let object = read_symbol_at_depth(environment, ast.kw_this(), depth - 1, super_span)?;
        let Value::Instance(ref instance) = object else {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NonInstanceThis,
                span: super_span,
            });
        };

        let method = super_class.find_method(method_name).ok_or(RuntimeError {
            kind: RuntimeErrorKind::UndefinedField { name: method_name },
            span: super_span,
        })?;

        Ok(Value::Function(Rc::new(
            method.bind(ast.kw_this(), Rc::clone(instance)),
        )))
    }

    /// Visit a field get.
    fn visit_get(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let object_index = data.left().ok_or(RuntimeError {
            kind: RuntimeErrorKind::NullNode,
            span,
        })?;
        let object_span = ast.span(object_index);
        let value = self.visit_expr(context, environment, ast, object_index)?;
        let Value::Instance(instance) = value else {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidGet,
                span: object_span,
            });
        };

        let field_name = data.symbol();

        #[expect(clippy::option_if_let_else, reason = "clearer this way.")]
        if let Some(field_value) = instance.fields.borrow().get(&field_name) {
            Ok(field_value.clone())
        } else if let Some(method) = instance.class.find_method(field_name) {
            // Bind the instance to the method
            let bound_method = method.bind(ast.kw_this(), Rc::clone(&instance));
            Ok(Value::Function(Rc::new(bound_method)))
        } else {
            let undefined_access = RuntimeError {
                kind: RuntimeErrorKind::UndefinedField { name: field_name },
                span,
            };
            Err(undefined_access)
        }
    }

    /// Visit a set.
    fn visit_set(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let object_index = data.left().ok_or(RuntimeError {
            kind: RuntimeErrorKind::NullNode,
            span,
        })?;
        let object_span = ast.span(object_index);
        let object = self.visit_expr(context, environment, ast, object_index)?;
        let Value::Instance(instance) = object else {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidSet,
                span: object_span,
            });
        };
        let field_name = data.symbol();

        let value_index = data.right().ok_or(RuntimeError {
            kind: RuntimeErrorKind::NullNode,
            span,
        })?;
        let value = self.visit_expr(context, environment, ast, value_index)?;

        instance
            .fields
            .borrow_mut()
            .insert(field_name, value.clone());

        Ok(value)
    }
    /// Visit an ident.
    fn visit_ident(
        environment: &SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let value = read_variable(environment, ast, index)?;
        Ok(value)
    }

    /// Visit a string literal.
    fn visit_string_literal(ast: &ResolvedAst, index: NodeIndex) -> Value {
        let data = ast.data(index);
        let sym = data.symbol();
        // TODO: Add runtime error for string intern mismatch
        let lexeme = ast.interner.resolve(sym).unwrap();
        Value::String(lexeme.into())
    }

    /// Visit a unary expression.
    fn visit_unary(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let op = UnaryOp::try_from(data.middle()).expect("binary op is invalid.");

        let operand = {
            let operand_index = unwrap_node!(data.left() => span)?;
            self.visit_expr(context, environment, ast, operand_index)?
        };

        let result = match op {
            UnaryOp::Neg => operand.neg(),
            UnaryOp::Not => Ok(Value::Bool(operand.not())),
        };
        result.map_err(|kind| RuntimeError { kind, span })
    }

    /// Visit a binary expression.
    fn visit_binary(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let data = ast.data(index);
        let span = ast.span(index);
        let lhs = unwrap_node!(data.left() => span)?;
        let rhs = unwrap_node!(data.right() => span)?;

        let op = BinaryOp::try_from(data.middle()).expect("binary op is invalid.");
        let result = match op {
            BinaryOp::Mul => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                lhs.mul(&rhs)
            }
            BinaryOp::Div => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                lhs.div(&rhs)
            }
            BinaryOp::Add => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                lhs.add(&rhs)
            }
            BinaryOp::Sub => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                lhs.sub(&rhs)
            }
            BinaryOp::Lt => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                lhs.less_than(&rhs)
            }
            BinaryOp::Le => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                lhs.less_than_or_equal(&rhs)
            }
            BinaryOp::Gt => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                lhs.greater_than(&rhs)
            }
            BinaryOp::Ge => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                lhs.greater_than_or_equal(&rhs)
            }
            BinaryOp::Ne => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                Ok(Value::Bool(!lhs.eq(&rhs)))
            }
            BinaryOp::Eq => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                let rhs = self.visit_expr(context, environment, ast, rhs)?;
                Ok(Value::Bool(lhs.eq(&rhs)))
            }
            BinaryOp::And => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                if lhs.truthy() {
                    return self.visit_expr(context, environment, ast, rhs);
                }
                Ok(lhs)
            }
            BinaryOp::Or => {
                let lhs = self.visit_expr(context, environment, ast, lhs)?;
                if !lhs.truthy() {
                    return self.visit_expr(context, environment, ast, rhs);
                }
                Ok(lhs)
            }
        };
        result.map_err(|kind| RuntimeError { kind, span })
    }

    /// Visit call.
    fn visit_call(
        &mut self,
        context: &mut impl IoContext,
        outer_environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        macro_rules! check_arity {
            ($nargs:expr, $nparams:expr, $span:expr) => {
                // Check that the argument list is the same length as the parameter list.
                if $nargs != ($nparams as usize) {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidArgumentCount {
                            expected: $nparams,
                            actual: $nargs,
                        },
                        span: $span,
                    });
                }
            };
        }
        let span = ast.span(index);
        let callee_node = convert_node!(ast.data(index).middle() => span)?;
        let callee = self.visit_expr(context, outer_environment, ast, callee_node)?;

        // Call arguments
        let arguments = ast.extra_data_slice(index);

        match callee {
            Value::Function(func) => {
                tracing::debug!(
                    "Calling with {} arguments, expecting {} parameters",
                    arguments.len(),
                    func.parameters.len()
                );
                check_arity!(
                    arguments.len(),
                    convert::TryInto::<u8>::try_into(func.parameters.len())
                        .expect("functions have a maximum of 255 parameters."),
                    span
                );
                self.visit_non_native_call(context, outer_environment, ast, &func, arguments)
            }
            Value::NativeFunction(func) => {
                check_arity!(arguments.len(), func.arity(), span);
                self.visit_native_call(context, outer_environment, ast, &*func, arguments)
            }
            Value::Class(class) => self.visit_object_instantiation(
                context,
                outer_environment,
                ast,
                &class,
                arguments,
                span,
            ),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidCallee,
                span,
            }),
        }
    }

    /// Visit a non-native call.
    fn visit_non_native_call(
        &mut self,
        context: &mut impl IoContext,
        outer_environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        func: &Function,
        arguments: &[u32],
    ) -> Result<Value, RuntimeError> {
        // Set up scope
        let mut inner_scope = func.closure.new_scope();

        // Define the arguments in the function scope
        for (symbol, arg_index) in func.parameters.iter().zip(
            arguments
                .iter()
                .map(|arg| NodeIndex::new(*arg).expect("invalid node.")),
        ) {
            let arg_value = self.visit_expr(context, outer_environment, ast, arg_index)?;
            inner_scope.declare(*symbol, arg_value);
        }

        tracing::debug!("Call scope = {inner_scope}");

        let result = match self.visit_stmt(context, &mut inner_scope, ast, func.body)? {
            ControlFlow::Return(value) => value,
            ControlFlow::Normal => Value::Nil,
        };

        if matches!(func.kind, FunctionKind::Constructor) {
            Ok(func
                .closure
                .access_at(ast.kw_this(), 0)
                .expect("`this` should be bound to the method."))
        } else {
            Ok(result)
        }
    }

    /// Visit a native call.
    fn visit_native_call(
        &mut self,
        context: &mut impl IoContext,
        outer_environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        func: &dyn NativeFunction,
        arguments: &[u32],
    ) -> Result<Value, RuntimeError> {
        // Set up scope
        let mut inner_scope = outer_environment.new_scope();

        // Define the arguments in the function scope
        for (symbol, arg_index) in func.parameters().iter().zip(
            arguments
                .iter()
                .map(|arg| NodeIndex::new(*arg).expect("invalid node.")),
        ) {
            let arg_value = self.visit_expr(context, outer_environment, ast, arg_index)?;
            inner_scope.declare(*symbol, arg_value);
        }

        func.call(&inner_scope, ast)
    }

    /// Visit object instantiation.
    fn visit_object_instantiation(
        &mut self,
        context: &mut impl IoContext,
        outer_environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        class: &Rc<Class>,
        arguments: &[u32],
        span: Span,
    ) -> Result<Value, RuntimeError> {
        let instance = Rc::new(Instance::new(Rc::clone(class)));
        let value = Value::Instance(Rc::clone(&instance));

        if let Some(constructor) = class.find_method(ast.init_sym()) {
            let bound_method = constructor.bind(ast.kw_this(), instance);

            if arguments.len() != bound_method.parameters.len() {
                return Err(RuntimeError {
                    kind: RuntimeErrorKind::InvalidArgumentCount {
                        expected: bound_method
                            .parameters
                            .len()
                            .try_into()
                            .expect("functions have a maximum of 255 parameters."),
                        actual: arguments.len(),
                    },
                    span,
                });
            }

            let _ = self.visit_non_native_call(
                context,
                outer_environment,
                ast,
                &bound_method,
                arguments,
            )?;
        } else if !arguments.is_empty() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidArgumentCount {
                    expected: 0,
                    actual: arguments.len(),
                },
                span,
            });
        }

        Ok(value)
    }
}

fn read_variable(
    environment: &SharedEnvironment,
    ast: &ResolvedAst,
    index: NodeIndex,
) -> Result<Value, RuntimeError> {
    let name = ast.data(index).symbol();
    let span = ast.span(index);
    let ident = ResolvedIdent { symbol: name, span };

    #[expect(clippy::option_if_let_else, reason = "clearer this way.")]
    match ast.resolution.get(&ident) {
        Some(depth) => environment.access_at(name, *depth),
        None => environment.access_global(name),
    }
    .ok_or(RuntimeError {
        kind: RuntimeErrorKind::InvalidAccess,
        span,
    })
}

fn read_local_symbol(
    environment: &SharedEnvironment,
    ast: &ResolvedAst,
    symbol: Symbol,
    span: Span,
) -> Result<(Value, usize), RuntimeError> {
    let ident = ResolvedIdent { symbol, span };
    ast.resolution
        .get(&ident)
        .map_or_else(
            || None,
            |depth| {
                environment
                    .access_at(symbol, *depth)
                    .map(|val| (val, *depth))
            },
        )
        .ok_or(RuntimeError {
            kind: RuntimeErrorKind::InvalidAccess,
            span,
        })
}

fn read_symbol_at_depth(
    environment: &SharedEnvironment,
    symbol: Symbol,
    distance: usize,
    span: Span,
) -> Result<Value, RuntimeError> {
    environment.access_at(symbol, distance).ok_or(RuntimeError {
        kind: RuntimeErrorKind::InvalidAccess,
        span,
    })
}

fn assign_variable(
    environment: &mut SharedEnvironment,
    ast: &ResolvedAst,
    symbol: Symbol,
    span: Span,
    value: Value,
) -> Result<(), RuntimeError> {
    let ident = ResolvedIdent { symbol, span };
    let result = match ast.resolution.get(&ident) {
        Some(depth) => environment.assign_at(symbol, value, *depth),
        None => environment.assign_global(symbol, value),
    };
    result.map_err(|_err| RuntimeError {
        kind: RuntimeErrorKind::InvalidAssign,
        span,
    })
}

#[expect(dead_code, reason = "meant for debugging.")]
fn dump_resolution_map(resolutions: &ResolutionMap, interner: &Interner) {
    tracing::info!("RESOLUTIONS:");
    let mut ordered_resolutions: Vec<_> = resolutions.iter().collect();
    ordered_resolutions.sort_by_key(|&(&key, _)| key.span.start);
    for (key, depth) in ordered_resolutions {
        let symbol = key.symbol;
        let span = key.span;

        let lexeme = interner.resolve(symbol).unwrap();

        tracing::info!(
            "\t{lexeme} [{:?}]: Span = {}..{} @ {}",
            symbol,
            span.start,
            span.end(),
            depth
        );
    }
}
