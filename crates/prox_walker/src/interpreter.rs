use crate::{
    environment::SharedEnvironment,
    error::{RuntimeError, RuntimeErrorKind},
    io::IoContext,
    value::{Function, FunctionKind, Value},
};
use core::default;
use core::ops;
use prox_interner::Symbol;
use prox_parser::{
    Span,
    ast::{Ast, BinaryOp, NodeData, NodeIndex, NodeTag},
    resolver::{ResolutionMap, ResolvedIdent},
};
use std::sync::Arc;

macro_rules! convert_node {
    ($index:expr) => {
        NodeIndex::new($index).ok_or(RuntimeError {
            kind: RuntimeErrorKind::NullNode,
            span: Span {
                start: 0,
                length: 1,
            },
        })?
    };
}

/// A resolved AST.
pub struct ResolvedAst {
    /// An AST.
    pub ast: Ast,
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
    pub fn run(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: ResolvedAst,
    ) -> Result<(), RuntimeError> {
        // The root node.
        let root = ast.root();

        match self.visit_stmt(context, environment, &ast, root) {
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
        match tag {
            NodeTag::Program => self.visit_program(context, environment, ast, index),
            NodeTag::Block => self.visit_block(context, environment, ast, index),
            NodeTag::StmtFnDecl => self.visit_fn_decl(environment, ast, index),
            NodeTag::StmtVarDecl => todo!(),
            NodeTag::StmtClassDecl => todo!(),
            NodeTag::If => self.visit_if(context, environment, ast, index),
            NodeTag::Return => self.visit_return(context, environment, ast, index),
            NodeTag::Print => self.visit_print(context, environment, ast, index),
            NodeTag::For => todo!(),
            NodeTag::While => todo!(),
            NodeTag::ExprStmt => todo!(),
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
        for stmt_index in extra_data.iter().copied() {
            self.visit_stmt(context, environment, ast, convert_node!(stmt_index))?;
        }
        Ok(ControlFlow::Normal)
    }

    /// Visit block.
    fn visit_block(
        &mut self,
        context: &mut impl IoContext,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let extra_data = ast.extra_data_slice(index);
        for stmt_index in extra_data.iter().copied() {
            let ControlFlow::Return(value) =
                self.visit_stmt(context, environment, ast, convert_node!(stmt_index))?
            else {
                continue;
            };
            return Ok(ControlFlow::Return(value));
        }
        Ok(ControlFlow::Normal)
    }

    /// Visit function declaration.
    fn visit_fn_decl(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, RuntimeError> {
        let data = ast.data(index);
        let name = Symbol::from(data.symbol());
        let (is_method, block, parameters) = ast.fn_decl_extra(index);
        let function = Function {
            name,
            parameters: parameters.iter().copied().map(Symbol::from).collect(),
            body: convert_node!(block),
            closure: environment.new_scope(),
            kind: FunctionKind::Normal,
        };

        environment.declare(name, Value::Function(Arc::new(function)));

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
        let value_node = ast.data(index).left().unwrap();
        let value = self.visit_expr(context, environment, ast, value_node)?;

        let _ = writeln!(context, "{value}").unwrap();
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
        let condition = data.right().unwrap();

        let flag = self.visit_expr(context, environment, ast, condition)?;

        if flag.truthy() {
            let then_clause = convert_node!(data.middle());
            self.visit_stmt(context, environment, ast, then_clause)
        } else if let Some(else_clause) = ast.if_extra(index) {
            let else_clause = convert_node!(else_clause);
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
        tracing::debug!(
            "Visiting {tag:?} expression with data = {:?}...",
            ast.data(index)
        );
        match tag {
            NodeTag::Unary => todo!(),
            NodeTag::Binary => self.visit_binary(context, environment, ast, index),
            NodeTag::Call => self.visit_call(context, environment, ast, index),
            NodeTag::Group => todo!(),
            NodeTag::Assignment => todo!(),
            NodeTag::SuperMethod => todo!(),
            NodeTag::FieldGet => todo!(),
            NodeTag::FieldSet => todo!(),
            NodeTag::Ident => self.visit_ident(environment, ast, index),
            NodeTag::Number => self.visit_number(ast, index),
            NodeTag::Boolean => todo!(),
            NodeTag::Nil => Ok(Value::Nil),
            NodeTag::Error => todo!(),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidExpr,
                span: ast.span(index),
            }),
        }
    }

    /// Visit a number.
    fn visit_number(&mut self, ast: &ResolvedAst, index: NodeIndex) -> Result<Value, RuntimeError> {
        let number = ast.data(index).number();
        Ok(Value::Number(number))
    }

    /// Visit an ident.
    fn visit_ident(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let value = self.read_variable(environment, ast, index).unwrap();
        Ok(value)
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
        let lhs = data.left().unwrap();
        let rhs = data.right().unwrap();

        let lhs = self.visit_expr(context, environment, ast, lhs)?;
        let rhs = self.visit_expr(context, environment, ast, rhs)?;

        let op = BinaryOp::try_from(data.middle()).expect("binary op is invalid.");
        let value = match op {
            BinaryOp::Mul => todo!(),
            BinaryOp::Div => todo!(),
            BinaryOp::Add => Value::Number(lhs.add(&rhs)),
            BinaryOp::Sub => Value::Number(lhs.sub(&rhs)),
            BinaryOp::Lt => Value::Bool(lhs.less_than(&rhs)),
            BinaryOp::Le => todo!(),
            BinaryOp::Gt => todo!(),
            BinaryOp::Ge => todo!(),
            BinaryOp::Ne => todo!(),
            BinaryOp::Eq => todo!(),
            BinaryOp::And => todo!(),
            BinaryOp::Or => todo!(),
        };
        Ok(value)
    }

    /// Visit call.
    fn visit_call(
        &mut self,
        context: &mut impl IoContext,
        outer_environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, RuntimeError> {
        let callee_node = convert_node!(ast.data(index).middle());
        let callee = self.visit_expr(context, outer_environment, ast, callee_node)?;

        let Value::Function(func) = callee else {
            let span = ast.span(index);
            return Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidCallee,
                span,
            });
        };

        // Set up scope
        let mut inner_scope = func.closure.new_scope();

        // Call arguments
        let arguments = ast.extra_data_slice(index);

        // Check that the argument list is the same length as the parameter list.
        if arguments.len() != func.parameters.len() {
            let span = ast.span(index);
            return Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidArgumentCount,
                span,
            });
        }

        // Define the arguments in the function scope
        for (symbol, arg_index) in func.parameters.iter().zip(
            arguments
                .iter()
                .map(|arg| NodeIndex::new(*arg).expect("invalid node.")),
        ) {
            let arg_value = self.visit_expr(context, outer_environment, ast, arg_index)?;
            inner_scope.declare(*symbol, arg_value);
        }

        // TODO(pavyamsiri): Handle constructors.
        match self.visit_stmt(context, &mut inner_scope, ast, func.body)? {
            ControlFlow::Return(value) => Ok(value),
            _ => {
                panic!("no return?");
            }
        }
    }
}

impl Interpreter {
    fn read_variable(
        &self,
        environment: &SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Option<Value> {
        let name = ast.data(index).symbol();
        let span = ast.span(index);
        let ident = ResolvedIdent { symbol: name, span };
        match ast.resolution.get(&ident) {
            Some(depth) => environment.access_at(&name, *depth),
            None => environment.access_global(&name),
        }
    }
}
