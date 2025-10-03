use crate::{
    environment::SharedEnvironment,
    value::{Function, FunctionKind, Value},
};
use core::default;
use prox_interner::Symbol;
use prox_parser::{
    Span,
    ast::{Ast, BinaryOp, NodeData, NodeIndex, NodeTag},
    resolver::{ResolutionMap, ResolvedIdent},
};
use std::sync::Arc;

macro_rules! convert_node {
    ($index:expr) => {
        NodeIndex::new($index).ok_or(InterpreterError::InvalidNode)?
    };
}

pub struct ResolvedAst {
    pub(crate) ast: Ast,
    pub(crate) resolution: ResolutionMap,
}

impl ResolvedAst {
    fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.ast.resolve(symbol)
    }

    fn span(&self, index: NodeIndex) -> Span {
        self.ast.span(index)
    }

    fn data(&self, index: NodeIndex) -> NodeData {
        self.ast.data(index)
    }

    fn tag(&self, index: NodeIndex) -> NodeTag {
        self.ast.tag(index)
    }

    fn fn_decl_extra(&self, index: NodeIndex) -> (bool, u32, &[u32]) {
        self.ast.fn_decl_extra(index)
    }

    fn if_extra(&self, index: NodeIndex) -> Option<u32> {
        self.ast.if_extra(index)
    }

    fn extra_data_slice(&self, index: NodeIndex) -> &[u32] {
        self.ast.extra_data_slice(index)
    }
}

#[derive(Debug)]
enum ControlFlow {
    Normal,
    Expression(Value),
    Return(Value),
}

#[derive(Debug)]
pub enum InterpreterError {
    InvalidNode,
    InvalidAccess,
    InvalidCallee,
    InvalidArgumentCount,
    InvalidStatement,
    InvalidExpr,
}

pub struct Interpreter;

impl Interpreter {
    pub fn run(&mut self, environment: &mut SharedEnvironment, ast: ResolvedAst) {
        let root = ast.ast.root();
        let value = self
            .visit_stmt(environment, &ast, root)
            .expect("encountered null node");
        println!("{value:?}");
    }

    /// Visit a statement.
    fn visit_stmt(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, InterpreterError> {
        let tag = ast.tag(index);
        println!("TAG = {tag:?}");
        match tag {
            NodeTag::Program => self.visit_program(environment, ast, index),
            NodeTag::Block => self.visit_block(environment, ast, index),
            NodeTag::StmtFnDecl => self.visit_fn_decl(environment, ast, index),
            NodeTag::StmtVarDecl => todo!(),
            NodeTag::StmtClassDecl => todo!(),
            NodeTag::If => self.visit_if(environment, ast, index),
            NodeTag::Return => self.visit_return(environment, ast, index),
            NodeTag::Print => self.visit_print(environment, ast, index),
            NodeTag::For => todo!(),
            NodeTag::While => todo!(),
            NodeTag::ExprStmt => todo!(),
            NodeTag::Error => todo!(),
            NodeTag::Ident
            | NodeTag::Number
            | NodeTag::Nil
            | NodeTag::Group
            | NodeTag::SuperMethod
            | NodeTag::FieldGet
            | NodeTag::FieldSet
            | NodeTag::Boolean
            | NodeTag::Unary
            | NodeTag::Assignment
            | NodeTag::Binary
            | NodeTag::Call => Err(InterpreterError::InvalidStatement),
        }
    }

    /// Visit program.
    fn visit_program(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, InterpreterError> {
        let extra_data = ast.extra_data_slice(index);
        for stmt_index in extra_data.iter().copied() {
            self.visit_stmt(environment, ast, convert_node!(stmt_index))?;
        }
        Ok(ControlFlow::Normal)
    }

    /// Visit block.
    fn visit_block(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, InterpreterError> {
        let extra_data = ast.extra_data_slice(index);
        for stmt_index in extra_data.iter().copied() {
            let ControlFlow::Return(value) =
                self.visit_stmt(environment, ast, convert_node!(stmt_index))?
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
    ) -> Result<ControlFlow, InterpreterError> {
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
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, InterpreterError> {
        let value_node = ast
            .data(index)
            .left()
            .ok_or(InterpreterError::InvalidNode)?;
        let value = self.visit_expr(environment, ast, value_node)?;

        println!("PRINT: {value}");
        Ok(ControlFlow::Normal)
    }

    /// Visit return.
    fn visit_return(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, InterpreterError> {
        let Some(value_node) = ast.data(index).left() else {
            return Ok(ControlFlow::Return(Value::Nil));
        };

        let value = self.visit_expr(environment, ast, value_node)?;
        Ok(ControlFlow::Return(value))
    }

    /// Visit if.
    fn visit_if(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<ControlFlow, InterpreterError> {
        let data = ast.data(index);
        let condition = data.right().ok_or(InterpreterError::InvalidNode)?;

        let flag = self.visit_expr(environment, ast, condition)?;

        if flag.truthy() {
            let then_clause = convert_node!(data.middle());
            self.visit_stmt(environment, ast, then_clause)
        } else if let Some(else_clause) = ast.if_extra(index) {
            let else_clause = convert_node!(else_clause);
            self.visit_stmt(environment, ast, else_clause)
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
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, InterpreterError> {
        let tag = ast.tag(index);
        println!("EXPR TAG= {tag:?} @ {index:?}");
        match tag {
            NodeTag::Unary => todo!(),
            NodeTag::Binary => self.visit_binary(environment, ast, index),
            NodeTag::Call => self.visit_call(environment, ast, index),
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
            _ => Err(InterpreterError::InvalidExpr),
        }
    }

    /// Visit a number.
    fn visit_number(
        &mut self,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, InterpreterError> {
        let number = ast.data(index).number();
        Ok(Value::Number(number))
    }

    /// Visit an ident.
    fn visit_ident(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, InterpreterError> {
        let value = self
            .read_variable(environment, ast, index)
            .ok_or(InterpreterError::InvalidAccess)?;
        Ok(value)
    }

    /// Visit a binary expression.
    fn visit_binary(
        &mut self,
        environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, InterpreterError> {
        let data = ast.data(index);
        let lhs = data.left().ok_or(InterpreterError::InvalidNode)?;
        let rhs = data.right().ok_or(InterpreterError::InvalidNode)?;

        let lhs = self.visit_expr(environment, ast, lhs)?;
        let rhs = self.visit_expr(environment, ast, rhs)?;

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
        outer_environment: &mut SharedEnvironment,
        ast: &ResolvedAst,
        index: NodeIndex,
    ) -> Result<Value, InterpreterError> {
        let callee_node = convert_node!(ast.data(index).middle());
        let callee = self.visit_expr(outer_environment, ast, callee_node)?;

        let Value::Function(func) = callee else {
            return Err(InterpreterError::InvalidCallee);
        };

        // Set up scope
        let mut inner_scope = func.closure.new_scope();

        // Call arguments
        let arguments = ast.extra_data_slice(index);

        // Check that the argument list is the same length as the parameter list.
        if arguments.len() != func.parameters.len() {
            return Err(InterpreterError::InvalidArgumentCount);
        }

        // Define the arguments in the function scope
        for (symbol, arg_index) in func.parameters.iter().zip(
            arguments
                .iter()
                .map(|arg| NodeIndex::new(*arg).expect("invalid node.")),
        ) {
            let arg_value = self.visit_expr(outer_environment, ast, arg_index)?;
            println!("{symbol:?} <- {arg_value}");
            inner_scope.declare(*symbol, arg_value);
        }

        // TODO(pavyamsiri): Handle constructors.
        match self.visit_stmt(&mut inner_scope, ast, func.body)? {
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
        println!("accessing {name:?} at {span:?}");
        let ident = ResolvedIdent { symbol: name, span };
        match ast.resolution.get(&ident) {
            Some(depth) => environment.access_at(&name, *depth),
            None => environment.access_global(&name),
        }
    }
}
