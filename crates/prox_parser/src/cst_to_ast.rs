use prox_lexer::{SourceCode, span::Span};

use crate::{
    ast::{Ast, AstBuilder, BinaryOp as AstBinaryOp, NodeIndex},
    cst::{
        parser::CorrectParse,
        tree::typed_trees::{
            BinaryOp as CstBinaryOp, Block, ClassDecl, CstNode as _, Declaration,
            DeclarationOrStatement, Expr, ExprCall, ExprLt, FnDecl, Ident, If, NumericLiteral,
            Print, Program, Return, Statement, VarDecl,
        },
    },
};

struct CstToAstConverter {
    builder: AstBuilder,
}

impl CstToAstConverter {
    pub fn new() -> Self {
        Self {
            builder: AstBuilder::new(),
        }
    }

    pub fn convert(mut self, program: &CorrectParse<'_>) -> Option<Ast> {
        let source = &program.source;
        let program = Program::ref_cast(&program.root)?;
        let mut statements = Vec::new();
        for decl_or_stmt in program.declarations_or_statements() {
            let (node, _) = self.convert_declaration_or_statement(source, &decl_or_stmt)?;
            statements.push(node);
        }

        println!("{statements:?}");

        Some(self.builder.finish(&statements))
    }

    fn convert_var_decl(
        &mut self,
        source: &SourceCode<'_>,
        decl: &VarDecl,
    ) -> Option<(NodeIndex, Span)> {
        let span = decl.get_span()?;
        let name = source.get_lexeme(decl.name()?.span())?;
        let symbol = self.builder.intern(name);

        let value = decl
            .initializer()
            .and_then(|init| init.value())
            .and_then(|expr| self.convert_expression(source, &expr))
            .map(|(expr, _)| expr);

        let node = self.builder.build_var_decl(symbol, value, span);
        Some((node, span))
    }

    fn convert_class_decl(
        &mut self,
        source: &SourceCode<'_>,
        decl: &ClassDecl,
    ) -> Option<(NodeIndex, Span)> {
        println!("{decl:?}");
        todo!();
    }

    fn convert_fn_decl(
        &mut self,
        source: &SourceCode<'_>,
        decl: &FnDecl,
    ) -> Option<(NodeIndex, Span)> {
        let fn_keyword = decl.fn_keyword()?;

        let name = source.get_lexeme(decl.name()?.span())?;
        let name_sym = self.builder.intern(name);

        println!("{name_sym:?}");
        let parameters: Vec<_> = decl
            .param_list()
            .filter_map(|param| param.name())
            .filter_map(|param| source.get_lexeme(param.span()))
            .map(|param| self.builder.intern(param))
            .collect();

        let body = decl.body()?;
        let (block, block_span) = self.convert_block(source, &body)?;
        let span = fn_keyword.merge(block_span);

        let node = self
            .builder
            .build_function(name_sym, &parameters, block, false, span);

        Some((node, span))
    }

    fn convert_block(
        &mut self,
        source: &SourceCode<'_>,
        block: &Block,
    ) -> Option<(NodeIndex, Span)> {
        let span = block.get_span()?;
        let statements: Vec<_> = block
            .declarations_or_statements()
            .filter_map(|decl_or_stmt| {
                self.convert_declaration_or_statement(source, &decl_or_stmt)
                    .map(|(node, _)| node)
            })
            .collect();

        let block_node = self.builder.build_block(&statements, span);

        Some((block_node, span))
    }

    fn convert_if(&mut self, source: &SourceCode<'_>, stmt: &If) -> Option<(NodeIndex, Span)> {
        let if_keyword = stmt.if_keyword()?;
        let condition = stmt.condition()?;
        let condition_node = self.convert_expression(source, &condition)?;
        let (then_clause, else_clause) = stmt.clauses();
        let then_node = self.convert_statement(source, &then_clause?)?;
        let else_node = else_clause.and_then(|clause| self.convert_statement(source, &clause));

        if let Some(else_node) = else_node {
            let span = if_keyword.merge(else_node.1);
            let if_node =
                self.builder
                    .build_if(condition_node.0, then_node.0, Some(else_node.0), span);
            Some((if_node, span))
        } else {
            let span = if_keyword.merge(then_node.1);
            let if_node = self
                .builder
                .build_if(condition_node.0, then_node.0, None, span);
            Some((if_node, span))
        }
    }

    fn convert_print(
        &mut self,
        source: &SourceCode<'_>,
        stmt: &Print,
    ) -> Option<(NodeIndex, Span)> {
        let span = stmt.get_span()?;
        let value = self.convert_expression(source, &stmt.value()?)?;
        let return_node = self.builder.build_print(value.0, span);

        Some((return_node, span))
    }

    fn convert_return(
        &mut self,
        source: &SourceCode<'_>,
        stmt: &Return,
    ) -> Option<(NodeIndex, Span)> {
        let span = stmt.get_span()?;
        let value = stmt
            .value()
            .and_then(|val| self.convert_expression(source, &val).map(|(val, _)| val));
        let return_node = self.builder.build_return(value, span);

        Some((return_node, span))
    }

    fn convert_declaration_or_statement(
        &mut self,
        source: &SourceCode<'_>,
        decl_or_stmt: &DeclarationOrStatement,
    ) -> Option<(NodeIndex, Span)> {
        match *decl_or_stmt {
            DeclarationOrStatement::Var(ref decl) => self.convert_var_decl(source, decl),
            DeclarationOrStatement::Class(ref decl) => self.convert_class_decl(source, decl),
            DeclarationOrStatement::Fn(ref decl) => self.convert_fn_decl(source, decl),
            DeclarationOrStatement::If(ref stmt) => self.convert_if(source, stmt),
            DeclarationOrStatement::Block(ref stmt) => self.convert_block(source, stmt),
            DeclarationOrStatement::ExprStmt(ref stmt) => todo!(),
            DeclarationOrStatement::For(ref stmt) => todo!(),
            DeclarationOrStatement::While(ref stmt) => todo!(),
            DeclarationOrStatement::Print(ref stmt) => self.convert_print(source, stmt),
            DeclarationOrStatement::Return(ref stmt) => self.convert_return(source, stmt),
        }
    }

    fn convert_statement(
        &mut self,
        source: &SourceCode<'_>,
        stmt: &Statement,
    ) -> Option<(NodeIndex, Span)> {
        match *stmt {
            Statement::If(ref stmt) => self.convert_if(source, stmt),
            Statement::Block(ref block) => self.convert_block(source, block),
            Statement::ExprStmt(ref stmt) => todo!(),
            Statement::For(ref stmt) => todo!(),
            Statement::While(ref stmt) => todo!(),
            Statement::Print(ref stmt) => self.convert_print(source, stmt),
            Statement::Return(ref stmt) => self.convert_return(source, stmt),
        }
    }

    fn convert_expression(
        &mut self,
        source: &SourceCode<'_>,
        expr: &Expr,
    ) -> Option<(NodeIndex, Span)> {
        macro_rules! handle_binary {
            ($node:expr) => {{
                let (lhs, rhs) = $node.operands()?;
                self.convert_binary(source, $node.op(), &lhs, &rhs)
            }};
        }

        match *expr {
            Expr::Ident(ref ident) => self.convert_ident(source, ident),
            Expr::NumericLiteral(ref literal) => self.convert_numeric_literal(source, literal),
            Expr::StringLiteral(ref literal) => todo!(),
            Expr::Lt(ref expr) => handle_binary!(expr),
            Expr::Mul(ref expr) => handle_binary!(expr),
            Expr::Div(ref expr) => handle_binary!(expr),
            Expr::Add(ref expr) => handle_binary!(expr),
            Expr::Sub(ref expr) => handle_binary!(expr),
            Expr::Le(ref expr) => handle_binary!(expr),
            Expr::Gt(ref expr) => handle_binary!(expr),
            Expr::Ge(ref expr) => handle_binary!(expr),
            Expr::Ne(ref expr) => handle_binary!(expr),
            Expr::Eq(ref expr) => handle_binary!(expr),
            Expr::And(ref expr) => handle_binary!(expr),
            Expr::Or(ref expr) => handle_binary!(expr),
            Expr::Call(ref call) => self.convert_call(source, call),
        }
    }

    fn convert_ident(
        &mut self,
        source: &SourceCode<'_>,
        ident: &Ident,
    ) -> Option<(NodeIndex, Span)> {
        let span = *ident.span();
        let lexeme = source.get_lexeme(&span)?;

        let symbol = self.builder.intern(lexeme);
        let node = self.builder.build_identifier(symbol, span);
        Some((node, span))
    }

    fn convert_numeric_literal(
        &mut self,
        source: &SourceCode<'_>,
        literal: &NumericLiteral,
    ) -> Option<(NodeIndex, Span)> {
        let span = *literal.span();
        let lexeme = source.get_lexeme(&span)?;

        let value = lexeme
            .parse::<f64>()
            .expect("numeric literal tokens should always be parseable to f64");
        println!("Parsing {lexeme:?} so value {value}");

        let node = self.builder.build_number(value, span);
        Some((node, span))
    }

    fn convert_binary(
        &mut self,
        source: &SourceCode<'_>,
        op: CstBinaryOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Option<(NodeIndex, Span)> {
        let left = self.convert_expression(source, lhs)?;
        let right = self.convert_expression(source, rhs)?;
        let span = left.1.merge(right.1);

        let node = self
            .builder
            .build_binary(convert_binary_op(op), left.0, right.0, span);
        Some((node, span))
    }

    fn convert_call(
        &mut self,
        source: &SourceCode<'_>,
        call: &ExprCall,
    ) -> Option<(NodeIndex, Span)> {
        let callee = self.convert_expression(source, &call.callee()?)?;

        let arg_list = call.arg_list()?;
        let list_span = arg_list.get_span()?;
        let span = callee.1.merge(list_span);

        let arguments: Vec<_> = arg_list
            .args()
            .filter_map(|arg| self.convert_expression(source, &arg.value()?))
            .map(|(node, _)| node)
            .collect();

        let node = self.builder.build_call(callee.0, &arguments, span);
        Some((node, span))
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

mod tests {
    use crate::{cst::parser::Parser, cst_to_ast::CstToAstConverter};

    #[test]
    fn create_ast() {
        let text = "
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 2) + fib(n - 1);
}

var start = clock();
print fib(35) == 9227465;
print clock() - start;
        ";
        let cst = Parser::new(text)
            .parse()
            .expect("can't handle errors right now.");

        let converter = CstToAstConverter::new();

        let res = converter
            .convert(&cst)
            .expect("conversion should be successful");
        println!("{res:?}");
        res.traverse();
    }
}
