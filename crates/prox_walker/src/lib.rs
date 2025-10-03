mod environment;
mod interpreter;
mod value;

use crate::{
    environment::SharedEnvironment,
    interpreter::{Interpreter, ResolvedAst},
};
use prox_parser::{cst::parser::Parser, cst_to_ast::CstToAstConverter, resolver::Resolver};

pub fn run(source: &str) {
    let cst = Parser::new(source).parse();
    let resolver = Resolver::default();
    let resolved_cst = resolver.resolve(cst).expect("resolution failed");

    let converter = CstToAstConverter::with_interner(resolved_cst.interner);

    let ast = converter
        .convert(&resolved_cst.source, &resolved_cst.root)
        .expect("conversion should be successful");
    let mut interpreter = Interpreter;
    let mut environment = SharedEnvironment::default();
    interpreter.run(
        &mut environment,
        ResolvedAst {
            ast,
            resolution: resolved_cst.resolution,
        },
    );
}

#[cfg(test)]
mod tests {
    use crate::run;

    #[test]
    fn walk() {
        let code = "
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 2) + fib(n - 1);
}

print fib(12);
";
        run(code);
    }
}
