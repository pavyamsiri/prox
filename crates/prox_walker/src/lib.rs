pub mod environment;
pub mod error;
pub mod interpreter;
pub mod io;
mod value;

use crate::{
    environment::SharedEnvironment,
    error::RuntimeError,
    interpreter::{Interpreter, ResolvedAst},
    io::{IoContext, StdoutContext},
};
use prox_parser::{cst::Parser, cst_to_ast::CstToAstConverter, resolver::Resolver};

pub fn run(source: &str, context: &mut impl IoContext) -> Result<(), RuntimeError> {
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
        context,
        &mut environment,
        ResolvedAst {
            ast,
            resolution: resolved_cst.resolution,
        },
    )
}

#[cfg(test)]
mod tests {
    use crate::{io::BufferContext, run};

    #[test]
    fn walk() {
        let code = "
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 2) + fib(n - 1);
}

print fib(12);
";
        let mut buffer = BufferContext::new();
        let _ = run(code, &mut buffer).expect("code should not result in runtime errors.");

        assert_eq!(&buffer.flush(), "144\n", "successful execution.");
    }
}
