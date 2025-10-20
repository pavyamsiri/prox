use core::fmt::Write as _;
use prox_parser::cst::Parser;
use prox_parser::cst_to_ast::CstToAstConverter;
use prox_parser::resolver::Resolver;
use prox_walker::environment::SharedEnvironment;
use prox_walker::interpreter::{Interpreter, ResolvedAst};
use prox_walker::io::BufferContext;

fn run_interpreter(text: &str) -> String {
    let mut buffer = String::new();

    let cst = Parser::new(text).parse();
    let source = cst.source.clone();
    let resolver = Resolver::default();
    let resolved_cst = match resolver.resolve(cst) {
        Ok(cst) => cst,
        Err(errors) => {
            for error in errors {
                error.format(&source, &mut buffer).unwrap();
                buffer.push('\n');
            }
            return buffer;
        }
    };

    let converter = CstToAstConverter::with_interner(resolved_cst.interner);

    let (ast, interner) = match converter.convert(&resolved_cst.source, &resolved_cst.root) {
        Ok(ast) => ast,
        Err(err) => {
            writeln!(&mut buffer, "{err}").unwrap();
            return buffer;
        }
    };

    let mut context = BufferContext::with_buffer(buffer);
    let mut interpreter = Interpreter;
    let mut ast = ResolvedAst {
        ast,
        resolution: resolved_cst.resolution,
        interner,
    };
    let mut environment = SharedEnvironment::with_interner(&mut ast.interner);
    let res = interpreter.run(&mut context, &mut environment, &ast);

    match res {
        Ok(()) => context.flush(),
        Err(err) => {
            let mut flushed_buffer = context.flush();
            err.format(text, &mut flushed_buffer, &ast.interner)
                .unwrap();
            flushed_buffer.push('\n');
            flushed_buffer
        }
    }
}

fn expected_output(code: &str) -> String {
    let mut output = String::new();
    for line in code.lines() {
        // Prints
        if let Some(pos) = line.rfind("// expect: ") {
            let needle = line[pos..].strip_prefix("// expect: ");
            if let Some(needle) = needle
                && !needle.is_empty()
            {
                output.push_str(needle);
                output.push('\n');
            }
        }

        // [line 2] Error at 'var': Expect expression.
        if let Some(err) = extract_line_error(line) {
            output.push_str(err);
            output.push('\n');
        }

        // Resolution errors
        if let Some(pos) = line.rfind("// Error") {
            let needle = line[pos..].strip_prefix("// ");
            if let Some(needle) = needle
                && !needle.is_empty()
            {
                output.push_str(needle);
                output.push('\n');
            }
        }

        // Runtime errors
        if let Some(pos) = line.rfind("// expect runtime error: ") {
            let needle = line[pos..].strip_prefix("// expect runtime error: ");
            if let Some(needle) = needle
                && !needle.is_empty()
            {
                output.push_str(needle);
                output.push('\n');
            }
        }
    }
    output
}

fn extract_line_error(line: &str) -> Option<&str> {
    // Pattern: [line X] Error message
    if let Some(pos) = line.find("// [line ") {
        let rest = &line[pos..];
        if let Some(bracket_pos) = rest.find(']') {
            let after_bracket = &rest[bracket_pos + 1..];
            if after_bracket.trim_start().starts_with("Error") {
                // Extract everything after "// "
                if let Some(error_part) = rest.strip_prefix("// ") {
                    return Some(error_part);
                }
            }
        }
    }
    None
}

/// Parse and maybe run the source code checking that the expected output and errors appear.
///
/// # Panics
/// When the expected output and actual output differ.
#[expect(
    clippy::allow_attributes,
    reason = "clippy/rust-analzyer is confused with the test organisation."
)]
#[allow(unreachable_pub, reason = "used by each test file.")]
pub fn check(code: &str) {
    let expected = expected_output(code);
    let output = run_interpreter(code);
    assert_eq!(expected, output, "check failed.");
}
