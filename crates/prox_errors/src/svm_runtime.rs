use crate::ReportableError;
use ariadne::{Color, Config, Label, Report as AReport, ReportKind, Source};
use core::ops;
use prox_span::Span;
use prox_svm::error::{RuntimeError, RuntimeErrorKind};
use std::io;

type Report<'err> = AReport<'err, (&'err str, ops::Range<usize>)>;

impl ReportableError for RuntimeError {
    #[expect(clippy::too_many_lines, reason = "clearer to keep all code here.")]
    fn report(&self, buffer: &mut String, path: &str, text: &str) {
        let mut output = io::Cursor::new(Vec::new());
        let span = self.span;

        let report = match self.kind {
            RuntimeErrorKind::InvalidAccess(_symbol) => format_generic_error(
                path,
                span,
                "Accessing a variable that has not yet beend declared.",
                "variable has not been declared yet.",
            ),
            RuntimeErrorKind::NonArithmeticOperand => format_generic_error(
                path,
                span,
                "Attempting unary operation that requires the operand to be a number.",
                "operand must be a number.",
            ),
            RuntimeErrorKind::NonArithmeticOperands => format_generic_error(
                path,
                span,
                "Attempting binary operation that requires both operands to be numbers.",
                "operands must both be numbers.",
            ),
            RuntimeErrorKind::InvalidAddOperands => format_generic_error(
                path,
                span,
                "Adding operands that can't be added together.",
                "operands must both be numbers or strings.",
            ),
            RuntimeErrorKind::InvalidCallee => format_generic_error(
                path,
                span,
                "Calling a value that is not callable.",
                "this is not a function or class.",
            ),
            RuntimeErrorKind::InvalidArgumentCount { expected, actual } => {
                let message = if actual > expected {
                    "called with too many arguments."
                } else {
                    "called with too few arguments."
                };
                let overall = format!(
                    "Calling function with {actual} arguments when it expects {expected} parameters."
                );
                format_generic_error(path, span, &overall, message)
            }
            RuntimeErrorKind::InvalidSubClass => format_generic_error(
                path,
                span,
                "This is not a valid subclass.",
                "this is not a class.",
            ),
            RuntimeErrorKind::InvalidSuperClass => format_generic_error(
                path,
                span,
                "This is not a valid superclass.",
                "this is not a class.",
            ),
            RuntimeErrorKind::InvalidGet => format_generic_error(
                path,
                span,
                "Attempted to access a property on a non-instance value.",
                "this is not an instance.",
            ),
            RuntimeErrorKind::InvalidSet => format_generic_error(
                path,
                span,
                "Attempted to modify a property on a non-instance value.",
                "this is not an instance.",
            ),
            RuntimeErrorKind::UndefinedField { .. } => format_generic_error(
                path,
                span,
                "Accessing an undefined field.",
                "field is undefined.",
            ),
            RuntimeErrorKind::Segfault => format_generic_error(
                path,
                span,
                "Encountered segfault during execution.",
                "segfaulted here.",
            ),
            RuntimeErrorKind::InvalidInstance => format_generic_error(
                path,
                span,
                "Stack base does not contain an instance.",
                "the stack base at this point does not contain an instance.",
            ),
            RuntimeErrorKind::InvalidOpenUpvalue => format_generic_error(
                path,
                span,
                "Encountered a closed upvalue in open upvalue chain.",
                "at this point there is a closed upvalue in open upvalue chain.",
            ),
            RuntimeErrorKind::InvalidSymbol { symbol, name } => format_generic_error(
                path,
                span,
                &format!("Failed to resolve the symbol {symbol} from the {name} interner.",),
                "tried to resolve symbol here.",
            ),
            RuntimeErrorKind::OutOfBoundsDereference { index, name } => format_generic_error(
                path,
                span,
                &format!(
                    "Tried to dereference handle {index} from {name} arena but it was out of bounds."
                ),
                "tried to dereference here.",
            ),
            RuntimeErrorKind::FreeDereference { index, name } => format_generic_error(
                path,
                span,
                &format!(
                    "Tried to dereference handle {index} from {name} arena but it pointed to a freed entry."
                ),
                "tried to dereference here.",
            ),
            RuntimeErrorKind::WrongGenerationDereference {
                index,
                expected,
                actual,
                name,
            } => format_generic_error(
                path,
                span,
                &format!(
                    "Tried to dereference handle {index} from {name} arena but it had generation {} when {} was expected.",
                    actual.raw(),
                    expected.raw(),
                ),
                "tried to dereference here.",
            ),
            RuntimeErrorKind::InvalidChunkDereference { id } => format_generic_error(
                path,
                span,
                &format!("Failed to dereference chunk {}.", id.to_usize()),
                "tried to dereference chunk here.",
            ),
            RuntimeErrorKind::InvalidMethodAttach => format_generic_error(
                path,
                span,
                "Failed to attach method to class.",
                "expected closure on top of stack.",
            ),
            RuntimeErrorKind::InvalidClassAttach => format_generic_error(
                path,
                span,
                "Failed to attach method to class.",
                "expected class second to top of stack.",
            ),
            RuntimeErrorKind::EmptyStack => format_generic_error(
                path,
                span,
                "Stack is unexpectedly empty.",
                "stack is empty.",
            ),
            RuntimeErrorKind::EmptyCallStack => format_generic_error(
                path,
                span,
                "Call stack is unexpectedly empty.",
                "call stack is empty.",
            ),
            RuntimeErrorKind::Io => {
                format_generic_error(path, span, "Could not write to IO.", "io failed here.")
            }
        };

        report
            .write((path, Source::from(text)), &mut output)
            .expect("write into buffer should not fail.");
        buffer.push_str(
            &String::from_utf8(output.into_inner())
                .expect("buffer consists of only valid UTF-8 bytes."),
        );
    }
}

fn format_generic_error<'err>(
    path: &'err str,
    span: Span,
    overall: &str,
    specific: &str,
) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message(overall)
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message(specific),
        )
        .finish()
}
