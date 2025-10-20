use crate::ReportableError;
use ariadne::{Color, Config, Label, Report as AReport, ReportKind, Source};
use core::ops;
use prox_span::Span;
use prox_walker::error::{RuntimeError, RuntimeErrorKind};
use std::io;

type Report<'err> = AReport<'err, (&'err str, ops::Range<usize>)>;

impl ReportableError for RuntimeError {
    fn report(&self, buffer: &mut String, path: &str, text: &str) {
        let mut output = io::Cursor::new(Vec::new());
        let span = self.span;

        let report = match self.kind {
            RuntimeErrorKind::InvalidAccess => format_invalid_access(path, span),
            RuntimeErrorKind::InvalidAssign => format_invalid_assign(path, span),
            RuntimeErrorKind::InvalidCallee => format_invalid_callee(path, span),
            RuntimeErrorKind::InvalidArgumentCount { expected, actual } => {
                format_invalid_num_arguments(path, span, expected as usize, actual)
            }
            RuntimeErrorKind::InvalidGet => format_invalid_get(path, span),
            RuntimeErrorKind::InvalidSet => format_invalid_set(path, span),
            RuntimeErrorKind::InvalidSuperClass => format_invalid_superclass(path, span),
            RuntimeErrorKind::NonArithmeticOperand => format_invalid_arithmetic_operand(path, span),
            RuntimeErrorKind::NonArithmeticOperands => {
                format_invalid_arithmetic_operands(path, span)
            }
            RuntimeErrorKind::InvalidAddOperands => format_invalid_add_operands(path, span),
            RuntimeErrorKind::UndefinedField { .. } => format_undefined_field(path, span),
            RuntimeErrorKind::Io => format_io_error(path, span),
            RuntimeErrorKind::NonInstanceThis => format_generic_error(
                path,
                span,
                "`this` is not an instance. This can only mean the AST is corrupted.",
                "`this` was accessed here.",
            ),
            RuntimeErrorKind::NonClassSuper => format_generic_error(
                path,
                span,
                "`super` is not a class. This can only mean the AST is corrupted.",
                "`super` was accessed here.",
            ),
            RuntimeErrorKind::ReturnFromNonFunction => format_generic_error(
                path,
                span,
                "Attempted to return a value in a non-function scope.",
                "tried to return here.",
            ),
            RuntimeErrorKind::NullNode => format_generic_error(
                path,
                span,
                "Tried to visit a node that is null.",
                "tried to access a null node here.",
            ),
            RuntimeErrorKind::InvalidStatement => format_generic_error(
                path,
                span,
                "Expected to visit a statement.",
                "this is not a statement.",
            ),
            RuntimeErrorKind::InvalidExpr => format_generic_error(
                path,
                span,
                "Expected to visit an expression",
                "this is not an expression.",
            ),
            RuntimeErrorKind::InvalidSymbol => format_generic_error(
                path,
                span,
                "Could not resolve internal symbol.",
                "this caused a symbol resolution that failed.",
            ),
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

fn format_undefined_field(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Accessing an undefined field.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("field is undefined."),
        )
        .finish()
}

fn format_invalid_access(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Accessing a variable that has not yet been declared.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("variable has not been declared yet."),
        )
        .finish()
}

fn format_invalid_assign(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Assigning a variable that has not yet been declared.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("variable has not been declared yet."),
        )
        .finish()
}

fn format_invalid_callee(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Calling a value that is not callable.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("this is not a function or class."),
        )
        .finish()
}

fn format_invalid_arithmetic_operand(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Attempting unary operation that require the operand to be a number.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("operand must be a number."),
        )
        .finish()
}

fn format_invalid_arithmetic_operands(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Attempting binary operation that requires both operands to be numbers.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("operands must both be numbers."),
        )
        .finish()
}

fn format_invalid_add_operands(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Adding operands that can't be added together.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("operands must both be numbers or strings."),
        )
        .finish()
}

fn format_invalid_num_arguments(
    path: &str,
    span: Span,
    expected: usize,
    actual: usize,
) -> Report<'_> {
    let message = if actual > expected {
        "called with too many arguments."
    } else {
        "called with too few arguments."
    };

    Report::build(ReportKind::Error, (path, span.range()))
        .with_message(format!(
            "Calling function with {actual} arguments when it expects {expected} arguments."
        ))
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message(message),
        )
        .finish()
}

fn format_invalid_get(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Attempted to access a property on a non-instance value.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("this is not an instance."),
        )
        .finish()
}

fn format_invalid_set(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Attempted to modify a field on a non-instance value.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("this is not an instance."),
        )
        .finish()
}

fn format_invalid_superclass(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("This is not a valid superclass.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("this is not a valid superclass."),
        )
        .finish()
}

fn format_io_error(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Encountered an error with IO while running this.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("IO error encountered here."),
        )
        .finish()
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
