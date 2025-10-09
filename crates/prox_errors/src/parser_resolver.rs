use crate::ReportableError;
use ariadne::{Color, Config, Label, LabelAttach, Report as AReport, ReportKind, Source};
use core::ops;
use prox_parser::Span;
use prox_parser::cst::tree::TreeKind;
use prox_parser::resolver::{ResolutionError, ResolvedIdent};
use std::io;

type Report<'err> = AReport<'err, (&'err str, ops::Range<usize>)>;

impl ReportableError for ResolutionError {
    fn report(&self, buffer: &mut String, path: &str, text: &str) {
        if let ResolutionError::Parser(ref err) = *self {
            err.report(buffer, path, text);
            return;
        };

        let mut output = io::Cursor::new(Vec::new());

        let report = match *self {
            ResolutionError::Parser(ref err) => {
                unreachable!("just handled this case above.");
            }
            ResolutionError::InvalidNode {
                span,
                actual,
                expected,
            } => format_invalid_node(path, &actual, &expected, &span),
            ResolutionError::SelfReferentialInheritance {
                destination,
                reference,
                span,
            } => format_self_inheritance(path, &destination, &reference, &span),
            ResolutionError::SelfReferentialInitializer {
                destination,
                reference,
                span,
            } => format_self_initialization(path, &destination, &reference, &span),
            ResolutionError::ShadowLocal { old, new, span } => {
                format_shadow_local(path, &old, &new, &span)
            }
            ResolutionError::NonFunctionReturn { span } => format_non_function_return(path, &span),
            ResolutionError::ReturnValueInConstructor { span, constructor } => {
                format_constructor_return(path, &span, &constructor)
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

fn format_invalid_node<'err>(
    path: &'err str,
    actual: &TreeKind,
    expected: &TreeKind,
    span: &Span,
) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message(format!("Expected a {expected:?}"))
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Yellow)
                .with_message(format!("but found {actual:?}",)),
        )
        .finish()
}

fn format_self_inheritance<'err>(
    path: &'err str,
    destination: &ResolvedIdent,
    reference: &ResolvedIdent,
    span: &Span,
) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("A class can't inherit from itself.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, destination.span.range()))
                .with_color(Color::Yellow)
                .with_order(0)
                .with_message("inheriting from"),
        )
        .with_label(
            Label::new((path, reference.span.range()))
                .with_color(Color::Yellow)
                .with_order(1)
                .with_message("itself"),
        )
        .finish()
}

fn format_self_initialization<'err>(
    path: &'err str,
    destination: &ResolvedIdent,
    reference: &ResolvedIdent,
    span: &Span,
) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Can't reference a variable in its own initializer.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, destination.span.range()))
                .with_color(Color::Yellow)
                .with_order(0)
                .with_message("initializing this variable"),
        )
        .with_label(
            Label::new((path, reference.span.range()))
                .with_color(Color::Yellow)
                .with_order(1)
                .with_message("with itself"),
        )
        .finish()
}

fn format_shadow_local<'err>(
    path: &'err str,
    old: &ResolvedIdent,
    new: &ResolvedIdent,
    span: &Span,
) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Can't shadow an already declared variable.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, old.span.range()))
                .with_color(Color::Yellow)
                .with_order(0)
                .with_message("the variable is declared here"),
        )
        .with_label(
            Label::new((path, new.span.range()))
                .with_color(Color::Yellow)
                .with_order(1)
                .with_message("and is being shadowed by this"),
        )
        .finish()
}

fn format_non_function_return<'err>(path: &'err str, span: &Span) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Trying to return from a non-function scope.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("returning from a non-function scope"),
        )
        .finish()
}

fn format_constructor_return<'err>(
    path: &'err str,
    span: &Span,
    constructor: &Span,
) -> Report<'err> {
    let total_span = span.merge(*constructor);
    Report::build(ReportKind::Error, (path, total_span.range()))
        .with_message("Trying to return a value in a constructor.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("returning with value in a constructor"),
        )
        .with_label(
            Label::new((path, constructor.range()))
                .with_color(Color::Yellow)
                .with_message("inside constructor"),
        )
        .finish()
}
