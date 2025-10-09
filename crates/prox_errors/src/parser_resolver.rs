use crate::ReportableError;
use ariadne::{Color, Config, Label, Report as AReport, ReportKind, Source};
use core::ops;
use prox_parser::Span;
use prox_parser::resolver::{ResolutionError, ResolvedIdent};
use std::io;

type Report<'err> = AReport<'err, (&'err str, ops::Range<usize>)>;

impl ReportableError for ResolutionError {
    fn report(&self, buffer: &mut String, path: &str, text: &str) {
        if let ResolutionError::Parser(ref err) = *self {
            err.report(buffer, path, text);
            return;
        }
        let mut output = io::Cursor::new(Vec::new());

        let report = match *self {
            ResolutionError::Parser(..) => {
                unreachable!("just handled this case above.");
            }
            ResolutionError::NotAProgram => format_not_a_program(
                path,
                &Span {
                    start: 0,
                    length: text.len(),
                },
            ),
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
            ResolutionError::NonClassSuper { span } => format_non_class_super(path, &span),
            ResolutionError::NonSubClassSuper { span } => format_non_subclass_super(path, &span),
            ResolutionError::NonClassThis { span } => format_non_class_this(path, &span),
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

fn format_not_a_program<'err>(path: &'err str, span: &Span) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("This is not a program which can only happen if the parsed root was changed.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_order(1)
                .with_message("corrupted parse?"),
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
                .with_color(Color::Red)
                .with_order(0)
                .with_message("inheriting from"),
        )
        .with_label(
            Label::new((path, reference.span.range()))
                .with_color(Color::Red)
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
                .with_color(Color::Red)
                .with_order(0)
                .with_message("initializing this variable"),
        )
        .with_label(
            Label::new((path, reference.span.range()))
                .with_color(Color::Red)
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
                .with_color(Color::Red)
                .with_order(0)
                .with_message("the variable is declared here"),
        )
        .with_label(
            Label::new((path, new.span.range()))
                .with_color(Color::Red)
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
                .with_color(Color::Red)
                .with_message("inside constructor"),
        )
        .finish()
}

fn format_non_class_super<'err>(path: &'err str, span: &Span) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Used `super` outside of a class.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("outside of a class."),
        )
        .finish()
}

fn format_non_subclass_super<'err>(path: &'err str, span: &Span) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Used `super` outside of a subclass.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("outside of a subclass."),
        )
        .finish()
}
fn format_non_class_this<'err>(path: &'err str, span: &Span) -> Report<'err> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Used `this` outside of a class.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Red)
                .with_message("outside of a class."),
        )
        .finish()
}
