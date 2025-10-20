use crate::ReportableError;
use ariadne::{Color, Config, Label, LabelAttach, Report as AReport, ReportKind, Source};
use core::ops;
use prox_parser::{Token, cst::ParseError as CstError};
use prox_span::Span;
use std::io;

type Report<'err> = AReport<'err, (&'err str, ops::Range<usize>)>;

impl ReportableError for CstError {
    fn report(&self, buffer: &mut String, path: &str, text: &str) {
        let mut output = io::Cursor::new(Vec::new());

        let report = match *self {
            CstError::Expected {
                actual,
                ref expected,
                ref context,
            } => format_unexpected(path, actual, expected, context),
            CstError::InvalidAssignment { lvalue, value } => {
                format_invalid_assignment(path, lvalue, value)
            }
            CstError::MissingDotAfterSuper {
                super_token,
                actual,
            } => format_missing_super_dot(path, super_token, actual),
            CstError::MissingSuperMethod {
                super_token,
                actual,
            } => format_missing_super_method(path, super_token, actual),
            CstError::TooManyArguments {
                list_start,
                list_end,
                ..
            } => format_too_many_arguments(path, list_start, list_end),
            CstError::TooManyParameters {
                list_start,
                list_end,
                ..
            } => format_too_many_parameters(path, list_start, list_end),
            CstError::MissingComma { context, actual } => {
                format_missing_comma(path, context, actual)
            }
            CstError::InvalidSuperclass { class_decl, actual } => {
                format_invalid_superclass(path, class_decl, actual)
            }
            CstError::MissingFunctionBodyLeftBrace { actual } => {
                format_missing_function_body_left_brace(path, actual)
            }
            CstError::InvalidCharacter { span } => format_invalid_character(path, span),
            CstError::MissingFieldName { actual } => format_missing_field(path, actual),
            CstError::UnterminatedString { span } => format_unterminated_string(path, span),
            CstError::MissingVariableName { actual } => format_missing_variable_name(path, actual),
            CstError::MissingSemicolon { actual } => format_missing_semicolon(path, actual),
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

fn format_unexpected<'err>(
    path: &'err str,
    actual: Token,
    expected: &str,
    context: &str,
) -> Report<'err> {
    Report::build(ReportKind::Error, (path, actual.span.range()))
        .with_message(format!("Expected a {expected}"))
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Yellow)
                .with_message(format!(
                    "but found {} while parsing {context}",
                    actual.tag.name()
                )),
        )
        .finish()
}

fn format_missing_super_method(path: &str, super_token: Token, actual: Token) -> Report<'_> {
    let total_span = super_token.span.merge(actual.span);
    Report::build(ReportKind::Error, (path, total_span.range()))
        .with_message("Missing a method name after super".to_owned())
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, super_token.span.range()))
                .with_color(Color::Yellow)
                .with_message("used super here".to_owned())
                .with_order(1),
        )
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Red)
                .with_message("without a method name".to_owned())
                .with_order(0),
        )
        .finish()
}

fn format_missing_super_dot(path: &str, super_token: Token, actual: Token) -> Report<'_> {
    let total_span = super_token.span.merge(actual.span);
    Report::build(ReportKind::Error, (path, total_span.range()))
        .with_message("Missing a dot after super".to_owned())
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, super_token.span.range()))
                .with_color(Color::Yellow)
                .with_message("used super here".to_owned())
                .with_order(1),
        )
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Red)
                .with_message("without a dot".to_owned())
                .with_order(0),
        )
        .finish()
}

fn format_invalid_assignment(path: &str, lvalue: Span, value: Span) -> Report<'_> {
    let total_span = lvalue.merge(value);
    Report::build(ReportKind::Error, (path, total_span.range()))
        .with_message("Invalid assignment target".to_owned())
        .with_config(Config::default().with_label_attach(LabelAttach::Middle))
        .with_label(
            Label::new((path, value.range()))
                .with_color(Color::Yellow)
                .with_message("tried assigning this value".to_owned())
                .with_order(0),
        )
        .with_label(
            Label::new((path, lvalue.range()))
                .with_color(Color::Red)
                .with_message("to an invalid lvalue")
                .with_order(1),
        )
        .finish()
}

fn format_too_many_arguments(path: &str, list_start: Token, list_end: Token) -> Report<'_> {
    let total_span = list_start.span.merge(list_end.span);
    Report::build(ReportKind::Error, (path, total_span.range()))
        .with_message("Called with more than 255 arguments.".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, total_span.range()))
                .with_color(Color::Red)
                .with_message("too many arguments".to_owned()),
        )
        .finish()
}

fn format_too_many_parameters(path: &str, list_start: Token, list_end: Token) -> Report<'_> {
    let total_span = list_start.span.merge(list_end.span);
    Report::build(ReportKind::Error, (path, total_span.range()))
        .with_message("Function with more than 255 parameters.".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, total_span.range()))
                .with_color(Color::Red)
                .with_message("too many parameters".to_owned()),
        )
        .finish()
}

fn format_missing_semicolon(path: &str, actual: Token) -> Report<'_> {
    Report::build(ReportKind::Error, (path, actual.span.range()))
        .with_message("Perhaps you missed a semicolon?".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Red)
                .with_message("missing semicolon before here."),
        )
        .finish()
}

fn format_missing_comma<'err>(
    path: &'err str,
    context: &'static str,
    actual: Token,
) -> Report<'err> {
    Report::build(ReportKind::Error, (path, actual.span.range()))
        .with_message("Perhaps you missed a comma?".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Red)
                .with_message(format!("missing comma here when parsing a {context} list")),
        )
        .finish()
}

fn format_invalid_superclass(path: &str, class_decl: Span, actual: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, actual.range()))
        .with_message("Superclass must be an identifier.".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, actual.range()))
                .with_color(Color::Red)
                .with_message("is not an identifier.".to_owned())
                .with_order(0),
        )
        .with_label(
            Label::new((path, class_decl.range()))
                .with_color(Color::Yellow)
                .with_message("class declared here".to_owned())
                .with_order(1),
        )
        .finish()
}

fn format_missing_function_body_left_brace(path: &str, actual: Token) -> Report<'_> {
    Report::build(ReportKind::Error, (path, actual.span.range()))
        .with_message("The function body is missing an opening brace.".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Yellow)
                .with_message("forgot a `{`?".to_owned())
                .with_order(1),
        )
        .finish()
}

fn format_invalid_character(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("Encountered an invalid character.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Yellow)
                .with_message("this character is not valid."),
        )
        .finish()
}

fn format_unterminated_string(path: &str, span: Span) -> Report<'_> {
    Report::build(ReportKind::Error, (path, span.range()))
        .with_message("String was opened but never closed.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, span.range()))
                .with_color(Color::Yellow)
                .with_message("missing a closing quote."),
        )
        .finish()
}

fn format_missing_field(path: &str, actual: Token) -> Report<'_> {
    Report::build(ReportKind::Error, (path, actual.span.range()))
        .with_message("Missing field name after `.`.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Yellow)
                .with_message("should be a field name here."),
        )
        .finish()
}

fn format_missing_variable_name(path: &str, actual: Token) -> Report<'_> {
    Report::build(ReportKind::Error, (path, actual.span.range()))
        .with_message("Missing variable name after `var`.")
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Yellow)
                .with_message("should be an identifier."),
        )
        .finish()
}
