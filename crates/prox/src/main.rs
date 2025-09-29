use ariadne::LabelAttach;
use ariadne::{Color, Config, Label, Report as ErrorReport, ReportKind, Source};
use clap::Parser as CLParser;
use color_eyre::Report;
use prox_lexer::span::Span;
use prox_lexer::token::Token;
use prox_parser::cst::{ParseError, Parser};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use tracing_subscriber::{EnvFilter, fmt, prelude::*};

const FRONTEND_ERROR: u8 = 65;

#[derive(Debug, CLParser)]
#[clap(name = "prox", version)]
pub struct CLArgs {
    #[clap(subcommand)]
    pub routine: CLCommand,
}

#[derive(Debug, clap::Subcommand)]
pub enum CLCommand {
    Tokenize { path: PathBuf },
    Parse { path: PathBuf },
}

fn main() -> ExitCode {
    fallable_main().expect("Encountered an error!")
}

fn fallable_main() -> Result<ExitCode, Report> {
    let args = CLArgs::parse();

    let filter = EnvFilter::builder().from_env()?;
    tracing_subscriber::registry()
        .with(fmt::layer().without_time())
        .with(filter)
        .init();

    match args.routine {
        CLCommand::Tokenize { path } => {
            eprintln!("Tokenizing {}...", path.display());
            let src = fs::read_to_string(&path)?;
            if !tokenize(&src) {
                return Ok(ExitCode::from(FRONTEND_ERROR));
            }
        }
        CLCommand::Parse { path } => {
            eprintln!("Parsing {}...", path.display());
            let src = fs::read_to_string(&path)?;
            if !parse(&src, &path) {
                return Ok(ExitCode::from(FRONTEND_ERROR));
            }
        }
    }
    Ok(ExitCode::SUCCESS)
}

fn tokenize(text: &str) -> bool {
    use prox_lexer::Lexer;

    let mut scanner = Lexer::new(text);
    let mut succeeded = true;
    let mut buffer = String::new();
    loop {
        buffer.clear();
        let token = scanner.next_token();
        succeeded &= token.is_error();
        Lexer::dump_token_cc(scanner.get_source(), &mut buffer, &token)
            .expect("writing to a string shouldn't normally error.");

        println!("{buffer}");
        if token.is_eof() {
            return succeeded;
        }
    }
}

fn parse(text: &str, path: &Path) -> bool {
    let path = &path.to_string_lossy();

    let parser = Parser::new(text);
    let (lookup, res, errors) = parser.parse();
    let mut buffer = String::new();
    res.dump(&lookup, &mut buffer, 0, true)
        .expect("can't handle formatting errors.");

    for error in errors {
        match error {
            ParseError::Expected {
                actual,
                context,
                expected,
            } => {
                ErrorReport::build(ReportKind::Error, (path, actual.span.range()))
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
                    .print((path, Source::from(lookup.get_text())))
                    .expect("not handling io errors.");
            }
            ParseError::InvalidAssignment { lvalue, value } => {
                format_invalid_assignment(path, text, lvalue, value);
            }
            ParseError::MissingDotAfterSuper {
                super_token,
                actual,
            } => {
                format_missing_super_dot(path, text, super_token, actual);
            }
            ParseError::MissingSuperMethod {
                super_token,
                actual,
            } => {
                format_missing_super_method(path, text, super_token, actual);
            }
            ParseError::TooManyArguments {
                list_start,
                list_end,
            } => format_too_many_arguments(path, text, list_start, list_end),
            ParseError::TooManyParameters {
                list_start,
                list_end,
            } => format_too_many_parameters(path, text, list_start, list_end),
            ParseError::MissingComma { context, actual } => {
                format_missing_comma(path, text, context, actual);
            }
            ParseError::InvalidSuperclass { class_decl, actual } => {
                format_invalid_superclass(path, text, class_decl, actual);
            }
        }
    }

    println!("{buffer}");

    true
}

fn format_missing_super_method(path: &str, text: &str, super_token: Token, actual: Token) {
    let total_span = super_token.span.merge(actual.span);
    ErrorReport::build(ReportKind::Error, (path, total_span.range()))
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
        .print((path, Source::from(text)))
        .expect("not handling io errors.");
}

fn format_missing_super_dot(path: &str, text: &str, super_token: Token, actual: Token) {
    let total_span = super_token.span.merge(actual.span);
    ErrorReport::build(ReportKind::Error, (path, total_span.range()))
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
        .print((path, Source::from(text)))
        .expect("not handling io errors.");
}

fn format_invalid_assignment(path: &str, text: &str, lvalue: Span, value: Span) {
    let total_span = lvalue.merge(value);
    ErrorReport::build(ReportKind::Error, (path, total_span.range()))
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
        .print((path, Source::from(text)))
        .expect("not handling io errors.");
}

fn format_too_many_arguments(path: &str, text: &str, list_start: Token, list_end: Token) {
    let total_span = list_start.span.merge(list_end.span);
    ErrorReport::build(ReportKind::Error, (path, total_span.range()))
        .with_message("Called with more than 255 arguments.".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, total_span.range()))
                .with_color(Color::Red)
                .with_message("too many arguments".to_owned()),
        )
        .finish()
        .print((path, Source::from(text)))
        .expect("not handling io errors.");
}

fn format_too_many_parameters(path: &str, text: &str, list_start: Token, list_end: Token) {
    let total_span = list_start.span.merge(list_end.span);
    ErrorReport::build(ReportKind::Error, (path, total_span.range()))
        .with_message("Function with more than 255 parameters.".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, total_span.range()))
                .with_color(Color::Red)
                .with_message("too many parameters".to_owned()),
        )
        .finish()
        .print((path, Source::from(text)))
        .expect("not handling io errors.");
}

fn format_missing_comma(path: &str, text: &str, context: &'static str, actual: Token) {
    ErrorReport::build(ReportKind::Error, (path, actual.span.range()))
        .with_message("Perhaps you missed a comma?".to_owned())
        .with_config(Config::default())
        .with_label(
            Label::new((path, actual.span.range()))
                .with_color(Color::Red)
                .with_message(format!("missing comma here when parsing a {context} list")),
        )
        .finish()
        .print((path, Source::from(text)))
        .expect("not handling io errors.");
}

fn format_invalid_superclass(path: &str, text: &str, class_decl: Span, actual: Span) {
    ErrorReport::build(ReportKind::Error, (path, actual.range()))
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
        .print((path, Source::from(text)))
        .expect("not handling io errors.");
}
