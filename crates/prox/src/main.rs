use ariadne::LabelAttach;
use clap::Parser;
use color_eyre::Report;
use prox_lexer::span;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use tracing_subscriber::{EnvFilter, fmt, prelude::*};

const FRONTEND_ERROR: u8 = 65;

#[derive(Debug, Parser)]
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
    use ariadne::{Color, Config, Label, Report as ErrorReport, ReportKind, Source};
    use prox_parser::cst::{ParseError, Parser};

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
            ParseError::Custom(msg) => {
                ErrorReport::build(ReportKind::Error, 0..0).with_message(msg);
            }
            ParseError::Multispan(items) => {
                let total_span = items
                    .iter()
                    .map(|val| val.0)
                    .reduce(span::Span::merge)
                    .expect("multispan errors have at least one span.");
                let mut builder = ErrorReport::build(ReportKind::Error, (path, total_span.range()))
                    .with_message("Invalid assigment l-value".to_owned())
                    .with_config(Config::default().with_compact(true));

                for (span, msg) in items {
                    builder = builder.with_label(
                        Label::new((path, span.range()))
                            .with_color(Color::Blue)
                            .with_message(msg),
                    );
                }
                builder
                    .finish()
                    .print((path, Source::from(lookup.get_text())))
                    .expect("not handling io errors.");
            }
            ParseError::InvalidAssignment { lvalue, op, value } => {
                let total_span = lvalue.0.merge(op).merge(value.0);
                ErrorReport::build(ReportKind::Error, (path, total_span.range()))
                    .with_message("Invalid assignment target".to_owned())
                    .with_config(Config::default().with_label_attach(LabelAttach::Middle))
                    .with_label(
                        Label::new((path, value.0.range()))
                            .with_color(Color::Yellow)
                            .with_message("tried assigning this value".to_owned())
                            .with_order(0),
                    )
                    .with_label(
                        Label::new((path, lvalue.0.range()))
                            .with_color(Color::Red)
                            .with_message(format!(
                                "to {} which is not a valid lvalue",
                                lvalue.1.name()
                            ))
                            .with_order(1),
                    )
                    .finish()
                    .print((path, Source::from(lookup.get_text())))
                    .expect("not handling io errors.");
            }
        }
    }

    println!("{buffer}");

    true
}
