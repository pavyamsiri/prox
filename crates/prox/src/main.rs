use clap::Parser as CLParser;
use color_eyre::Report;
use prox_errors::ReportableError as _;
use prox_parser::cst::Parser;
use prox_parser::cst_to_ast::CstToAstConverter;
use prox_parser::resolver::Resolver;
use prox_walker::environment::SharedEnvironment;
use prox_walker::interpreter::{Interpreter, ResolvedAst};
use prox_walker::io::StdoutContext;
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
    /// Tokenize a file.
    Tokenize { path: PathBuf },
    /// Parse a file into a CST.
    Parse { path: PathBuf },
    /// Interpret a file using a tree-walk interpreter.
    Walk { path: PathBuf },
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
        CLCommand::Walk { path } => {
            eprintln!("Walking {}...", path.display());
            let src = fs::read_to_string(&path)?;
            if !walk(&src, &path) {
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
        Lexer::dump_token_cc(&scanner.get_source(), &mut buffer, &token)
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
    let cst = parser.parse();
    let resolver = Resolver::default();
    let Err(errors) = resolver.resolve(cst) else {
        return true;
    };

    let mut buffer = String::new();

    for error in errors {
        error.report(&mut buffer, path, text);
    }

    println!("{buffer}");

    false
}

fn walk(text: &str, path: &Path) -> bool {
    tracing::info!("{text}");
    let path = &path.to_string_lossy();
    tracing::debug!("Parsing...");
    let cst = Parser::new(text).parse();
    let resolver = Resolver::default();
    tracing::debug!("Resolving...");
    let resolved_cst = match resolver.resolve(cst) {
        Ok(cst) => cst,
        Err(errors) => {
            let mut buffer = String::new();
            for error in errors {
                error.report(&mut buffer, path, text);
            }
            println!("{buffer}");
            return false;
        }
    };
    tracing::debug!("Converting...");

    let converter = CstToAstConverter::with_interner(resolved_cst.interner);

    let mut ast = match converter.convert(&resolved_cst.source, &resolved_cst.root) {
        Ok(ast) => ast,
        Err(err) => {
            tracing::error!("{err}");
            return false;
        }
    };

    let mut context = StdoutContext;
    let mut interpreter = Interpreter;
    let mut environment = SharedEnvironment::with_interner(ast.get_interner_mut());
    let res = interpreter.run(
        &mut context,
        &mut environment,
        &ResolvedAst {
            ast,
            resolution: resolved_cst.resolution,
        },
    );

    match res {
        Ok(()) => true,
        Err(err) => {
            let mut buffer = String::new();
            err.report(&mut buffer, path, text);
            println!("{buffer}");
            false
        }
    }
}
