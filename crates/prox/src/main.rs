use clap::Parser;
use color_eyre::Report;
use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

#[derive(Debug, Parser)]
#[clap(name = "taurox", version)]
pub struct CLArgs {
    #[clap(subcommand)]
    pub routine: CLCommand,
}

#[derive(Debug, clap::Subcommand)]
pub enum CLCommand {
    Tokenize { path: PathBuf },
}

fn main() -> ExitCode {
    fallable_main().expect("Encountered an error!")
}

fn fallable_main() -> Result<ExitCode, Report> {
    let args = CLArgs::parse();
    match args.routine {
        CLCommand::Tokenize { path } => {
            eprintln!("Tokenizing {}...", path.display());
            let src = fs::read_to_string(&path)?;
            if !tokenize(&src) {
                return Ok(ExitCode::from(65));
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
