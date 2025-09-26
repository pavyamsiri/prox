mod lexer;
mod source;
mod span;
mod state;
pub mod token;

pub use lexer::Lexer;
pub use source::SourceLookup;

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn it_works() {
        let mut lexer = Lexer::new("print \"hello world");
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let token = lexer.next_token();
            tokens.push(token);
            if token.is_eof() {
                break;
            }
        }
        for token in tokens {
            let lexeme = Lexer::lexeme(lexer.get_source(), &token)
                .expect("Token came from lexer so it is guaranteed to be valid.");
            if token.is_error() {
                println!("ERROR: {token:?} -> {lexeme}");
            } else {
                println!("_____: {token:?} -> {lexeme}");
            }
        }
        assert_eq!("Hello world", "Hello world".to_owned());
    }
}
