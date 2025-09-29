use prox_lexer::token::TokenKind;

/// The only valid first tokens of an expression.
pub(super) const EXPR_FIRST: &[TokenKind] = &[
    // Atoms
    TokenKind::NumericLiteral,
    TokenKind::KeywordTrue,
    TokenKind::KeywordFalse,
    TokenKind::KeywordNil,
    TokenKind::Ident,
    TokenKind::StringLiteral,
    TokenKind::KeywordThis,
    TokenKind::KeywordSuper,
    // Groupings
    TokenKind::LeftParenthesis,
    // Unary operators
    TokenKind::Bang,
    TokenKind::Minus,
];

/// The tokens which can only be binary ops.
pub(super) const BINARY_OP_ONLY: &[TokenKind] = &[
    TokenKind::Plus,
    TokenKind::Star,
    TokenKind::Slash,
    TokenKind::KeywordAnd,
    TokenKind::KeywordOr,
    TokenKind::LessThan,
    TokenKind::LessThanEqual,
    TokenKind::GreaterThan,
    TokenKind::GreaterThanEqual,
];

/// The only valid first tokens of a statement.
pub(super) const STMT_FIRST: &[TokenKind] = &[
    // For
    TokenKind::KeywordFor,
    // If
    TokenKind::KeywordIf,
    // Print
    TokenKind::KeywordPrint,
    // Return
    TokenKind::KeywordReturn,
    // While
    TokenKind::KeywordWhile,
    // Block
    TokenKind::LeftBrace,
    // Atoms
    TokenKind::NumericLiteral,
    TokenKind::KeywordTrue,
    TokenKind::KeywordFalse,
    TokenKind::KeywordNil,
    TokenKind::Ident,
    TokenKind::StringLiteral,
    TokenKind::KeywordThis,
    TokenKind::KeywordSuper,
    // Groupings
    TokenKind::LeftParenthesis,
    // Unary operators
    TokenKind::Bang,
    TokenKind::Minus,
];

/// The only valid first tokens of a declaration.
pub(super) const DECL_FIRST: &[TokenKind] = &[
    // Var
    TokenKind::KeywordVar,
    // Class
    TokenKind::KeywordClass,
    // Fun
    TokenKind::KeywordFun,
    // For
    TokenKind::KeywordFor,
    // If
    TokenKind::KeywordIf,
    // Print
    TokenKind::KeywordPrint,
    // Return
    TokenKind::KeywordReturn,
    // While
    TokenKind::KeywordWhile,
    // Block
    TokenKind::LeftBrace,
    // Atoms
    TokenKind::NumericLiteral,
    TokenKind::KeywordTrue,
    TokenKind::KeywordFalse,
    TokenKind::KeywordNil,
    TokenKind::Ident,
    TokenKind::StringLiteral,
    TokenKind::KeywordThis,
    TokenKind::KeywordSuper,
    // Groupings
    TokenKind::LeftParenthesis,
    // Unary operators
    TokenKind::Bang,
    TokenKind::Minus,
];

/// The recovery set for statements.
pub(super) const STMT_RECOVERY: &[TokenKind] = &[
    // Declarations
    TokenKind::KeywordClass,
    TokenKind::KeywordVar,
    TokenKind::KeywordFun,
];

/// All valid first tokens of an expression as a pattern.
macro_rules! expr_first {
    () => {
        // Atoms
        prox_lexer::token::TokenKind::NumericLiteral
            | prox_lexer::token::TokenKind::KeywordTrue
            | prox_lexer::token::TokenKind::KeywordFalse
            | prox_lexer::token::TokenKind::KeywordNil
            | prox_lexer::token::TokenKind::Ident
            | prox_lexer::token::TokenKind::StringLiteral
            | prox_lexer::token::TokenKind::KeywordThis
            | prox_lexer::token::TokenKind::KeywordSuper
            // Groupings
            | prox_lexer::token::TokenKind::LeftParenthesis
            // Unary operators
            | prox_lexer::token::TokenKind::Bang
            | prox_lexer::token::TokenKind::Minus
    };
}
pub(super) use expr_first;

/// All valid first tokens of a statement as a pattern.
macro_rules! stmt_first {
    () => {
        prox_lexer::token::TokenKind::KeywordFor
            | prox_lexer::token::TokenKind::KeywordIf
            | prox_lexer::token::TokenKind::KeywordPrint
            | prox_lexer::token::TokenKind::KeywordReturn
            | prox_lexer::token::TokenKind::KeywordWhile
            | prox_lexer::token::TokenKind::LeftBrace
            | prox_lexer::token::TokenKind::NumericLiteral
            | prox_lexer::token::TokenKind::KeywordTrue
            | prox_lexer::token::TokenKind::KeywordFalse
            | prox_lexer::token::TokenKind::KeywordNil
            | prox_lexer::token::TokenKind::Ident
            | prox_lexer::token::TokenKind::StringLiteral
            | prox_lexer::token::TokenKind::KeywordThis
            | prox_lexer::token::TokenKind::KeywordSuper
            | prox_lexer::token::TokenKind::LeftParenthesis
            | prox_lexer::token::TokenKind::Bang
            | prox_lexer::token::TokenKind::Minus
    };
}
pub(super) use stmt_first;
