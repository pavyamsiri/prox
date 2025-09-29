use prox_lexer::token::TokenKind;

/// Return the binding power of the given token if it is a valid infix operator.
pub(super) const fn infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    let bp = match op {
        // 3. Multiplicative operators
        TokenKind::Star | TokenKind::Slash => (13, 14),
        // 4. Additive operators
        TokenKind::Plus | TokenKind::Minus => (11, 12),
        // 5. Comparison operators
        TokenKind::LessThan
        | TokenKind::LessThanEqual
        | TokenKind::GreaterThan
        | TokenKind::GreaterThanEqual => (9, 10),
        // 6. Equality operators
        TokenKind::EqualEqual | TokenKind::BangEqual => (7, 9),
        _ => return None,
    };
    Some(bp)
}

/// Return the binding power of the given token if it is a valid short circuiting infix operator.
pub(super) const fn short_circuit_infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    let bp = match op {
        // 7. Logical AND operator
        TokenKind::KeywordAnd => (5, 6),
        // 8. Logical OR operator
        TokenKind::KeywordOr => (3, 4),
        _ => return None,
    };
    Some(bp)
}

/// Return the binding power of the given token if it is a valid assignment infix operator.
pub(super) const fn assignment_infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    let bp = match op {
        // 9. Assignment operator
        TokenKind::Equal => (2, 1),
        _ => return None,
    };
    Some(bp)
}

/// Return the binding power of the given token if it is a valid prefix operator.
pub(super) const fn prefix_binding_power(op: TokenKind) -> Option<u8> {
    let bp = match op {
        // 2. Unary operators
        TokenKind::Bang | TokenKind::Minus => 15,
        _ => return None,
    };
    Some(bp)
}

/// Return the binding power of the given token if it is a valid postfix operator.
pub(super) const fn postfix_binding_power(op: TokenKind) -> Option<u8> {
    let bp = match op {
        // 0. Access operator
        TokenKind::Dot => 19,
        // 1. Call operator
        TokenKind::LeftParenthesis => 17,
        _ => return None,
    };
    Some(bp)
}
