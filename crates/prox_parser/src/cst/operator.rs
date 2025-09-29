use crate::cst::tree::TreeKind;
use prox_lexer::token::TokenKind;

/// Return the binding power of the given token if it is a valid infix operator.
pub(super) const fn infix_binding_power(op: TokenKind) -> Option<(TreeKind, u8, u8)> {
    let bp = match op {
        // 3. Multiplicative operators
        TokenKind::Star => (TreeKind::ExprBinaryStar, 13, 14),
        TokenKind::Slash => (TreeKind::ExprBinarySlash, 13, 14),
        // 4. Additive operators
        TokenKind::Plus => (TreeKind::ExprBinaryPlus, 11, 12),
        TokenKind::Minus => (TreeKind::ExprBinaryMinus, 11, 12),
        // 5. Comparison operators
        TokenKind::LessThan => (TreeKind::ExprBinaryLess, 9, 10),
        TokenKind::LessThanEqual => (TreeKind::ExprBinaryLessEqual, 9, 10),
        TokenKind::GreaterThan => (TreeKind::ExprBinaryGreater, 9, 10),
        TokenKind::GreaterThanEqual => (TreeKind::ExprBinaryGreaterEqual, 9, 10),
        // 6. Equality operators
        TokenKind::EqualEqual => (TreeKind::ExprBinaryEqualEqual, 7, 9),
        TokenKind::BangEqual => (TreeKind::ExprBinaryBangEqual, 7, 9),
        _ => return None,
    };
    Some(bp)
}

/// Return the binding power of the given token if it is a valid short circuiting infix operator.
pub(super) const fn short_circuit_infix_binding_power(op: TokenKind) -> Option<(TreeKind, u8, u8)> {
    let bp = match op {
        // 7. Logical AND operator
        TokenKind::KeywordAnd => (TreeKind::ExprBinaryAnd, 5, 6),
        // 8. Logical OR operator
        TokenKind::KeywordOr => (TreeKind::ExprBinaryOr, 3, 4),
        _ => return None,
    };
    Some(bp)
}

/// Return the binding power of the given token if it is a valid assignment infix operator.
pub(super) const fn assignment_infix_binding_power(op: TokenKind) -> Option<(TreeKind, u8, u8)> {
    let bp = match op {
        // 9. Assignment operator
        TokenKind::Equal => (TreeKind::ExprBinaryAssignment, 2, 1),
        _ => return None,
    };
    Some(bp)
}

/// Return the binding power of the given token if it is a valid prefix operator.
pub(super) const fn prefix_binding_power(op: TokenKind) -> Option<(TreeKind, u8)> {
    let bp = match op {
        // 2. Unary operators
        TokenKind::Minus => (TreeKind::ExprUnaryMinus, 15),
        TokenKind::Bang => (TreeKind::ExprUnaryBang, 15),
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
