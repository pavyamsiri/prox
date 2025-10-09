mod common;

use common::check;

#[test]
fn line_at_eof() {
    const CODE: &str = r#"print "ok"; // expect: ok
// comment    "#;
    check(CODE);
}

#[test]
fn only_line_comment_and_line() {
    const CODE: &str = "// comment
    ";
    check(CODE);
}

#[test]
fn only_line_comment() {
    const CODE: &str = "// comment    ";
    check(CODE);
}

#[test]
fn unicode() {
    const CODE: &str = r#"// Unicode characters are allowed in comments.
//
// Latin 1 Supplement: £§¶ÜÞ
// Latin Extended-A: ĐĦŋœ
// Latin Extended-B: ƂƢƩǁ
// Other stuff: ឃᢆ᯽₪ℜ↩⊗┺░
// Emoji: ☃☺♣

print "ok"; // expect: ok
    "#;
    check(CODE);
}
