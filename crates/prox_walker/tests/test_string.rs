mod common;

use common::check;

#[test]
fn error_after_multiline() {
    const CODE: &str = r#"// Tests that we correctly track the line info across multiline strings.
var a = "1
2
3
";

err; // // expect runtime error: Undefined variable 'err'.
    "#;
    check(CODE);
}

#[test]
fn multiline() {
    const CODE: &str = r#"var a = "1
2
3";
print a;
// expect: 1
// expect: 2
// expect: 3
    "#;
    check(CODE);
}

#[test]
fn literals() {
    const CODE: &str = r#"print "(" + "" + ")";   // expect: ()
print "a string"; // expect: a string

// Non-ASCII.
print "A~¶Þॐஃ"; // expect: A~¶Þॐஃ
    "#;
    check(CODE);
}

#[test]
fn unterminated() {
    const CODE: &str = r#"// [line 2] Error: Unterminated string.
"this string has no close quote
    "#;
    check(CODE);
}
