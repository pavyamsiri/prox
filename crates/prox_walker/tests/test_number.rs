mod common;

use common::check;

#[test]
fn leading_dot() {
    const CODE: &str = "// [line 2] Error at '.': Expect expression.
.123;
    ";
    check(CODE);
}

#[test]
fn trailing_dot() {
    const CODE: &str = "// [line 2] Error at ';': Expect property name after '.'.
123.;
    ";
    check(CODE);
}

#[test]
fn decimal_point_at_eof() {
    const CODE: &str = "// [line 3] Error at '': Expect property name after '.'.
// [line 3] Error at '': Expect ';' after statement.
123.";
    check(CODE);
}

#[test]
fn nan_equality() {
    const CODE: &str = "var nan = 0/0;

print nan == 0; // expect: false
print nan != 1; // expect: true

// NaN is not equal to self.
print nan == nan; // expect: false
print nan != nan; // expect: true
    ";
    check(CODE);
}

#[test]
fn literals() {
    const CODE: &str = "print 123;     // expect: 123
print 987654;  // expect: 987654
print 0;       // expect: 0
print -0;      // expect: -0

print 123.456; // expect: 123.456
print -0.001;  // expect: -0.001
    ";
    check(CODE);
}
