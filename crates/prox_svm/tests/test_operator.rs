mod common;

use common::check;

#[test]
fn not_class() {
    const CODE: &str = "class Bar {}
print !Bar;      // expect: false
print !Bar();    // expect: false
    ";
    check(CODE);
}

#[test]
fn multiply_num_nonnum() {
    const CODE: &str = r#"1 * "1"; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn divide_nonnum_num() {
    const CODE: &str = r#""1" / 1; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn subtract_nonnum_num() {
    const CODE: &str = r#""1" - 1; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn comparison() {
    const CODE: &str = "print 1 < 2;    // expect: true
print 2 < 2;    // expect: false
print 2 < 1;    // expect: false

print 1 <= 2;    // expect: true
print 2 <= 2;    // expect: true
print 2 <= 1;    // expect: false

print 1 > 2;    // expect: false
print 2 > 2;    // expect: false
print 2 > 1;    // expect: true

print 1 >= 2;    // expect: false
print 2 >= 2;    // expect: true
print 2 >= 1;    // expect: true

// Zero and negative zero compare the same.
print 0 < -0; // expect: false
print -0 < 0; // expect: false
print 0 > -0; // expect: false
print -0 > 0; // expect: false
print 0 <= -0; // expect: true
print -0 <= 0; // expect: true
print 0 >= -0; // expect: true
print -0 >= 0; // expect: true
    ";
    check(CODE);
}

#[test]
fn equals() {
    const CODE: &str = r#"print nil == nil; // expect: true

print true == true; // expect: true
print true == false; // expect: false

print 1 == 1; // expect: true
print 1 == 2; // expect: false

print "str" == "str"; // expect: true
print "str" == "ing"; // expect: false

print nil == false; // expect: false
print false == 0; // expect: false
print 0 == "0"; // expect: false
    "#;
    check(CODE);
}

#[test]
fn equals_method() {
    const CODE: &str = "// Bound methods have identity equality.
class Foo {
  method() {}
}

var foo = Foo();
var fooMethod = foo.method;

// Same bound method.
print fooMethod == fooMethod; // expect: true

// Different closurizations.
print foo.method == foo.method; // expect: false
    ";
    check(CODE);
}

#[test]
fn equals_class() {
    const CODE: &str = r#"// Bound methods have identity equality.
class Foo {}
class Bar {}

print Foo == Foo; // expect: true
print Foo == Bar; // expect: false
print Bar == Foo; // expect: false
print Bar == Bar; // expect: true

print Foo == "Foo"; // expect: false
print Foo == nil;   // expect: false
print Foo == 123;   // expect: false
print Foo == true;  // expect: false
    "#;
    check(CODE);
}

#[test]
fn subtract_num_nonnum() {
    const CODE: &str = r#"1 - "1"; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn add_bool_nil() {
    const CODE: &str =
        "true + nil; // expect runtime error: Operands must be two numbers or two strings.";
    check(CODE);
}

#[test]
fn less_num_nonnum() {
    const CODE: &str = r#"1 < "1"; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn negate_nonnum() {
    const CODE: &str = r#"-"s"; // expect runtime error: Operand must be a number.
    "#;
    check(CODE);
}

#[test]
fn divide_num_nonnum() {
    const CODE: &str = r#"1 / "1"; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn less_nonnum_num() {
    const CODE: &str = r#""1" < 1; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn not() {
    const CODE: &str = r#"print !true;     // expect: false
print !false;    // expect: true
print !!true;    // expect: true

print !123;      // expect: false
print !0;        // expect: false

print !nil;     // expect: true

print !"";       // expect: false

fun foo() {}
print !foo;      // expect: false
    "#;
    check(CODE);
}

#[test]
fn add() {
    const CODE: &str = r#"print 123 + 456; // expect: 579
print "str" + "ing"; // expect: string
    "#;
    check(CODE);
}

#[test]
fn divide() {
    const CODE: &str = "print 8 / 2;         // expect: 4
print 12.34 / 12.34;  // expect: 1
    ";
    check(CODE);
}

#[test]
fn greater_or_equal_nonnum_num() {
    const CODE: &str = r#""1" >= 1; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn multiply() {
    const CODE: &str = "print 5 * 3; // expect: 15
print 12.34 * 0.3; // expect: 3.702
    ";
    check(CODE);
}

#[test]
fn subtract() {
    const CODE: &str = "print 4 - 3; // expect: 1
print 1.2 - 1.2; // expect: 0
    ";
    check(CODE);
}

#[test]
fn greater_num_nonnum() {
    const CODE: &str = r#"1 > "1"; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn add_nil_nil() {
    const CODE: &str =
        "nil + nil; // expect runtime error: Operands must be two numbers or two strings.
    ";
    check(CODE);
}

#[test]
fn not_equals() {
    const CODE: &str = r#"print nil != nil; // expect: false

print true != true; // expect: false
print true != false; // expect: true

print 1 != 1; // expect: false
print 1 != 2; // expect: true

print "str" != "str"; // expect: false
print "str" != "ing"; // expect: true

print nil != false; // expect: true
print false != 0; // expect: true
print 0 != "0"; // expect: true
    "#;
    check(CODE);
}

#[test]
fn negate() {
    const CODE: &str = "print -(3); // expect: -3
print --(3); // expect: 3
print ---(3); // expect: -3
    ";
    check(CODE);
}

#[test]
fn add_bool_num() {
    const CODE: &str =
        "true + 123; // expect runtime error: Operands must be two numbers or two strings.
    ";
    check(CODE);
}

#[test]
fn add_num_nil() {
    const CODE: &str =
        "1 + nil; // expect runtime error: Operands must be two numbers or two strings.";
    check(CODE);
}

#[test]
fn less_or_equal_num_nonnum() {
    const CODE: &str = r#"1 <= "1"; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn greater_or_equal_num_nonnum() {
    const CODE: &str = r#"1 >= "1"; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn less_or_equal_nonnum_num() {
    const CODE: &str = r#""1" <= 1; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn greater_nonnum_num() {
    const CODE: &str = r#""1" > 1; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn multiply_nonnum_num() {
    const CODE: &str = r#""1" * 1; // expect runtime error: Operands must be numbers.
    "#;
    check(CODE);
}

#[test]
fn add_bool_string() {
    const CODE: &str = r#"true + "s"; // expect runtime error: Operands must be two numbers or two strings.
    "#;
    check(CODE);
}

#[test]
fn add_string_nil() {
    const CODE: &str = r#""s" + nil; // expect runtime error: Operands must be two numbers or two strings.
    "#;
    check(CODE);
}
