mod common;

use common::check;

#[test]
fn associativity() {
    const CODE: &str = r#"var a = "a";
var b = "b";
var c = "c";

// Assignment is right-associative.
a = b = c;
print a; // expect: c
print b; // expect: c
print c; // expect: c
    "#;
    check(CODE);
}

#[test]
fn global() {
    const CODE: &str = r#"var a = "before";
print a; // expect: before

a = "after";
print a; // expect: after

print a = "arg"; // expect: arg
print a; // expect: arg
    "#;
    check(CODE);
}

#[test]
fn grouping() {
    const CODE: &str = r#"var a = "a";
(a) = "value"; // Error at '=': Invalid assignment target.
    "#;
    check(CODE);
}

#[test]
fn prefix_operator() {
    const CODE: &str = r#"var a = "a";
!a = "value"; // Error at '=': Invalid assignment target.
    "#;
    check(CODE);
}

#[test]
fn infix_operator() {
    const CODE: &str = r#"var a = "a";
var b = "b";
a + b = "value"; // Error at '=': Invalid assignment target.
    "#;
    check(CODE);
}

#[test]
fn local() {
    const CODE: &str = r#"{
  var a = "before";
  print a; // expect: before

  a = "after";
  print a; // expect: after

  print a = "arg"; // expect: arg
  print a; // expect: arg
}

    "#;
    check(CODE);
}

#[test]
fn syntax() {
    const CODE: &str = r#"// Assignment on RHS of variable.
var a = "before";
var c = a = "var";
print a; // expect: var
print c; // expect: var
    "#;
    check(CODE);
}

#[test]
fn to_this() {
    const CODE: &str = r#"class Foo {
  Foo() {
    this = "value"; // Error at '=': Invalid assignment target.
  }
}

Foo();
    "#;
    check(CODE);
}

#[test]
fn undefined() {
    const CODE: &str = r#"
unknown = "what"; // expect runtime error: Undefined variable 'unknown'.
    "#;
    check(CODE);
}
