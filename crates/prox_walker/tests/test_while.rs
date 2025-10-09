mod common;

use common::check;

#[test]
fn class_in_body() {
    const CODE: &str = "// [line 2] Error at 'class': Expect expression.
while (true) class Foo {}
    ";
    check(CODE);
}

#[test]
fn return_closure() {
    const CODE: &str = r#"fun f() {
  while (true) {
    var i = "i";
    fun g() { print i; }
    return g;
  }
}

var h = f();
h(); // expect: i
    "#;
    check(CODE);
}

#[test]
fn closure_in_body() {
    const CODE: &str = "var f1;
var f2;
var f3;

var i = 1;
while (i < 4) {
  var j = i;
  fun f() { print j; }

  if (j == 1) f1 = f;
  else if (j == 2) f2 = f;
  else f3 = f;

  i = i + 1;
}

f1(); // expect: 1
f2(); // expect: 2
f3(); // expect: 3
    ";
    check(CODE);
}

#[test]
fn fun_in_body() {
    const CODE: &str = "// [line 2] Error at 'fun': Expect expression.
while (true) fun foo() {}
    ";
    check(CODE);
}

#[test]
fn syntax() {
    const CODE: &str = "// Single-expression body.
var c = 0;
while (c < 3) print c = c + 1;
// expect: 1
// expect: 2
// expect: 3

// Block body.
var a = 0;
while (a < 3) {
  print a;
  a = a + 1;
}
// expect: 0
// expect: 1
// expect: 2

// Statement bodies.
while (false) if (true) 1; else 2;
while (false) while (true) 1;
while (false) for (;;) 1;
    ";
    check(CODE);
}

#[test]
fn var_in_body() {
    const CODE: &str = "// [line 2] Error at 'var': Expect expression.
while (true) var foo;
    ";
    check(CODE);
}

#[test]
fn return_inside() {
    const CODE: &str = r#"fun f() {
  while (true) {
    var i = "i";
    return i;
  }
}

print f();
// expect: i
    "#;
    check(CODE);
}
