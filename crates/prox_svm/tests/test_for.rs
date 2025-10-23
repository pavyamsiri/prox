mod common;

use common::check;

#[test]
fn class_in_body() {
    const CODE: &str = "// [line 2] Error at 'class': Expect expression.
for (;;) class Foo {}
    ";
    check(CODE);
}

#[test]
fn return_closure() {
    const CODE: &str = r#"fun f() {
  for (;;) {
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
fn statement_condition() {
    const CODE: &str = "// [line 2] Error at '{': Expect expression.
for (var a = 1; {}; a = a + 1) {}
    ";
    check(CODE);
}

#[test]
fn closure_in_body() {
    const CODE: &str = "var f1;
var f2;
var f3;

for (var i = 1; i < 4; i = i + 1) {
  var j = i;
  fun f() {
    print i;
    print j;
  }

  if (j == 1) f1 = f;
  else if (j == 2) f2 = f;
  else f3 = f;
}

f1(); // expect: 4
      // expect: 1
f2(); // expect: 4
      // expect: 2
f3(); // expect: 4
      // expect: 3
    ";
    check(CODE);
}

#[test]
fn fun_in_body() {
    const CODE: &str = "// [line 2] Error at 'fun': Expect expression.
for (;;) fun foo() {}
    ";
    check(CODE);
}

#[test]
fn syntax() {
    const CODE: &str = r#"// Single-expression body.
for (var c = 0; c < 3;) print c = c + 1;
// expect: 1
// expect: 2
// expect: 3

// Block body.
for (var a = 0; a < 3; a = a + 1) {
  print a;
}
// expect: 0
// expect: 1
// expect: 2

// No clauses.
fun foo() {
  for (;;) return "done";
}
print foo(); // expect: done

// No variable.
var i = 0;
for (; i < 2; i = i + 1) print i;
// expect: 0
// expect: 1

// No condition.
fun bar() {
  for (var i = 0;; i = i + 1) {
    print i;
    if (i >= 2) return;
  }
}
bar();
// expect: 0
// expect: 1
// expect: 2

// No increment.
for (var i = 0; i < 2;) {
  print i;
  i = i + 1;
}
// expect: 0
// expect: 1

// Statement bodies.
for (; false;) if (true) 1; else 2;
for (; false;) while (true) 1;
for (; false;) for (;;) 1;
    "#;
    check(CODE);
}

#[test]
fn var_in_body() {
    const CODE: &str = "// [line 2] Error at 'var': Expect expression.
for (;;) var foo;
    ";
    check(CODE);
}

#[test]
fn scope() {
    const CODE: &str = r#"{
  var i = "before";

  // New variable is in inner scope.
  for (var i = 0; i < 1; i = i + 1) {
    print i; // expect: 0

    // Loop body is in second inner scope.
    var i = -1;
    print i; // expect: -1
  }
}

{
  // New variable shadows outer variable.
  for (var i = 0; i > 0; i = i + 1) {}

  // Goes out of scope after loop.
  var i = "after";
  print i; // expect: after

  // Can reuse an existing variable.
  for (i = 0; i < 1; i = i + 1) {
    print i; // expect: 0
  }
}
    "#;
    check(CODE);
}

#[test]
fn return_inside() {
    const CODE: &str = r#"fun f() {
  for (;;) {
    var i = "i";
    return i;
  }
}

print f();
// expect: i
    "#;
    check(CODE);
}

#[test]
fn statement_initializer() {
    const CODE: &str = "// [line 2] Error at '{': Expect expression.
for ({}; a < 2; a = a + 1) {}
    ";
    check(CODE);
}

#[test]
fn statement_increment() {
    const CODE: &str = "// [line 2] Error at '{': Expect expression.
for (var a = 1; a < 2; {}) {}
    ";
    check(CODE);
}
