mod common;

use common::check;

#[test]
fn after_while() {
    const CODE: &str = r#"fun f() {
  while (true) return "ok";
}

print f(); // expect: ok
    "#;
    check(CODE);
}

#[test]
fn in_method() {
    const CODE: &str = r#"class Foo {
  method() {
    return "ok";
    print "bad";
  }
}

print Foo().method(); // expect: ok
    "#;
    check(CODE);
}

#[test]
fn in_function() {
    const CODE: &str = r#"fun f() {
  return "ok";
  print "bad";
}

print f(); // expect: ok
    "#;
    check(CODE);
}

#[test]
fn after_else() {
    const CODE: &str = r#"fun f() {
  if (false) "no"; else return "ok";
}

print f(); // expect: ok
    "#;
    check(CODE);
}

#[test]
fn at_top_level() {
    const CODE: &str = r#"return "wat"; // Error at 'return': Can't return from top-level code.
    "#;
    check(CODE);
}

#[test]
fn after_if() {
    const CODE: &str = r#"fun f() {
  if (true) return "ok";
}

print f(); // expect: ok
    "#;
    check(CODE);
}

#[test]
fn return_nil_if_no_value() {
    const CODE: &str = r#"fun f() {
  return;
  print "bad";
}

print f(); // expect: nil
    "#;
    check(CODE);
}
