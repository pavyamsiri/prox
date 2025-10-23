mod common;

use common::check;

#[test]
fn shadow_global() {
    const CODE: &str = r#"var a = "global";
{
  var a = "shadow";
  print a; // expect: shadow
}
print a; // expect: global
    "#;
    check(CODE);
}

#[test]
fn undefined_global() {
    const CODE: &str =
        "print notDefined;  // expect runtime error: Undefined variable 'notDefined'.
    ";
    check(CODE);
}

#[test]
fn redeclare_global() {
    const CODE: &str = r#"var a = "1";
var a;
print a; // expect: nil
    "#;
    check(CODE);
}

#[test]
fn use_global_in_initializer() {
    const CODE: &str = r#"var a = "value";
var a = a;
print a; // expect: value
    "#;
    check(CODE);
}

#[test]
fn duplicate_parameter() {
    const CODE: &str = r#"fun foo(arg,
        arg) { // Error at 'arg': Already a variable with this name in this scope.
  "body";
}
    "#;
    check(CODE);
}

#[test]
fn local_from_method() {
    const CODE: &str = r#"var foo = "variable";

class Foo {
  method() {
    print foo;
  }
}

Foo().method(); // expect: variable
    "#;
    check(CODE);
}

#[test]
fn unreached_undefined() {
    const CODE: &str = r#"if (false) {
  print notDefined;
}

print "ok"; // expect: ok
    "#;
    check(CODE);
}

#[test]
fn use_local_in_initializer() {
    const CODE: &str = r#"var a = "outer";
{
  var a = a; // Error at 'a': Can't read local variable in its own initializer.
}
    "#;
    check(CODE);
}

#[test]
fn in_nested_block() {
    const CODE: &str = r#"{
  var a = "outer";
  {
    print a; // expect: outer
  }
}
    "#;
    check(CODE);
}

#[test]
fn scope_reuse_in_different_blocks() {
    const CODE: &str = r#"{
  var a = "first";
  print a; // expect: first
}

{
  var a = "second";
  print a; // expect: second
}
    "#;
    check(CODE);
}

#[test]
fn redefine_global() {
    const CODE: &str = r#"var a = "1";
var a = "2";
print a; // expect: 2
    "#;
    check(CODE);
}

#[test]
fn use_this_as_var() {
    const CODE: &str = r#"// [line 2] Error at 'this': Expect variable name.
var this = "value";
    "#;
    check(CODE);
}

#[test]
fn shadow_local() {
    const CODE: &str = r#"{
  var a = "local";
  {
    var a = "shadow";
    print a; // expect: shadow
  }
  print a; // expect: local
}
    "#;
    check(CODE);
}

#[test]
fn in_middle_of_block() {
    const CODE: &str = r#"{
  var a = "a";
  print a; // expect: a
  var b = a + " b";
  print b; // expect: a b
  var c = a + " c";
  print c; // expect: a c
  var d = b + " d";
  print d; // expect: a b d
}
    "#;
    check(CODE);
}

#[test]
fn use_nil_as_var() {
    const CODE: &str = r#"// [line 2] Error at 'nil': Expect variable name.
var nil = "value";
    "#;
    check(CODE);
}

#[test]
fn early_bound() {
    const CODE: &str = r#"var a = "outer";
{
  fun foo() {
    print a;
  }

  foo(); // expect: outer
  var a = "inner";
  foo(); // expect: outer
}
    "#;
    check(CODE);
}

#[test]
fn use_false_as_var() {
    const CODE: &str = r#"// [line 2] Error at 'false': Expect variable name.
var false = "value";
    "#;
    check(CODE);
}

#[test]
fn duplicate_local() {
    const CODE: &str = r#"{
  var a = "value";
  var a = "other"; // Error at 'a': Already a variable with this name in this scope.
}
    "#;
    check(CODE);
}

#[test]
fn undefined_local() {
    const CODE: &str = "{
  print notDefined;  // expect runtime error: Undefined variable 'notDefined'.
}
    ";
    check(CODE);
}

#[test]
fn collide_with_parameter() {
    const CODE: &str = "fun foo(a) {
  var a; // Error at 'a': Already a variable with this name in this scope.
}
    ";
    check(CODE);
}

#[test]
fn shadow_and_local() {
    const CODE: &str = r#"{
  var a = "outer";
  {
    print a; // expect: outer
    var a = "inner";
    print a; // expect: inner
  }
}
    "#;
    check(CODE);
}

#[test]
fn uninitialized() {
    const CODE: &str = "var a;
print a; // expect: nil
    ";
    check(CODE);
}
