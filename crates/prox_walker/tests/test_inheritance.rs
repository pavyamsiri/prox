mod common;

use common::check;

#[test]
fn inherit_from_number() {
    const CODE: &str = "var Number = 123;
class Foo < Number {} // expect runtime error: Superclass must be a class.
    ";
    check(CODE);
}

#[test]
fn constructor() {
    const CODE: &str = r#"class A {
  init(param) {
    this.field = param;
  }

  test() {
    print this.field;
  }
}

class B < A {}

var b = B("value");
b.test(); // expect: value
    "#;
    check(CODE);
}

#[test]
fn inherit_from_function() {
    const CODE: &str = "fun foo() {}

class Subclass < foo {} // expect runtime error: Superclass must be a class.
    ";
    check(CODE);
}

#[test]
fn parenthesized_superclass() {
    const CODE: &str = "class Foo {}

// [line 4] Error at '(Foo)': Expect superclass name.
class Bar < (Foo) {}
    ";
    check(CODE);
}

#[test]
fn inherit_from_nil() {
    const CODE: &str = "var Nil = nil;
class Foo < Nil {} // expect runtime error: Superclass must be a class.
    ";
    check(CODE);
}

#[test]
fn set_fields_from_base_class() {
    const CODE: &str = r#"class Foo {
  foo(a, b) {
    this.field1 = a;
    this.field2 = b;
  }

  fooPrint() {
    print this.field1;
    print this.field2;
  }
}

class Bar < Foo {
  bar(a, b) {
    this.field1 = a;
    this.field2 = b;
  }

  barPrint() {
    print this.field1;
    print this.field2;
  }
}

var bar = Bar();
bar.foo("foo 1", "foo 2");
bar.fooPrint();
// expect: foo 1
// expect: foo 2

bar.bar("bar 1", "bar 2");
bar.barPrint();
// expect: bar 1
// expect: bar 2

bar.fooPrint();
// expect: bar 1
// expect: bar 2
    "#;
    check(CODE);
}

#[test]
fn inherit_methods() {
    const CODE: &str = r#"class Foo {
  methodOnFoo() { print "foo"; }
  override() { print "foo"; }
}

class Bar < Foo {
  methodOnBar() { print "bar"; }
  override() { print "bar"; }
}

var bar = Bar();
bar.methodOnFoo(); // expect: foo
bar.methodOnBar(); // expect: bar
bar.override(); // expect: bar
    "#;
    check(CODE);
}
