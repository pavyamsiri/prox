mod common;

use common::check;

#[test]
fn local_inherit_self() {
    const CODE: &str = "{
  class Foo < Foo {} // Error at 'Foo': A class can't inherit from itself.
}
// [c line 5] Error at end: Expect '}' after block.
    ";
    check(CODE);
}

#[test]
fn inherited_method() {
    const CODE: &str = r#"class Foo {
  inFoo() {
    print "in foo";
  }
}

class Bar < Foo {
  inBar() {
    print "in bar";
  }
}

class Baz < Bar {
  inBaz() {
    print "in baz";
  }
}

var baz = Baz();
baz.inFoo(); // expect: in foo
baz.inBar(); // expect: in bar
baz.inBaz(); // expect: in baz
    "#;
    check(CODE);
}

#[test]
fn inherit_self() {
    const CODE: &str = "class Foo < Foo {} // Error at 'Foo': A class can't inherit from itself.
    ";
    check(CODE);
}

#[test]
fn local_reference_self() {
    const CODE: &str = "{
  class Foo {
    returnSelf() {
      return Foo;
    }
  }

  print Foo().returnSelf(); // expect: Foo
}
    ";
    check(CODE);
}

#[test]
fn local_inherit_other() {
    const CODE: &str = "class A {}

fun f() {
  class B < A {}
  return B;
}

print f(); // expect: B
    ";
    check(CODE);
}

#[test]
fn empty() {
    const CODE: &str = "class Foo {}

print Foo; // expect: Foo
    ";
    check(CODE);
}

#[test]
fn reference_self() {
    const CODE: &str = "class Foo {
  returnSelf() {
    return Foo;
  }
}

print Foo().returnSelf(); // expect: Foo
    ";
    check(CODE);
}
