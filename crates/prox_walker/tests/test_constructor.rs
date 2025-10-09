mod common;

use common::check;

#[test]
fn return_in_nested_function() {
    const CODE: &str = r#"class Foo {
  init() {
    fun init() {
      return "bar";
    }
    print init(); // expect: bar
  }
}

print Foo(); // expect: <object Foo>
    "#;
    check(CODE);
}

#[test]
fn arguments() {
    const CODE: &str = r#"class Foo {
  init(a, b) {
    print "init"; // expect: init
    this.a = a;
    this.b = b;
  }
}

var foo = Foo(1, 2);
print foo.a; // expect: 1
print foo.b; // expect: 2
    "#;
    check(CODE);
}

#[test]
fn early_return() {
    const CODE: &str = r#"class Foo {
  init() {
    print "init";
    return;
    print "nope";
  }
}

var foo = Foo(); // expect: init
print foo; // expect: <object Foo>
    "#;
    check(CODE);
}

#[test]
fn default() {
    const CODE: &str = "class Foo {}

var foo = Foo();
print foo; // expect: <object Foo>
    ";
    check(CODE);
}

#[test]
fn missing_arguments() {
    const CODE: &str = "class Foo {
  init(a, b) {}
}

var foo = Foo(1); // expect runtime error: Expected 2 arguments but got 1.
    ";
    check(CODE);
}

#[test]
fn call_init_explicitly() {
    const CODE: &str = r#"class Foo {
  init(arg) {
    print "Foo.init(" + arg + ")";
    this.field = "init";
  }
}

var foo = Foo("one"); // expect: Foo.init(one)
foo.field = "field";

var foo2 = foo.init("two"); // expect: Foo.init(two)
print foo2; // expect: <object Foo>

// Make sure init() doesn't create a fresh instance.
print foo.field; // expect: init
    "#;
    check(CODE);
}

#[test]
fn call_init_early_return() {
    const CODE: &str = r#"class Foo {
  init() {
    print "init";
    return;
    print "nope";
  }
}

var foo = Foo(); // expect: init
print foo.init(); // expect: init
// expect: <object Foo>
    "#;
    check(CODE);
}

#[test]
fn return_value() {
    const CODE: &str = r#"class Foo {
  init() {
    return "result"; // Error at 'return': Can't return a value from an initializer.
  }
}
    "#;
    check(CODE);
}

#[test]
fn default_arguments() {
    const CODE: &str = "class Foo {}

var foo = Foo(1, 2, 3); // expect runtime error: Expected 0 arguments but got 3.
    ";
    check(CODE);
}

#[test]
fn init_not_method() {
    const CODE: &str = r#"class Foo {
  init(arg) {
    print "Foo.init(" + arg + ")";
    this.field = "init";
  }
}

fun init() {
  print "not initializer";
}

init(); // expect: not initializer
    "#;
    check(CODE);
}

#[test]
fn extra_arguments() {
    const CODE: &str = "class Foo {
  init(a, b) {
    this.a = a;
    this.b = b;
  }
}

var foo = Foo(1, 2, 3, 4); // expect runtime error: Expected 2 arguments but got 4.
    ";
    check(CODE);
}
