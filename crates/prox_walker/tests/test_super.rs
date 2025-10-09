mod common;

use common::check;

#[test]
fn super_without_dot() {
    const CODE: &str = "class A {}

class B < A {
  method() {
    // [line 6] Error at ';': Expect '.' after 'super'.
    super;
  }
}
    ";
    check(CODE);
}

#[test]
fn constructor() {
    const CODE: &str = r#"class Base {
  init(a, b) {
    print "Base.init(" + a + ", " + b + ")";
  }
}

class Derived < Base {
  init() {
    print "Derived.init()";
    super.init("a", "b");
  }
}

Derived();
// expect: Derived.init()
// expect: Base.init(a, b)
    "#;
    check(CODE);
}

#[test]
fn call_same_method() {
    const CODE: &str = r#"class Base {
  foo() {
    print "Base.foo()";
  }
}

class Derived < Base {
  foo() {
    print "Derived.foo()";
    super.foo();
  }
}

Derived().foo();
// expect: Derived.foo()
// expect: Base.foo()
    "#;
    check(CODE);
}

#[test]
fn no_superclass_method() {
    const CODE: &str = "class Base {}

class Derived < Base {
  foo() {
    super.doesNotExist(1); // expect runtime error: Undefined property 'doesNotExist'.
  }
}

Derived().foo();
    ";
    check(CODE);
}

#[test]
fn closure() {
    const CODE: &str = r#"class Base {
  toString() { return "Base"; }
}

class Derived < Base {
  getClosure() {
    fun closure() {
      return super.toString();
    }
    return closure;
  }

  toString() { return "Derived"; }
}

var closure = Derived().getClosure();
print closure(); // expect: Base
    "#;
    check(CODE);
}

#[test]
fn indirectly_inherited() {
    const CODE: &str = r#"class A {
  foo() {
    print "A.foo()";
  }
}

class B < A {}

class C < B {
  foo() {
    print "C.foo()";
    super.foo();
  }
}

C().foo();
// expect: C.foo()
// expect: A.foo()
    "#;
    check(CODE);
}

#[test]
fn super_at_top_level() {
    const CODE: &str = r#"super.foo("bar"); // Error at 'super': Can't use 'super' outside of a class.
super.foo; // Error at 'super': Can't use 'super' outside of a class.
    "#;
    check(CODE);
}

#[test]
fn missing_arguments() {
    const CODE: &str = r#"class Base {
  foo(a, b) {
    print "Base.foo(" + a + ", " + b + ")";
  }
}

class Derived < Base {
  foo() {
    super.foo(1); // expect runtime error: Expected 2 arguments but got 1.
  }
}

Derived().foo();
    "#;
    check(CODE);
}

#[test]
fn super_in_top_level_function() {
    const CODE: &str = "  super.bar(); // Error at 'super': Can't use 'super' outside of a class.
fun foo() {
}
    ";
    check(CODE);
}

#[test]
fn super_without_name() {
    const CODE: &str = "class A {}

class B < A {
  method() {
    super.; // Error at ';': Expect superclass method name.
  }
}
    ";
    check(CODE);
}

#[test]
fn reassign_superclass() {
    const CODE: &str = r#"class Base {
  method() {
    print "Base.method()";
  }
}

class Derived < Base {
  method() {
    super.method();
  }
}

class OtherBase {
  method() {
    print "OtherBase.method()";
  }
}

var derived = Derived();
derived.method(); // expect: Base.method()
Base = OtherBase;
derived.method(); // expect: Base.method()
    "#;
    check(CODE);
}

#[test]
fn call_other_method() {
    const CODE: &str = r#"class Base {
  foo() {
    print "Base.foo()";
  }
}

class Derived < Base {
  bar() {
    print "Derived.bar()";
    super.foo();
  }
}

Derived().bar();
// expect: Derived.bar()
// expect: Base.foo()
    "#;
    check(CODE);
}

#[test]
fn parenthesized() {
    const CODE: &str = "class A {
  method() {}
}

class B < A {
  method() {
    // [line 8] Error at ')': Expect '.' after 'super'.
    (super).method();
  }
}
    ";
    check(CODE);
}

#[test]
fn super_in_inherited_method() {
    const CODE: &str = r#"class A {
  say() {
    print "A";
  }
}

class B < A {
  test() {
    super.say();
  }

  say() {
    print "B";
  }
}

class C < B {
  say() {
    print "C";
  }
}

C().test(); // expect: A
    "#;
    check(CODE);
}

#[test]
fn bound_method() {
    const CODE: &str = r#"class A {
  method(arg) {
    print "A.method(" + arg + ")";
  }
}

class B < A {
  getClosure() {
    return super.method;
  }

  method(arg) {
    print "B.method(" + arg + ")";
  }
}


var closure = B().getClosure();
closure("arg"); // expect: A.method(arg)
    "#;
    check(CODE);
}

#[test]
fn this_in_superclass_method() {
    const CODE: &str = r#"class Base {
  init(a) {
    this.a = a;
  }
}

class Derived < Base {
  init(a, b) {
    super.init(a);
    this.b = b;
  }
}

var derived = Derived("a", "b");
print derived.a; // expect: a
print derived.b; // expect: b
    "#;
    check(CODE);
}

#[test]
fn no_superclass_call() {
    const CODE: &str = "class Base {
  foo() {
    super.doesNotExist(1); // Error at 'super': Can't use 'super' in a class with no superclass.
  }
}

Base().foo();
    ";
    check(CODE);
}

#[test]
fn super_in_closure_in_inherited_method() {
    const CODE: &str = r#"class A {
  say() {
    print "A";
  }
}

class B < A {
  getClosure() {
    fun closure() {
      super.say();
    }
    return closure;
  }

  say() {
    print "B";
  }
}

class C < B {
  say() {
    print "C";
  }
}

C().getClosure()(); // expect: A
    "#;
    check(CODE);
}

#[test]
fn no_superclass_bind() {
    const CODE: &str = "class Base {
  foo() {
    super.doesNotExist; // Error at 'super': Can't use 'super' in a class with no superclass.
  }
}

Base().foo();
    ";
    check(CODE);
}

#[test]
fn extra_arguments() {
    const CODE: &str = r#"class Base {
  foo(a, b) {
    print "Base.foo(" + a + ", " + b + ")";
  }
}

class Derived < Base {
  foo() {
    print "Derived.foo()"; // expect: Derived.foo()
    super.foo("a", "b", "c", "d"); // expect runtime error: Expected 2 arguments but got 4.
  }
}

Derived().foo();
    "#;
    check(CODE);
}
