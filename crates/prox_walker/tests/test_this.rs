mod common;

use common::check;

#[test]
fn this_in_method() {
    const CODE: &str = r#"class Foo {
  bar() { return this; }
  baz() { return "baz"; }
}

print Foo().bar().baz(); // expect: baz
    "#;
    check(CODE);
}

#[test]
fn closure() {
    const CODE: &str = r#"class Foo {
  getClosure() {
    fun closure() {
      return this.toString();
    }
    return closure;
  }

  toString() { return "Foo"; }
}

var closure = Foo().getClosure();
print closure(); // expect: Foo
    "#;
    check(CODE);
}

#[test]
fn nested_class() {
    const CODE: &str = "class Outer {
  method() {
    print this; // expect: <object Outer>

    fun f() {
      print this; // expect: <object Outer>

      class Inner {
        method() {
          print this; // expect: <object Inner>
        }
      }

      Inner().method();
    }
    f();
  }
}

Outer().method();
    ";
    check(CODE);
}

#[test]
fn this_at_top_level() {
    const CODE: &str = "this; // Error at 'this': Can't use 'this' outside of a class.
    ";
    check(CODE);
}

#[test]
fn this_in_top_level_function() {
    const CODE: &str = "fun foo() {
  this; // Error at 'this': Can't use 'this' outside of a class.
}
    ";
    check(CODE);
}

#[test]
fn nested_closure() {
    const CODE: &str = r#"class Foo {
  getClosure() {
    fun f() {
      fun g() {
        fun h() {
          return this.toString();
        }
        return h;
      }
      return g;
    }
    return f;
  }

  toString() { return "Foo"; }
}

var closure = Foo().getClosure();
print closure()()(); // expect: Foo
    "#;
    check(CODE);
}
