mod common;

use common::check;

#[test]
fn shadow_closure_with_local() {
    const CODE: &str = r#"{
  var foo = "closure";
  fun f() {
    {
      print foo; // expect: closure
      var foo = "shadow";
      print foo; // expect: shadow
    }
    print foo; // expect: closure
  }
  f();
}
    "#;
    check(CODE);
}

#[test]
fn unused_later_closure() {
    const CODE: &str = r#"// This is a regression test. When closing upvalues for discarded locals, it
// wouldn't make sure it discarded the upvalue for the correct stack slot.
//
// Here we create two locals that can be closed over, but only the first one
// actually is. When "b" goes out of scope, we need to make sure we don't
// prematurely close "a".
var closure;

{
  var a = "a";

  {
    var b = "b";
    fun returnA() {
      return a;
    }

    closure = returnA;

    if (false) {
      fun returnB() {
        return b;
      }
    }
  }

  print closure(); // expect: a
}
    "#;
    check(CODE);
}

#[test]
fn assign_to_closure() {
    const CODE: &str = r#"var f;
var g;

{
  var local = "local";
  fun f_() {
    print local;
    local = "after f";
    print local;
  }
  f = f_;

  fun g_() {
    print local;
    local = "after g";
    print local;
  }
  g = g_;
}

f();
// expect: local
// expect: after f

g();
// expect: after f
// expect: after g
    "#;
    check(CODE);
}

#[test]
fn close_over_function_parameter() {
    const CODE: &str = r#"var f;

fun foo(param) {
  fun f_() {
    print param;
  }
  f = f_;
}
foo("param");

f(); // expect: param
    "#;
    check(CODE);
}

#[test]
fn unused_closure() {
    const CODE: &str = r#"// This is a regression test. There was a bug where the VM would try to close
// an upvalue even if the upvalue was never created because the codepath for
// the closure was not executed.

{
  var a = "a";
  if (false) {
    fun foo() { a; }
  }
}

// If we get here, we didn't segfault when a went out of scope.
print "ok"; // expect: ok
    "#;
    check(CODE);
}

#[test]
fn reference_closure_multiple_times() {
    const CODE: &str = r#"var f;

{
  var a = "a";
  fun f_() {
    print a;
    print a;
  }
  f = f_;
}

f();
// expect: a
// expect: a
    "#;
    check(CODE);
}

#[test]
fn reuse_closure_slot() {
    const CODE: &str = r#"{
  var f;

  {
    var a = "a";
    fun f_() { print a; }
    f = f_;
  }

  {
    // Since a is out of scope, the local slot will be reused by b. Make sure
    // that f still closes over a.
    var b = "b";
    f(); // expect: a
  }
}
    "#;
    check(CODE);
}

#[test]
fn close_over_later_variable() {
    const CODE: &str = r#"// This is a regression test. There was a bug where if an upvalue for an
// earlier local (here "a") was captured *after* a later one ("b"), then it
// would crash because it walked to the end of the upvalue list (correct), but
// then didn't handle not finding the variable.

fun f() {
  var a = "a";
  var b = "b";
  fun g() {
    print b; // expect: b
    print a; // expect: a
  }
  g();
}
f();
    "#;
    check(CODE);
}

#[test]
fn open_closure_in_function() {
    const CODE: &str = r#"{
  var local = "local";
  fun f() {
    print local; // expect: local
  }
  f();
}
    "#;
    check(CODE);
}

#[test]
fn close_over_method_parameter() {
    const CODE: &str = r#"var f;

class Foo {
  method(param) {
    fun f_() {
      print param;
    }
    f = f_;
  }
}

Foo().method("param");
f(); // expect: param
    "#;
    check(CODE);
}

#[test]
fn assign_to_shadowed_later() {
    const CODE: &str = r#"var a = "global";

{
  fun assign() {
    a = "assigned";
  }

  var a = "inner";
  assign();
  print a; // expect: inner
}

print a; // expect: assigned
    "#;
    check(CODE);
}

#[test]
fn nested_closure() {
    const CODE: &str = r#"var f;

fun f1() {
  var a = "a";
  fun f2() {
    var b = "b";
    fun f3() {
      var c = "c";
      fun f4() {
        print a;
        print b;
        print c;
      }
      f = f4;
    }
    f3();
  }
  f2();
}
f1();

f();
// expect: a
// expect: b
// expect: c
    "#;
    check(CODE);
}

#[test]
fn closed_closure_in_function() {
    const CODE: &str = r#"var f;

{
  var local = "local";
  fun f_() {
    print local;
  }
  f = f_;
}

f(); // expect: local
    "#;
    check(CODE);
}
