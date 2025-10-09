mod common;

use common::check;

#[test]
fn var_in_then() {
    const CODE: &str = "// [line 2] Error at 'var': Expect expression.
if (true) var foo;
    ";
    check(CODE);
}

#[test]
fn var_in_else() {
    const CODE: &str = r#"// [line 2] Error at 'var': Expect expression.
if (true) "ok"; else var foo;
    "#;
    check(CODE);
}

#[test]
fn r#else() {
    const CODE: &str = r#"// Evaluate the 'else' expression if the condition is false.
if (true) print "good"; else print "bad"; // expect: good
if (false) print "bad"; else print "good"; // expect: good

// Allow block body.
if (false) nil; else { print "block"; } // expect: block
    "#;
    check(CODE);
}

#[test]
fn truth() {
    const CODE: &str = r#"// False and nil are false.
if (false) print "bad"; else print "false"; // expect: false
if (nil) print "bad"; else print "nil"; // expect: nil

// Everything else is true.
if (true) print true; // expect: true
if (0) print 0; // expect: 0
if ("") print "empty"; // expect: empty
    "#;
    check(CODE);
}

#[test]
fn fun_in_then() {
    const CODE: &str = "// [line 2] Error at 'fun': Expect expression.
if (true) fun foo() {}
    ";
    check(CODE);
}

#[test]
fn dangling_else() {
    const CODE: &str = r#"// A dangling else binds to the right-most if.
if (true) if (false) print "bad"; else print "good"; // expect: good
if (false) if (true) print "bad"; else print "bad";
    "#;
    check(CODE);
}

#[test]
fn class_in_then() {
    const CODE: &str = "// [line 2] Error at 'class': Expect expression.
if (true) class Foo {}
    ";
    check(CODE);
}

#[test]
fn class_in_else() {
    const CODE: &str = r#"// [line 2] Error at 'class': Expect expression.
if (true) "ok"; else class Foo {}
    "#;
    check(CODE);
}

#[test]
fn r#if() {
    const CODE: &str = r#"// Evaluate the 'then' expression if the condition is true.
if (true) print "good"; // expect: good
if (false) print "bad";

// Allow block body.
if (true) { print "block"; } // expect: block

// Assignment in if condition.
var a = false;
if (a = true) print a; // expect: true
    "#;
    check(CODE);
}

#[test]
fn fun_in_else() {
    const CODE: &str = r#"// [line 2] Error at 'fun': Expect expression.
if (true) "ok"; else fun foo() {}
    "#;
    check(CODE);
}
