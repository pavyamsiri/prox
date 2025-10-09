mod common;

use common::check;

#[test]
fn bool() {
    const CODE: &str = "
true(); // expect runtime error: Can only call functions and classes.
     ";
    check(CODE);
}

#[test]
fn nil() {
    const CODE: &str = "
nil(); // expect runtime error: Can only call functions and classes.
     ";
    check(CODE);
}
#[test]
fn num() {
    const CODE: &str = "
123(); // expect runtime error: Can only call functions and classes.
     ";
    check(CODE);
}
#[test]
fn object() {
    const CODE: &str = "class Foo {}

var foo = Foo();
foo(); // expect runtime error: Can only call functions and classes.
     ";
    check(CODE);
}
#[test]
fn string() {
    const CODE: &str = r#"
"str"(); // expect runtime error: Can only call functions and classes.
     "#;
    check(CODE);
}
