mod common;

use common::check;

#[test]
fn literal() {
    const CODE: &str = "print nil; // expect: nil";
    check(CODE);
}
