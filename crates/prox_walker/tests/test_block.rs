mod common;

use common::check;

#[test]
fn empty() {
    const CODE: &str = r#"{} // By itself.
// In a statement.
if (true) {}
if (false) {} else {}

print "ok"; // expect: ok
    "#;
    check(CODE);
}

#[test]
fn scope() {
    const CODE: &str = r#"var a = "outer";
{
  var a = "inner";
  print a; // expect: inner
}

print a; // expect: outer
    "#;
    check(CODE);
}
