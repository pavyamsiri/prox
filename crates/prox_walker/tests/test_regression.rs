mod common;

use common::check;

#[test]
fn issue_40() {
    const CODE: &str = r#"fun caller(g) {
  g();
  // g should be a function, not nil.
  print g == nil; // expect: false
}

fun callCaller() {
  var capturedVar = "before";
  var a = "a";

  fun f() {
    // Commenting the next line out prevents the bug!
    capturedVar = "after";

    // Returning anything also fixes it, even nil:
    //return nil;
  }

  caller(f);
}

callCaller();
    "#;
    check(CODE);
}

#[test]
fn issue_394() {
    const CODE: &str = "{
  class A {}
  class B < A {}
  print B; // expect: B
}
    ";
    check(CODE);
}
