mod common;

use common::check;

#[test]
fn missing_argument() {
    const CODE: &str = "// [line 2] Error at ';': Expect expression.
print;";
    check(CODE);
}
