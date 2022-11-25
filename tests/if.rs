mod util;
use predicates::prelude::predicate;

#[test]
fn if_expr() {
    util::get_bin()
        .arg("tests/programs/if/if_expr.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("30"));
}

#[test]
fn fib_rec() {
    util::get_bin()
        .arg("tests/programs/if/fib_rec.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("6765"));
}
