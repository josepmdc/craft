mod util;
use predicates::prelude::predicate;

#[test]
fn while_increment() {
    util::get_bin()
        .arg("tests/programs/while/while.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("100"));
}

#[test]
fn fib_iter() {
    util::get_bin()
        .arg("tests/programs/while/fib_iter.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("6765"));
}
