mod util;
use predicates::prelude::predicate;

#[test]
fn simple_return() {
    util::get_bin()
        .arg("tests/programs/return/return.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("20"));
}
