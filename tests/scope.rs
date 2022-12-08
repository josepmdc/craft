mod util;
use predicates::prelude::predicate;

#[test]
fn variable_out_of_scope() {
    util::get_bin()
        .arg("tests/programs/scope/out_of_scope.cft")
        .assert()
        .failure()
        .stderr(predicate::str::contains("out of scope"));
}

#[test]
fn variable_in_scope() {
    util::get_bin()
        .arg("tests/programs/scope/in_scope.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("15"));
}
