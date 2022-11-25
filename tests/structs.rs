mod util;
use predicates::prelude::predicate;

#[test]
fn simple_struct() {
    util::get_bin()
        .arg("tests/programs/structs/simple_struct.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("8"));
}

#[test] #[ignore]
fn nested_structs() {
    util::get_bin()
        .arg("tests/programs/structs/nested_structs.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("42"));
}
