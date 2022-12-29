mod util;

use predicates::prelude::predicate;

#[test]
fn simple_array() {
    util::get_bin()
        .arg("tests/programs/arrays/simple_array.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("[1, 2, 3]\n[1, 2, 3]"));
}

#[test]
fn array_out_of_bounds() {
    util::get_bin()
        .arg("tests/programs/arrays/out_of_bounds.cft")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Index out of bounds"));
}
