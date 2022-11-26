mod util;
use predicates::prelude::predicate;

#[test]
fn printf_string() {
    util::get_bin()
        .arg("tests/programs/printf/print_string.cft")
        .assert()
        .success()
        .stdout(predicate::str::contains("asdf -> xd"));
}
