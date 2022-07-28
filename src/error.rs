pub fn report(line: i32, col: i32, message: String) {
    println!("[line {}, col {}] Error: {}", line, col, message);
}
