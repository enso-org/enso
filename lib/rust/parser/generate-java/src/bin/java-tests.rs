//! Generates Java format tests.
//!
//! Usage:
//! ```console
//! java-tests > GeneratedFormatTests.java
//! javac -d generated-java/ GeneratedFormatTests.java && java GeneratedFormatTests
//! ```



// ============================
// === Java Test Generation ===
// ============================

fn main() {
    let cases = enso_parser_generate_java::generate_testcases();
    print!("{}", cases.to_java());
}
