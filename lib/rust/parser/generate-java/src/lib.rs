//! Supports generation of Java types corresponding to `enso-parser`'s AST types, and testing and
//! debugging the translation process.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use enso_metamodel::meta;
use enso_reflect::Reflect;


// ==============
// === Export ===
// ==============

pub mod serialization;



// =====================
// === Configuration ===
// =====================

/// The package for the generated code.
pub const PACKAGE: &str = "org.enso.syntax2";
/// The package for the non-generated serialization support code.
pub const SERIALIZATION_SUPPORT: &str = "org.enso.syntax2";
/// The fully-qualified name of an `Either` type.
pub const EITHER_TYPE: &str = "org.enso.syntax2.Either";



// ==================
// === Test Cases ===
// ==================

use enso_parser::syntax;

/// Generate accept/reject test case set for the parser types rooted at `syntax::Tree`.
pub fn generate_testcases() -> meta::serialization::TestCases {
    let root = syntax::Tree::reflect();
    let root_id = root.id;
    let (graph, rust_to_meta) = enso_metamodel::rust::to_meta(root);
    let root = rust_to_meta[&root_id];
    meta::serialization::testcases(&graph, root)
}



// ===========================
// === Rust Format Testing ===
// ===========================

#[cfg(test)]
mod test {
    /// Check Rust deserialization against test cases.
    #[test]
    fn test_format() {
        let cases = super::generate_testcases();
        for (i, case) in cases.accept.iter().enumerate() {
            if let Err(err) = enso_parser::serialization::deserialize_tree(case) {
                panic!("accept{i} fail: {err:?}");
            }
        }
        for (i, case) in cases.reject.iter().enumerate() {
            if let Ok(tree) = enso_parser::serialization::deserialize_tree(case) {
                panic!("reject{i} fail: accepted: {tree:?}");
            }
        }
    }
}
