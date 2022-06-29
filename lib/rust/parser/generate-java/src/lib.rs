use enso_reflect::abstracted;
use enso_reflect::Reflect;

pub mod serialization;



// =====================
// === Configuration ===
// =====================

pub const PACKAGE: &str = "org.enso.syntax2";
pub const SERIALIZATION_SUPPORT: &str = "org.enso.syntax2.serialization";
pub const EITHER_TYPE: &str = "org.enso.syntax2.serialization.Either";



// ==================
// === Test Cases ===
// ==================

use enso_parser::syntax;

/// Generate accept/reject test case set for the parser types rooted at `syntax::Tree`.
pub fn generate_testcases() -> abstracted::serialization::TestCases {
    let root = syntax::Tree::reflect();
    let root_id = root.id;
    let (graph, rust_to_abstracted) = enso_reflect::rust::to_abstracted(root);
    let root = rust_to_abstracted[&root_id];
    abstracted::serialization::testcases(&graph, root)
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
            if let Err(err) = enso_parser::syntax::tree::test_support::deserialize(case) {
                panic!("accept{i} fail: {err:?}");
            }
        }
        for (i, case) in cases.reject.iter().enumerate() {
            if let Ok(tree) = enso_parser::syntax::tree::test_support::deserialize(case) {
                panic!("reject{i} fail: accepted: {tree:?}");
            }
        }
    }
}
