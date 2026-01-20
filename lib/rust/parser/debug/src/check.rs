//! Syntax tree validation

use enso_parser::source::code::debug::LocationCheck;

// ========================
// === Span consistency ===
// ========================

/// Check the internal consistency of the `Tree` and `Token` spans from the given root, and validate
/// that every character in the given range is covered exactly once in the token spans.
pub fn validate_spans(
    tree: &enso_parser::syntax::tree::Tree,
    expected_span: std::ops::Range<u32>,
    locations: &mut LocationCheck,
) -> Result<(), String> {
    let mut sum_span = None;
    fn concat<T: PartialEq + std::fmt::Debug + Copy>(
        a: &Option<std::ops::Range<T>>,
        b: &std::ops::Range<T>,
    ) -> Result<std::ops::Range<T>, String> {
        Ok(match a {
            Some(a) => {
                if a.end != b.start {
                    return Err("AST must exactly represent source code".to_owned());
                }
                a.start..b.end
            }
            None => b.clone(),
        })
    }
    sum_span = Some(concat(&sum_span, &tree.span.left_offset.code.range())?);
    tree.visit_items(|item| match item {
        enso_parser::syntax::item::Ref::Token(token) => {
            if !(token.left_offset.is_empty() && token.code.is_empty()) {
                sum_span = Some(concat(&sum_span, &token.left_offset.code.range()).unwrap());
                sum_span = Some(concat(&sum_span, &token.code.range()).unwrap());
            }
            let left_offset = token.left_offset.code.range();
            let code = token.code.range();
            locations.extend(&[left_offset.start, left_offset.end, code.start, code.end]);
        }
        enso_parser::syntax::item::Ref::Tree(tree) => {
            let children_span =
                concat(&Some(tree.span.left_offset.code.range()), &tree.span.range()).unwrap();
            let children_span_ = children_span.start.utf16..children_span.end.utf16;
            validate_spans(tree, children_span_, locations).unwrap();
            sum_span = Some(concat(&sum_span, &children_span).unwrap());
            let left_offset = tree.span.left_offset.code.range();
            let code = tree.span.range();
            locations.extend(&[left_offset.start, left_offset.end, code.start, code.end]);
        }
    });
    if expected_span.is_empty() {
        assert!(
            sum_span.is_none_or(|range| range.is_empty()),
            "AST must exactly represent source code"
        );
    } else {
        let sum_span = sum_span.unwrap_or_default();
        let sum_span = sum_span.start.utf16..sum_span.end.utf16;
        assert_eq!(sum_span, expected_span, "AST must exactly represent source code");
    }
    Ok(())
}
