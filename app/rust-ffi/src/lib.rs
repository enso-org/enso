use wasm_bindgen::prelude::*;

use enso_parser::Parser;
use enso_parser::syntax::token::TokenOperatorProperties;

thread_local! {
    pub static PARSER: Parser = Parser::new();
}

#[wasm_bindgen]
pub fn parse_module(code: &str) -> Vec<u8> {
    let ast = PARSER.with(|parser| parser.parse_module(code));
    enso_parser::format::serialize(&ast).expect("Failed to serialize AST to binary format")
}

#[wasm_bindgen]
pub fn parse_block(code: &str) -> Vec<u8> {
    let ast = PARSER.with(|parser| parser.parse_block(code));
    enso_parser::format::serialize(&ast).expect("Failed to serialize AST to binary format")
}

#[wasm_bindgen]
pub fn is_ident_or_operator(code: &str) -> u32 {
    let parsed = enso_parser::lexer::run(code);
    if parsed.internal_error.is_some() {
        return 0;
    }
    let token = match &parsed.value[..] {
        [token] => token,
        _ => return 0,
    };
    match &token.variant {
        enso_parser::syntax::token::Variant::Ident(_) => 1,
        enso_parser::syntax::token::Variant::Operator(_) => 2,
        _ => 0,
    }
}

/// What should separate this expression from self argument applied to it.
///
/// See `selfArgSeparator` docs in app/ydoc-shared/src/ast/token.ts
///
/// Returns:
/// * -1 - if `self` should be joined with dot.
/// * non-negative integer - if `self` should be joined with returned number of spaces.
#[wasm_bindgen]
pub fn self_arg_separator(code: &str) -> i32 {
    let parsed = enso_parser::lexer::run(code);
    if parsed.internal_error.is_some() {
        return 0;
    }
    let (token, right_spacing) = match &parsed.value[..] {
        [token, next_token, ..] => (token, next_token.left_offset.visible.width_in_spaces),
        [token] => (token, 1),
        [] => return -1,
    };
    if token.operator_properties().is_some() { right_spacing as i32 } else { -1 }
}

#[wasm_bindgen]
pub fn is_numeric_literal(code: &str) -> bool {
    let parsed = PARSER.with(|parser| parser.parse_block(code));
    let enso_parser::syntax::tree::Variant::BodyBlock(body) = parsed.variant else { return false };
    let [stmt] = &body.statements[..] else { return false };
    let Some(stmt) = &stmt.expression else { return false };
    let enso_parser::syntax::tree::Variant::ExpressionStatement(stmt) = &stmt.variant else {
        return false;
    };
    match &stmt.expression.variant {
        enso_parser::syntax::tree::Variant::Number(_) => true,
        enso_parser::syntax::tree::Variant::UnaryOprApp(app) => {
            app.opr.code == "-"
                && matches!(&app.rhs.variant, enso_parser::syntax::tree::Variant::Number(_))
        }
        _ => false,
    }
}

#[wasm_bindgen(start)]
fn main() {
    console_error_panic_hook::set_once();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_numeric_literal() {
        assert!(is_numeric_literal("1234"));
        assert!(is_numeric_literal("-1234"));
        assert!(!is_numeric_literal(""));
        assert!(!is_numeric_literal("-"));
        assert!(!is_numeric_literal("1-234"));
        assert!(!is_numeric_literal("1234!"));
        assert!(!is_numeric_literal("1234e5"));
    }

    #[test]
    fn test_checking_ident_or_operator() {
        assert_eq!(is_ident_or_operator("abc"), 1);
        assert_eq!(is_ident_or_operator("Abc"), 1);
        assert_eq!(is_ident_or_operator("abc 14"), 0);
        assert_eq!(is_ident_or_operator("+"), 2);
        assert_eq!(is_ident_or_operator("+ 2"), 0);
        assert_eq!(is_ident_or_operator("[]"), 0);

        assert_eq!(self_arg_separator("abc"), -1);
        assert_eq!(self_arg_separator("abc 14"), -1);
        assert_eq!(self_arg_separator("+"), 1);
        assert_eq!(self_arg_separator("+ 2"), 1);
        assert_eq!(self_arg_separator("+2"), 0);
        assert_eq!(self_arg_separator("- 2"), 1);
        assert_eq!(self_arg_separator("-2"), 0);
        assert_eq!(self_arg_separator("[]"), -1);
    }
}
