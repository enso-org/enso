use wasm_bindgen::prelude::*;

use enso_parser::Parser;



thread_local! {
    pub static PARSER: Parser = Parser::new();
}

#[wasm_bindgen]
pub fn parse_doc_to_json(docs: &str) -> String {
    let docs = enso_doc_parser::parse(docs);
    serde_json::to_string(&docs).expect("Failed to serialize Doc Sections to JSON")
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

fn starts_with_ident_or_operator<const ONLY_ONE_TOKEN_ALLOWED: bool>(code: &str) -> u32 {
    let parsed = enso_parser::lexer::run(code);
    if parsed.internal_error.is_some() {
        return 0;
    }
    let token = match &parsed.value[..] {
        [token] => token,
        [token, ..] if !ONLY_ONE_TOKEN_ALLOWED => token,
        _ => return 0,
    };
    match &token.variant {
        enso_parser::syntax::token::Variant::Ident(_) => 1,
        enso_parser::syntax::token::Variant::Operator(_) => 2,
        _ => 0,
    }
}

#[wasm_bindgen]
pub fn is_ident_or_operator(code: &str) -> u32 {
    starts_with_ident_or_operator::<true>(code)
}

#[wasm_bindgen]
pub fn is_first_token_ident_or_operator(code: &str) -> u32 {
    starts_with_ident_or_operator::<false>(code)
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
        enso_parser::syntax::tree::Variant::UnaryOprApp(app) =>
            app.opr.code == "-"
                && app.rhs.as_ref().map_or(false, |rhs| {
                    matches!(rhs.variant, enso_parser::syntax::tree::Variant::Number(_))
                }),
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

        assert_eq!(is_first_token_ident_or_operator("abc"), 1);
        assert_eq!(is_first_token_ident_or_operator("abc 14"), 1);
        assert_eq!(is_first_token_ident_or_operator("+"), 2);
        assert_eq!(is_first_token_ident_or_operator("+ 2"), 2);
        assert_eq!(is_first_token_ident_or_operator("[]"), 0);
    }
}
