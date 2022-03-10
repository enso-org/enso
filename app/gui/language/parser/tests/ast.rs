//! Tests specific to Ast rather than parser itself but placed here because they depend on parser
//! to easily generate test input.

// TODO: [mwu]
//  That means that likely either `parser` should be merged with `ast` or that we should have a
//  separate `ast_ops` crate that depends on both. Now it is better to tests here than none but
//  a decision should be made as to which way we want to go.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use parser::prelude::*;

use ast::opr;
use ast::opr::GeneralizedInfix;
use ast::prefix;
use ast::test_utils::expect_single_line;
use ast::HasRepr;
use wasm_bindgen_test::wasm_bindgen_test;



wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
pub fn to_assignment_test() {
    let parser = parser::Parser::new_or_panic();
    let is_assignment = |code: &str| {
        let ast = parser.parse(code.to_string(), default()).unwrap();
        let line = expect_single_line(&ast);
        ast::opr::to_assignment(line).is_some()
    };

    let expected_assignments = vec!["a = 5", "a=5", "foo bar = a b c", "(x,y) = pos"];
    let expected_not_assignments = vec!["= 5", "a=", "=", "foo", "a->b", "a+b"];

    for code in expected_assignments {
        assert!(is_assignment(code), "{} expected to be recognized as assignment", code);
    }
    for code in expected_not_assignments {
        assert!(!is_assignment(code), "{} expected to not be recognized as assignment", code);
    }
}

#[wasm_bindgen_test]
pub fn generalized_infix_test() {
    let parser = parser::Parser::new_or_panic();
    let make_gen_infix = |code: &str| {
        let ast = parser.parse(code.to_string(), default()).unwrap();
        let line = expect_single_line(&ast);
        GeneralizedInfix::try_new(line)
    };

    let infix = make_gen_infix("a+b").unwrap();
    assert_eq!(infix.name(), "+");
    assert_eq!(infix.left.map(|op| op.arg).repr(), "a");
    assert_eq!(infix.right.map(|op| op.arg).repr(), "b");

    let right = make_gen_infix("+b").unwrap();
    assert_eq!(right.name(), "+");
    assert_eq!(right.right.map(|op| op.arg).repr(), "b");

    let left = make_gen_infix("a+").unwrap();
    assert_eq!(left.name(), "+");
    assert_eq!(left.left.map(|op| op.arg).repr(), "a");

    let sides = make_gen_infix("+").unwrap();
    assert_eq!(sides.name(), "+");

    let var_as_infix = make_gen_infix("a");
    assert!(var_as_infix.is_none());
}

#[wasm_bindgen_test]
pub fn flatten_prefix_test() {
    fn expect_pieces(flattened: &prefix::Chain, pieces: Vec<&str>) {
        let mut piece_itr = pieces.iter();
        assert_eq!(flattened.args.len() + 1, pieces.len()); // +1 because `func` piece is separate field
        assert_eq!(&flattened.func.repr(), piece_itr.next().unwrap());
        flattened.args.iter().zip(piece_itr).for_each(|(lhs, rhs)| {
            assert_eq!(&lhs.repr(), rhs);
        })
    }

    let parser = parser::Parser::new_or_panic();
    let case = |code: &str, expected_pieces: Vec<&str>| {
        let ast = parser.parse(code.into(), default()).unwrap();
        let ast = ast::test_utils::expect_single_line(&ast);
        let flattened = prefix::Chain::from_ast_non_strict(ast);
        expect_pieces(&flattened, expected_pieces);
        assert_eq!(flattened.repr(), code);
    };

    case("a", vec!["a"]);
    case("a b c d", vec!["a", " b", " c", " d"]);
    case("+ a b c", vec!["+", " a", " b", " c"]);
    case("a b + c d", vec!["a b + c d"]); // nothing to flatten, this is infix, not prefix
}

#[wasm_bindgen_test]
pub fn flatten_infix_test() {
    fn expect_pieces(flattened: &opr::Chain, target: &str, pieces: Vec<&str>) {
        assert_eq!(flattened.target.as_ref().map(|a| &a.arg).repr(), target);

        let piece_itr = pieces.iter();
        assert_eq!(flattened.args.len(), pieces.len());
        flattened.args.iter().zip(piece_itr).for_each(|(lhs, rhs)| {
            assert_eq!(lhs.operand.as_ref().map(|a| &a.arg).repr(), *rhs);
        })
    }

    let parser = parser::Parser::new_or_panic();
    let case = |code: &str, target: &str, expected_pieces: Vec<&str>| {
        let ast = parser.parse(code.into(), default()).unwrap();
        let ast = ast::test_utils::expect_single_line(&ast);
        let flattened = opr::Chain::try_new(ast).unwrap();
        expect_pieces(&flattened, target, expected_pieces);
    };

    case("a+b+c", "a", vec!["b", "c"]);
    case("a,b,c", "c", vec!["b", "a"]);
    case("a+b*c+d", "a", vec!["b*c", "d"]);
}
