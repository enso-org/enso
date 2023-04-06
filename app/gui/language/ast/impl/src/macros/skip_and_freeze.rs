//! Helper functions and data structures for [`SKIP`] and [`FREEZE`] macros support.
//!
//! [`SKIP`] and [`FREEZE`] macros change the behavior of the expression they are prepending.
//!
//! When used together in a single expression, [`SKIP`] macro always takes the priority and is
//! placed at the beginning. For example, `SKIP FREEZE foo`.

use enso_prelude::*;

use crate::known;
use crate::Ast;
use crate::HasRepr;



// =================
// === Constants ===
// =================

/// The text representation of the [`SKIP`] macro's name.
pub const SKIP_MACRO_IDENTIFIER: &str = "SKIP";
/// The text representation of the [`FREEZE`] macro's name.
pub const FREEZE_MACRO_IDENTIFIER: &str = "FREEZE";



// ==================
// === MacrosInfo ===
// ==================

/// The information about macros attached to the expression.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
#[allow(missing_docs)]
pub struct MacrosInfo {
    pub skip:   bool,
    pub freeze: bool,
}

impl MacrosInfo {
    /// Check if provided AST contains macros and fill in the info.
    pub fn from_ast(ast: &Ast) -> Self {
        let is_skip = is_macro_call(ast, SKIP_MACRO_IDENTIFIER);
        let is_freeze = if is_skip {
            if let Some(body) = maybe_prefix_macro_body(ast) {
                is_macro_call(&body, FREEZE_MACRO_IDENTIFIER)
            } else {
                false
            }
        } else {
            is_macro_call(ast, FREEZE_MACRO_IDENTIFIER)
        };
        Self { skip: is_skip, freeze: is_freeze }
    }

    /// `true` if either macros is used.
    pub fn has_any_macros(&self) -> bool {
        self.skip || self.freeze
    }

    /// The count of used macros.
    pub fn macros_count(&self) -> usize {
        self.skip as usize + self.freeze as usize
    }
}



// ========================
// === Helper functions ===
// ========================

/// Remove all macros from the AST.
pub fn without_macros(ast: &Ast) -> Ast {
    let macros_info = MacrosInfo::from_ast(ast);
    if macros_info.skip {
        let skip_body = prefix_macro_body(ast);
        if macros_info.freeze {
            prefix_macro_body(&skip_body)
        } else {
            skip_body
        }
    } else if macros_info.freeze {
        prefix_macro_body(ast)
    } else {
        ast.clone()
    }
}

/// Execute [`f`], preserving the usage of the [`SKIP`] macro. [`f`] receives AST without [`SKIP`],
/// and the macro would be preserved in the final result if it existed. Preserves the id of the AST.
pub fn preserving_skip(ast: &mut Ast, f: impl FnOnce(&mut Ast)) -> Ast {
    preserving_macro(ast, f, SKIP_MACRO_IDENTIFIER, |info| info.skip)
}

/// Execute [`f`], preserving the usage of the [`SKIP`] macro. [`f`] receives AST without [`SKIP`],
/// and the macro would be preserved in the final result if it existed. Preserves the id of the AST.
pub fn preserving_freeze(ast: &mut Ast, f: impl FnOnce(&mut Ast)) -> Ast {
    preserving_macro(ast, f, FREEZE_MACRO_IDENTIFIER, |info| info.freeze)
}

/// A combination oof [`preserving_skip`] and [`preserving_freeze`]. Preserves both macros.
pub fn preserving_skip_and_freeze(ast: &mut Ast, f: impl FnOnce(&mut Ast)) -> Ast {
    let skip = SKIP_MACRO_IDENTIFIER;
    let freeze = FREEZE_MACRO_IDENTIFIER;
    let is_skipped = |info: &MacrosInfo| info.skip;
    let is_frozen = |info: &MacrosInfo| info.freeze;
    let preserve_freeze = move |ast: &mut Ast| *ast = preserving_macro(ast, f, freeze, is_frozen);
    preserving_macro(ast, preserve_freeze, skip, is_skipped)
}

/// Helper function for preserving macros in AST.
///
/// [`f`] receives AST without [`SKIP`], and the macro would be preserved in the final result if it
/// existed. Preserves the id of the AST.
fn preserving_macro(
    ast: &mut Ast,
    f: impl FnOnce(&mut Ast),
    macro_name: &str,
    does_contain_macro: impl Fn(&MacrosInfo) -> bool,
) -> Ast {
    let macros_info = MacrosInfo::from_ast(ast);
    let original_ast_id = ast.id.expect("Node AST must bear an ID");
    let is_macro_present = does_contain_macro(&macros_info);
    let mut ast = if is_macro_present { prefix_macro_body(ast) } else { ast.clone() };
    f(&mut ast);
    if is_macro_present {
        prepend_with_macro(&mut ast, macro_name);
        ast = ast.with_id(original_ast_id);
    }
    ast
}

/// Check if AST contains a prefix-like macro call with a given name.
pub fn is_macro_call(ast: &Ast, identifier: &str) -> bool {
    if let Ok(prefix) = known::Prefix::try_from(ast) {
        let name = crate::identifier::name(&prefix.func);
        name == Some(identifier)
    } else {
        // TODO: Check for a [`Tree`] macro (https://github.com/enso-org/enso/issues/5572).
        false
    }
}

/// Returns a body (argument) of the prefix-like macro. See [`is_macro_call`] docs.
pub fn maybe_prefix_macro_body(ast: &Ast) -> Option<Ast> {
    if let Ok(prefix) = known::Prefix::try_from(ast) {
        Some(prefix.arg.clone())
    } else {
        // TODO: Check for a [`Tree`] macro (https://github.com/enso-org/enso/issues/5572).
        None
    }
}

/// Same as [`maybe_prefix_macro_body`], but logs the error and returns the argument in case of
/// failure.
pub fn prefix_macro_body(ast: &Ast) -> Ast {
    if let Some(ast) = maybe_prefix_macro_body(ast) {
        ast
    } else {
        error!("Failed to extract prefix macro body from expression {}.", ast.repr());
        ast.clone()
    }
}

/// Construct a prefix-like macro call with [`ast`] as a body (argument).
pub fn prepend_with_macro(ast: &mut Ast, macro_identifier: &str) {
    let func: Ast = crate::Cons { name: String::from(macro_identifier) }.into();
    *ast = crate::Prefix { func, off: 1, arg: ast.clone() }.into();
}

#[cfg(test)]
mod tests {
    use super::*;

    fn foo() -> Ast {
        Ast::var("foo")
    }

    fn bar() -> Ast {
        Ast::var("bar")
    }

    fn skip_foo() -> Ast {
        Ast::prefix(Ast::cons(SKIP_MACRO_IDENTIFIER), foo())
    }

    fn freeze_foo() -> Ast {
        Ast::prefix(Ast::cons(FREEZE_MACRO_IDENTIFIER), foo())
    }

    fn skip_freeze_foo() -> Ast {
        Ast::prefix(Ast::cons(SKIP_MACRO_IDENTIFIER), freeze_foo())
    }


    #[test]
    fn test_macros_info_from_ast() {
        // === foo ===

        let ast = foo();
        let macros_info = MacrosInfo::from_ast(&ast);
        assert!(!macros_info.has_any_macros());
        assert!(!macros_info.skip);
        assert!(!macros_info.freeze);


        // === SKIP foo ===

        let ast = skip_foo();
        assert!(is_macro_call(&ast, SKIP_MACRO_IDENTIFIER));
        let macros_info = MacrosInfo::from_ast(&ast);
        assert!(macros_info.has_any_macros());
        assert!(macros_info.skip);
        assert!(!macros_info.freeze);


        // === FREEZE foo ===

        let ast = freeze_foo();
        let macros_info = MacrosInfo::from_ast(&ast);
        assert!(macros_info.has_any_macros());
        assert!(!macros_info.skip);
        assert!(macros_info.freeze);


        // === SKIP FREEZE foo ===

        let ast = skip_freeze_foo();
        let macros_info = MacrosInfo::from_ast(&ast);
        assert!(macros_info.has_any_macros());
        assert!(macros_info.skip);
        assert!(macros_info.freeze);
    }

    #[test]
    fn test_maybe_prefix_macro_body() {
        assert!(maybe_prefix_macro_body(&foo()).is_none());
        assert!(maybe_prefix_macro_body(&skip_foo()).is_some());
        assert!(maybe_prefix_macro_body(&freeze_foo()).is_some());
        assert!(maybe_prefix_macro_body(&skip_freeze_foo()).is_some());
    }

    #[test]
    fn test_prepend_with_macro() {
        let mut ast = foo();
        prepend_with_macro(&mut ast, SKIP_MACRO_IDENTIFIER);
        assert_eq!(ast.repr(), skip_foo().repr());
    }

    #[test]
    fn test_without_macros() {
        let ast = skip_foo();
        assert_eq!(without_macros(&ast).repr(), foo().repr());
        let ast = freeze_foo();
        assert_eq!(without_macros(&ast).repr(), foo().repr());
        let ast = skip_freeze_foo();
        assert_eq!(without_macros(&ast).repr(), foo().repr());
        let ast = foo();
        assert_eq!(without_macros(&ast).repr(), foo().repr());
    }

    #[test]
    fn test_preserving_skip() {
        let mut ast = skip_foo();
        let original_id = ast.id;
        let skip_bar = preserving_skip(&mut ast, |ast| *ast = bar());
        assert_eq!(skip_bar.id, original_id);
        assert_eq!(skip_bar.repr(), "SKIP bar");
        let mut ast = skip_freeze_foo();
        assert_eq!(preserving_skip(&mut ast, |ast| *ast = foo()).repr(), "SKIP foo");
        let mut ast = foo();
        assert_eq!(preserving_skip(&mut ast, |ast| *ast = bar()).repr(), "bar");
    }

    #[test]
    fn test_preserving_freeze() {
        let mut ast = freeze_foo();
        let original_id = ast.id;
        let skip_bar = preserving_freeze(&mut ast, |ast| *ast = bar());
        assert_eq!(skip_bar.id, original_id);
        assert_eq!(skip_bar.repr(), "FREEZE bar");
        let mut ast = skip_freeze_foo();
        assert_eq!(preserving_freeze(&mut ast, |ast| *ast = foo()).repr(), "FREEZE foo");
        let mut ast = foo();
        assert_eq!(preserving_freeze(&mut ast, |ast| *ast = bar()).repr(), "bar");
    }

    #[test]
    fn test_preserving_skip_and_freeze() {
        let mut ast = skip_freeze_foo();
        let original_id = ast.id;
        let with_bar = preserving_skip_and_freeze(&mut ast, |ast| *ast = bar());
        assert_eq!(with_bar.id, original_id);
        assert_eq!(with_bar.repr(), "SKIP FREEZE bar");
        let mut ast = skip_foo();
        assert_eq!(preserving_skip_and_freeze(&mut ast, |ast| *ast = bar()).repr(), "SKIP bar");
        let mut ast = freeze_foo();
        assert_eq!(preserving_skip_and_freeze(&mut ast, |ast| *ast = bar()).repr(), "FREEZE bar");
        let mut ast = foo();
        assert_eq!(preserving_skip_and_freeze(&mut ast, |ast| *ast = bar()).repr(), "bar");
    }
}
