//! Helper functions and data structures for [`SKIP`] and [`FREEZE`] macros support.
//!
//! [`SKIP`] and [`FREEZE`] macros change the behavior of the expression they are prepending.
//!
//! When used together in a single expression, [`SKIP`] macro always takes the priority and is
//! placed at the beginning. For example, `SKIP FREEZE foo`.

use enso_prelude::*;

use crate::known;
use crate::Ast;
use crate::Crumbable;
use crate::HasRepr;



// =================
// === Constants ===
// =================

/// The text representation of the [`SKIP`] macro's name.
pub const SKIP_MACRO_IDENTIFIER: &str = "skip";
/// The text representation of the [`FREEZE`] macro's name.
pub const FREEZE_MACRO_IDENTIFIER: &str = "freeze";



// ==================
// === MacrosInfo ===
// ==================

/// The information about macros attached to the expression.
#[derive(Debug, Clone, Copy, Default)]
#[allow(missing_docs)]
pub struct MacrosInfo {
    pub skip:   bool,
    pub freeze: bool,
}

impl MacrosInfo {
    /// Check if provided AST contains macros and fill in the info.
    pub fn from_ast(ast: &Ast) -> Self {
        let is_skip = is_macro_call(ast, SKIP_MACRO_IDENTIFIER);
        let mut is_freeze = false;
        if is_skip {
            if let Some(body) = maybe_prefix_macro_body(ast) {
                is_freeze = is_macro_call(&body, FREEZE_MACRO_IDENTIFIER);
            }
        } else {
            is_freeze = is_macro_call(ast, FREEZE_MACRO_IDENTIFIER);
        }
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
pub fn without_macros(ast: &Ast, macros_info: &MacrosInfo) -> Ast {
    if macros_info.skip {
        let mut body = prefix_macro_body(ast);
        if macros_info.freeze {
            body = prefix_macro_body(&body);
        }
        body
    } else {
        prefix_macro_body(ast)
    }
}

/// Execute [`f`], preserving the usage of the [`SKIP`] macro. [`f`] receives AST without [`SKIP`],
/// and the macro would be preserved in the final result if it existed.
pub fn preserving_skip(ast: &mut Ast, macros_info: &MacrosInfo, f: impl Fn(&mut Ast)) -> Ast {
    let mut ast = if macros_info.skip { prefix_macro_body(ast) } else { ast.clone() };
    f(&mut ast);
    if macros_info.skip {
        prepend_with_macro(&mut ast, SKIP_MACRO_IDENTIFIER);
    }
    ast
}

/// Check if AST contains a prefix-like macro call with a given name.
///
/// We check for both [`known::Prefix`] and [`known::Match`], because first one is used when we
/// modify AST using this module, and the second one can be provided by the Engine.
///
/// Using [`known::Match`] everywhere would be perfect, but it is extremely annoying to construct
/// without using the parser.
pub fn is_macro_call(ast: &Ast, identifier: &str) -> bool {
    if let Ok(prefix) = known::Prefix::try_from(ast) {
        let name = crate::identifier::name(&prefix.func);
        name == Some(identifier)
    } else if let Ok(macro_match) = known::Match::try_from(ast) {
        let first_segment = &macro_match.segs.head;
        let name = crate::identifier::name(&first_segment.head);
        name == Some(identifier)
    } else {
        false
    }
}

/// Returns a body (argument) of the prefix-like macro. See [`is_macro_call`] docs.
pub fn maybe_prefix_macro_body(ast: &Ast) -> Option<Ast> {
    if let Ok(prefix) = known::Prefix::try_from(ast) {
        Some(prefix.arg.clone())
    } else if let Ok(macro_match) = known::Match::try_from(ast) {
        let body_crumb = macro_match.iter_subcrumbs().nth(1)?;
        let body_ast = macro_match.get(&body_crumb).ok()?;
        Some(body_ast.clone())
    } else {
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
