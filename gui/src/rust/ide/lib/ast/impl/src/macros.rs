//! Utilities for dealing with macro-related parts of AST and language, including `Match` shape and
//! such constructs as lambda expressions.


use crate::prelude::*;

use crate::crumbs::AmbiguousCrumb;
use crate::crumbs::Located;
use crate::crumbs::MatchCrumb;
use crate::known;



// ===============
// === Imports ===
// ===============

/// The keyword introducing an qualified import declaration. See:
/// https://dev.enso.org/docs/enso/syntax/imports.html#import-syntax
pub const QUALIFIED_IMPORT_KEYWORD:&str = "import";
/// The keyword introducing an unqualified import declaration.
pub const UNQUALIFIED_IMPORT_KEYWORD:&str = "from";
/// The keyword introducing an unqualified export declaration.
pub const QUALIFIED_EXPORT_KEYWORD:&str = "export";

/// If the given AST node is an import declaration, returns it as a Match (which is the only shape
/// capable of storing import declarations). Returns `None` otherwise.
pub fn ast_as_import_match(ast:&Ast) -> Option<known::Match> {
    let macro_match = known::Match::try_from(ast).ok()?;
    is_match_import(&macro_match).then_some(macro_match)
}

/// Check if the given macro match node is an import declaration.
pub fn is_match_import(ast:&known::Match) -> bool {
    let segment = &ast.segs.head;
    let keyword = crate::identifier::name(&segment.head);
    if keyword.contains_if(|str| *str == UNQUALIFIED_IMPORT_KEYWORD) {
        let second_segment = &ast.segs.tail.first();
        match second_segment {
            Some(seg) => {
                let keyword_2 = crate::identifier::name(&seg.head);
                if keyword_2.contains_if(|str| *str == QUALIFIED_IMPORT_KEYWORD) {
                    return true
                }
            }
            None => return false
        }
    }
    keyword.contains_if(|str| *str == QUALIFIED_IMPORT_KEYWORD)
}

/// Check if the given ast node is an import declaration.
pub fn is_ast_import(ast:&Ast) -> bool {
    ast_as_import_match(ast).is_some()
}



// ===============
// === Lambdas ===
// ===============

/// Describes the lambda-expression's three pieces: the argument, the arrow operator and the body.
#[allow(missing_docs)]
#[derive(Clone,Debug)]
pub struct LambdaInfo<'a> {
    pub arg  : Located<&'a Ast>,
    pub opr  : Located<&'a Ast>,
    pub body : Located<&'a Ast>,
}

/// If this is the builtin macro for `->` (lambda expression), returns it as known `Match`.
pub fn as_lambda_match(ast:&Ast) -> Option<known::Match> {
    let macro_match = known::Match::try_from(ast).ok()?;
    let segment     = &macro_match.segs.head;
    crate::opr::is_arrow_opr(&segment.head).then_some(macro_match)
}

/// Describes the given Ast as lambda, if this is a matched `->` builtin macro.
pub fn as_lambda(ast:&Ast) -> Option<LambdaInfo> {
    let _              = as_lambda_match(ast)?;
    let mut child_iter = ast.iter_subcrumbs();
    let arg            = ast.get_located(child_iter.next()?).ok()?;
    let opr            = ast.get_located(child_iter.next()?).ok()?;
    let body           = ast.get_located(child_iter.next()?).ok()?;
    let is_arrow       = crate::opr::is_arrow_opr(opr.item);
    is_arrow.then_some(LambdaInfo {arg,opr,body})
}



// ===================
// === Match Utils ===
// ===================

impl crate::Match<Ast> {
    /// Iterates matched ASTs. Skips segment heads ("keywords").
    /// For example, for `(a)` it iterates only over `a`, skkipping segment heads `(` and `)`.
    pub fn iter_pat_match_subcrumbs(&self) -> impl Iterator<Item=MatchCrumb> + '_ {
        self.iter_subcrumbs().filter(|crumb| {
            use crate::crumbs::SegmentMatchCrumb;
            match crumb {
                MatchCrumb::Segs {val,..} => val != &SegmentMatchCrumb::Head,
                _                         => true
            }
        })
    }
}



// =======================
// === Ambiguous Utils ===
// =======================

impl crate::Ambiguous<Ast> {
    /// Iterates matched ASTs. Skips segment heads ("keywords").
    /// For example, for `(a)` it iterates only over `a`, skkipping segment heads `(` and `)`.
    pub fn iter_pat_match_subcrumbs(&self) -> impl Iterator<Item=AmbiguousCrumb> + '_ {
        self.iter_subcrumbs().filter(|crumb| {
            crumb.field != crate::crumbs::AmbiguousSegmentCrumb::Head
        })
    }
}
