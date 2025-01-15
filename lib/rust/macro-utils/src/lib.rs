//! A number of helper functions meant to be used in the procedural enso-shapely-macros
//! definitions.

// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]

use proc_macro2::TokenTree;
use syn::WhereClause;
use syn::WherePredicate;



// ===================
// === Token Utils ===
// ===================

/// Is the given token an identifier matching to a given string?
pub fn matching_ident(token: &TokenTree, name: &str) -> bool {
    match token {
        TokenTree::Ident(ident) => *ident == name,
        _ => false,
    }
}



// ============
// === Repr ===
// ============

/// Returns names of the named fields.
pub fn field_names(fields: &syn::FieldsNamed) -> Vec<&syn::Ident> {
    fields
        .named
        .iter()
        .map(|field| field.ident.as_ref().expect("Impossible: no name on a named field."))
        .collect()
}



// ==================
// === Path Utils ===
// ==================

/// Checks if a given `Path` consists of a single identifier same as given string.
pub fn path_matching_ident(path: &syn::Path, str: impl AsRef<str>) -> bool {
    path.get_ident().map_or(false, |ident| ident == str.as_ref())
}



// ======================
// === Index Sequence ===
// ======================

/// For given length, returns a sequence of Literals like `[0,1,2…]`. These are unsuffixed
/// usize literals, so e.g. can be used to identify the tuple unnamed fields.
pub fn index_sequence(len: usize) -> Vec<syn::Index> {
    (0..len).map(syn::Index::from).collect()
}

/// For given length returns sequence of identifiers like `[field0,field1,…]`.
pub fn identifier_sequence(len: usize) -> Vec<syn::Ident> {
    let format_field = |ix| quote::format_ident!("field{}", ix);
    (0..len).map(format_field).collect()
}



// ===================
// === WhereClause ===
// ===================

/// Creates a new where clause from provided sequence of where predicates.
pub fn new_where_clause(predicates: impl IntoIterator<Item = WherePredicate>) -> WhereClause {
    let predicates = syn::punctuated::Punctuated::from_iter(predicates);
    WhereClause { where_token: Default::default(), predicates }
}
