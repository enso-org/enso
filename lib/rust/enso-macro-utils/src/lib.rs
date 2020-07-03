//! A number of helper functions meant to be used in the procedural enso-shapely-macros
//! definitions.

#![warn(missing_docs)]
#![feature(trait_alias)]

use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use std::iter::FromIterator;
use syn::visit::Visit;
use syn::WhereClause;
use syn::WherePredicate;
use syn;



// =====================
// === Trait Aliases ===
// =====================

pub trait Str = Into<String> + AsRef<str>;



// ==========================
// === Token Stream Utils ===
// ==========================

/// Maps all the tokens in the stream using a given function.
pub fn map_tokens<F:Fn(TokenTree) -> TokenTree>
(input:TokenStream, f:F) -> TokenStream {
    let ret_iter = input.into_iter().map(f);
    TokenStream::from_iter(ret_iter)
}

/// Rewrites stream replacing each token with a sequence of tokens returned by
/// the given function. The groups (e.g. token tree within braces) are unpacked,
/// rewritten and repacked into groups -- the function is applied recursively.
pub fn rewrite_stream
<F:Fn(TokenTree) -> TokenStream + Copy>
(input:TokenStream, f:F) -> TokenStream {
    let mut ret = TokenStream::new();
    for token in input.into_iter() {
        match token {
            proc_macro2::TokenTree::Group(group) => {
                let delim  = group.delimiter();
                let span   = group.span();
                let rewritten = rewrite_stream(group.stream(), f);
                let mut new_group = proc_macro2::Group::new(delim,rewritten);
                new_group.set_span(span);
                let new_group = vec![TokenTree::from(new_group)];
                ret.extend(new_group.into_iter())
            }
            _ => ret.extend(f(token)),
        }
    }
    ret
}



// ===================
// === Token Utils ===
// ===================

/// Is the given token an identifier matching to a given string?
pub fn matching_ident(token:&TokenTree, name:&str) -> bool {
    match token {
        TokenTree::Ident(ident) => *ident == name,
        _                       => false,
    }
}



// ============
// === Repr ===
// ============

/// Obtains text representation of given `ToTokens`-compatible input.
pub fn repr<T: quote::ToTokens>(t:&T) -> String {
    quote!(#t).to_string()
}



// ===================
// === Field Utils ===
// ===================

/// Collects all fields, named or not.
pub fn fields_list(fields:&syn::Fields) -> Vec<&syn::Field> {
    match fields {
        syn::Fields::Named  (ref f) => f.named  .iter().collect(),
        syn::Fields::Unnamed(ref f) => f.unnamed.iter().collect(),
        syn::Fields::Unit           => Default::default(),
    }
}

/// Returns token that refers to the field.
///
/// It is the field name for named field and field index for unnamed fields.
pub fn field_ident_token(field:&syn::Field, index:syn::Index) -> TokenStream {
    match &field.ident {
        Some(ident) => quote!(#ident),
        None        => quote!(#index),
    }
}

/// Returns names of the named fields.
pub fn field_names(fields:&syn::FieldsNamed) -> Vec<&syn::Ident> {
    fields.named.iter().map(|field| {
        field.ident.as_ref().expect("Impossible: no name on a named field.")
    }).collect()
}



// ==================
// === Path Utils ===
// ==================

/// Checks if a given `Path` consists of a single identifier same as given string.
pub fn path_matching_ident(path:&syn::Path, str:impl Str) -> bool {
    path.get_ident().map_or(false, |ident| ident == str.as_ref())
}



// ======================
// === Index Sequence ===
// ======================

/// For given length, returns a sequence of Literals like `[0,1,2…]`. These are unsuffixed
/// usize literals, so e.g. can be used to identify the tuple unnamed fields.
pub fn index_sequence(len:usize) -> Vec<syn::Index> {
    (0..len).map(syn::Index::from).collect()
}

/// For given length returns sequence of identifiers like `[field0,field1,…]`.
pub fn identifier_sequence(len:usize) -> Vec<syn::Ident> {
    let format_field = |ix| quote::format_ident!("field{}",ix);
    (0..len).map(format_field).collect()
}



// =======================
// === Type Path Utils ===
// =======================

/// Obtain list of generic arguments on the path's segment.
pub fn path_segment_generic_args
(segment:&syn::PathSegment) -> Vec<&syn::GenericArgument> {
    match segment.arguments {
        syn::PathArguments::AngleBracketed(ref args) =>
            args.args.iter().collect(),
        _ =>
            Vec::new(),
    }
}

/// Obtain list of generic arguments on the path's last segment.
///
/// Empty, if path contains no segments.
pub fn ty_path_generic_args
(ty_path:&syn::TypePath) -> Vec<&syn::GenericArgument> {
    ty_path.path.segments.last().map_or(Vec::new(), path_segment_generic_args)
}

/// Obtain list of type arguments on the path's last segment.
pub fn ty_path_type_args
(ty_path:&syn::TypePath) -> Vec<&syn::Type> {
    ty_path_generic_args(ty_path).iter().filter_map( |generic_arg| {
        match generic_arg {
            syn::GenericArgument::Type(t) => Some(t),
            _                             => None,
        }
    }).collect()
}

/// Last type argument of the last segment on the type path.
pub fn last_type_arg(ty_path:&syn::TypePath) -> Option<&syn::GenericArgument> {
    ty_path_generic_args(ty_path).last().copied()
}



// =====================
// === Collect Types ===
// =====================

/// Visitor that accumulates all visited `syn::TypePath`.
#[derive(Default)]
pub struct TypeGatherer<'ast> {
    /// Observed types accumulator.
    pub types: Vec<&'ast syn::TypePath>
}

impl<'ast> Visit<'ast> for TypeGatherer<'ast> {
    fn visit_type_path(&mut self, node:&'ast syn::TypePath) {
        self.types.push(node);
        syn::visit::visit_type_path(self, node);
    }
}

/// All `TypePath`s in the given's `Type` subtree.
pub fn gather_all_types(node:&syn::Type) -> Vec<&syn::TypePath> {
    let mut type_gather = TypeGatherer::default();
    type_gather.visit_type(node);
    type_gather.types
}

/// All text representations of `TypePath`s in the given's `Type` subtree.
pub fn gather_all_type_reprs(node:&syn::Type) -> Vec<String> {
    gather_all_types(node).iter().map(|t| repr(t)).collect()
}



// =======================
// === Type Dependency ===
// =======================

/// Naive type equality test by comparing its representation with a string.
pub fn type_matches_repr(ty:&syn::Type, target_repr:&str) -> bool {
    repr(ty) == target_repr
}

/// Naive type equality test by comparing their text representations.
pub fn type_matches(ty:&syn::Type, target_param:&syn::GenericParam) -> bool {
    type_matches_repr(ty, &repr(target_param))
}

/// Does type depends on the given type parameter.
pub fn type_depends_on(ty:&syn::Type, target_param:&syn::GenericParam) -> bool {
    let target_param = repr(target_param);
    let relevant_types = gather_all_types(ty);
    relevant_types.iter().any(|ty| repr(ty) == target_param)
}

/// Does enum variant depend on the given type parameter.
pub fn variant_depends_on
(var:&syn::Variant, target_param:&syn::GenericParam) -> bool {
    var.fields.iter().any(|field| type_depends_on(&field.ty, target_param))
}



// ===================
// === WhereClause ===
// ===================

/// Creates a new where clause from provided sequence of where predicates.
pub fn new_where_clause(predicates:impl IntoIterator<Item=WherePredicate>) -> WhereClause {
    let predicates = syn::punctuated::Punctuated::from_iter(predicates);
    WhereClause {where_token:Default::default(),predicates}
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use proc_macro2::TokenStream;

    fn parse<T:syn::parse::Parse>(code:&str) -> T {
        syn::parse_str(code).unwrap()
    }

    #[test]
    fn repr_round_trips() {
        let program = "pub fn repr<T: quote::ToTokens>(t: &T) -> String {}";
        let tokens = parse::<TokenStream>(program);
        let quoted_program = repr(&tokens);
        let tokens2 = parse::<TokenStream>(&quoted_program);
        // check only second round-trip, first is allowed to break whitespace
        assert_eq!(repr(&tokens), repr(&tokens2));
    }

    #[test]
    fn fields_list_test() {
        let tuple_like     = "struct Unnamed(i32, String, T);";
        let proper_struct  = "struct Named{i: i32, s: String, t: T}";
        let expected_types = vec!["i32", "String", "T"];

        fn assert_field_types(program:&str, expected_types:&[&str]) {
            let tokens = parse::<syn::ItemStruct>(program);
            let fields = fields_list(&tokens.fields);
            let types  = fields.iter().map(|f| repr(&f.ty));
            assert_eq!(Vec::from_iter(types), expected_types);
        }

        assert_field_types(tuple_like, &expected_types);
        assert_field_types(proper_struct, &expected_types);
    }

    #[test]
    fn type_dependency() {
        let param:syn::GenericParam = parse("T");
        let depends                 = |code| {
            let ty:syn::Type = parse(code);
            type_depends_on(&ty, &param)
        };

        // sample types that depend on `T`
        let dependents = vec!{
            "T",
            "Option<T>",
            "Pair<T, U>",
            "Pair<U, T>",
            "Pair<U, (T,)>",
            "&T",
            "&'t mut T",
        };
        // sample types that do not depend on `T`
        let independents = vec!{
            "Tt",
            "Option<Tt>",
            "Pair<Tt, U>",
            "Pair<U, Tt>",
            "Pair<U, Tt>",
            "i32",
            "&str",
        };
        for dependent in dependents {
            assert!(depends(dependent), "{} must depend on {}"
                    , repr(&dependent), repr(&param));
        }
        for independent in independents {
            assert!(!depends(independent), "{} must not depend on {}"
                    , repr(&independent), repr(&param));
        }
    }

    #[test]
    fn collecting_type_path_args() {
        fn check(expected_type_args:Vec<&str>, ty_path:&str) {
            let ty_path = parse(ty_path);
            let args    = super::ty_path_type_args(&ty_path);
            assert_eq!(expected_type_args.len(), args.len());
            let zipped  = expected_type_args.iter().zip(args.iter());
            for (expected,got) in zipped {
                assert_eq!(expected, &repr(got));
            }
        }
        check(vec!["T"]     , "std::Option<T>");
        check(vec!["U"]     , "std::Option<U>");
        check(vec!["A", "B"], "Either<A,B>");
        assert_eq!(super::last_type_arg(&parse("i32")), None);
        assert_eq!(repr(&super::last_type_arg(&parse("Foo<C>"))), "C");
    }
}
