use prelude::*;

use quote::quote;
use syn;
use syn::visit::{self, Visit};

/// Obtains text representation of given `ToTokens`-compatible input.
pub fn repr<T: quote::ToTokens>(t:&T) -> String {
    quote!(#t).to_string()
}

/// Collects all fields, named or not.
pub fn fields_list(fields:&syn::Fields) -> Vec<&syn::Field> {
    match fields {
        syn::Fields::Named  (ref f) => f.named  .iter().collect(),
        syn::Fields::Unnamed(ref f) => f.unnamed.iter().collect(),
        syn::Fields::Unit           => default(),
    }
}

// =====================
// === Collect Types ===
// =====================

/// Visitor that accumulates all visited `syn::TypePath`.
pub struct TypeGatherer<'ast> {
    pub types: Vec<&'ast syn::TypePath>
}

impl TypeGatherer<'_> {
    pub fn new() -> Self {
        let types = default();
        Self { types }
    }
}

impl<'ast> Visit<'ast> for TypeGatherer<'ast> {
    fn visit_type_path(&mut self, node:&'ast syn::TypePath) {
        self.types.push(node);
        visit::visit_type_path(self, node);
    }
}

/// All `TypePath`s in the given's `Type` subtree.
pub fn gather_all_types(node:&syn::Type) -> Vec<&syn::TypePath> {
    let mut type_gather = TypeGatherer::new();
    type_gather.visit_type(node);
    type_gather.types
}

/// All text representations of `TypePath`s in the given's `Type` subtree.
pub fn gather_all_type_reprs(node:&syn::Type) -> Vec<String> {
    gather_all_types(node).iter().map(|t| repr(t)).collect()
}

// === Type Dependency ===

pub fn type_depends_on(ty:&syn::Type, target_param:&syn::GenericParam) -> bool {
    let target_param = repr(target_param);
    gather_all_types(ty).iter().any(|ty| repr(ty) == target_param)
}

pub fn type_matches(ty:&syn::Type, target_param:&syn::GenericParam) -> bool {
    repr(ty) == repr(target_param)
}


#[cfg(test)]
mod tests {
    use super::*;
    use proc_macro2::TokenStream;
    use syn::*;

    #[test]
    fn repr_round_trips() {
        let program = "pub fn repr<T: quote::ToTokens>(t: &T) -> String {}";
        let tokens = parse_str::<TokenStream>(program).unwrap();
        let quoted_program = repr(&tokens);
        let tokens2 = parse_str::<TokenStream>(&quoted_program).unwrap();
        // check only second round-trip, first is allowed to break whitespace
        assert_eq!(repr(&tokens), repr(&tokens2));
    }

    #[test]
    fn fields_list_test() {
        let tuple_like     = "struct Unnamed(i32, String, T);";
        let proper_struct  = "struct Named{i: i32, s: String, t: T}";
        let expected_types = vec!["i32", "String", "T"];

        fn assert_field_types(program:&str, expected_types:&[&str]) {
            let tokens = parse_str::<syn::ItemStruct>(program).unwrap();
            let fields = fields_list(&tokens.fields);
            let types  = fields.iter().map(|f| repr(&f.ty));
            assert_eq!(Vec::from_iter(types), expected_types);
        }

        assert_field_types(tuple_like, &expected_types);
        assert_field_types(proper_struct, &expected_types);
    }

    #[test]
    fn type_dependency() {
        let param:syn::GenericParam = syn::parse_str("T").unwrap();
        let depends                 = |code| {
            let ty:syn::Type = syn::parse_str(code).unwrap();
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
}
