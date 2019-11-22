use prelude::*;

use quote::quote;
use syn;

/// Obtains text representation of given `ToTokens`-compatible input.
pub fn repr<T: quote::ToTokens>(t: &T) -> String {
    quote!(#t).to_string()
}

/// Collects all fields, named or not.
pub fn fields_list(fields: &syn::Fields) -> Vec<&syn::Field> {
    match fields {
        syn::Fields::Named(ref f) => { f.named.iter().collect() }
        syn::Fields::Unnamed(ref f) => { f.unnamed.iter().collect() }
        syn::Fields::Unit => default()
    }
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

        fn assert_field_types(program: &str, expected_types: &[&str]) {
            let tokens = parse_str::<syn::ItemStruct>(program).unwrap();
            let fields = fields_list(&tokens.fields);
            let types  = fields.iter().map(|f| repr(&f.ty));
            assert_eq!(Vec::from_iter(types), expected_types);
        }

        assert_field_types(tuple_like, &expected_types);
        assert_field_types(proper_struct, &expected_types);
    }
}
