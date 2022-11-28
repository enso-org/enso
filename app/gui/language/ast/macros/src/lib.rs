//! Helper macros used when defining AST structures.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]



extern crate proc_macro;

mod token;

use crate::token::TokenDescription;

use enso_macro_utils::gather_all_type_reprs;
use enso_macro_utils::repr;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashSet;



// ==============
// === Macros ===
// ==============

/// A macro that shall be applied to all AST nodes.
///
/// Derives all the traits that are expected to be implemented by AST nodes.
///
/// Implicitly applied by `ast` on target and generated types. User should not
/// need to use this macro directly.
#[proc_macro_attribute]
pub fn ast_node(
    _meta: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let output = quote! {
        #[derive(Clone,Eq,PartialEq,Debug)]
        #[derive(Iterator)]
        #[derive(Serialize,Deserialize)]
        #input
    };
    output.into()
}

/// Marks target declaration as `ast_node`. If it is an enumeration, also
/// applies `to_variant_types`.
#[proc_macro_attribute]
pub fn ast(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs: TokenStream = attrs.into();
    let decl = syn::parse_macro_input!(input as syn::DeriveInput);
    let output = match &decl.data {
        syn::Data::Enum { .. } => quote! {
            #[to_variant_types(#attrs)]
            #[ast_node]
            #decl
        },
        _ => quote! {
            #[ast_node]
            #decl
        },
    };
    output.into()
}

// ==============
// === Macros ===
// ==============

// Note [Expansion Example]
// ~~~~~~~~~~~~~~~~~~~~~~~~
// In order to make the definition easier to read, an example expansion of the
// following definition was provided for each quotation:
//
// #[to_variant_types]
// pub enum Shape<T> {
//     Var(Var),
//     App(App<T>),
// }

/// Produces declaration of the structure for given source enum variant.
fn mk_product_type(
    is_flat: bool,
    decl: &syn::DeriveInput,
    variant: &syn::Variant,
) -> syn::ItemStruct {
    use syn::ItemStruct;
    let fields = &variant.fields;
    let fields = fields.iter();
    let types = fields.flat_map(|f| gather_all_type_reprs(&f.ty));
    let types = types.collect::<HashSet<_>>();
    let ty_vars = decl.generics.params.iter().cloned();
    let params = ty_vars.filter(|v| types.contains(&repr(&v))).collect();
    let attrs = decl.attrs.clone();
    let vis = decl.vis.clone();
    let struct_token = syn::token::Struct { span: Span::call_site() };
    let ident_flat = variant.ident.clone();
    let ident_nested = format!("{}{}", decl.ident, variant.ident);
    let ident_nested = Ident::new(&ident_nested, Span::call_site());
    let ident = if is_flat { ident_flat } else { ident_nested };
    let generics = syn::Generics { params, ..Default::default() };
    let mut fields = variant.fields.clone();
    let semi_token = None;
    fields.iter_mut().for_each(|f| f.vis = vis.clone());
    ItemStruct { attrs, vis, struct_token, ident, generics, fields, semi_token }
}

/// Generates rewritten enumeration declaration.
///
/// Each constructor will be a single-elem tuple holder for extracted type.
fn gen_variant_decl(ident: &syn::Ident, variant: &syn::ItemStruct) -> TokenStream {
    let variant_ident = &variant.ident;
    let params = variant.generics.params.iter();
    quote! {
        // See note [Expansion Example]
        // App(ShapeApp<T>),
        // Var(ShapeVar),
        #ident(#variant_ident<#(#params),*>)
    }
}

/// Generate `From` trait implementations converting from each of extracted
/// types back into primary enumeration.
/// Generate `TryFrom` implementation from primary enumeration into each
/// extracted type.
#[allow(clippy::cognitive_complexity)]
fn gen_from_impls(
    ident: &syn::Ident,
    decl: &syn::DeriveInput,
    variant: &syn::ItemStruct,
) -> TokenStream {
    let sum_label = &decl.ident;
    let variant_label = &variant.ident;
    let variant_name = variant_label.to_string();

    let sum_params = &decl.generics.params.iter().cloned().collect::<Vec<_>>();
    let variant_params = &variant.generics.params.iter().cloned().collect::<Vec<_>>();

    quote! {
        // See note [Expansion Example]
        // impl<T> From<App<T>> for Shape<T> {
        //     fn from(t: App<T>) -> Self { Shape::App(t) }
        // }
        // ...
        impl<#(#sum_params),*> From<#variant_label<#(#variant_params),*>>
        for #sum_label<#(#sum_params),*> {
            fn from(t: #variant_label<#(#variant_params),*>) -> Self {
                #sum_label::#ident(t)
            }
        }


        // impl<'t, T> TryFrom<&'t Shape<T>> for &'t Infix<T> {
        //     type Error = WrongEnum;
        //     fn try_from(value: &'t Shape<T>) -> Result<Self, Self::Error> {
        //         match value {
        //             Shape::Infix(elem) => Ok (elem),
        //             _ => {
        //                 let error = WrongEnum {
        //                     expected_con : "Infix" };
        //                 Err(error)
        //             },
        //         }
        //     }
        // }
        impl<'t, #(#sum_params),*> TryFrom<&'t #sum_label<#(#sum_params),*>>
        for &'t #variant_label<#(#variant_params),*> {
            type Error = WrongEnum;

            fn try_from
            (value: &'t #sum_label<#(#sum_params),*>)
            -> Result<Self, Self::Error> {
                match value {
                    #sum_label::#ident(elem) => Ok(elem),
                    _  => {
                        let error = WrongEnum {
                            expected_con: #variant_name.to_string() };
                        Err(error)
                    },
                }
            }
        }

        // same as above but for values
        impl<#(#sum_params),*> TryFrom<#sum_label<#(#sum_params),*>>
        for #variant_label<#(#variant_params),*> {
            type Error = WrongEnum;

            fn try_from
            (value: #sum_label<#(#sum_params),*>)
            -> Result<Self, Self::Error> {
                match value {
                    #sum_label::#ident(elem) => Ok(elem),
                    _  => {
                        let error = WrongEnum {
                            expected_con: #variant_name.to_string() };
                        Err(error)
                    },
                }
            }
        }
    }
}

/// Rewrites enum definition by creating a new type for each constructor.
///
/// Each nested constructor will be converted to a new `struct` and placed in
/// the parent scope. The created type name will be {EnumName}{ConstructorName}.
/// To name generated types with only their constructor name, use `flat`
/// attribute: `#[ast(flat)]`.
#[proc_macro_attribute]
pub fn to_variant_types(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs: TokenStream = attrs.into();
    let decl = syn::parse_macro_input!(input as syn::DeriveInput);
    let ident = &decl.ident;
    let ty_vars = &decl.generics.params;
    let variants = match &decl.data {
        syn::Data::Enum(ref data) => data.variants.iter(),
        _ => unimplemented!(),
    }
    .collect::<Vec<_>>();

    let is_flat = repr(&attrs) == "flat";
    let structs = variants.iter().map(|v| mk_product_type(is_flat, &decl, v));
    let structs = structs.collect::<Vec<_>>();

    let variant_idents = variants.iter().map(|v| &v.ident).collect::<Vec<_>>();
    let variant_decls =
        variant_idents.iter().zip(structs.iter()).map(|(i, v)| gen_variant_decl(i, v));
    let variant_froms =
        variant_idents.iter().zip(structs.iter()).map(|(i, v)| gen_from_impls(i, &decl, v));

    // Handle single value, unnamed params as created by user.
    let structs = structs.iter().filter(|v| match &v.fields {
        syn::Fields::Unnamed(f) => f.unnamed.len() != 1,
        _ => true,
    });

    let decl_attrs = &decl.attrs;
    let output = quote! {
        #(#decl_attrs)*
        pub enum #ident <#ty_vars> {
            #(#variant_decls),*
        }
        #(#structs)*
        #(#variant_froms)*
    };
    output.into()
}

/// Creates a `HasTokens` implementations for a given enum type.
///
/// Given type may only consist of single-elem tuple-like variants.
/// The implementation uses underlying HasTokens implementation for
/// stored values.
#[proc_macro_derive(HasTokens)]
pub fn derive_has_tokens(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl = syn::parse_macro_input!(input as syn::DeriveInput);
    let ret = match decl.data {
        syn::Data::Enum(ref e) => token::derive_for_enum(&decl, e),
        _ => quote! {},
    };
    proc_macro::TokenStream::from(ret)
}

/// Provides only `HasTokens` implementation.
#[proc_macro]
pub fn has_tokens(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let maker = syn::parse::<TokenDescription>(input).unwrap();
    maker.has_tokens().into()
}

/// Generates `HasTokens` instances that are just sum of their parts.
///
/// Takes 1+ parameters:
/// * first goes the typename for which implementations are generated (can take type parameters, as
///   long as they implement `HasTokens`)
/// * then arbitrary number (0 or more) of expressions, that shall yield values implementing
///   `HasTokens`. The `self` can be used in th expressions.
///
/// For example, for invocation:
/// ```text
/// has_tokens!(SegmentExpr<T>, EXPR_QUOTE, self.value, EXPR_QUOTE);
/// ```
/// the following output is produced:
///    ```text
///    impl<T: HasTokens> HasTokens for SegmentExpr<T> {
///        fn feed_to(&self, consumer:&mut impl TokenConsumer) {
///            EXPR_QUOTE.feed(consumer);
///            self.value.feed(consumer);
///            EXPR_QUOTE.feed(consumer);
///        }
///    }
///    ```

/// Generates `HasTokens` implementations for spaceless AST that panics when used.
#[proc_macro]
pub fn spaceless_ast(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    crate::token::spaceless_ast(input)
}
