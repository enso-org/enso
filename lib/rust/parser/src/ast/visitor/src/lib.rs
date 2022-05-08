//! Definition of [`Visitor`] deriving. It implements the visitor pattern for [`Ast`].

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


extern crate proc_macro;

use enso_macro_utils::field_names;
use enso_macro_utils::identifier_sequence;
use enso_macro_utils::index_sequence;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Data;
use syn::DataEnum;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Fields;
use syn::Variant;



/// ======================
/// === Derive Visitor ===
/// ======================

/// Implements [`AstVisitable`], [`AstVisitableMut`], [`SpanVisitable`], and [`SpanVisitableMut`].
/// These traits are defined in the [`crate::ast`] module. Macros in this module hardcode the names
/// of the traits and are not implemented in a generic way because the current Rust implementation
/// does not understand generic definition. See the [`crate::ast`] module to learn more about the
/// design and the Rust compiler issue.
#[proc_macro_derive(Visitor)]
pub fn derive_visitor(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl = syn::parse_macro_input!(input as DeriveInput);
    let ident = &decl.ident;
    let (_impl_generics, ty_generics, _inherent_where_clause_opt) = &decl.generics.split_for_impl();
    let body = gen_body(quote!(AstVisitable::visit), &decl.data, false);
    let body_mut = gen_body(quote!(AstVisitableMut::visit_mut), &decl.data, true);
    let body_span = gen_body(quote!(SpanVisitable::visit_span), &decl.data, false);
    let body_span_mut = gen_body(quote!(SpanVisitableMut::visit_span_mut), &decl.data, true);

    let output = quote! {
        impl<'a> AstVisitable<'a> for #ident #ty_generics {
            fn visit<T: AstVisitor<'a>>(&'a self, visitor:&mut T) {
                visitor.before_visiting_children();
                #body
                visitor.after_visiting_children();
            }
        }

        impl<'a> AstVisitableMut<'a> for #ident #ty_generics {
            fn visit_mut<T: AstVisitorMut>(&'a mut self, visitor:&mut T) {
                visitor.before_visiting_children();
                #body_mut
                visitor.after_visiting_children();
            }
        }

        impl<'a> SpanVisitable<'a> for #ident #ty_generics {
            fn visit_span<T: SpanVisitor<'a>>(&'a self, visitor:&mut T) {
                visitor.before_visiting_children();
                #body_span
                visitor.after_visiting_children();
            }
        }

        impl<'a> SpanVisitableMut<'a> for #ident #ty_generics {
            fn visit_span_mut<T: SpanVisitorMut>(&'a mut self, visitor:&mut T) {
                visitor.before_visiting_children();
                #body_span_mut
                visitor.after_visiting_children();
            }
        }
    };
    output.into()
}

fn gen_body(f: TokenStream, data: &Data, is_mut: bool) -> TokenStream {
    match data {
        Data::Struct(t) => body_for_struct(&f, t, is_mut),
        Data::Enum(t) => body_for_enum(&f, t),
        Data::Union(_) => panic!("Untagged union types not supported."),
    }
}

fn body_for_struct(f: &TokenStream, data: &DataStruct, is_mut: bool) -> TokenStream {
    match &data.fields {
        Fields::Unit => quote!({}),
        Fields::Unnamed(fields) => {
            let indices = index_sequence(fields.unnamed.len());
            if is_mut {
                quote!(#( #f(&mut self.#indices, visitor); )*)
            } else {
                quote!(#( #f(&self.#indices, visitor); )*)
            }
        }
        Fields::Named(fields) => {
            let names = field_names(fields);
            if is_mut {
                quote!(#( #f(&mut self.#names, visitor); )*)
            } else {
                quote!(#( #f(&self.#names, visitor); )*)
            }
        }
    }
}

/// Prepares a match arm for a single variant that `clone_ref`s such value.
fn arm_for_variant(f: &TokenStream, variant: &Variant) -> TokenStream {
    let variant_ident = &variant.ident;
    match &variant.fields {
        Fields::Unit => {
            quote!(Self::#variant_ident => {})
        }
        Fields::Named(fields) => {
            let names = field_names(fields);
            quote!(Self::#variant_ident { #(#names),* } => {
                #( #f(#names, visitor); )*
            })
        }
        Fields::Unnamed(fields) => {
            let names = identifier_sequence(fields.unnamed.len());
            quote!(Self::#variant_ident(#(#names),*) => {
                #( #f(#names, visitor); )*
            })
        }
    }
}

fn body_for_enum(f: &TokenStream, data: &DataEnum) -> TokenStream {
    let make_arm = |variant| arm_for_variant(f, variant);
    let arms = data.variants.iter().map(make_arm);
    let body = quote!(match self { #(#arms)* });
    body
}
