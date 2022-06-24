//! # Rust reflection
//!
//! This crate implements a macro, `#[derive(Reflect)]`, which adds runtime reflection support to
//! datatype definitions. Its main motivation is type-driven code generation.
//!
//! ## Using `#[derive(Reflect)]` versus writing a proc macro
//!
//! Proc macros have some limitations. A proc macro should be:
//! - A pure function
//! - from syntax to syntax
//! - operating on each item in isolation.
//!
//! This crate doesn't have these limitations; it supports supports reasoning about the whole
//! typegraph, and has no unusual restrictions about side effects. However, a user of this crate
//! must depend on its subject code to obtain the reflection data at runtime; so automatic
//! generation of Rust code during compilation requires use of a build script to perform the
//! reflection/code-generation step.

// === Features ===
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

use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::Token;

mod analyze;
mod runtime;

use runtime::Quote;



// ========================
// === Type Definitions ===
// ========================

/// A complete *shallow* description of a type definition.
#[derive(Debug)]
pub(crate) struct Type {
    ident:          syn::Ident,
    generics:       Punctuated<TokenStream, Token![,]>,
    lifetimes:      Punctuated<TokenStream, Token![,]>,
    generic_params: Punctuated<TokenStream, Token![,]>,
    data:           Data,
    attrs:          ContainerAttrs,
}

#[derive(Debug)]
enum Data {
    Struct(Fields),
    Enum(Vec<Variant>),
}

#[derive(Debug)]
struct NamedField {
    name:    syn::Ident,
    type_:   syn::Type,
    subtype: bool,
    refer:   Option<syn::Type>,
    flatten: bool,
    hide:    bool,
}

#[derive(Debug)]
struct UnnamedField {
    type_: syn::Type,
}

#[derive(Debug)]
enum Fields {
    Named { fields: Vec<NamedField> },
    Unnamed(Vec<UnnamedField>),
    Unit,
}

#[derive(Debug)]
struct Variant {
    ident:       syn::Ident,
    fields:      Fields,
    transparent: bool,
}

#[derive(Debug, Default)]
struct ContainerAttrs {
    /// If true, the container must have exactly one field; the container will not appear in
    /// reflection data; all references to it are treated as references to the contained type.
    transparent: bool,
}



// ======================
// === Derive Reflect ===
// ======================

/// Derive a function providing information at runtime about the type's definition. See [`crate`]
/// for detailed documentation.
#[proc_macro_derive(Reflect, attributes(reflect))]
pub fn derive_reflect(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let type_ = analyze::analyze(input.into());
    let ident = &type_.ident;
    let generics = &type_.generics;
    let mut generic_bounds = type_.lifetimes.clone();
    let type_bounds = type_
        .generic_params
        .iter()
        .map(|param| (quote! { #param: reflect::Reflect }).into_token_stream());
    generic_bounds.extend(type_bounds);
    let type_expr = type_.quote();
    let static_lifetimes: Vec<_> = type_.lifetimes.iter().map(|_| quote! { 'static }).collect();
    let static_types = type_
        .generic_params
        .iter()
        .map(|param| (quote! { <#param as reflect::Reflect>::Static }).into_token_stream());
    let mut static_params = vec![];
    static_params.extend(static_lifetimes.iter().cloned());
    static_params.extend(static_types);
    let mut subtype_erased = quote! { Self::Static };
    if let Some(ty) = subtype_field_type(&type_.data) {
        // If we find the field's type in the parameters:
        // - Create a dummy type as a `dyn` of all traits bounds we have for that parameter.
        //   - (This won't support for unsized types.)
        // - Use our lifetime-erased type, with the matching parameter instantiated with the dummy
        //   type.
        // (This will only support a field whose type is exactly a type parameter. A more general
        //  implementation that erases one type parameter found recursively in the field type would
        //  support things like Box<Param> subtypes.)
        // TODO: Use a dyn object of satisfying all necessary trait bounds.
        //  Not needed for `enso-parser` because we aren't using any structs with traits bounds.
        // TODO: We should validate that the parameter doesn't occur in the type of any other field.
        let erased_types = type_.generic_params.iter().cloned().map(|param| {
            let param_ty: syn::Type = syn::parse2(param.clone()).unwrap();
            if param_ty == ty {
                quote! { () }
            } else {
                param
            }
        });
        let mut erased_params = vec![];
        erased_params.extend(static_lifetimes.iter().cloned());
        erased_params.extend(erased_types);
        subtype_erased = quote! { #ident<#(#erased_params),*> };
    }
    let impl_reflect = quote! {
        impl<#generic_bounds> reflect::Reflect for #ident<#generics> {
            type Static = #ident<#(#static_params),*>;
            type SubtypeErased = #subtype_erased;
            fn reflect() -> reflect::rust::TypeData {
                #type_expr
            }
        }
    };
    impl_reflect.into()
}

fn subtype_field_type(data: &Data) -> Option<syn::Type> {
    match data {
        Data::Struct(Fields::Named { fields }) => {
            let mut type_ = None;
            for field in fields {
                if field.subtype {
                    let err = "A struct cannot have more than one field with #[reflect(subtype)].";
                    assert_eq!(type_, None, "{}", err);
                    type_ = Some(field.type_.clone());
                }
            }
            type_
        }
        _ => None,
    }
}
