//! # Rust reflection
//!
//! This crate implements a macro, `#[derive(Reflect)]`, which adds runtime reflection support to
//! datatype definitions. Its main motivation is type-driven code generation.
//!
//! ## General Attributes
//!
//! ### `#[reflect(skip)]` (field attribute)
//! The field will be excluded from reflection data.
//! When this attribute is present, the field's type does not need to implement `Reflect`.
//!
//! ### `#[reflect(as = "OtherType")]` (field attribute)
//! The field's type in the reflection data will be `OtherType` rather than the field's real type.
//! When this attribute is present, the field's real type does not need to implement `Reflect`. The
//! alternative type specified must implement `Reflect`.
//!
//! ## Attributes for Abstraction Customization
//!
//! Application of `#[derive(Reflect)]` to data types is enough to enable reflection over Rust
//! types. However, if the types will be abstracted with `enso_metamodel::meta` (i.e. for
//! transpilation to another language), some customization is likely: Direct translation into
//! another language would reproduce Rust patterns where they are likely not necessary (on top of
//! the target-language patterns introduced by the translation), resulting in an overly-complex
//! data model. In order to avert this (without using heuristics, which would result in
//! unpredictable output), this crate supports helper attributes to inform the abstractor about the
//! use of Rust patterns that can be erased in translation.
//!
//! ### `#[reflect(transparent)]` (struct attribute)
//! Only applicable to single-field `struct`s. The type will be not appear in abstracted reflection
//! data; all references will appear as references to the contained type.
//!
//! ### `#[reflect(hide)]` (field attribute)
//! In target languages that support it, the field will be hidden from direct access. In the Java
//! target, this prevents the generation of accessors.
//!
//! ### `#[reflect(rename)]` (field attribute)
//! Represent the type as if the field were defined with a different name. This is useful if the
//! field's name is mangled to avoid colliding with a Rust keyword, like `type`.
//!
//! ### `#[reflect(flatten)]` (field attribute)
//! In abstracted reflection data, the field will be replaced in this `struct` with the contents of
//! its type, which must be a `struct` type.
//!
//! To reduce the chance of name conflicts, the names of inserted fields will be created by
//! prepending the name of the flattened-away field to the names of the fields originating from the
//! inner type. Other field attributes such as [`hide`](#reflecthide-field-attribute) that were
//! applied to the flattened field will be inherited by the inserted fields.
//!
//! #### Example:
//! This input code:
//! ```ignore
//! #[derive(Reflect)]
//! struct Outer {
//!     first: u32,
//!     #[reflect(flatten, hide)]
//!     inner: Inner,
//!     last: u32,
//! }
//!
//! #[derive(Reflect)]
//! struct Inner {
//!     value0: u32,
//!     value1: u32,
//! }
//! ```
//!
//! Will be represented the same as this input:
//! ```ignore
//! #[derive(Reflect)]
//! struct Outer {
//!     first: u32,
//!     #[reflect(hide)]
//!     inner_value0: u32,
//!     #[reflect(hide)]
//!     inner_value1: u32,
//!     last: u32,
//! }
//! ```
//!
//! ### `#[reflect(subtype)]` (field attribute)
//! In the abstracted representation, the containing type will be made the parent of the field's
//! type. There must be no references to the field's type except through the containing type. The
//! field's type must be an `enum`, or a generic parameter.
//! If the field's type is a generic parameter, the parameter must be instantiated with one `enum`,
//! and may be instantiated with any types that are members of the `enum` (this can only occur with
//! `#[reflect(inline)]`; see below). References to the type instantiated with the `enum` will
//! become references to the resulting parent type; references to `struct` instantiatons will become
//! references to the resulting child types.
//!
//! ### `#[reflect(inline)]` (`enum` variant attribute)
//! In the abstracted representation, no type will be generated for the variant (which must be a
//! single-field variant); the contained type will instead by treated as a member of the `enum`.
//!
//! # Using `#[derive(Reflect)]` versus writing a proc macro
//!
//! Proc macros have some limitations. A proc macro should be:
//! - A pure function
//! - from syntax to syntax
//! - operating on each item in isolation.
//!
//! This crate doesn't have these limitations; it supports reasoning about the whole typegraph, and
//! has no restrictions about side effects. However, a user of this crate must depend on its subject
//! code to obtain the reflection data at runtime; so automatic generation of Rust code during
//! compilation requires use of a build script to perform the reflection/code-generation step.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
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

/// Represents a type definition.
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
    refer:   Option<Box<syn::Type>>,
    flatten: bool,
    hide:    bool,
    rename:  Option<syn::LitStr>,
}

impl NamedField {
    pub fn new(name: syn::Ident, type_: syn::Type) -> Self {
        let subtype = Default::default();
        let refer = Default::default();
        let flatten = Default::default();
        let hide = Default::default();
        let rename = Default::default();
        Self { name, type_, subtype, refer, flatten, hide, rename }
    }
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
    let with_bound = |param| (quote! { #param: enso_reflect::Reflect }).into_token_stream();
    let type_bounds = type_.generic_params.iter().map(with_bound);
    generic_bounds.extend(type_bounds);
    let type_expr = type_.quote();
    let static_lifetimes: Vec<_> = type_.lifetimes.iter().map(|_| quote! { 'static }).collect();
    let to_static =
        |param| (quote! { <#param as enso_reflect::Reflect>::Static }).into_token_stream();
    let static_types = type_.generic_params.iter().map(to_static);
    let mut static_params = vec![];
    static_params.extend(static_lifetimes.iter().cloned());
    static_params.extend(static_types);
    let mut subtype_erased = quote! { Self::Static };
    if let Some(ty) = subtype_field_type(&type_.data) {
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
        impl<#generic_bounds> enso_reflect::Reflect for #ident<#generics> {
            type Static = #ident<#(#static_params),*>;
            type SubtypeErased = #subtype_erased;
            fn reflect() -> enso_reflect::metamodel::rust::TypeData {
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
