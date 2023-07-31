//! A proc-macro supporting generation of `ensogl_core::display::Object` implementations.
//! Invoked as: `#[derive(display::Object)]` (after importing `ensogl_core::display`).

// === Features ===
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(proc_macro_span)]
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



// =================
// === Constants ===
// =================

/// Identifies the field that provides a display object instance.
const DISPLAY_OBJECT: &str = "display_object";
/// Identifies the field that provides the display object that receives focus.
const FOCUS_RECEIVER: &str = "focus_receiver";



// ================================
// === Derive `display::Object` ===
// ================================

/// Derive [`display::Object`] for the annotated `struct`, by delegating its functions to the values
/// of its fields.
///
/// ## Determining the `display_object` field
///
/// The decision of which field is the `display_object` field is complex, but the result should be
/// intuitive:
/// - If there is exactly one field, it is the `display_object` field.
/// - If there is a field named `display_object`, it is the `display_object` field.
/// - Otherwise, the ambiguity must be resolved by annotating some field with `#[display_object]`.
///
/// ## Determining the `focus_receiver` field
///
/// By default, `focus_receiver()` delegates to the same field as `display_object()`. However, this
/// can be overridden by annotating some field with `#[focus_receiver]`.
///
/// # Limitations
///
/// - The annotated type must be a struct with named fields.
/// - The types of the `display_object` and `focus_receiver` fields must be invariant to the
///   parameters of the annotated type.
#[proc_macro_derive(Object, attributes(display_object, focus_receiver))]
pub fn derive_display_object(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let syn::Data::Struct(data) = &input.data else {
        panic!("Only `struct` declarations are supported.")
    };
    let struct_ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let syn::Fields::Named(fields) = &data.fields else {
        panic!("Only named-field structs are supported.")
    };
    let FieldData { display_object, focus_receiver } = FieldData::analyze(fields);
    let display_object_ident = display_object.ident.as_ref().unwrap();
    let focus_receiver_ident = focus_receiver.ident.as_ref().unwrap();
    let label = make_label(struct_ident);
    (quote::quote! {
        impl #impl_generics display::Object for #struct_ident #ty_generics #where_clause {
            fn display_object(&self) -> &display::object::Instance {
                self.#display_object_ident.display_object()
            }

            fn focus_receiver(&self) -> &display::object::Instance {
                self.#focus_receiver_ident.focus_receiver()
            }

            fn object_type(&self) -> Option<&'static str> {
                Some(#label)
            }
        }
    })
    .into()
}



// ================================================
// === Trait data determined from struct fields ===
// ================================================

struct FieldData<'a> {
    display_object: &'a syn::Field,
    focus_receiver: &'a syn::Field,
}

impl<'a> FieldData<'a> {
    fn analyze(fields: &'a syn::FieldsNamed) -> Self {
        let mut display_object_field = vec![];
        let mut focus_receiver_field = vec![];
        let mut field_named_display_object = None;
        let is_attr = |name: &str, a: &syn::Attribute| {
            a.path().segments.len() < 2
                && a.path()
                    .segments
                    .first()
                    .map(|segment| segment.ident == name)
                    .unwrap_or_default()
        };
        for field in &fields.named {
            if field.ident.as_ref().unwrap() == DISPLAY_OBJECT {
                field_named_display_object = Some(field);
            }
            for attr in &field.attrs {
                if is_attr(DISPLAY_OBJECT, attr) {
                    display_object_field.push(field);
                }
                if is_attr(FOCUS_RECEIVER, attr) {
                    focus_receiver_field.push(field);
                }
            }
        }
        let sole_field = (fields.named.len() < 2).then_some(()).and_then(|_| fields.named.first());
        let display_object = match display_object_field[..] {
            [] if let Some(field) = sole_field => field,
            [] if let Some(field) = field_named_display_object => field,
            [] => panic!("`{DISPLAY_OBJECT}` attribute must be applied to identify a field."),
            [field] => field,
            _ => panic!("`{DISPLAY_OBJECT}` attribute must not be applied more than once."),
        };
        let focus_receiver = match focus_receiver_field[..] {
            [] => display_object,
            [field] => field,
            _ => panic!("`{FOCUS_RECEIVER}` attribute must not be applied more than once."),
        };
        FieldData { display_object, focus_receiver }
    }
}



// ================================
// === Trait data for debugging ===
// ================================

fn make_label<L: core::fmt::Display>(name: L) -> String {
    let span = proc_macro::Span::call_site();
    let file = span.source_file().path();
    let path = file.as_path().to_string_lossy();
    let line = span.start().line;
    format!("{name} ({path}:{line})")
}
