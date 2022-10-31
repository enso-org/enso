//! This module defines a macro that makes it easy to access multiple values from a theme at the
//! same time. It creates a FRP endpoint that contains all the desired values, and will be updated
//! if any of the values changes.

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use syn::DeriveInput;
use syn::Path;
use syn::Type;



// =================
// === Constant ===
// =================

const BASE_PATH_ATTRIBUTE_NAME: &str = "base_path";
// Sadly we can't use `path` here, because it conflicts with Rust's builtin attribute.
const PATH_ATTRIBUTE_NAME: &str = "theme_path";
const ACCESSOR_ATTRIBUTE_NAME: &str = "accessor";


// ==================
// === ThemeTypes ===
// ==================

/// Provides a convenient way to reason about the three types that have special theme support.
enum ThemeTypes {
    Color,
    String,
    Number,
    /// The type is not directly supported by the macro, but can be accessed using a custom
    /// accessor.
    Unknown,
}

impl ThemeTypes {
    /// Return the corresponging [`ThemeType`] for the given [`Type`].
    fn from_ty(ty: &Type) -> Self {
        match ty {
            Type::Path(type_path)
                if type_path.clone().into_token_stream().to_string().contains("ImString") =>
                Self::String,
            Type::Path(type_path) if type_path.clone().into_token_stream().to_string() == "f32" =>
                Self::Number,
            Type::Path(type_path)
                if type_path.clone().into_token_stream().to_string().contains("Rgba") =>
                Self::Color,
            _ => Self::Unknown,
        }
    }
}



// =================
// === FromTheme ===
// =================


/// Builds an FRP that combines all the single FRP endpoints from the theme into a single FRP output
/// that contains all the values in the struct the derive was applied on.
fn build_frp(
    struct_name: &Ident,
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> TokenStream {
    let mut frp_content = TokenStream::new();
    let mut prev_value: Option<Ident> = None;

    for field in fields.iter() {
        let field_name =
            field.ident.as_ref().expect("Encountered unnamed struct field. This cannot happen.");
        let field_update = format_ident!("{}_update", field_name);
        let update = if let Some(prev_value) = prev_value {
            quote! {
                #field_update <- all(&#prev_value, &#field_name);
            }
        } else {
            quote! {
                #field_update <- all(&init, &#field_name);
            }
        };
        frp_content.extend(update);
        prev_value = Some(field_update);
    }

    let destruct_pattern = make_destruct_pattern(fields);
    let struct_init = make_struct_inits(fields);
    let prev_value = prev_value.expect("Struct did not contain any fields.");
    let struct_generation = quote! {
            layout_update <- #prev_value.map(|#destruct_pattern|{
            #struct_name {
                    #struct_init
                }
            });
    };
    frp_content.extend(struct_generation);
    quote! {
        frp::extend! { network
            init <- source_();
            #frp_content;
        }

    }
}

/// Create the field initializers for the struct holding the theme values.
fn make_struct_inits(
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> TokenStream {
    let mut combined = TokenStream::new();

    for field in fields.iter() {
        let field_name = field
            .ident
            .as_ref()
            .expect("Encountered unnamed struct field. This cannot not happen.");
        // Keep in mind that all [`Copy`] types also implement [`Clone`].
        let init = quote! {
            #field_name : #field_name.clone(),
        };
        combined.extend(init);
    }
    combined
}


/// Return a token stream that allows the destructuring of the tuple type that is created by the
/// FRP created in `build_frp`.
fn make_destruct_pattern(
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> TokenStream {
    let mut combined = quote! { _ }; // Discard init argument, which is the innermost one.

    for field in fields.iter() {
        let field_name = field
            .ident
            .as_ref()
            .expect("Encountered unnamed struct field. This cannot not happen.");

        combined = quote! {
            ( #combined, #field_name)
        }
    }
    combined
}

/// Iterate the metadata in the list of attributes and return the first path that was supplied with
/// the `attribute_name` attribute.
fn get_path_from_metadata(metadata: &[syn::Attribute], attribute_name: &str) -> Option<Path> {
    let mut result = None;
    for attr in metadata.iter() {
        match attr.parse_meta() {
            Ok(syn::Meta::NameValue(syn::MetaNameValue { ref path, ref lit, .. }))
                if path.get_ident().unwrap() == attribute_name =>
            {
                let module_name = lit.into_token_stream().to_string().replace('\"', "");
                let path: Path = syn::parse_str(&module_name)
                    .expect("Could not parse value as valid module path.");
                result = Some(path);
            }
            _ => (),
        }
    }
    result
}

/// Return the value of the `attribute_name` metadata value on the given [`syn::Field`].
fn get_path_metadata_for_field(field: &syn::Field, attribute_name: &str) -> Option<Path> {
    get_path_from_metadata(&field.attrs, attribute_name)
}

/// Return the value of the `attribute_name` metadata value on the given [`DeriveInput].
fn get_path_metadata_for_struct(data_struct: &DeriveInput, attribute_name: &str) -> Option<Path> {
    get_path_from_metadata(&data_struct.attrs, attribute_name)
}

/// Creates the code that accesses the values from the theme and provides the FRP endpoints for each
/// value. The FRP endpoints are typed according to the type of the value in the struct where the
/// derive is applied. Invalid dtypes lead to a panic with an error message indicating the issue.
fn make_theme_getters(
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
    base_path: Option<&Path>,
) -> TokenStream {
    fields
        .iter()
        .map(|f| {
            let field_name = &f.ident;
            let field_path = get_path_metadata_for_field(f, PATH_ATTRIBUTE_NAME);
            let field_path = match (base_path, field_path) {
                (Some(base_path), None) => quote! { #base_path::#field_name },
                (_, Some(field_path)) => quote! { #field_path },
                (None, None) => panic!("Neither `base_path` nor `path` attributes were set."),
            };

            let accessor = get_path_from_metadata(&f.attrs, ACCESSOR_ATTRIBUTE_NAME)
                .map(|accessor| {
                    quote! { #accessor(&network, style, #field_path) }
                })
                .unwrap_or_else(|| match ThemeTypes::from_ty(&f.ty) {
                    ThemeTypes::Color => quote! {
                      StyleWatchFrp::get_color(style, #field_path)
                    },
                    ThemeTypes::String => quote! {
                      StyleWatchFrp::get_text(style, #field_path)
                    },
                    ThemeTypes::Number => quote! {
                      StyleWatchFrp::get_number(style, #field_path)
                    },
                    ThemeTypes::Unknown => panic!(
                        "Unknown type for theme value, but no accessor was provided. \
                        Use the `#[accessor]` attribute to provide a custom accessor function"
                    ),
                });

            quote! {
                let #field_name = #accessor;
            }
        })
        .collect()
}

/// Entry point for the derive macro. Creates an `impl` block with a `from_theme`  method and a
/// helper struct holding the FRP endpoint that provides the theme values, as well as an
/// initialisation endpoint that can be used to initialise the values in the caller, after setting
/// up the FRP network.
pub fn expand(input: DeriveInput) -> TokenStream {
    use syn::Data;
    use syn::DataStruct;
    use syn::Fields;

    let base_path = get_path_metadata_for_struct(&input, BASE_PATH_ATTRIBUTE_NAME);

    let fields = match input.data {
        Data::Struct(DataStruct { fields: Fields::Named(fields), .. }) => fields.named,
        _ => panic!("The `FromTheme` derive macro only works on structs with named fields"),
    };

    let theme_getters = make_theme_getters(&fields, base_path.as_ref());
    let st_name = input.ident;
    let frp_network_description = build_frp(&st_name, &fields);
    let update_struct_name = format_ident!("{}FromThemeFRP", st_name);
    let out = quote! {
        #[automatically_derived]
        /// FRP endpoints with [`#st_name`] derived from style FRP. Generated by [`FromTheme`]
        /// macro.
        pub struct #update_struct_name {
            /// Event emitted each time the style will be updated with the sructure derived from it.
            pub update: enso_frp::Stream<#st_name>,
            /// Emit this event when you want to initialize all nodes dependant on the `update`
            /// field.
            pub init: enso_frp::stream::WeakNode<enso_frp::SourceData>
        }

        #[automatically_derived]
        impl #st_name {
            /// Create FRP endpoints used to obtain [`#st_name`] from a style sheet.
            pub fn from_theme(network: &enso_frp::Network, style: &StyleWatchFrp) -> #update_struct_name {
                #theme_getters
                #frp_network_description
                #update_struct_name {update:layout_update,init}
            }
        }
    };
    out
}
