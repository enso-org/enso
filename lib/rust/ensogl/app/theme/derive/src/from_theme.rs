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


// ==================
// === ThemeTypes ===
// ==================

/// Provides a convenient way to reason bout the three types that have special theme support.
enum ThemeTypes {
    Color,
    String,
    Number,
}

impl ThemeTypes {
    /// Return the corresponging [`ThemeType`] for the given [`Type`]. Panics with an error message
    ///  indicating the wrong type if this is not a alidt heme type.
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
            _ => panic!(
                "The type `{:?}` cannot be derived from a theme. Only `String`,\
             `color::Rgba` and `f32` are supported.",
                ty
            ),
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

/// Create the field initializers for the struct holding the theme values. They differ between
/// types that implement copy or clone.
fn make_struct_inits(
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> TokenStream {
    let mut combined = TokenStream::new();

    for field in fields.iter() {
        let field_name = field
            .ident
            .as_ref()
            .expect("Encountered unnamed struct field. This cannot not happen.");
        let init = match ThemeTypes::from_ty(&field.ty) {
            ThemeTypes::Color | ThemeTypes::Number => quote! {
                #field_name:*#field_name,
            },
            ThemeTypes::String => quote! {
                #field_name:#field_name.clone(),
            },
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
/// the `base_path` attribute.
fn get_path_from_metadata(metadata: &[syn::Attribute]) -> Option<Path> {
    let mut base_path = None;
    for attr in metadata.iter() {
        match attr.parse_meta() {
            Ok(syn::Meta::NameValue(syn::MetaNameValue { ref path, ref lit, .. }))
                if path.get_ident().unwrap() == BASE_PATH_ATTRIBUTE_NAME =>
            {
                let module_name = lit.into_token_stream().to_string().replace('\"', "");
                let path: Path = syn::parse_str(&module_name)
                    .expect("Could not parse value as valid module path.");
                base_path = Some(path);
            }
            _ => (),
        }
    }
    base_path
}

/// Return the value of the `base_path` metadata value on the given [`syn::Field`].
fn get_path_metadata_for_field(field: &syn::Field) -> Option<Path> {
    get_path_from_metadata(&field.attrs)
}

/// Return the value of the `base_path` metadata value on the given [`DeriveInput].
fn get_path_metadata_for_struct(data_struct: &DeriveInput) -> Option<Path> {
    get_path_from_metadata(&data_struct.attrs)
}

/// Creates the code that accesses the values from the theme and provides the FRP endpoints for each
/// value. The FRP endpoints are typed according to the type of the value in the struct where the
/// derive is applied. Invalid dtypes lead to a panic with an error message indicating the issue.
fn make_theme_getters(
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> TokenStream {
    fields
        .iter()
        .map(|f| {
            let field_name = &f.ident;
            let field_base_path = get_path_metadata_for_field(f);

            let accessor = match ThemeTypes::from_ty(&f.ty) {
                ThemeTypes::Color => quote! {
                  get_color
                },
                ThemeTypes::String => quote! {
                  get_text
                },
                ThemeTypes::Number => quote! {
                  get_number
                },
            };
            if let Some(field_base_path) = field_base_path {
                quote! {
                    let #field_name = {
                        use #field_base_path as theme_path;
                        style.#accessor(theme_path::#field_name)
                    };
                }
            } else {
                quote! {
                    let #field_name = style.#accessor(theme_path::#field_name);
                }
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

    let base_path = get_path_metadata_for_struct(&input)
        .expect("Attribute `base_path` required to derive `FromTheme`.");

    let fields = match input.data {
        Data::Struct(DataStruct { fields: Fields::Named(fields), .. }) => fields.named,
        _ => panic!("The `FromTheme` derive macro only works on structs with named fields"),
    };

    let theme_getters = make_theme_getters(&fields);
    let st_name = input.ident;
    let frp_network_description = build_frp(&st_name, &fields);
    let update_struct_name = format_ident!("{}FromThemeFRP", st_name);
    let out = quote! {
        #[automatically_derived]
        struct #update_struct_name {
           update: enso_frp::Stream<#st_name>,
            init: enso_frp::stream::WeakNode<enso_frp::SourceData>
        }

        #[automatically_derived]
        impl #st_name {
            pub fn from_theme(network: &enso_frp::Network, style: &StyleWatchFrp) -> #update_struct_name {
                use #base_path as theme_path;
                #theme_getters
                #frp_network_description
                #update_struct_name {update:layout_update,init}
            }
        }
    };
    out
}
