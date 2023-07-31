//! This module defines a macro that makes it easy to access multiple values from a theme at the
//! same time. It creates a FRP endpoint that contains all the desired values, and will be updated
//! if any of the values changes.

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::DeriveInput;
use syn::Path;
use syn_1 as syn;



// =================
// === Constant ===
// =================

const BASE_PATH_ATTRIBUTE_NAME: &str = "base_path";
// Sadly we can't use `path` here, because it conflicts with Rust's builtin attribute.
const PATH_ATTRIBUTE_NAME: &str = "theme_path";



// =================
// === FromTheme ===
// =================


/// Builds an FRP that combines all the single FRP endpoints from the theme into a single FRP output
/// that contains all the values in the struct the derive was applied on.
fn build_frp(
    struct_name: &Ident,
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> TokenStream {
    let mut struct_init = TokenStream::new();
    let mut frp_content = TokenStream::new();
    for field in fields.iter() {
        let field_name =
            field.ident.as_ref().expect("Encountered unnamed struct field. This cannot happen.");
        frp_content.extend(quote! {
            layout_update_init_debounced <+ #field_name;
        });
        struct_init.extend(quote! {
            #field_name : #field_name.value(),
        });
    }

    quote! {
        __ensogl_core::frp::extend! { network
            layout_update_init_debounced <- any_(...);
            layout_update_needed <- any_(...);
            layout_update_needed <+ layout_update_init_debounced.debounce();
            #frp_content
            layout_sampler <- layout_update_needed.map(move |()| {
                #struct_name {
                    #struct_init
                }
            }).sampler();
        }
        // In order to make sure that the style value is initialized on first access, we need to
        // force an update immediately. Then, in order to fire the update after FRP network
        // initialization, we need to emit the init value in the next microtask.
        layout_update_needed.emit();
        layout_update_init_debounced.emit();
    }
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
            let field_type = &f.ty;
            let field_path = get_path_metadata_for_field(f, PATH_ATTRIBUTE_NAME);
            let field_path = match (base_path, field_path) {
                (Some(base_path), None) => quote! { #base_path::#field_name },
                (_, Some(field_path)) => quote! { #field_path },
                (None, None) => panic!("Neither `base_path` nor `path` attributes were set."),
            };

            quote! {
                let #field_name = style.access::<#field_type>(#field_path);
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

    let ensogl_core = ensogl_core_crate();
    let base_path = get_path_metadata_for_struct(&input, BASE_PATH_ATTRIBUTE_NAME);

    let fields = match input.data {
        Data::Struct(DataStruct { fields: Fields::Named(fields), .. }) => fields.named,
        _ => panic!("The `FromTheme` derive macro only works on structs with named fields"),
    };

    let theme_getters = make_theme_getters(&fields, base_path.as_ref());
    let st_name = input.ident;
    let frp_network_description = build_frp(&st_name, &fields);
    let out = quote! {
        #[automatically_derived]
        impl #ensogl_core::display::style::FromTheme for #st_name {
            fn from_theme(
                network: &#ensogl_core::frp::Network,
                style: &#ensogl_core::display::shape::StyleWatchFrp,
            ) -> #ensogl_core::frp::Sampler<#st_name> {
                use #ensogl_core as __ensogl_core;
                #theme_getters
                #frp_network_description
                layout_sampler
            }
        }
    };
    out
}

fn ensogl_core_crate() -> Ident {
    use proc_macro_crate::crate_name;
    use proc_macro_crate::FoundCrate;

    match crate_name("ensogl-core").or_else(|_| crate_name("ensogl")) {
        Ok(FoundCrate::Itself) => Ident::new("crate", Span::call_site()),
        Ok(FoundCrate::Name(name)) => Ident::new(&name, Span::call_site()),
        Err(proc_macro_crate::Error::CrateNotFound { .. }) =>
            panic!("Could not find 'ensogl-core' or 'ensogl' in dependencies."),
        Err(e) => panic!("{e:?}"),
    }
}
