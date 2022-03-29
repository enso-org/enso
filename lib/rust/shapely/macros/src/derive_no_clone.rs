use crate::prelude::*;

use syn::DeriveInput;



// ===================
// === Entry Point ===
// ===================

/// Makes sure that the structure does not derive [`Clone`] and that it implements custom [`Drop`]
/// implementation.
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl = syn::parse_macro_input!(input as DeriveInput);
    let ident = &decl.ident;
    let (impl_generics, ty_generics, _) = &decl.generics.split_for_impl();
    let output = quote! {
        impl #impl_generics !Clone for #ident #ty_generics {}
        impl #impl_generics ImplementsDrop for #ident #ty_generics {}
    };
    output.into()
}
