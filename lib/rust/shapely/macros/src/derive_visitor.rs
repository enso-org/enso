use crate::prelude::*;

use enso_macro_utils::field_names;
use enso_macro_utils::identifier_sequence;
use enso_macro_utils::index_sequence;
use enso_macro_utils::path_matching_ident;
use syn::Attribute;
use syn::Data;
use syn::DataEnum;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Fields;
use syn::Ident;
use syn::Lit;
use syn::Meta;
use syn::MetaNameValue;
use syn::NestedMeta;
use syn::Variant;
use syn::WhereClause;
use syn::WherePredicate;



pub trait HasFields {
    fn field_types(&self) -> Vec<syn::Type>;
}

impl HasFields for Variant {
    fn field_types(&self) -> Vec<syn::Type> {
        match &self.fields {
            Fields::Unit => Default::default(),
            Fields::Named(fields) => fields.named.iter().map(|t| t.ty.clone()).collect(),
            Fields::Unnamed(fields) => fields.unnamed.iter().map(|t| t.ty.clone()).collect(),
        }
    }
}

impl HasFields for DataEnum {
    fn field_types(&self) -> Vec<syn::Type> {
        self.variants.iter().map(|t| t.field_types()).flatten().collect()
    }
}

impl HasFields for Data {
    fn field_types(&self) -> Vec<syn::Type> {
        match self {
            Data::Enum(data) => data.variants.iter().map(|t| t.field_types()).flatten().collect(),
            Data::Union(data) => data.fields.named.iter().map(|t| t.ty.clone()).collect(),
            Data::Struct(data) => data.fields.iter().map(|t| t.ty.clone()).collect(),
        }
    }
}


// ============================
// === CloneRef for structs ===
// ============================

pub fn body_for_struct(f: &TokenStream, data: &DataStruct, is_mut: bool) -> TokenStream {
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
        _ => panic!(),
    }
}



// ==========================
// === CloneRef for enums ===
// ==========================

/// Prepares a match arm for a single variant that `clone_ref`s such value.
pub fn arm_for_variant(f: &TokenStream, variant: &Variant) -> TokenStream {
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



pub fn body_for_enum(f: &TokenStream, data: &DataEnum) -> TokenStream {
    let make_arm = |variant| arm_for_variant(f, variant);
    let arms = data.variants.iter().map(make_arm);
    let body = quote!(match self { #(#arms)* });
    body
}



// ===================
// === Entry Point ===
// ===================

pub fn gen_body(f: TokenStream, data: &Data, is_mut: bool) -> TokenStream {
    match data {
        Data::Struct(t) => body_for_struct(&f, t, is_mut),
        Data::Enum(t) => body_for_enum(&f, t),
        // Data::Union(_) => panic!("CloneRef cannot be derived for an untagged union input."),
        _ => panic!(),
    }
}

/// Derives `CloneRef` implementation, refer to `crate::derive_clone_ref` for details.
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
    // println!("{:#}", output);

    output.into()
}
