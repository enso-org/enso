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


// ==============
// === Consts ===
// ==============

/// Name of the custom attribute allowing customizing behavior of the generated `CloneRef`
/// implementation.
const CLONE_REF_ATTR: &str = "clone_ref";

/// Name of the property within customization attribute that allows defining custom bounds for
/// the generated `CloneRef` implementation.
const BOUND_NAME: &str = "bound";



// ============================
// === CloneRef for structs ===
// ============================

/// `clone_ref` function body for a given `struct` definition.
pub fn body_for_struct(ident: &Ident, data: &DataStruct) -> TokenStream {
    match data.fields {
        // Fields::Unit =>
        // // Foo
        //     quote!( #ident ),
        // Fields::Unnamed(ref fields) => {
        //     let indices = index_sequence(fields.unnamed.len());
        //     // Foo(self.0.clone_ref())
        //     quote!(
        //         #ident(#(self.#indices.clone_ref()),*)
        //     )
        // }
        Fields::Named(ref fields) => {
            let names = field_names(fields);
            quote!(
                let sub_visitor = self.sub_visitor();
                #( sub_visitor.visit(&elem.#names) );*
            )
        }
        _ => panic!(),
    }
}



// ==========================
// === CloneRef for enums ===
// ==========================

/// Prepares a match arm for a single variant that `clone_ref`s such value.
pub fn arm_for_variant(data_ident: &Ident, variant: &Variant) -> TokenStream {
    let fields = &variant.fields;
    let variant_ident = &variant.ident;
    match fields {
        Fields::Unit => {
            quote!(
                #data_ident::#variant_ident => {}
            )
        }
        // Fields::Named(fields) => {
        //     let names = field_names(fields);
        //     // Enum::Var {field0} => Enum::Var {field0 : field0.clone_ref()}
        //     quote!(
        //         #data_ident::#variant_ident { #(#names),* } =>
        //             #data_ident::#variant_ident {
        //                 #( #names : #names.clone_ref() ),*
        //             }
        //     )
        // }
        Fields::Unnamed(fields) => {
            let names = identifier_sequence(fields.unnamed.len());
            // Enum::Var(field0) => Enum::Var(field0.clone_ref())
            quote!(
                #data_ident::#variant_ident(#(#names),*) => {
                    let sub_visitor = self.sub_visitor();
                    #( sub_visitor.visit(#names) );*
                }
            )
        }
        _ => todo!(),
    }
}



pub fn body_for_enum(ident: &Ident, data: &DataEnum) -> TokenStream {
    if !data.variants.is_empty() {
        let make_arm = |variant| arm_for_variant(ident, variant);
        let arms = data.variants.iter().map(make_arm);
        let body = quote!(match elem { #(#arms)* });
        body
    } else {
        panic!()
    }
}



// ===================
// === Entry Point ===
// ===================

/// Derives `CloneRef` implementation, refer to `crate::derive_clone_ref` for details.
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let self_param = quote::format_ident!("T");

    let decl = syn::parse_macro_input!(input as DeriveInput);
    let ident = &decl.ident;
    let body = match &decl.data {
        Data::Struct(data_struct) => body_for_struct(ident, data_struct),
        Data::Enum(data_enum) => body_for_enum(ident, data_enum),
        // Data::Union(_) => panic!("CloneRef cannot be derived for an untagged union input."),
        _ => panic!(),
    };


    let (impl_generics, ty_generics, inherent_where_clause_opt) = &decl.generics.split_for_impl();



    let field_types = decl.data.field_types();
    let bounds = quote! {#self_param:Visitor, #(#self_param::SubVisitor: Visit<#field_types>,)*};
    let output = quote! {
        impl<#self_param> Visit <#ident #ty_generics> for #self_param
        where #bounds {
            fn visit(&mut self, elem:&#ident #ty_generics) {
                #body
            }
        }
    };
    println!("{:#}", output);

    output.into()
}



// pub fn bounds_for_variant(self_param: &Ident, variant: &Variant) -> TokenStream {
//     let fields = &variant.fields;
//     let variant_ident = &variant.ident;
//     match fields {
//         Fields::Unit => {
//             quote!()
//         }
//         // Fields::Named(fields) => {
//         //     let names = field_names(fields);
//         //     // Enum::Var {field0} => Enum::Var {field0 : field0.clone_ref()}
//         //     quote!(
//         //         #data_ident::#variant_ident { #(#names),* } =>
//         //             #data_ident::#variant_ident {
//         //                 #( #names : #names.clone_ref() ),*
//         //             }
//         //     )
//         // }
//         Fields::Unnamed(fields) => {
//             let bounds = fields.unnamed.iter().map(|t| {
//                 let ty = &t.ty;
//                 quote!(#self_param:Visitor<#ty>,)
//             });
//             quote!(#( #bounds )*)
//         }
//         _ => todo!(),
//     }
// }
