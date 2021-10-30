use crate::prelude::*;

use enso_macro_utils::field_names;
use enso_macro_utils::identifier_sequence;
use enso_macro_utils::index_sequence;
use enso_macro_utils::path_matching_ident;
use syn::Attribute;
use syn::DeriveInput;
use syn::Data;
use syn::DataEnum;
use syn::DataStruct;
use syn::Fields;
use syn::Ident;
use syn::Lit;
use syn::Meta;
use syn::MetaNameValue;
use syn::NestedMeta;
use syn::Variant;
use syn::WhereClause;
use syn::WherePredicate;



// ==============
// === Consts ===
// ==============

/// Name of the custom attribute allowing customizing behavior of the generated `CloneRef`
/// implementation.
const CLONE_REF_ATTR:&str = "clone_ref";

/// Name of the property within customization attribute that allows defining custom bounds for
/// the generated `CloneRef` implementation.
const BOUND_NAME:&str = "bound";



// ============================
// === CloneRef for structs ===
// ============================

/// `clone_ref` function body for a given `struct` definition.
pub fn body_for_struct(ident:&Ident, data:&DataStruct) -> TokenStream {
    match data.fields {
        Fields::Unit =>
            // Foo
            quote!( #ident ),
        Fields::Unnamed(ref fields) => {
            let indices = index_sequence(fields.unnamed.len());
            // Foo(self.0.clone_ref())
            quote!(
                #ident(#(self.#indices.clone_ref()),*)
            )
        }
        Fields::Named(ref fields)  => {
            let names = field_names(fields);
            // Foo { field0 : self.field0.clone_ref() }
            quote!(
                #ident {
                    #(#names : self.#names.clone_ref()),*
                }
            )
        }
    }
}



// ==========================
// === CloneRef for enums ===
// ==========================

/// Prepares a match arm for a single variant that `clone_ref`s such value.
pub fn arm_for_variant(data_ident:&Ident,variant:&Variant) -> TokenStream {
    let fields = &variant.fields;
    let variant_ident = &variant.ident;
    match fields {
        Fields::Unit => {
            // Enum::Var => Enum::Var
            quote!(
                #data_ident::#variant_ident => #data_ident::#variant_ident
            )
        }
        Fields::Named(fields) => {
            let names = field_names(fields);
            // Enum::Var {field0} => Enum::Var {field0 : field0.clone_ref()}
            quote!(
                #data_ident::#variant_ident { #(#names),* } =>
                    #data_ident::#variant_ident {
                        #( #names : #names.clone_ref() ),*
                    }
            )
        }
        Fields::Unnamed(fields) => {
            let names = identifier_sequence(fields.unnamed.len());
            // Enum::Var(field0) => Enum::Var(field0.clone_ref())
            quote!(
                #data_ident::#variant_ident(#(#names),*) =>
                    #data_ident::#variant_ident(
                        #( #names.clone_ref() ),*
                    )
            )
        }
    }
}

/// `clone_ref` function body for a given `enum` definition.
pub fn body_for_enum(ident:&Ident, data:&DataEnum) -> TokenStream {
    if data.variants.is_empty() {
        quote!(panic!("There cannot exist value of empty enum, so its clone_ref must not be called."))
    } else {
        let make_arm = |variant| arm_for_variant(ident,variant);
        let arms     = data.variants.iter().map(make_arm);
        quote!(
            match self { #(#arms),* }
        )
    }
}



// ============================
// === Bounds customization ===
// ============================

/// Checks if the given attribute is our customization attribute.
pub fn is_clone_ref_customization(attr:&Attribute) -> bool {
    path_matching_ident(&attr.path,CLONE_REF_ATTR)
}

/// Checks if the given Meta name-val pair defines user-provided bounds.
pub fn is_custom_bound(name_val:&MetaNameValue) -> bool {
    path_matching_ident(&name_val.path,BOUND_NAME)
}

/// If this is our customization attribute, we retrieve user-provided bounds for the generated
/// `CloneRef` implementation.
///
/// Returns `None` is this is third-party attribute.
/// Panics if this is our attribute but the syntax is not correct.
pub fn clone_ref_bounds(attr:&Attribute) -> Option<Vec<WherePredicate>> {
    // Silently ignore foreign attributes. Be picky only about our one.
    is_clone_ref_customization(attr).then(|| ())?;

    let meta = attr.parse_meta().expect("Failed to parse attribute contents.");
    let list = match meta {
        Meta::List(ml) => ml.nested,
        _              => panic!("Attribute contents does not conform to meta item."),
    };
    if list.len() > 1 {
        panic!("Only a single entry within `{}` attribute is allowed.",CLONE_REF_ATTR);
    }
    let bound_value = match list.first() {
        Some(NestedMeta::Meta(Meta::NameValue(name_val))) => {
            if is_custom_bound(name_val) {
                &name_val.lit
            } else {
                panic!("`{}` attribute can define value only for `{}`.",CLONE_REF_ATTR,BOUND_NAME)
            }
        }
        Some(_) =>
            panic!("`{}` attribute must contain a single name=value assignment.",CLONE_REF_ATTR),
        None =>
            panic!("`{}` attribute must not be empty.",CLONE_REF_ATTR),
    };
    let bound_str = if let Lit::Str(lit_str) = bound_value {
        lit_str
    } else {
        panic!("`{}` value must be a string literal describing `where` predicates.",BOUND_NAME)
    };
    let bounds_text = format!("where {}", bound_str.value());
    let bounds      = syn::parse_str::<WhereClause>(&bounds_text);
    let bounds      = bounds.unwrap_or_else(|_| {
        panic!("Failed to parse user-provided where clause: `{}`.",bounds_text)
    });
    let ret = bounds.predicates.into_iter().collect();
    Some(ret)
}



// ===================
// === Entry Point ===
// ===================

/// Derives `CloneRef` implementation, refer to `crate::derive_clone_ref` for details.
pub fn derive
(input:proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl   = syn::parse_macro_input!(input as DeriveInput);
    let ident  = &decl.ident;
    let body   = match &decl.data {
        Data::Struct(data_struct) => body_for_struct(ident,data_struct),
        Data::Enum(data_enum)     => body_for_enum(ident,data_enum),
        Data::Union(_)            =>
            panic!("CloneRef cannot be derived for an untagged union input."),
    };

    let (impl_generics, ty_generics, inherent_where_clause_opt) = &decl.generics.split_for_impl();

    // Where clause must contain both user-provided bounds and bounds inherent due to type
    // declaration-level where clause.
    let user_requested_bounds = decl.attrs.iter().filter_map(clone_ref_bounds).flatten();
    let mut where_clause      = enso_macro_utils::new_where_clause(user_requested_bounds);
    for inherent_where_clause in inherent_where_clause_opt {
        where_clause.predicates.extend(inherent_where_clause.predicates.iter().cloned())
    }

    let output = quote!{
        impl #impl_generics CloneRef for #ident #ty_generics
        #where_clause {
            fn clone_ref(&self) -> Self {
                #body
            }
        }

        impl #impl_generics From<& #ident #ty_generics> for #ident #ty_generics
        #where_clause {
            fn from(t:& #ident #ty_generics) -> Self {
                t.clone_ref()
            }
        }
    };
    output.into()
}
