use super::*;

use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::DeriveInput;
use syn::GenericParam;
use syn::Token;



// ===============
// === Analyze ===
// ===============

/// Parse `syn` syntax and produce type definitions.
pub(crate) fn analyze(input: TokenStream) -> Type {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let attrs: ContainerAttrs = input.attrs.iter().collect();
    let mut lifetimes: Punctuated<_, Token![,]> = Punctuated::new();
    let mut generic_params: Punctuated<_, Token![,]> = Punctuated::new();
    for param in input.generics.params {
        match param {
            GenericParam::Type(type_) => generic_params.push(type_.ident.to_token_stream()),
            GenericParam::Lifetime(lifetime) => lifetimes.push(lifetime.lifetime.to_token_stream()),
            GenericParam::Const(_) => unimplemented!("Reflect for const generics"),
        }
    }
    let mut generics = lifetimes.clone();
    generics.extend(generic_params.clone());
    let data = match input.data {
        syn::Data::Struct(struct_) => {
            let (fields, fields_attrs) = parse_fields(struct_.fields);
            let FieldsAttrs {} = fields_attrs;
            Data::Struct(fields)
        }
        syn::Data::Enum(enum_) =>
            Data::Enum(enum_.variants.into_iter().map(Variant::from).collect()),
        syn::Data::Union(_) => panic!(),
    };
    Type { ident, generics, lifetimes, generic_params, data, attrs }
}



// ===============
// === Parsing ===
// ===============

#[derive(Debug, Default)]
struct FieldsAttrs {}

fn parse_fields(fields: syn::Fields) -> (Fields, FieldsAttrs) {
    let attrs = FieldsAttrs::default();
    let fields = match fields {
        syn::Fields::Named(syn_fields) => {
            let mut fields = vec![];
            for field in syn_fields.named {
                let name = field.ident.unwrap();
                let type_ = field.ty;
                let mut skip = false;
                let mut subtype = false;
                for attr in field.attrs {
                    let annotations = parse_field_attrs(&attr);
                    for annotation in annotations {
                        match annotation {
                            FieldAttr::Skip => skip = true,
                            FieldAttr::Subtype => subtype = true,
                        }
                    }
                }
                if skip {
                    continue;
                }
                fields.push(NamedField { name, type_, subtype });
            }
            Fields::Named { fields }
        }
        syn::Fields::Unnamed(fields) =>
            Fields::Unnamed(fields.unnamed.into_iter().map(UnnamedField::from).collect()),
        syn::Fields::Unit => Fields::Unit,
    };
    (fields, attrs)
}

impl From<syn::Field> for UnnamedField {
    fn from(field: syn::Field) -> Self {
        let type_ = field.ty;
        UnnamedField { type_ }
    }
}

impl From<syn::Variant> for Variant {
    fn from(variant: syn::Variant) -> Self {
        if variant.discriminant.is_some() {
            unimplemented!("Explicit discriminators.");
        }
        let (fields, _fields_attrs) = parse_fields(variant.fields);
        let mut transparent = false;
        for attr in &variant.attrs {
            for attr in parse_variant_attrs(attr) {
                match attr {
                    VariantAttr::Inline => transparent = true,
                }
            }
        }
        let ident = variant.ident;
        Variant { ident, fields, transparent }
    }
}



// =========================
// === Helper attributes ===
// =========================

/// Helper attribute identifier. Must match the value `attributes(_)` parameter in the
/// `proc_macro_derive` annotation on this crate's entry point.
const HELPER_ATTRIBUTE_PATH: &str = "reflect";


// === Field Attributes ===

#[derive(PartialEq, Eq)]
enum FieldAttr {
    Skip,
    Subtype,
}

fn parse_field_attrs(attr: &syn::Attribute) -> Vec<FieldAttr> {
    if attr.style != syn::AttrStyle::Outer {
        return Default::default();
    }
    match attr.path.get_ident() {
        Some(ident) if ident == HELPER_ATTRIBUTE_PATH => (),
        _ => return Default::default(),
    }
    let meta = attr.parse_meta().unwrap();
    match meta {
        syn::Meta::List(metalist) => metalist
            .nested
            .iter()
            .map(|meta| {
                let ident = match meta {
                    syn::NestedMeta::Meta(syn::Meta::Path(path)) => path.get_ident().unwrap(),
                    _ => panic!(),
                };
                match ident.to_string().as_str() {
                    "skip" => FieldAttr::Skip,
                    "subtype" => FieldAttr::Subtype,
                    _ => panic!("Unexpected helper attribute: {}", attr.into_token_stream()),
                }
            })
            .collect(),
        syn::Meta::Path(_) | syn::Meta::NameValue(_) =>
            panic!("Unexpected helper attribute type: {}", attr.into_token_stream()),
    }
}


// === Variant Attributes ===

#[derive(PartialEq, Eq)]
enum VariantAttr {
    Inline,
}

fn parse_variant_attrs(attr: &syn::Attribute) -> Vec<VariantAttr> {
    if attr.style != syn::AttrStyle::Outer {
        return Default::default();
    }
    match attr.path.get_ident() {
        Some(ident) if ident == HELPER_ATTRIBUTE_PATH => (),
        _ => return Default::default(),
    }
    let meta = attr.parse_meta().unwrap();
    match meta {
        syn::Meta::List(metalist) => metalist
            .nested
            .iter()
            .map(|meta| {
                let ident = match meta {
                    syn::NestedMeta::Meta(syn::Meta::Path(path)) => path.get_ident().unwrap(),
                    _ => panic!(""),
                };
                match ident.to_string().as_str() {
                    "inline" => VariantAttr::Inline,
                    _ => panic!("Unexpected helper attribute: {}", attr.into_token_stream()),
                }
            })
            .collect(),
        syn::Meta::Path(_) | syn::Meta::NameValue(_) =>
            panic!("Unexpected helper attribute type: {}", attr.into_token_stream()),
    }
}


// === Container Attributes ===

#[derive(PartialEq, Eq)]
enum ContainerAttr {
    Transparent,
}

fn parse_container_attrs(attr: &syn::Attribute) -> Vec<ContainerAttr> {
    if attr.style != syn::AttrStyle::Outer {
        return Default::default();
    }
    match attr.path.get_ident() {
        Some(ident) if ident == HELPER_ATTRIBUTE_PATH => (),
        _ => return Default::default(),
    }
    let meta = attr.parse_meta().unwrap();
    match meta {
        syn::Meta::List(metalist) => metalist
            .nested
            .iter()
            .map(|meta| {
                let ident = match meta {
                    syn::NestedMeta::Meta(syn::Meta::Path(path)) => path.get_ident().unwrap(),
                    _ => panic!(),
                };
                match ident.to_string().as_str() {
                    "transparent" => ContainerAttr::Transparent,
                    _ => panic!("Unexpected helper attribute: {}", attr.into_token_stream()),
                }
            })
            .collect(),
        syn::Meta::Path(_) | syn::Meta::NameValue(_) =>
            panic!("Unexpected helper attribute type: {}", attr.into_token_stream()),
    }
}

impl<'a> FromIterator<&'a syn::Attribute> for ContainerAttrs {
    fn from_iter<T: IntoIterator<Item = &'a syn::Attribute>>(iter: T) -> Self {
        let mut transparent = false;
        for attr in iter {
            let annotations = parse_container_attrs(&attr);
            for annotation in annotations {
                if annotation == ContainerAttr::Transparent {
                    transparent = true;
                }
            }
        }
        ContainerAttrs { transparent }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::analyze::analyze;
    use quote::quote;

    #[test]
    fn accept_inputs() {
        let inputs = [
            quote! {
                struct Foo;
            },
            quote! {
                struct Bar {
                    bar: u32,
                    baar: &'static str,
                }
            },
            quote! {
                enum Baz {
                    Bar(Bar),
                    Baz,
                }
            },
            quote! {
                struct Quux<T> {
                    quux: T,
                }
            },
            quote! {
                struct Quuux<T> {
                    quux: Box<T>,
                }
            },
            quote! {
                struct Code<'s> {
                    repr: std::borrow::Cow<'s, str>,
                }
            },
        ];
        for input in inputs {
            analyze(input.into());
        }
    }
}
