//! Parse syntax into (macro execution-time) representations of data structure definitions.

use super::*;

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
            GenericParam::Const(_) => unimplemented!("Reflect for const generics."),
        }
    }
    let mut generics = lifetimes.clone();
    generics.extend(generic_params.clone());
    let data = match input.data {
        syn::Data::Struct(struct_) => Data::Struct(parse_fields(struct_.fields)),
        syn::Data::Enum(enum_) =>
            Data::Enum(enum_.variants.into_iter().map(Variant::from).collect()),
        syn::Data::Union(_) => unimplemented!("Reflect for `union`s."),
    };
    Type { ident, generics, lifetimes, generic_params, data, attrs }
}



// ===============
// === Parsing ===
// ===============

fn parse_fields(fields: syn::Fields) -> Fields {
    match fields {
        syn::Fields::Named(syn_fields) => {
            let mut fields = vec![];
            'fields: for field in syn_fields.named {
                let mut field_ = NamedField::new(field.ident.unwrap(), field.ty);
                let mut annotations = Default::default();
                for attr in field.attrs {
                    parse_field_attrs(&attr, &mut annotations);
                }
                for annotation in annotations {
                    match annotation {
                        FieldAttr::Flatten => field_.flatten = true,
                        FieldAttr::Hide => field_.hide = true,
                        FieldAttr::Subtype => field_.subtype = true,
                        FieldAttr::As(ty) => field_.refer = Some(ty),
                        FieldAttr::Rename(ident) => field_.rename = Some(ident),
                        // NOTE: Implementing `skip` at analysis time makes our Rust information
                        // incomplete. For `reflect` to be used to generate Rust deserialization
                        // code, we'd need to emit a field with a type that is a marker type,
                        // which we'd filter out when abstracting.
                        FieldAttr::Skip => continue 'fields,
                    }
                }
                fields.push(field_);
            }
            Fields::Named { fields }
        }
        syn::Fields::Unnamed(fields) =>
            Fields::Unnamed(fields.unnamed.into_iter().map(UnnamedField::from).collect()),
        syn::Fields::Unit => Fields::Unit,
    }
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
        let fields = parse_fields(variant.fields);
        let mut transparent = false;
        let mut annotations = Default::default();
        for attr in &variant.attrs {
            parse_variant_attrs(attr, &mut annotations);
        }
        for annotation in annotations {
            match annotation {
                VariantAttr::Inline => transparent = true,
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
const INVALID_HELPER_SYNTAX: &str = "Unknown helper attribute syntax";
const UNKNOWN_HELPER: &str = "Unknown helper attribute";


// === Field Attributes ===

#[derive(PartialEq, Eq)]
enum FieldAttr {
    Flatten,
    Hide,
    Skip,
    Subtype,
    As(Box<syn::Type>),
    Rename(syn::LitStr),
}

fn parse_field_attrs(attr: &syn::Attribute, out: &mut Vec<FieldAttr>) {
    if attr.style != syn::AttrStyle::Outer || !attr.path().is_ident(HELPER_ATTRIBUTE_PATH) {
        return;
    }
    match &attr.meta {
        syn::Meta::List(metalist) => {
            metalist.parse_nested_meta(|meta| {
                out.push(parse_field_annotation(&meta)?);
                Ok(())
            })
        }
        .expect(INVALID_HELPER_SYNTAX),
        syn::Meta::Path(_) | syn::Meta::NameValue(_) =>
            panic!("{}: {}.", INVALID_HELPER_SYNTAX, attr.meta.to_token_stream()),
    }
}

fn parse_field_annotation(meta: &syn::meta::ParseNestedMeta) -> syn::Result<FieldAttr> {
    let ident = meta.path.get_ident().expect(INVALID_HELPER_SYNTAX);
    Ok(match ident.to_string().as_str() {
        "flatten" => FieldAttr::Flatten,
        "hide" => FieldAttr::Hide,
        "skip" => FieldAttr::Skip,
        "subtype" => FieldAttr::Subtype,
        "as" => FieldAttr::As(Box::new(meta.value()?.parse()?)),
        "rename" => FieldAttr::Rename(meta.value()?.parse()?),
        _ => panic!("{}: {}.", UNKNOWN_HELPER, meta.path.clone().into_token_stream()),
    })
}


// === Variant Attributes ===

#[derive(PartialEq, Eq)]
enum VariantAttr {
    Inline,
}

fn parse_variant_attrs(attr: &syn::Attribute, out: &mut Vec<VariantAttr>) {
    if attr.style != syn::AttrStyle::Outer || !attr.path().is_ident(HELPER_ATTRIBUTE_PATH) {
        return;
    }
    match &attr.meta {
        syn::Meta::List(metalist) => {
            metalist
                .parse_nested_meta(|meta| {
                    if meta.path.is_ident("inline") {
                        out.push(VariantAttr::Inline);
                        Ok(())
                    } else {
                        panic!("{}: {}.", UNKNOWN_HELPER, attr.meta.clone().into_token_stream());
                    }
                })
                .expect(INVALID_HELPER_SYNTAX);
        }
        syn::Meta::Path(_) | syn::Meta::NameValue(_) =>
            panic!("{}: {}.", INVALID_HELPER_SYNTAX, attr.meta.clone().into_token_stream()),
    }
}


// === Container Attributes ===

#[derive(PartialEq, Eq)]
enum ContainerAttr {
    Transparent,
}

fn parse_container_attrs(attr: &syn::Attribute, out: &mut Vec<ContainerAttr>) {
    if attr.style != syn::AttrStyle::Outer {
        return;
    }
    if !attr.path().is_ident(HELPER_ATTRIBUTE_PATH) {
        return;
    }
    match &attr.meta {
        syn::Meta::List(metalist) => {
            metalist
                .parse_nested_meta(|meta| {
                    if meta.path.is_ident("transparent") {
                        out.push(ContainerAttr::Transparent);
                        Ok(())
                    } else {
                        panic!("{}: {}.", UNKNOWN_HELPER, attr.into_token_stream());
                    }
                })
                .expect(INVALID_HELPER_SYNTAX);
        }
        syn::Meta::Path(_) | syn::Meta::NameValue(_) =>
            panic!("{}: {}.", INVALID_HELPER_SYNTAX, attr.into_token_stream()),
    }
}

impl<'a> FromIterator<&'a syn::Attribute> for ContainerAttrs {
    fn from_iter<T: IntoIterator<Item = &'a syn::Attribute>>(iter: T) -> Self {
        let mut transparent = false;
        let mut annotations = Default::default();
        for attr in iter {
            parse_container_attrs(attr, &mut annotations);
        }
        for annotation in annotations {
            match annotation {
                ContainerAttr::Transparent => transparent = true,
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
            analyze(input);
        }
    }
}
