//! Convert compile-time type definitions to syntax that evaluates to corresponding runtime values.

use super::*;

use syn::punctuated::Punctuated;
use syn::Token;



// =============
// === Quote ===
// =============

/// Convert a value to syntax that evaluates to an analogous value at runtime.
pub(crate) trait Quote {
    fn quote(&self) -> TokenStream;
}


// === Implementations ===

impl Quote for Type {
    fn quote(&self) -> TokenStream {
        let ident = &self.ident;
        let generics = &self.generics;
        let data = self.data.quote(self.attrs.transparent);
        let name = self.ident.to_string();
        quote! {
            enso_reflect::metamodel::rust::TypeData {
                id: enso_reflect::type_id::<#ident<#generics>>(),
                name: #name.to_owned(),
                data: #data,
                subtype_erased: enso_reflect::generic_id::<Self>(),
            }
        }
    }
}

impl Data {
    fn quote(&self, transparent: bool) -> TokenStream {
        match self {
            Data::Struct(fields) => {
                let fields = fields.quote();
                quote! {
                    enso_reflect::metamodel::rust::Data::Struct(enso_reflect::metamodel::rust::Struct {
                        fields: #fields,
                        transparent: #transparent,
                    })
                }
            }
            Data::Enum(variants) => {
                assert!(!transparent, "`#[reflect(transparent)]` is not applicable to `enum`s.");
                let variants: Punctuated<_, Token![,]> =
                    variants.iter().map(Quote::quote).collect();
                quote! {
                    enso_reflect::metamodel::rust::Data::Enum(enso_reflect::metamodel::rust::Enum {
                        variants: vec![#variants],
                    })
                }
            }
        }
    }
}

impl Quote for Fields {
    fn quote(&self) -> TokenStream {
        match self {
            Fields::Named { fields } => {
                let fields: Punctuated<_, Token![,]> = fields.iter().map(Quote::quote).collect();
                quote! { enso_reflect::metamodel::rust::Fields::Named(vec![#fields]) }
            }
            Fields::Unnamed(fields) => {
                let fields: Punctuated<_, Token![,]> = fields.iter().map(Quote::quote).collect();
                quote! { enso_reflect::metamodel::rust::Fields::Unnamed(vec![#fields]) }
            }
            Fields::Unit => quote! { enso_reflect::metamodel::rust::Fields::Unit },
        }
    }
}

impl Quote for NamedField {
    fn quote(&self) -> TokenStream {
        let name = self.name.to_string();
        let typename = match &self.refer {
            Some(ty) => ty,
            None => &self.type_,
        };
        let subtype = self.subtype;
        let flatten = self.flatten;
        let hide = self.hide;
        quote! {
            enso_reflect::metamodel::rust::NamedField {
                name: #name.to_owned(),
                type_: enso_reflect::reflect_lazy::<#typename>(),
                subtype: #subtype,
                flatten: #flatten,
                hide: #hide,
            }
        }
    }
}

impl Quote for UnnamedField {
    fn quote(&self) -> TokenStream {
        let typename = &self.type_;
        quote! {
            enso_reflect::metamodel::rust::UnnamedField {
                type_: enso_reflect::reflect_lazy::<#typename>(),
            }
        }
    }
}

impl Quote for Variant {
    fn quote(&self) -> TokenStream {
        let ident = self.ident.to_string();
        let fields = self.fields.quote();
        let inline = self.transparent;
        let quoted = quote! {
            enso_reflect::metamodel::rust::Variant {
                ident: #ident.to_owned(),
                fields: #fields,
                inline: #inline,
            }
        };
        quoted
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::analyze::analyze;
    use crate::Quote;
    use quote::quote;

    #[test]
    fn accept_simple_inputs() {
        let inputs = [
            quote! {
                struct Foo;
            },
            quote! {
                struct Bar {
                    bar: Foo,
                    baar: &'static str,
                }
            },
        ];
        for input in inputs {
            analyze(input).quote();
        }
    }

    #[test]
    fn accept_generics() {
        let inputs = [
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
        ];
        for input in inputs {
            analyze(input).quote();
        }
    }

    #[test]
    fn accept_generic_lifetimes() {
        let inputs = [quote! {
            struct Code<'s> {
                repr: std::borrow::Cow<'s, str>,
            }
        }];
        for input in inputs {
            analyze(input).quote();
        }
    }
}
