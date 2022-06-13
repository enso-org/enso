//! Convert compile-time type definitions to syntax that evaluates to corresponding runtime values.

use super::*;

use syn::punctuated::Punctuated;
use syn::Token;

/// Values that can be converted to syntax that evaluates to an analogous type at runtime.
pub(crate) trait Quote {
    fn quote(&self) -> TokenStream;
}

impl Quote for Type {
    fn quote(&self) -> TokenStream {
        let ident = &self.ident;
        let generics = &self.generics;
        let data = self.data.quote();
        let name = self.ident.to_string();
        let transparent = self.attrs.transparent;
        quote! {
            reflect::rust::TypeData {
                id: reflect::rust::TypeId::of::<#ident<#generics>>(),
                name: #name.to_owned(),
                data: #data,
                transparent: #transparent,
                subtype_erased: Self::subtype_erased_type(),
            }
        }
    }
}

impl Quote for Data {
    fn quote(&self) -> TokenStream {
        match self {
            Data::Struct(fields) => {
                let fields = fields.quote();
                quote! {
                    reflect::rust::Data::Struct(reflect::rust::Struct {
                        fields: #fields,
                    })
                }
            }
            Data::Enum(variants) => {
                let variants: Punctuated<_, Token![,]> =
                    variants.into_iter().map(Quote::quote).collect();
                quote! {
                    reflect::rust::Data::Enum(reflect::rust::Enum {
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
                let fields: Punctuated<_, Token![,]> =
                    fields.into_iter().map(Quote::quote).collect();
                quote! { reflect::rust::Fields::Named(vec![#fields]) }
            }
            Fields::Unnamed(fields) => {
                let fields: Punctuated<_, Token![,]> =
                    fields.into_iter().map(Quote::quote).collect();
                quote! { reflect::rust::Fields::Unnamed(vec![#fields]) }
            }
            Fields::Unit => quote! { reflect::rust::Fields::Unit },
        }
    }
}

impl Quote for NamedField {
    fn quote(&self) -> TokenStream {
        let name = self.name.to_string();
        let typename = &self.type_;
        let subtype = self.subtype;
        quote! {
            reflect::rust::NamedField {
                name: #name.to_owned(),
                type_: reflect::rust::LazyType {
                    id: reflect::rust::TypeId::of::<#typename>(),
                    evaluate: <#typename as reflect::Reflect>::reflect,
                },
                subtype: #subtype,
            }
        }
    }
}

impl Quote for UnnamedField {
    fn quote(&self) -> TokenStream {
        let typename = &self.type_;
        quote! {
            reflect::rust::UnnamedField {
                type_: reflect::rust::LazyType {
                    id: reflect::rust::TypeId::of::<#typename>(),
                    evaluate: <#typename as reflect::Reflect>::reflect,
                },
            }
        }
    }
}

impl Quote for Variant {
    fn quote(&self) -> TokenStream {
        let ident = self.ident.to_string();
        let fields = self.fields.quote();
        let transparent = self.transparent;
        let quoted = quote! {
            reflect::rust::Variant {
                ident: #ident.to_owned(),
                fields: #fields,
                transparent: #transparent,
            }
        };
        quoted.into()
    }
}

#[cfg(test)]
mod tests {
    use super::analyze::analyze;
    use crate::Quote;
    use quote::quote;

    #[test]
    fn accept_inputs() {
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
            analyze(input.into()).quote();
        }
    }

    #[test]
    fn aaaaaa() {
        let input = quote! {
            pub struct Code<'s> {
                pub repr: std::borrow::Cow<'s, str>,
            }
        };
        panic!("{}", analyze(input.into()).quote());
    }
}
