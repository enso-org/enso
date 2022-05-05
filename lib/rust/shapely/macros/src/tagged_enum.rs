use crate::prelude::*;

use syn::Data;
use syn::DeriveInput;
use syn::Fields;

use inflector::cases::snakecase::to_snake_case;



// ===================
// === Entry Point ===
// ===================

/// ```text
/// #[tagged_enum]
/// pub enum Ast {
///     Ident {
///         name: String
///     }
///     App {
///         func: Ast,
///         arg: Ast
///     }
/// }
/// ```
pub fn run(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl = syn::parse_macro_input!(input as DeriveInput);
    let data = match &decl.data {
        Data::Enum(data) => data,
        _ => panic!("This macro is meant for enum structs only."),
    };

    let mut output = vec![];

    let vis = &decl.vis;
    let enum_name = &decl.ident;
    let enum_attrs = &decl.attrs;
    let variant_names: Vec<_> = data.variants.iter().map(|v| &v.ident).collect();

    // Generates:
    // pub enum Ast {
    //     Ident(Ident),
    //     App(App)
    // }
    //
    // impl Debug for Ast {
    //     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    //         match self {
    //             Self::Ident(t) => Debug::fmt(&t,f),
    //             Self::App(t) => Debug::fmt(&t,f),
    //         }
    //     }
    // }
    output.push(quote! {
        #(#enum_attrs)*
        #vis enum #enum_name {
            #(#variant_names(#variant_names)),*
        }

        impl Debug for #enum_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #(Self::#variant_names(t) => Debug::fmt(&t,f)),*
                }
            }
        }
    });

    // Generates:
    // #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    // pub AstVariant {
    //     Ident,
    //     App
    // }
    //
    // impl From<&Ast> for AstVariant {
    //     fn from(t:&Ast) -> Self {
    //         match t {
    //             Ast::Ident(_) => AstVariant::Ident,
    //             Ast::App(_) => AstVariant::App,
    //         }
    //     }
    // }
    //
    // impl Ast {
    //     pub fn variant(&self) -> AstVariant {
    //         self.into()
    //     }
    //
    //     pub fn is(&self, variant: AstVariant) -> bool {
    //         self.variant() == variant
    //     }
    // }
    let enum_variant_name = quote::format_ident!("{}Variant", enum_name);
    output.push(quote! {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #vis enum #enum_variant_name {
            #(#variant_names),*
        }

        impl From<&#enum_name> for #enum_variant_name {
            fn from(t:&#enum_name) -> Self {
                match t {
                    #(#enum_name::#variant_names(_) => Self::#variant_names),*
                }
            }
        }

        impl #enum_name {
            #[inline(always)]
            pub fn variant(&self) -> #enum_variant_name {
                self.into()
            }

            #[inline(always)]
            pub fn is(&self, variant: #enum_variant_name) -> bool {
                self.variant() == variant
            }
        }
    });

    for variant in &data.variants {
        // Generates:
        // pub struct Ident {
        //     name: String
        // }
        // pub struct App {
        //    func: Ast,
        //    args: Ast
        // }
        let variant_attrs = &variant.attrs;
        let variant_name = &variant.ident;
        let fields = &variant.fields;
        let fields = if fields.is_empty() { quote!({}) } else { quote!(#fields) };
        output.push(quote! {
            #(#enum_attrs)*
            #(#variant_attrs)*
            #[derive(Debug)]
            #vis struct #variant_name #fields
        });



        // ====================
        // === Constructors ===
        // ====================

        // impl Ast {
        //     pub fn ident(name: String) -> Self {
        //         Self::Ident(Ident{name})
        //     }
        //     pub fn app(func: Ast, args: Ast) -> Self {
        //         Self::App(App{func, args})
        //     }
        // }
        let variant_snake_name = to_snake_case(&variant_name.to_string());
        let variant_snake_ident = quote::format_ident!("{}", variant_snake_name);
        let (names, types) = match &variant.fields {
            Fields::Unit => (vec![], vec![]),
            Fields::Named(fields) => {
                let names = fields.named.iter().map(|f| f.ident.as_ref().unwrap()).collect();
                let types = fields.named.iter().map(|f| &f.ty).collect();
                (names, types)
            }
            _ => panic!(),
        };
        output.push(quote! {
            impl #enum_name {
                #[inline(always)]
                pub fn #variant_snake_ident(#(#names: #types),*) -> Self {
                    Self::#variant_name (#variant_name { #(#names),* })
                }
            }
        });

        // Generates:
        // impl Ast {
        //     pub fn is_ident(&self) -> bool {
        //         self.is(AstVariant::Ident)
        //     }
        //
        //     pub fn is_app(&self) -> bool {
        //         self.is(AstVariant::App)
        //     }
        // }
        let variant_check_ident = quote::format_ident!("is_{}", variant_snake_name);
        output.push(quote! {
            impl #enum_name {
                #[inline(always)]
                pub fn #variant_check_ident(&self) -> bool {
                    self.is(#enum_variant_name::#variant_name)
                }
            }
        });

        // Generates:
        // impl From<Ident> for Ast {
        //     fn from(variant: Ident) -> Self {
        //         Self::Ident(variant)
        //     }
        //     fn from(variant: App) -> Self {
        //         Self::App(variant)
        //     }
        // }
        output.push(quote! {
            impl From<#variant_name> for #enum_name {
                #[inline(always)]
                fn from(variant: #variant_name) -> Self {
                    Self::#variant_name(variant)
                }
            }
        });
    }



    let output = quote! {
        #(#output)*
    };

    output.into()
}
