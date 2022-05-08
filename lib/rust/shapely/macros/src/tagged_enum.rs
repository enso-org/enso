use crate::prelude::*;

use inflector::cases::snakecase::to_snake_case;
use syn::Data;
use syn::DeriveInput;
use syn::Fields;



// ===================
// === Entry Point ===
// ===================

/// Transforms Rust enums into enums where each variant is a separate type. It also implements
/// several traits (such as conversions between variants and the enum type) and defines utility
/// functions, such as constructors.
///
/// To learn more about what code is being generated, parts of the code generation were provided
/// with comments showing the output of application of this macro to the following structure:
///
/// ```text
/// #[tagged_enum(boxed)]
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
pub fn run(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let is_boxed = attr.into_iter().any(|t| t.to_string() == "boxed");
    let decl = syn::parse_macro_input!(input as DeriveInput);
    let data = match &decl.data {
        Data::Enum(data) => data,
        _ => panic!("This macro is meant for enum structs only."),
    };

    let mut output = vec![];



    // ========================
    // === Main Enum Struct ===
    // ========================

    // pub enum Ast {
    //     Ident(Box<Ident>),
    //     App(Box<App>)
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
    let vis = &decl.vis;
    let enum_name = &decl.ident;
    let enum_attrs = &decl.attrs;
    let variant_names: Vec<_> = data.variants.iter().map(|v| &v.ident).collect();
    let variant_bodies =
        variant_names.iter().map(|v| if is_boxed { quote!(Box<#v>) } else { quote!(#v) });
    output.push(quote! {
        #(#enum_attrs)*
        #vis enum #enum_name {
            #(#variant_names(#variant_bodies)),*
        }

        impl Debug for #enum_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #(Self::#variant_names(t) => Debug::fmt(&t,f)),*
                }
            }
        }
    });



    // ===========================
    // === Variant Enum Struct ===
    // ===========================

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
        // =======================
        // === Variant Structs ===
        // =======================

        // pub struct Ident {
        //     pub name: String
        // }
        // pub struct App {
        //     pub func: Ast,
        //     pub args: Ast
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
        //         Self::Ident(Box::new(Ident{name}))
        //     }
        //     pub fn app(func: Ast, args: Ast) -> Self {
        //         Self::App(Box::new(App{func, args}))
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
        let cons = if is_boxed {
            quote!(Box::new(#variant_name { #(#names),* }))
        } else {
            quote!(#variant_name { #(#names),* })
        };
        output.push(quote! {
            impl #enum_name {
                #[inline(always)]
                pub fn #variant_snake_ident(#(#names: #types),*) -> Self {
                    Self::#variant_name (#cons)
                }
            }
        });



        // ========================================
        // === Unnamed Struct Like Constructors ===
        // ========================================

        // pub fn Ident(name: String) -> Ident {
        //     Ident {name}
        // }
        // pub fn App(func: Ast, args: Ast) -> App {
        //     App {func, args}
        // }
        output.push(quote! {
            #[inline(always)]
            #[allow(non_snake_case)]
            pub fn #variant_name(#(#names: #types),*) -> #variant_name {
                #variant_name { #(#names),* }
            }
        });



        // ======================
        // === Variant Checks ===
        // ======================

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



        // ===================
        // === Conversions ===
        // ===================

        // impl From<Ident> for Ast {
        //     fn from(variant: Ident) -> Self {
        //         Self::Ident(Box::new(variant))
        //     }
        // }
        //
        // impl From<Ident> for Ast {
        //     fn from(variant: Ident) -> Self {
        //         Self::Ident(Box::new(variant))
        //     }
        // }
        let cons = if is_boxed { quote!(Box::new(variant)) } else { quote!(variant) };
        output.push(quote! {
            impl From<#variant_name> for #enum_name {
                #[inline(always)]
                fn from(variant: #variant_name) -> Self {
                    Self::#variant_name(#cons)
                }
            }
        });
    }



    // =============================
    // === Final Code Generation ===
    // =============================

    let output = quote! {
        #(#output)*
    };

    output.into()
}
