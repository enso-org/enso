use convert_case::Case;
use convert_case::Casing;
use quote::quote;
use syn::AttrStyle;
use syn::Attribute;
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
/// pub enum Ast<'s> {
///     Ident {
///         token: Token<'s>
///     }
///     App {
///         func: Ast<'s>,
///         arg: Ast<'s>,
///     }
/// }
/// ```
///
/// # Attributes
/// Attributes defined after `#[tagged_enum]` and not in a section (see below) will be applied to
/// the enum and also all the variants structs produced; this is the default because it is
/// appropriate for common attributes like `#[derive(..)]`.
///
/// The attribute `#[tagged_enum(apply-attrs-to = "enum")]` starts an attribute section; any
/// attributes in the section will be applied only to the enum itself.
///
/// The attribute `#[tagged_enum(apply-attrs-to = "variants")]` starts an attribute section; any
/// attributes in the section will be applied only to the variant structs produced.
///
/// An attribute can be placed before the `#[tagged_enum]` if its proc macro needs to operate on
/// the enum before expanding `tagged_enum`; otherwise, to avoid confusion, attributes should not be
/// placed before `#[tagged_enum]`, as the results would differ for *active* or *inert*
/// attributes[1].
/// [1]: https://doc.rust-lang.org/reference/attributes.html#active-and-inert-attributes

pub fn run(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut is_boxed = false;
    let attrs: Vec<_> = attr.into_iter().collect();
    if attrs.len() == 1 && &attrs[0].to_string() == "boxed" {
        is_boxed = true;
    } else if !attrs.is_empty() {
        panic!("Unsupported attributes: {attrs:?}");
    }
    let mut decl = syn::parse_macro_input!(input as DeriveInput);
    let (enum_attrs, variant_types_attrs, variants_attrs) =
        split_attr_sections(std::mem::take(&mut decl.attrs));
    let (impl_generics, ty_generics, inherent_where_clause_opt) = &decl.generics.split_for_impl();
    let mut where_clause = enso_macro_utils::new_where_clause(vec![]);
    if let Some(inherent_where_clause) = inherent_where_clause_opt {
        where_clause.predicates.extend(inherent_where_clause.predicates.iter().cloned())
    }

    let data = match &decl.data {
        Data::Enum(data) => data,
        _ => panic!("This macro is meant for enum structs only."),
    };

    let mut output = vec![];



    // ========================
    // === Main Enum Struct ===
    // ========================

    // pub enum Ast<'s> {
    //     Ident(Box<Ident<'s>>),
    //     App(Box<App<'s>>)
    // }
    //
    // impl<'s> Debug for Ast<'s> {
    //     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    //         match self {
    //             Self::Ident(t) => Debug::fmt(&t,f),
    //             Self::App(t) => Debug::fmt(&t,f),
    //         }
    //     }
    // }
    let vis = &decl.vis;
    let enum_name = &decl.ident;
    let variant_names: Vec<_> = data.variants.iter().map(|v| &v.ident).collect();
    let variant_bodies = variant_names.iter().map(|v| {
        if is_boxed {
            quote!(Box<#v #ty_generics>)
        } else {
            quote!(#v #ty_generics)
        }
    });
    let variants_attrs = quote! { #(#variants_attrs)* };
    output.push(quote! {
        #(#enum_attrs)*
        #[allow(missing_docs)]
        #vis enum #enum_name #ty_generics #where_clause {
            #(
                #variants_attrs
                #variant_names(#variant_bodies)
            ),*
        }

        impl #impl_generics Debug for #enum_name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #(Self::#variant_names(t) => Debug::fmt(&t,f)),*
                }
            }
        }
    });


    for variant in &data.variants {
        // =======================
        // === Variant Structs ===
        // =======================

        // pub struct Ident<'s> {
        //     pub token: Token<'s>
        // }
        // pub struct App<'s> {
        //     pub func: Ast<'s>,
        //     pub args: Ast<'s>,
        // }
        let variant_attrs = &variant.attrs;
        let variant_name = &variant.ident;
        let fields = &variant.fields;
        let fields = if fields.is_empty() { quote!({}) } else { quote!(#fields) };
        output.push(quote! {
            #(#variant_types_attrs)*
            #(#variant_attrs)*
            #[derive(Debug)]
            #[allow(missing_docs)]
            #vis struct #variant_name #ty_generics #fields #where_clause
        });



        // ====================
        // === Constructors ===
        // ====================

        // impl<'s> Ast<'s> {
        //     pub fn ident(name: String) -> Self {
        //         Self::Ident(Box::new(Ident{name}))
        //     }
        //     pub fn app(func: Ast, args: Ast) -> Self {
        //         Self::App(Box::new(App{func, args}))
        //     }
        // }
        let variant_snake_name = variant_name.to_string().to_case(Case::Snake);
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
            impl #impl_generics #enum_name #ty_generics #where_clause {
                /// Constructor.
                #[inline(always)]
                #[allow(clippy::too_many_arguments)]
                pub fn #variant_snake_ident(#(#names: #types),*) -> Self {
                    Self::#variant_name (#cons)
                }
            }
        });



        // ========================================
        // === Unnamed Struct Like Constructors ===
        // ========================================

        // pub fn Ident<'s>(token: Token<'s>) -> Token<'s> {
        //     Ident {name}
        // }
        // pub fn App<'s>(func: Ast<'s>, args: Ast<'s>) -> App<'s> {
        //     App {func, args}
        // }
        output.push(quote! {
            /// Constructor.
            #[inline(always)]
            #[allow(non_snake_case)]
            #[allow(clippy::too_many_arguments)]
            pub fn #variant_name #impl_generics (#(#names: #types),*)
            -> #variant_name #ty_generics #where_clause {
                #variant_name { #(#names),* }
            }
        });


        // ====================
        // === Type erasure ===
        // ====================

        // impl<'s> From<Ident<'s>> for Ast<'s> {
        //     fn from(variant: Ident<'s>) -> Self {
        //         Self::Ident(Box::new(variant))
        //     }
        // }
        //
        // impl<'s> From<App<'s>> for Ast<'s> {
        //     fn from(variant: App<'s>) -> Self {
        //         Self::App(Box::new(variant))
        //     }
        // }
        let cons = if is_boxed { quote!(Box::new(variant)) } else { quote!(variant) };
        output.push(quote! {
            impl #impl_generics From<#variant_name #ty_generics> for #enum_name #ty_generics
            #where_clause {
                #[inline(always)]
                fn from(variant: #variant_name #ty_generics) -> Self {
                    Self::#variant_name(#cons)
                }
            }
        });


        // ===================
        // === Downcasting ===
        // ===================

        // impl<'s> TryFrom<Ast<'s>> for Ident<'s> {
        //     type Error = ();
        //
        //     fn try_from(ast: Ast<'s>) -> Result<Self, Self::Error> {
        //         match ast {
        //             Ast::Ident(ident) => Ok(ident),
        //             _ => Err(()),
        //         }
        //     }
        // }
        //
        // impl<'s> TryFrom<Ast<'s>> for App<'s> {
        //     type Error = ();
        //
        //     fn try_from(ast: Ast<'s>) -> Result<Self, Self::Error> {
        //         match ast {
        //             Ast::App(app) => Ok(app),
        //             _ => Err(()),
        //         }
        //     }
        // }
        let variant_value = if is_boxed {
            quote!(Box<#variant_name #ty_generics>)
        } else {
            quote!(#variant_name #ty_generics)
        };
        output.push(quote! {
            impl #impl_generics TryFrom<#enum_name #ty_generics> for #variant_value #where_clause {
                type Error = ();

                #[inline(always)]
                fn try_from(any: #enum_name #ty_generics) -> Result<Self, Self::Error> {
                    match any {
                        #enum_name::#variant_name(variant) => Ok(variant),
                        _ => Err(()),
                    }
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



// ==================
// === Attributes ===
// ==================

/// The path used to identify helper-attributes that configure the macro.
/// E.g. `tagged_enum` in `#[tagged_enum(apply_attributes_to = "variants")]`
const HELPER_ATTRIBUTE_PATH: &str = "tagged_enum";

enum Attr {
    ApplyAttributesTo(ApplyAttributesTo),
}

enum ApplyAttributesTo {
    Enum,
    VariantTypes,
    Variants,
}

fn parse_attr(attr: &Attribute) -> Option<Attr> {
    if attr.style != AttrStyle::Outer || !attr.path().is_ident(HELPER_ATTRIBUTE_PATH) {
        return None;
    }

    let name_value = "Parsing name-value argument";
    let syn::MetaNameValue { path, value, .. } = attr.parse_args().expect(name_value);
    match path.get_ident().expect("Unsupported helper-attribute name").to_string().as_str() {
        "apply_attributes_to" => Some(Attr::ApplyAttributesTo({
            let value = match value {
                syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(lit_str), .. }) => lit_str.value(),
                _ => panic!("Expected a LitStr in argument to helper-attribute."),
            };
            match value.as_str() {
                "enum" => ApplyAttributesTo::Enum,
                "variant-types" => ApplyAttributesTo::VariantTypes,
                "variants" => ApplyAttributesTo::Variants,
                _ => panic!("Unexpected value in string argument to helper-attribute."),
            }
        })),
        _ => panic!("Unsupported helper-attribute name: {path:?}."),
    }
}

fn split_attr_sections(attrs: Vec<Attribute>) -> (Vec<Attribute>, Vec<Attribute>, Vec<Attribute>) {
    let mut enum_attrs = vec![];
    let mut variant_types_attrs = vec![];
    let mut variants_attrs = vec![];
    let mut attr_section = None;
    for attr in attrs {
        if let Some(attr) = parse_attr(&attr) {
            match attr {
                Attr::ApplyAttributesTo(apply_to) => attr_section = Some(apply_to),
            }
            continue;
        }
        match attr_section {
            None => {
                enum_attrs.push(attr.clone());
                variant_types_attrs.push(attr);
            }
            Some(ApplyAttributesTo::Enum) => enum_attrs.push(attr),
            Some(ApplyAttributesTo::VariantTypes) => variant_types_attrs.push(attr),
            Some(ApplyAttributesTo::Variants) => variants_attrs.push(attr),
        }
    }
    (enum_attrs, variant_types_attrs, variants_attrs)
}
