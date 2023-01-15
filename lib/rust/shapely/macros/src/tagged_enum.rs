use crate::prelude::*;

use inflector::cases::snakecase::to_snake_case;
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
    for inherent_where_clause in inherent_where_clause_opt {
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



    // ==========================
    // === Marker Enum Struct ===
    // ==========================

    // #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    // pub enum AstMarker {
    //     Ident,
    //     App
    // }
    //
    // impl<'s> From<&Ast<'s>> for AstMarker {
    //     fn from(t:&Ast<'s>) -> Self {
    //         match t {
    //             Ast::Ident(_) => AstMarker::Ident,
    //             Ast::App(_) => AstMarker::App,
    //         }
    //     }
    // }
    //
    // impl<'s> Ast<'s> {
    //     pub fn marker(&self) -> AstMarker {
    //         self.into()
    //     }
    //
    //     pub fn is(&self, marker: AstMarker) -> bool {
    //         self.marker() == marker
    //     }
    // }
    let enum_marker_name = quote::format_ident!("{}Marker", enum_name);
    output.push(quote! {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[allow(missing_docs)]
        #vis enum #enum_marker_name {
            #(#variant_names),*
        }

        impl #impl_generics From<&#enum_name #ty_generics> for #enum_marker_name #where_clause {
            fn from(t:&#enum_name #ty_generics) -> Self {
                match t {
                    #(#enum_name::#variant_names(_) => Self::#variant_names),*
                }
            }
        }

        impl #impl_generics #enum_name #ty_generics #where_clause {
            /// Abstract variant representation of this struct.
            #[inline(always)]
            pub fn marker(&self) -> #enum_marker_name {
                self.into()
            }

            /// Check whether this struct is the given variant.
            #[inline(always)]
            pub fn is(&self, marker: #enum_marker_name) -> bool {
                self.marker() == marker
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
            impl #impl_generics #enum_name #ty_generics #where_clause {
                /// Constructor.
                #[inline(always)]
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
            pub fn #variant_name #impl_generics (#(#names: #types),*)
            -> #variant_name #ty_generics #where_clause {
                #variant_name { #(#names),* }
            }
        });



        // ======================
        // === Variant Checks ===
        // ======================

        // impl<'s> Ast<'s> {
        //     pub fn is_ident(&self) -> bool {
        //         self.is(AstMarker::Ident)
        //     }
        //
        //     pub fn is_app(&self) -> bool {
        //         self.is(AstMarker::App)
        //     }
        // }
        let variant_check_ident = quote::format_ident!("is_{}", variant_snake_name);
        output.push(quote! {
            impl #impl_generics #enum_name #ty_generics #where_clause {
                /// Check if this struct is the given variant.
                #[inline(always)]
                pub fn #variant_check_ident(&self) -> bool {
                    self.is(#enum_marker_name::#variant_name)
                }
            }
        });


        // ===================
        // === Conversions ===
        // ===================

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
    if attr.style != AttrStyle::Outer {
        return None;
    }
    if attr.path.get_ident()? != HELPER_ATTRIBUTE_PATH {
        return None;
    }
    let name_value = "Parsing name-value argument";
    let syn::MetaNameValue { lit, path, .. } = attr.parse_args().expect(name_value);
    match path.get_ident().expect("Unsupported helper-attribute name").to_string().as_str() {
        "apply_attributes_to" => Some(Attr::ApplyAttributesTo({
            let value = match lit {
                syn::Lit::Str(lit_str) => lit_str.value(),
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
