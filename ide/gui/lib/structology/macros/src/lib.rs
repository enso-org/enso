extern crate proc_macro;

use basegl_prelude::*;

use inflector::Inflector;
use proc_macro2::{TokenStream, Ident, Span};
use quote::quote;
use syn;


////////////////////////////////////////////////

/// In order to make the definition easier to read, an example expansion of the
/// following definition was provided for each quotation:
///
/// #[derive(Iterator)]
/// pub struct Foo<S, T> { foo: T }
#[proc_macro_derive(Iterator)]
pub fn derive_iterator
(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl   = syn::parse_macro_input!(input as syn::DeriveInput);
    let data   = &decl.data;
    let params = &decl.generics.params.iter().collect::<Vec<_>>();
    match params.last() {
        Some(last_param) => derive_iterator_for(&decl, &last_param),
        None             => proc_macro::TokenStream::from(quote! {})
    }
}

fn derive_iterator_for
( decl         : &syn::DeriveInput
, target_param : &syn::GenericParam
) -> proc_macro::TokenStream {
    let data           = &decl.data;
    let params         = &decl.generics.params.iter().collect::<Vec<_>>();
    let target_param_str = repr(&target_param);
    let matched_fields: Vec<TokenStream> = match *data {
        syn::Data::Struct(ref data) => {
            fields_list(&data.fields).iter().enumerate().filter_map(|(i, f)| {
                let type_matched = repr(&f.ty) == target_param_str;
                type_matched.as_some_from(|| {
                    match &f.ident {
                        Some(ident) => quote!(#ident),
                        None => {
                            let ix = syn::Index::from(i);
                            quote!(#ix)
                        }
                    }
                })
            }).collect()
        }
        syn::Data::Enum(_) | syn::Data::Union(_) => unimplemented!(),
    };
    let data           = &decl.ident;
    let t_iterator     = format!("{}Iterator"    , data);
    let t_iterator_mut = format!("{}IteratorMut" , data);
    let iterator       = t_iterator.to_snake_case();
    let iterator_mut   = t_iterator_mut.to_snake_case();
    let t_iterator     = Ident::new(&t_iterator     , Span::call_site());
    let t_iterator_mut = Ident::new(&t_iterator_mut , Span::call_site());
    let iterator       = Ident::new(&iterator       , Span::call_site());
    let iterator_mut   = Ident::new(&iterator_mut   , Span::call_site());
    let iter_body_ref  = quote! {
        structology::IterForGenerator
            (move || { #(yield &t.#matched_fields;)* })
    };
    let iter_body_mut  = quote! {
        structology::IterForGenerator
            (move || { #(yield &mut t.#matched_fields;)* })
    };
    let iter_body_dummy = quote! { structology::EmptyGenerator::new() };
    let empty           = matched_fields.is_empty();
    let iter_body       = if empty { &iter_body_dummy } else { &iter_body_ref };
    let iter_body_mut   = if empty { &iter_body_dummy } else { &iter_body_mut };
    let expanded        = quote! {
        // type FooIterator<'t, T> = impl Iterator<Item = &'t T>;
        type #t_iterator<'t, #(#params),*> =
            impl Iterator<Item = &'t #target_param>;

        // pub fn foo_iterator<'t, T>
        // (t: &'t Foo<T>) -> FooIterator<'t, T> {
        //    structology::IterForGenerator(move || {
        //        yield &t.foo;
        //    })
        // }
        pub fn #iterator<'t, #(#params),*>
        (t: &'t #data<#(#params),*>) -> #t_iterator<'t, #(#params),*> {
            #iter_body
        }

        // type FooIteratorMut<'t, T> = impl Iterator<Item = &'t mut T>;
        type #t_iterator_mut<'t, #(#params),*> =
            impl Iterator<Item = &'t mut #target_param>;

        // pub fn foo_iterator_mut<'t, T>
        // (t: &'t mut Foo<T>) -> FooIteratorMut<'t, T> {
        //    structology::IterForGenerator(move || {
        //        yield &t.foo;
        //    })
        // }
        pub fn #iterator_mut<'t, #(#params),*>
        (t: &'t mut #data<#(#params),*>) -> #t_iterator_mut<'t, #(#params),*> {
            #iter_body_mut
        }

        // impl<'t, T> IntoIterator for &'t Foo<T> {
        //     type Item     = &'t T;
        //     type IntoIter = FooIterator<'t, T>;
        //     fn into_iter(self) -> FooIterator<'t, T> {
        //         foo_iterator(self)
        //     }
        // }
        impl<'t, #(#params),*> IntoIterator for &'t #data<#(#params),*> {
            type Item     = &'t #target_param;
            type IntoIter = #t_iterator<'t, #(#params),*>;
            fn into_iter(self) -> #t_iterator<'t, #(#params),*> {
                #iterator(self)
            }
        }

        // impl<'t, T> IntoIterator for &'t mut Foo<T> {
        //     type Item     = &'t mut T;
        //     type IntoIter = FooIteratorMut<'t, T>;
        //     fn into_iter(self) -> FooIteratorMut<'t, T> {
        //         foo_iterator_mut(self)
        //     }
        // }
        impl<'t, #(#params),*> IntoIterator for &'t mut #data<#(#params),*> {
            type Item     = &'t mut #target_param;
            type IntoIter = #t_iterator_mut<'t, #(#params),*>;
            fn into_iter(self) -> #t_iterator_mut<'t, #(#params),*> {
                #iterator_mut(self)
            }
        }

        // impl Foo<T> {
        //     pub fn iter(&self) -> FooIterator<'_, T> {
        //         #foo_iterator(self)
        //     }
        //     pub fn iter_mut(&mut self) -> FooIteratorMut<'_, T> {
        //         #foo_iterator_mut (self)
        //     }
        // }
        impl<#(#params),*> #data<#(#params),*> {
            pub fn iter(&self) -> #t_iterator<'_, #(#params),*> {
                #iterator(self)
            }
            pub fn iter_mut(&mut self) -> #t_iterator_mut<'_, #(#params),*> {
                #iterator_mut(self)
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn fields_list(fields: &syn::Fields) -> Vec<&syn::Field> {
    match fields {
        syn::Fields::Named   (ref f) => { f.named.iter().collect()   }
        syn::Fields::Unnamed (ref f) => { f.unnamed.iter().collect() }
        syn::Fields::Unit            => default()
    }
}

fn field_type(field: &syn::Field) -> String {
    let tp = &field.ty;
    quote!(#tp).to_string()
}

#[proc_macro_derive(AstNode)]
pub fn derive_ast_node
(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let expanded  = quote! {
        #[derive(Debug)] 
    };
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn ast_node
(_meta: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let output = quote! {
        #[derive(Eq, PartialEq, Debug)]
        #[derive(Iterator)]
        #[derive(Serialize, Deserialize)]
        #input
    };
    output.into()
}

#[proc_macro_attribute]
pub fn ast
( attrs : proc_macro::TokenStream
, input : proc_macro::TokenStream
) -> proc_macro::TokenStream {
    let attrs: TokenStream = attrs.into();
    let decl   = syn::parse_macro_input!(input as syn::DeriveInput);
    let output = match &decl.data {
        syn::Data::Enum { .. } => quote! {
            #[to_variant_types(#attrs)]
            #[ast_node]
            #decl
        },
        _ => quote! {
            #[ast_node]
            #decl
        }
    };
    output.into()
}



fn repr<T: quote::ToTokens>(t: &T) -> String {
    quote!(#t).to_string()
}


use syn::visit::{self, Visit};
use syn::{File, ItemFn};

struct TypeGather {
    pub types: Vec<String>
}

impl TypeGather {
    pub fn new() -> Self {
        let types = default();
        Self { types }
    }
}

impl<'ast> Visit<'ast> for TypeGather {
    fn visit_type_path(&mut self, node: &'ast syn::TypePath) {
        self.types.push(repr(node));
        visit::visit_type_path(self, node);
    }
}

fn gather_all_types(node: &syn::Type) -> Vec<String> {
    let mut type_gather = TypeGather::new();
    type_gather.visit_type(node);
    type_gather.types
}


fn mk_product_type
( is_flat : bool
, decl    : &syn::DeriveInput
, variant : &syn::Variant
) -> syn::ItemStruct {
    use syn::ItemStruct;
    let fields       = &variant.fields;
    let types        = fields.iter().flat_map(|f| {gather_all_types(&f.ty) });
    let types        = types.collect::<HashSet<_>>();
    let ty_vars      = decl.generics.params.iter().cloned();
    let params       = ty_vars.filter(|v| types.contains(&repr(&v))).collect();
    let attrs        = decl.attrs.clone();
    let vis          = decl.vis.clone();
    let struct_token = syn::token::Struct { span: Span::call_site() };
    let ident_flat   = variant.ident.clone();
    let ident_nested = format!("{}{}", decl.ident, variant.ident);
    let ident_nested = Ident::new(&ident_nested, Span::call_site());
    let ident        = if is_flat { ident_flat } else { ident_nested };
    let generics     = syn::Generics { params, .. default() };
    let mut fields   = variant.fields.clone();
    let semi_token   = None;
    fields.iter_mut().for_each(|f| f.vis = vis.clone());
    ItemStruct { attrs, vis, struct_token, ident, generics, fields, semi_token }
}

fn gen_variant_decl
(ident: &syn::Ident, variant: &syn::ItemStruct) -> TokenStream {
    let variant_ident = &variant.ident;
    let params = variant.generics.params.iter();
    quote! {
        // App(ShapeApp<T>),
        // Var(ShapeVar),
        #ident(#variant_ident<#(#params),*>)
    }
}

fn gen_from_impls
(ident: &syn::Ident, decl: &syn::DeriveInput, variant: &syn::ItemStruct) -> TokenStream {
    let sum_label = &decl.ident;
    let variant_label = &variant.ident;
    let sum_params = &decl.generics.params.iter().cloned().collect::<Vec<_>>();
    let variant_params = &variant.generics.params.iter().cloned().collect::<Vec<_>>();
    ;
    quote! {
        // impl<T> From<App<T>> for Shape<T> {
        //     fn from(t: App<T>) -> Self { Shape::App(t) }
        // }
        // ...
        impl<#(#sum_params),*> From<#variant_label<#(#variant_params),*>>
        for #sum_label<#(#sum_params),*> {
            fn from(t: #variant_label<#(#variant_params),*>) -> Self {
                #sum_label::#ident(t)
            }
        }
    }
}

/// In order to make the definition easier to read, an example expansion of the
/// following definition was provided for each quotation:
///
/// #[to_variant_types]
/// pub enum Shape<T> {
///     Var(Var),
///     App(App<T>),
/// }
#[proc_macro_attribute]
pub fn to_variant_types
( attrs: proc_macro::TokenStream
, input: proc_macro::TokenStream
) -> proc_macro::TokenStream {
    let attrs: TokenStream = attrs.into();
    let decl     = syn::parse_macro_input!(input as syn::DeriveInput);
    let ident    = &decl.ident;
    let ty_vars  = &decl.generics.params;
    let variants = match &decl.data {
        syn::Data::Enum(ref data) => data.variants.iter(),
        _ => unimplemented!()
    }.collect::<Vec<_>>();

    let is_flat        = repr(&attrs) == "flat";
    let variant_idents = variants.iter().map(|v| &v.ident).collect::<Vec<_>>();
    let structs        = variants.iter().map(|v| mk_product_type(is_flat, &decl, v));
    let structs        = structs.collect::<Vec<_>>();
    let variant_decls  = variant_idents.iter().zip(structs.iter()).map(|(i,v)| gen_variant_decl(i,&v));
    let variant_froms  = variant_idents.iter().zip(structs.iter()).map(|(i,v)| gen_from_impls(i, &decl, &v));

    // Handle single value, unnamed params as created by user.
    let structs = structs.iter().filter(|v| match &v.fields {
        syn::Fields::Unnamed(f) => f.unnamed.len() != 1,
        _                       => true
    });

    let output = quote! {
        #[derive(Eq, PartialEq, Debug)]
        #[derive(Serialize, Deserialize)]
        pub enum #ident <#ty_vars> {
            #(#variant_decls),*
        }
        #(#structs)*
        #(#variant_froms)*
    };
    output.into()
}

