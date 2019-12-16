//! This crate defines a custom derive macro `Iterator`. Should not be used
//! directly, but only through `shapely` crate, as it provides utilities
//! necessary for the generated code to compile.

extern crate proc_macro;

use prelude::*;

use inflector::Inflector;
use proc_macro2::{TokenStream, Ident, Span};
use quote::quote;
use syn;
use macro_utils::{fields_list, repr};


/// For `struct Foo<T>` provides:
/// * `IntoIterator` implementations for `&'t Foo<T>` and `&mut 't Foo<T>`;
/// * `iter` and `into_iter` methods.
///
/// The iterators will go over each field that declared type is same as the
/// struct's last type parameter.
///
/// Caller must have the following features enabled:
/// ```
/// #![feature(generators)]
/// #![feature(type_alias_impl_trait)]
/// ```
///
/// When used on type that takes no type parameters, like `struct Foo`, does
/// nothing but yields no errors.
#[proc_macro_derive(Iterator)]
pub fn derive_iterator
(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl   = syn::parse_macro_input!(input as syn::DeriveInput);
    let params = &decl.generics.params.iter().collect::<Vec<_>>();
    match params.last() {
        Some(last_param) => derive_iterator_for(&decl, &last_param),
        None             => proc_macro::TokenStream::from(quote! {})
    }
}

/// Derives iterator that iterates over fields of type `target_param`.
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
        syn::Data::Enum(_) | syn::Data::Union(_) => {
            println!("derive(Iterator) for non-struct type is not implemented");
            println!("{} will get no-op Iterator impls", decl.ident);
            Vec::new()
        }
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
        shapely::GeneratingIterator
            (move || { #(yield &t.#matched_fields;)* })
    };
    let iter_body_mut  = quote! {
        shapely::GeneratingIterator
            (move || { #(yield &mut t.#matched_fields;)* })
    };
    let iter_body_dummy = quote! { shapely::EmptyIterator::new() };
    let empty           = matched_fields.is_empty();
    let iter_body       = if empty { &iter_body_dummy } else { &iter_body_ref };
    let iter_body_mut   = if empty { &iter_body_dummy } else { &iter_body_mut };
    let expanded        = quote! {
        // See Note [Expansion Example] - for this and further examples meaning.
        // type FooIterator<'t, T> = impl Iterator<Item = &'t T>;
        type #t_iterator<'t, #(#params),*> =
            impl Iterator<Item = &'t #target_param>;

        // pub fn foo_iterator<'t, T>
        // (t: &'t Foo<T>) -> FooIterator<'t, T> {
        //    shapely::GeneratingIterator(move || {
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
        //    shapely::GeneratingIterator(move || {
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

// Note [Expansion Example]
// ~~~~~~~~~~~~~~~~~~~~~~~~
// In order to make the definition easier to read, an example expansion of the
// following definition was provided for each quotation:
//
// #[derive(Iterator)]
// pub struct Foo<S, T> { foo: T }
