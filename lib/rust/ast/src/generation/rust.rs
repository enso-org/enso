//! This module exports jni bindings generator.

use std::collections::HashMap  as Map;
use std::collections::BTreeSet as Set;
use super::Generator;
use super::ast::*;
use proc_macro2::TokenStream;
use quote::quote;
use itertools::Itertools;
use std::marker::PhantomData;




// ======================
// === Rust Generator ===
// ======================

#[derive(Debug,Clone,Copy)]
pub struct Rust;

/// The Rust generator source type.
pub type Source = super::api2::Source<Rust>;




// === Generator Impls ===

impl Generator<Source> for &Vec<Class> {
    fn write(self, source:&mut Source) {
        let body = self.iter().map(|class| source.tokens(class));

        source.code = quote! {
            pub struct Rust;
            impl Api for Rust {
                #(#body)*
            }
        }
    }
}

impl Generator<Source> for &Class {
    fn write(self, source:&mut Source) {
        if let None = source.extends.get(&self.typ.name).cloned() {
            let name = &self.typ.name;
            let body = source.generics.get(&name).cloned().unwrap().into_iter().map(|typ| {
                let fields  = self.args.iter().map(|(ref name,_)| name);
                let fn_name = &super::api2::name::var(&typ.name);
                let ty_name = &typ.name;
                let ty_args = typ.args.iter().map(|typ| source.tokens(typ)).collect_vec();
                let fn_args = self.args.iter().map(|(ref name,ref typ)| {
                    let typ = source.tokens(typ);
                    quote!(#name: #typ)
                }).collect_vec();
               quote! {
                    type #ty_name = #name<#(#ty_args),*>;
                    fn #fn_name(#(#fn_args),*) -> <Self as Api>::#ty_name { #name{#(#fields),*} }
               }
            });
            source.code = quote!(#(#body)*);
        }
    }
}

impl Generator<Source> for &Type {
    fn write(self, source:&mut Source) {
        let name = &self.name;
        if super::scala::name::type_map(&name).is_none() {
            source.code = quote!(Self::#name)
        } else {
            let args    = self.args.iter().map(|typ| source.tokens(typ));
            source.code = quote!(#name<#(#args),*>);
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote as parse;



    #[test]
    fn test_file() {
        let program = parse! {
            struct Foo<X>(X);
            struct Bar(Foo<Foo<Foo<i32>>>);
        };
        let output = quote! {
            pub struct Rust;

            impl Api for Rust {
                type FooFooFooi32 = Foo<Self::FooFooi32>;
                fn foo_foo_fooi_32(val0: Self::X) -> <Self as Api>::FooFooFooi32 { Foo { val0 } }
                type FooFooi32 = Foo<Self::Fooi32>;
                fn foo_fooi_32(val0: Self::X) -> <Self as Api>::FooFooi32 { Foo { val0 } }
                type Fooi32 = Foo<i32 <> >;
                fn fooi_32(val0: Self::X) -> <Self as Api>::Fooi32 { Foo { val0 } }
                type Bar = Bar<>;
                fn bar(val0: Self::Foo) -> <Self as Api>::Bar { Bar { val0 } }
            }
        };
        let     source  = super::super::api2::Collector::new(program);
        let     classes = source.classes;
        let mut source  = Source::new(source.extends, source.generics);

        assert_eq!(source.tokens(&classes).to_string(), output.to_string())
    }
}
