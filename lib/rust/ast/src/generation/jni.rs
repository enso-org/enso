// //! This module exports jni bindings generator.
//
// use std::collections::HashMap  as Map;
// use std::collections::BTreeSet as Set;
// use super::Generator;
// use super::ast::*;
// use proc_macro2::TokenStream;
// use quote::quote;
// use itertools::Itertools;
//
//
// // ======================
// // === Rust Generator ===
// // ======================
//
// #[derive(Debug,Clone,Copy)]
// pub struct Collector;
//
// #[derive(Debug,Clone,Default)]
// pub struct Source {
//     /// Generated code.
//     code: TokenStream,
//     /// Map from variants to types (i.e. Some => Option).
//     extends: Map<Name,Name>,
//     /// Set of generic parameters a type is used with.
//     generics: Map<Name,Set<Type>>,
// }
//
//
// impl Source {
//     /// Get TokenStream for given generator.
//     pub fn tokens(&mut self, ast:impl Generator<Self>) -> TokenStream {
//         ast.write(self);
//         std::mem::replace(&mut self.code, quote!())
//     }
//     /// Generates a rust ast impl.
//     pub fn generate(api:super::api2::Source) -> (super::api2::Source, TokenStream) {
//         let mut this = Source {code:quote!(), extends:api.extends, generics:api.generics};
//         let     body = api.classes.iter().map(|class| this.tokens(class));
//
//         let tokens   = quote! {
//             pub struct Rust;
//             impl Api for Rust {
//                 #(#body)*
//             }
//         };
//         (super::api2::Source{classes:api.classes, extends:this.extends, generics:this.generics}, tokens)
//     }
// }
//
//
// // === Generator Impls ===
//
// impl Generator<Source> for &Class {
//     fn write(self, source:&mut Source) {
//         if let None = source.extends.get(&self.typ.name).cloned() {
//             let name = &self.typ.name;
//             let body = source.generics.get(&name).cloned().unwrap().into_iter().map(|typ| {
//                 let fields  = self.args.iter().map(|(ref name,_)| name);
//                 let fn_name = &super::api2::name::var(&typ.name);
//                 let ty_name = &typ.name;
//                 let ty_args = typ.args.iter().map(|typ| source.tokens(typ)).collect_vec();
//                 let fn_args = self.args.iter().map(|(ref name,ref typ)| {
//                     let typ = source.tokens(typ);
//                     quote!(#name: #typ)
//                 }).collect_vec();
//                quote! {
//                     type #ty_name = #name<#(#ty_args),*>;
//                     fn #fn_name(#(#fn_args),*) -> <Self as Api>::#ty_name { #name{#(#fields),*} }
//                 }
//             });
//             source.code = quote!(#(#body)*);
//         }
//     }
// }
//
// impl Generator<Source> for &Type {
//     fn write(self, source:&mut Source) {
//         let name = &self.name;
//         if super::scala::name::type_map(&name).is_none() {
//             source.code = quote!(Self::#name)
//         } else {
//             let args    = self.args.iter().map(|typ| source.tokens(typ));
//             source.code = quote!(#name<#(#args),*>);
//         }
//     }
// }
//
//
//
// // =============
// // === Tests ===
// // =============
//
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use syn::parse_quote as parse;
//
//
//
//     #[test]
//     fn test_file() {
//         let program = parse! {
//             struct Foo<X>(X);
//             struct Bar(Foo<Foo<Foo<i32>>>);
//         };
//         let output = quote! {
//             pub struct Rust;
//
//             impl Api for Rust {
//                 type FooFooFooi32 = Foo<Self::FooFooi32>;
//                 fn foo_foo_fooi_32(val0: Self::X) -> <Self as Api>::FooFooFooi32 { Foo { val0 } }
//                 type FooFooi32 = Foo<Self::Fooi32>;
//                 fn foo_fooi_32(val0: Self::X) -> <Self as Api>::FooFooi32 { Foo { val0 } }
//                 type Fooi32 = Foo<i32 <> >;
//                 fn fooi_32(val0: Self::X) -> <Self as Api>::Fooi32 { Foo { val0 } }
//                 type Bar = Bar<>;
//                 fn bar(val0: Self::Foo) -> <Self as Api>::Bar { Bar { val0 } }
//             }
//         };
//         let api = super::super::api2::Source::new(program);
//         println!("{:?}", api.generics);
//
//         assert_eq!(Source::generate(api).1.to_string(), output.to_string())
//     }
// }
//
