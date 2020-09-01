// //! This module exports jni bindings generator.
//
// use std::collections::HashMap  as Map;
// use std::collections::BTreeSet as Set;
// use syn;
// use super::Generator;
// use super::ast::*;
// use proc_macro2::TokenStream;
// use quote::ToTokens;
// use quote::quote;
// use syn::AngleBracketedGenericArguments;
//
//
// // =======================
// // === Rust  Generator ===
// // =======================
//
// #[derive(Debug,Clone,Copy)]
// pub struct Collector;
//
// /// A state of Scala source code.
// #[derive(Debug,Clone,Default)]
// pub struct Source<'a,Type> {
//     /// The content of the file.
//     code:TokenStream,
//     /// Set of constructors a type has.
//     methods:Map<Name,Vec<Class<'a>>>,
//     /// Set of generic parameters a type is used with.
//     generics:Map<Name,Set<Name>>,
//
// }
//
// impl<'a> Source<'a,Collector> {
//     /// Associates a generic parameter with the given type.
//     pub fn add_generic(&mut self, typ:Name, args:Name) {
//         self.generics.entry(typ).or_insert(Set::new()).insert(args);
//     }
// }
//
// impl<'a,Type> Source<'a,Type> {
//     /// Get TokenStream for given generator.
//     pub fn code(&mut self, ast:impl Generator<Self>) -> TokenStream {
//         ast.write(self);
//         std::mem::replace(&mut self.code, quote!())
//     }
// }
//
// #[derive(Debug,Clone)]
// pub struct Method<'a> {
//     pub obj: Class<'a>,
//     pub typ: Option<Name>,
// }
//
// pub fn method(obj:Class, typ:Option<Name>) -> Method {
//     Method{obj,typ}
// }
//
//
// // == Impls ==
//
// impl<'a> Method<'a> {
//     pub fn tokens(&self, source:&mut Source<'a,Collector>, generics:Option<Generics<'a>>) -> TokenStream {
//         let typ  = &self.obj.name;
//         let name = name::var(&self.obj.name);
//         let args = self.obj.fields().map(|(name,typ)| {
//             let name = &name;
//             let typ  = source.code(typ);
//             quote!(#name:#typ)
//         });
//
//         match &self.typ {
//             Some(typ) => quote!(           fn #name(&self, #(#args),*) -> #typ),
//             None      => quote!(type #typ; fn #name(&self, #(#args),*) -> #typ),
//         }
//     }
// }
//
// // == ScalaGenerator Impls ==
//
// impl<'a> Generator<Source<'a,Collector>> for &'a File {
//     fn write(self, source:&mut Source<'a,Collector>) {
//         let name    = &self.name;
//         let object  = Object{name:self.name.clone(), lines:&self.content.items[..]};
//         let content = source.code(object);
//
//         source.code = quote!(trait #name { #content })
//     }
// }
//
// impl<'a> Generator<Source<'a,Collector>> for &'a Stdlib {
//     fn write(self, source:&mut Source<'a,Collector>) {
//         source.code = quote!{
//             type Uuid;
//             fn uuid(&self) -> Self::Uuid;
//         }
//     }
// }
//
// impl<'a,T> Generator<Source<'a,T>> for Object<'a> {
//     fn write(self, source:&mut Source<'a,T>) {
//         let lines = self.lines().map(|item| match item {
//             Term::Object(val) => source.code(val),
//             Term::Type  (val) => source.code(val),
//             Term::Class (val) => source.code(val),
//             Term::Enum  (val) => source.code(val),
//         });
//
//         source.code = quote!(#(#lines)*)
//     }
// }
//
// impl<'a> Generator<Source<'a,Collector>> for TypeAlias<'a> {
//     fn write(self, source:&mut Source<'a,Collector>) {
//         let name = &self.name;
//
//         source.code = quote!(type #name;);
//     }
// }
//
// impl<'a> Generator<Source<'a,Collector>> for Class<'a> {
//     fn write(self, source:&mut Source<'a,Collector>) {
//         source.code = method(self, None).tokens(source, None)
//     }
// }
//
// impl<'a> Generator<Source<'a,Collector>> for Enum<'a> {
//     fn write(self, source:&mut Source<'a,Collector>) {
//         let enum_    = &self.val.name;
//         let generics = Generics::from(&self.val.generics);
//         let variants = self.variants().map(|variant| {
//             match variant {
//                 Variant::Named{name, fields} => {
//                     Class{name, generics, fields}
//                 }
//                 Variant::Unnamed{ref class, ..} => {
//                     let name = &self.name.clone().add(&class).to_var();
//                     quote!(fn #name(val:#class) -> Self::#name;) //TODO
//                 }
//             }
//         });
//
//         source.code = quote!(type #enum_; #(#variants)*)
//     }
// }
//
// impl<'a> Generator<Source<'a,Collector>> for Type<'a> {
//     fn write(self, source:&mut Source<'a,Collector>) {
//         source.code = quote!();
//         if let syn::Type::Path(path) = self.typ {
//             let typ = path.path.segments.last().unwrap();
//             let boxed = typ.ident.to_string().as_str() == "Box";
//             let name  = &typ.ident;
//             if let syn::PathArguments::AngleBracketed(params) = &typ.arguments {
//                 let mut args = name.to_string();
//                 for typ in params.args.iter() {
//                     if let syn::GenericArgument::Type(typ) = typ {
//                         args += &source.code(Type::from(typ)).to_string();
//                     }
//                 }
//                 let args  = Name::from(args);
//                 let args_ = &args;
//                 source.code = quote!(#args_);
//
//                 source.add_generic(typ(&name), args);
//             } else {
//                 source.code = quote!(#name)
//             }
//         }
//     }
// }
//
// impl ToTokens for &Name {
//     fn to_tokens(&self, tokens: &mut TokenStream) {
//         self.name.to_tokens(tokens)
//     }
// }
//
// pub mod name {
//     use crate::generation::ast::Name;
//     use inflector::Inflector;
//
//
//
//     /// Creates a Rust type name.
//     ///
//     /// `foo_bar` => `FooBar`
//     pub fn typ(name:&Name) -> Name {
//         let mut string = name.name.to_camel_case();
//         string[0..1].make_ascii_uppercase();
//         string.into()
//     }
//
//     /// Creates a Rust variable name (camel case).
//     ///
//     /// `Foo_bar` => `fooBar`
//     pub fn var(name:&Name) -> Name {
//         let mut name = name.name.to_snake_case();
//         name[0..1].make_ascii_lowercase();
//         name.into()
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
//
//
//
//     #[test]
//     fn test_file() {
//         let rust:syn::File = syn::parse_quote! {
//             type A<X> = B<X,Y>;
//
//             pub enum FooBarBaz {
//                 Foo(a::Foo),
//                 Bar(a::Bar),
//                 Baz(b::Baz),
//             }
//             mod a {
//                 struct Foo {}
//                 struct Bar {x:usize, y:u8, z:b::Type}
//             }
//             mod b {
//                 type Type = Baz;
//
//                 enum Baz {
//                     Baz1 {},
//                     Baz2 {foo_bar:Box<Vec<i32>>},
//                 }
//             }
//         };
//
//         let jni:syn::File = syn::parse_quote! {
//             pub trait Zero {
//                 type AnyFoo;
//                 type FooBarBaz;
//
//                 fn foo_bar_baz_foo(&self, val:Self::Fooi32) -> Self::FooBarBaz;
//                 fn foo_bar_baz_bar(&self, val:Self::Bar)    -> Self::FooBarBaz;
//                 fn foo_bar_baz_baz(&self, val:Self::Baz)    -> Self::FooBarBaz;
//
//                 fn any_foo(&self) -> Self::AnyFoo;
//
//                 type Bar;
//
//                 fn bar(x:usize, y:u8, z:Type);
//
//                 type Type;
//                 type Baz;
//
//                 fn baz1(&self) -> Self::Baz;
//                 fn baz2(foo_bar:Self::VecI32) -> Self::Baz;
//             }
//         };
//
//         let file = File::new("Ast", "org.enso.ast".into(), rust);
//         let src:Source<'_> = (&file).source();
//
//         assert_eq!(src.code.to_string(), quote!(#jni).to_string());
//     }
// }
//
//
//
// //
// // impl<'a> Zero for ZeroImpl<'a> {
// //     type AnyFoo = JObject<'a>;
// //     type FooBarBaz = JObject<'a>;
// //
// //     fn foo_bar_baz_foo(&self, val:Self::FooI32) -> Self::FooBarBaz {
// //         self.foo_bar_baz_foo.call(&[val])
// //     }
// //
// //     fn foo_bar_baz_bar(&self, val:Self::Bar) -> Self::FooBarBaz {
// //         self.foo_bar_baz_bar.call(&[val])
// //     }
// //
// //     fn foo_bar_baz_baz(&self, val:Self::Baz) -> Self::FooBarBaz {
// //         self.foo_bar_baz_baz.call(&[val])
// //     }
// //
// //
// //     fn foo(&self) -> Self::Foo {
// //         self.foo.call(&[])
// //     }
// //
// //     type Bar = JObject<'a>;
// //
// //     fn bar(&self, x: usize, y: u8, z: Self::Type) {
// //         self.bar.call(&[x, y, z])
// //     }
// //
// //     type Type = JObject<'a>;
// //     type Baz = JObject<'a>;
// //
// //     fn baz1(&self) -> Self::Baz {
// //         self.baz1.call(&[])
// //     }
// //
// //     type VecI32 = JObject<'a>;
// //
// //     fn baz2(&self, foo_bar:Self::VecI32) -> Self::Baz {
// //         self.baz2.call(&[self.boxed(foo_bar)])
// //     }
// // }
