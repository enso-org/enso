//! This module exports jni bindings generator.

use std::collections::HashMap;
use syn;
use super::Generator;
use super::ast::*;
use proc_macro2::TokenStream;
use quote::ToTokens;
use quote::quote;


// =======================
// === Scala Generator ===
// =======================

/// A state of Scala source code.
#[derive(Debug,Clone,Default)]
pub struct Source {
    /// The content of the file.
    code:TokenStream,
    /// Type parameter monomorphization.
    params:HashMap<Name,Name>
}

impl Source {
    /// Get TokenStream for given generator.
    pub fn code(&mut self, ast:impl Generator<Source>) -> TokenStream {
        ast.write(self);
        std::mem::replace(&mut self.code, quote!())
    }
}

#[derive(Debug,Clone)]
pub struct Method<'a> {
    pub obj: Class<'a>,
    pub typ: Option<Name>,
}

pub fn method(obj:Class, typ:Option<Name>) -> Method {
    Method{obj,typ}
}


// == Impls ==

impl<'a> Method<'a> {
    pub fn tokens(&self, source:&mut Source, generics:Option<Generics<'a>>) -> TokenStream {
        let typ  = &self.obj.name;
        let name = &self.obj.name.to_var();
        let args = self.obj.fields().map(|(name,typ)| {
            let name = &name;
            let typ  = source.code(typ);
            quote!(#name:#typ)
        });

        match &self.typ {
            Some(typ) => quote!(           fn #name(&self, #(#args),*) -> #typ),
            None      => quote!(type #typ; fn #name(&self, #(#args),*) -> #typ),
        }
    }
}

// == ScalaGenerator Impls ==

impl Generator<Source> for File {
    fn write(self, source:&mut Source) {
        let name    = &self.name;
        let content = source.code(Object{name:self.name.clone(), lines:&self.content.items[..]});

        source.code = quote!(trait #name { #content })
    }
}

impl Generator<Source> for Stdlib {
    fn write(self, source:&mut Source) {
        source.code = quote!{
            type Uuid;
            fn uuid(&self) -> Self::Uuid;
        }
    }
}

impl<'a> Generator<Source> for Object<'a> {
    fn write(self, source:&mut Source) {
        let lines = self.lines().map(|item| match item {
            Term::Object(val) => source.code(val),
            Term::Type  (val) => source.code(val),
            Term::Class (val) => source.code(val),
            Term::Enum  (val) => source.code(val),
        });

        source.code = quote!(#(#lines)*)
    }
}

impl<'a> Generator<Source> for TypeAlias<'a> {
    fn write(self, source:&mut Source) {
        let name = &self.name;

        source.code = quote!(type #name;);
    }
}

impl<'a> Generator<Source> for Class<'a> {
    fn write(self, source:&mut Source) {
        source.code = method(self, None).tokens(source, None)
    }
}

impl<'a> Generator<Source> for Enum<'a> {
    fn write(self, source:&mut Source) {
        let name     = &self.name;
        let generics = Generics::from(&self.val.generics);
        let variants = self.variants().map(|variant| {
            match variant {
                Variant::Named{name, fields} => {
                    let name = self.name.add(&name);
                    method(Class{name:name.clone(),generics,fields},Some(name)).tokens(source, None)
                }
                Variant::Unnamed{ref class, ..} => {
                    let name = &self.name.add(&class).to_var();
                    quote!(fn #name(val:#class) -> Self::#name;) //TODO
                }
            }
        });

        source.code = quote!(type #name; #(#variants)*)
    }
}

// impl<'a> Generator<Source> for &Generics<'a> {
//     fn write(self, source:&mut Source) {
//         if self.generics.params.is_empty() {return}
//         source.write("[");
//         for (i, param) in self.generics.params.iter().enumerate() {
//             if let syn::GenericParam::Type(typ) = param {
//                 write!(source, ", ".when(i!=0), &Name::typ(&typ.ident));
//             }
//         }
//         source.write("]");
//     }
// }

impl<'a> Generator<Source> for Type<'a> {
    fn write(self, source:&mut Source) {
        source.code = quote!();
        if let syn::Type::Path(path) = self.typ {
            let path = path.path.segments.iter().map(|typ| {
                let boxed = typ.ident.to_string().as_str() == "Box";
                let name  = &typ.ident;
                if let syn::PathArguments::AngleBracketed(typ) = &typ.arguments {
                    let args = typ.args.iter().flat_map(|typ|
                        if let syn::GenericArgument::Type(typ) = typ {
                            Some(source.code(Type::from(typ)))
                        } else { None }
                    );
                    quote!(#name<#(#args),*>)
                } else { quote!(#name) }
            });
            source.code = quote!(Self::#(#path)::*)
        }
    }
}

impl ToTokens for &Name {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;



    #[test]
    fn test_file() {
        let rust:syn::File = syn::parse_quote! {
            type A<X> = B<X,Y>;

            pub enum FooBarBaz {
                Foo(a::Foo),
                Bar(a::Bar),
                Baz(b::Baz),
            }
            mod a {
                struct Foo {}
                struct Bar {x:usize, y:u8, z:b::Type}
            }
            mod b {
                type Type = Baz;

                enum Baz {
                    Baz1 {},
                    Baz2 {foo_bar:Box<Vec<i32>>},
                }
            }
        };

        let jni:syn::File = syn::parse_quote! {
            pub trait Zero {
                type AnyFoo;
                type FooBarBaz;
            
                fn foo_bar_baz_foo(&self, val:Self::FooI32) -> Self::FooBarBaz;
                fn foo_bar_baz_bar(&self, val:Self::Bar) -> Self::FooBarBaz;
                fn foo_bar_baz_baz(&self, val:Self::Baz) -> Self::FooBarBaz;
            
                fn foo(&self) -> Self::AnyFoo;
            
                type Bar;
            
                fn bar(x:usize, y:u8, z:Type);
            
                type Type;
                type Baz;
            
                fn baz1(&self) -> Self::Baz;
                fn baz2(foo_bar:Self::VecI32) -> Self::Baz;
            }
        };

        let src:Source = File::new("Ast", "org.enso.ast".into(), rust).source();

        assert_eq!(src.code.to_string(), quote!(#jni).to_string());
    }
}



// 
// impl<'a> Zero for ZeroImpl<'a> {    
//     type AnyFoo = JObject<'a>;
//     type FooBarBaz = JObject<'a>;
// 
//     fn foo_bar_baz_foo(&self, val:Self::FooI32) -> Self::FooBarBaz {
//         self.foo_bar_baz_foo.call(&[val])
//     }
// 
//     fn foo_bar_baz_bar(&self, val:Self::Bar) -> Self::FooBarBaz {
//         self.foo_bar_baz_bar.call(&[val])
//     }
// 
//     fn foo_bar_baz_baz(&self, val:Self::Baz) -> Self::FooBarBaz {
//         self.foo_bar_baz_baz.call(&[val])
//     }
// 
// 
//     fn foo(&self) -> Self::Foo {
//         self.foo.call(&[])
//     }
// 
//     type Bar = JObject<'a>;
// 
//     fn bar(&self, x: usize, y: u8, z: Self::Type) {
//         self.bar.call(&[x, y, z])
//     }
// 
//     type Type = JObject<'a>;
//     type Baz = JObject<'a>;
// 
//     fn baz1(&self) -> Self::Baz {
//         self.baz1.call(&[])
//     }
//     
//     type VecI32 = JObject<'a>;
// 
//     fn baz2(&self, foo_bar:Self::VecI32) -> Self::Baz {
//         self.baz2.call(&[self.boxed(foo_bar)])
//     }
// }