//! This module exports jni bindings generator.

use std::collections::HashMap  as Map;
use std::collections::BTreeSet as Set;
use super::Generator;
use super::ast::*;
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::marker::PhantomData;
use quote::quote;
use itertools::__std_iter::FromIterator;


// ======================
// === Rust Generator ===
// ======================

#[derive(Debug,Clone,Default)]
pub struct Source<T> {
    /// Generated code.
    pub code: TokenStream,
    /// Map from variants to types (i.e. Some => Option).
    pub extends: Map<Name,Name>,
    /// Set of generic parameters a type is used with.
    pub generics: Map<Name,Set<Type>>,
    /// Phantom data.
    t: PhantomData<T>,
}


impl<T> Source<T> {
    pub fn new(extends:Map<Name,Name>, generics:Map<Name,Set<Type>>) -> Self {
        Self{code:quote!(), extends, generics, t:PhantomData::default()}
    }

    /// Get TokenStream for given generator.
    pub fn tokens(&mut self, ast:impl Generator<Self>) -> TokenStream {
        ast.write(self);
        std::mem::replace(&mut self.code, quote!())
    }

    pub fn into<U>(self) -> Source<U> {
        Source{code:quote!(), extends:self.extends, generics:self.generics, t:PhantomData::default()}
    }
}

/// A generator state.
#[derive(Debug,Clone,Default)]
pub struct Collector {
    /// Vector of all available classes.
    pub classes: Vec<Class>,
    /// Map from variants to types (i.e. Some => Option).
    pub extends: Map<Name,Name>,
    /// Set of generic parameters a type is used with.
    pub generics: Map<Name,Set<Type>>,
}

impl Collector {
    pub fn new(file:syn::File) -> Self {
        let mut result = Collector::default();
        File::new("", "".into(), file).write(&mut result);
        result
    }


    pub fn add_method(&mut self, class:Class) {
        let args = class.args.into_iter().map(|(name, typ)|
            (name, self.monomorphize(&typ).1)
        ).collect();
        self.classes.push(Class{typ:class.typ, args});
    }

    pub fn monomorphize(&mut self, typ:&Type) -> (Name, Type) {
        let mut uid  = typ.name.str.clone();
        let mut args = vec![];
        for arg in typ.args.iter() {
            let (name, typ) = self.monomorphize(&arg);
            uid.push_str(&name.str);
            args.push(typ);
        }
        if super::scala::name::type_map(&typ.name).is_some() {
            return (Name(uid), Type{name:typ.name.clone(), args});
        }
        let alias = Type{name:Name(&uid), args};
        self.generics.entry(typ.name.clone()).or_insert(Set::new()).insert(alias);
        return (Name(&uid), Type{name:Name(&uid), args:vec![]});
    }

    pub fn apply(&self, typ:&Type, vars:&Map<&Name,&Type>) -> Type {
        if let Some(&typ) = vars.get(&typ.name) { return typ.clone() }

        let args = typ.args.iter().map(|typ| self.apply(typ,&vars)).collect();

        Type{name:typ.name.clone(), args}
    }

    pub fn types(mut self) -> Vec<AssociatedType> {
        let mut types = vec![];
        for class in &self.classes {
            let name = self.extends.get(&class.typ.name).unwrap_or_else(||&class.typ.name).clone();
            for typ in self.generics.remove(&name).unwrap() {
                let vars  = class.typ.args.iter().map(|t| &t.name).zip(typ.args.iter()).collect();
                let args  = class.args.iter().map(|(name,typ)|
                    (name.clone(), self.apply(&typ, &vars))
                ).collect();
                types.push(AssociatedType {name:name.clone(), class:Class{typ, args}})
            }
        }
        types
    }

    pub fn api(self) -> TokenStream {
        let types = self.types();
        quote! {
            trait Api {
                #(#types);*;
            }
        }
    }
    pub fn scala(self) -> TokenStream {

    }
}

#[derive(Debug,Clone)]
pub struct AssociatedType { pub name: Name, pub class: Class }

impl AssociatedType {
    pub fn typ(&self, typ:&Type) -> TokenStream {
        let args = typ.args.iter().map(|typ| to_tokens(typ));
        let name = &typ.name;
        if super::scala::name::type_map(&name).is_none() {
            quote!(<Self as Api>::#name)
        } else {
            quote!(#name<#(#args),*>)
        }
    }
}

// === Generator Impls ===

impl Generator<Collector> for File {
    fn write(self, source:&mut Collector) {
        Object{name:self.name.clone(), lines:self.content.items}.write(source);
    }
}

impl Generator<Collector> for Object {
    fn write(self, source:&mut Collector) {
        for item in self.lines() {
            match item {
                Term::Object(val) => val.write(source),
                Term::Type  (val) => val.write(source),
                Term::Class (val) => val.write(source),
                Term::Enum  (val) => val.write(source),
            }
        }
    }
}

impl Generator<Collector> for TypeAlias {
    fn write(self, source:&mut Collector) {
        if self.val.args.is_empty() {
            source.monomorphize(&self.val);
        }
    }
}

impl Generator<Collector> for Class {
    fn write(self, source:&mut Collector) {
        source.add_method(self.clone());
        if self.typ.args.is_empty() {
            source.monomorphize(&self.typ);
        }
    }
}

impl Generator<Collector> for Enum {
    fn write(self, source:&mut Collector) {
        for (_, variant) in self.variants.into_iter() {
            source.extends.insert(variant.typ.name.clone(), self.typ.name.clone());
            source.add_method(variant);
        }
        if self.typ.args.is_empty() {
            source.monomorphize(&self.typ);
        }
    }
}



// === ToTokens Impls ===

impl ToTokens for &Name {
    fn to_tokens(&self, tokens:&mut TokenStream) {
        syn::Ident::new(&self.str, proc_macro2::Span::call_site()).to_tokens(tokens)
    }
}

impl ToTokens for &AssociatedType {
    fn to_tokens(&self, tokens:&mut TokenStream) {
        let typ = &self.class.typ.name;
        let fun = &name::var(&typ);
        let arg = self.class.args.iter().map(|(ref name, ref typ)| {
            let typ = sef.typ(typ);
            quote!(#name:#typ)
        });

        quote!(fn #fun(#(#arg),*) -> <Self as Api>::#typ).to_tokens(tokens)
    }
}



// ========================
// === Name Conversions ===
// ========================

pub mod name {
    use crate::generation::ast::Name;
    use inflector::Inflector;


    /// https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html
    pub fn type_map(Name:&Name) -> String {
       match name.str.as_str() {
           "usize" | "isize" | "u64" | "i64" => "L",
           "u32"   | "i32"                   => "I",
           "u16"   | "i16"   | "i8"          => "S",
           "u8"                              => "B",
           "boolean"                         => "Z",
           "char"                            => "C",
           "f64"                             => "D",
           "f32"                             => "F",
           _                                 => None?,
       }
    }


    /// Creates a Rust type name `foo_bar => FooBar`.
    pub fn typ(name:&Name) -> Name {
        let mut string = name.str.to_camel_case();
        string[0..1].make_ascii_uppercase();
        string.into()
    }

    /// Creates a Rust variable name `FooBar => foo_bar`.
    pub fn var(name:&Name) -> Name {
        let mut name = name.str.to_snake_case();
        name[0..1].make_ascii_lowercase();
        name.into()
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
    fn test_classes() {
        let rust = parse! {
            pub enum FooBarBaz {
                Foo(a::Foo),
                Bar(a::Bar),
                Baz(b::Baz),
            }
            mod a {
                struct Foo {x:Box<Vec<i32>>}
                struct Bar {x:usize, y:u8  }
            }
        };
        let classes = vec![
            Class::from(&parse!(struct Foo {variant:Foo})),
            Class::from(&parse!(struct Bar {variant:Bar})),
            Class::from(&parse!(struct Baz {variant:Baz})),
            Class::from(&parse!(struct Foo {x:Box<Vec<i32>>})),
            Class::from(&parse!(struct Bar {x:usize, y:u8  })),
        ];
        assert_eq!(Collector::new(rust).classes, classes);
    }


    #[test]
    fn test_monomorphization() {
        let     generics = Collector::new(parse!(struct A(B<X,Y>, C<B<X,Box<i32>>>);)).generics;
        let     boxi32 = Type{name:Name("Box"),args:vec![Name("i32").into()]};
        let mut expected = Map::new();
        let mut set      = Set::new();

        set.insert(Type{name:Name("BXY"), args:vec![Name("X").into(), Name("Y").into()]});
        set.insert(Type{name:Name("BXBoxi32"), args:vec![Name("X").into(), boxi32]});
        expected.insert(Name("B"), std::mem::replace(&mut set, Set::new()));

        set.insert(Type{name:Name("CBXBoxi32"), args:vec![Name("BXBoxi32").into()]});
        expected.insert(Name("C"), std::mem::replace(&mut set, Set::new()));

        set.insert(Name("A").into());
        expected.insert(Name("A"), std::mem::replace(&mut set, Set::new()));

        set.insert(Name("X").into());
        expected.insert(Name("X"), std::mem::replace(&mut set, Set::new()));

        set.insert(Name("Y").into());
        expected.insert(Name("Y"), std::mem::replace(&mut set, Set::new()));

        assert_eq!(generics, expected);
    }

    #[test]
    fn test_foo() {
        let source = Collector::new(parse!{
            struct A(B<X,Y>, C<B<X,Box<i32>>>);
            struct B<X,Y>(X,Y);
            struct C<T>(T);
        });
        let expected = quote! {
            trait Api {
                type A;
                fn a(val0:<Self as Api>::BXY, val1:<Self as Api>::CBXBoxi32) -> <Self as Api>::A;
                type BXBoxi32;
                fn bx_boxi_32(val0:<Self as Api>::X, val1:Box<i32 <> >) -> <Self as Api>::BXBoxi32;
                type BXY;
                fn bxy(val0:<Self as Api>::X, val1:<Self as Api>::Y) -> <Self as Api>::BXY;
                type CBXBoxi32;
                fn cbx_boxi_32(val0:<Self as Api>::BXBoxi32) -> <Self as Api>::CBXBoxi32;
            }
        };
        assert_eq!(source.api().to_string(), expected.to_string())
    }
}
