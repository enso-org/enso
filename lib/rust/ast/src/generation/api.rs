//! This module exports jni bindings generator.

use std::collections::HashMap  as Map;
use std::collections::BTreeSet as Set;
use super::Generator;
use super::ast::*;
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::marker::PhantomData;
use quote::quote;
use crate::generation::types;


// ======================
// === Rust Generator ===
// ======================

#[derive(Debug,Clone,Default)]
pub struct Source<T> {
    /// Generated code.
    pub code: TokenStream,
    /// Map from type
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
        (&File::new("", "".into(), file)).write(&mut result);
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
        let     path = vec![];
        for arg in typ.args.iter() {
            let (name, typ) = self.monomorphize(&arg);
            uid.push_str(&name.str);
            args.push(typ);
        }
        if super::scala::name::type_map(&typ.name).is_some() {
            return (Name(uid), Type{name:typ.name.clone(), path, args});
        }
        let alias = Type{name:Name(&uid), path, args};
        self.generics.entry(typ.name.clone()).or_insert(Set::new()).insert(alias);
        return (Name(&uid), Type::from(Name(&uid)));
    }

    pub fn apply(&self, typ:&Type, vars:&Map<&Name,&Type>) -> Type {
        if let Some(&typ) = vars.get(&typ.name) { return typ.clone() }

        let args = typ.args.iter().map(|typ| self.apply(typ,&vars)).collect();

        Type{name:typ.name.clone(), path: vec![], args}
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
        let objects = self.types();
        let types   = objects.iter().map(|obj| &obj.class.typ.name);
        let funs    = objects.iter().map(|obj| obj.fun());

        quote! {
            trait Api {
                #(type #types);*;

                #(#funs);*;
            }
        }
    }

    pub fn rust(self) -> TokenStream {
        let objects  = self.types();
        let types    = objects.iter().map(|obj| {
            let name = &obj.class.typ.name;
            let typ  = &obj.name;
            let args = obj.class.typ.args.iter().map(AssociatedType::typ);
            quote!(#name = #typ<#(#args),*>)
        });
        let funs     = objects.iter().map(|obj| {
            let fun  = obj.fun();
            let typ  = &obj.name;
            let args = obj.class.args.iter().map(|(name, _)| name);
            quote!(#fun { #typ{#(#args),*} })
        });

        quote! {
            pub struct Rust;
            impl Api for Rust {
                #(type #types);*;

                #(#funs)*
            }
        }
    }

    pub fn scala(self) -> TokenStream {
        let objects  = self.types();
        let types    = objects.iter().map(|obj| &obj.class.typ.name);
        let funs     = objects.iter().map(|obj| {
            let fun  = obj.fun();
            let typ  = &name::var(&obj.name);
            let args = obj.class.args.iter().map(|(name,_)| name);
            quote!(#fun { self.#typ.init(&[#(#args.into()),*]) })
        });

        let fields  = objects.iter().map(|obj| name::var(&obj.name)).collect::<Vec<_>>();
        let objects = objects.iter().map(|obj| {
            let     name = format!("org.enso.ast$Ast${}", &obj.name.str);
            let mut args = String::from("(");
            for (_,typ) in &obj.class.args {
                if let Some(name) = super::types::builtin(&typ.name) {
                    args += name.scala
                } else if let Some(name) = super::types::stdlib(&typ.name) {
                    args += name.scala
                } else {
                    args += format!("Lorg.enso.ast$Ast${};", &typ.name.str).as_str()
                }
            }
            args += ");";
            quote!(Object::new(&env,#name,#args))
        });


        quote! {
            pub struct Scala<'a> {
                pub env:&'a JNIEnv<'a>,
                #(pub #fields:Object<'a>),*
            }

            impl<'a> Scala<'a> {
                pub fn new(env:&JNIEnv<'a>) -> Self {
                    Self { env, #(#fields:#objects),* }
                }
            }

            impl<'a> Api for Scala<'a> {
                #(type #types = JObject<'a>);*;

                #(#funs)*
            }
        }
    }
}

#[derive(Debug,Clone)]
pub struct AssociatedType { pub name: Name, pub class: Class }

impl AssociatedType {
    pub fn fun(&self) -> TokenStream {
        let typ = &self.class.typ.name;
        let fun = &name::var(&typ);
        let arg = self.class.args.iter().map(|(ref name, ref typ)| {
            let typ = Self::typ(typ);
            quote!(#name:#typ)
        });

        quote!(fn #fun(&self, #(#arg),*) -> <Self as Api>::#typ)
    }

    pub fn typ(typ:&Type) -> TokenStream {
        let args = typ.args.iter().map(Self::typ);
        let name = &typ.name;
        if !types::builtin(&name) {
            quote!(<Self as Api>::#name)
        } else {
            quote!(#name<#(#args),*>)
        }
    }
}

// === Generator Impls ===

impl Generator<Collector> for &File {
    fn write(self, source:&mut Collector) {
        Module {name:self.name.clone(), lines:&self.content.items[..]}.write(source);
    }
}

impl<'a> Generator<Collector> for &Module<'a> {
    fn write(self, source:&mut Collector) {
        for item in self.lines {
            match item {
                syn::Item::Mod   (val) => Module::from(val).write(source),
                syn::Item::Type  (val) => TypeAlias::from(val).write(source),
                syn::Item::Struct(val) => Class::from(val).write(source),
                syn::Item::Enum  (val) => Enum::from(val).write(source),
                _                      => (),
            }
        }
    }
}

impl Generator<Collector> for &TypeAlias {
    fn write(self, source:&mut Collector) {
        if self.val.args.is_empty() {
            source.monomorphize(&self.val);
        }
    }
}

impl Generator<Collector> for &Class {
    fn write(self, source:&mut Collector) {
        source.add_method(self.clone());
        if self.typ.args.is_empty() {
            source.monomorphize(&self.typ);
        }
    }
}

impl Generator<Collector> for &Enum {
    fn write(self, source:&mut Collector) {
        for (_, variant) in &self.variants {
            source.extends.insert(variant.typ.name.clone(), self.typ.name.clone());
            source.add_method(variant.clone());
        }
        if self.typ.args.is_empty() {
            source.monomorphize(&self.typ);
        }
    }
}



// === ToTokens Impls ===

impl ToTokens for Name {
    fn to_tokens(&self, tokens:&mut TokenStream) {
        syn::Ident::new(&self.str, proc_macro2::Span::call_site()).to_tokens(tokens)
    }
}



// ========================
// === Name Conversions ===
// ========================

pub mod name {
    use crate::generation::ast::Name;
    use inflector::Inflector;



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
        let     boxi32   = Type{name:Name("Box"), path:vec![], args:vec![Name("i32").into()]};
        let mut expected = Map::new();
        let mut set      = Set::new();

        set.insert(Type{name:Name("BXY"), path:vec![], args:vec![Name("X").into(), Name("Y").into()]});
        set.insert(Type{name:Name("BXBoxi32"), path:vec![], args:vec![Name("X").into(), boxi32]});
        expected.insert(Name("B"), std::mem::replace(&mut set, Set::new()));

        set.insert(Type{name:Name("CBXBoxi32"), path:vec![], args:vec![Name("BXBoxi32").into()]});
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
    fn test_api() {
        let source = Collector::new(parse!{
            struct A(B<X,Y>, C<B<X,Box<i32>>>);
            struct B<X,Y>(X,Y);
            struct C<T>(T);
        });
        let expected = quote! {
            trait Api {
                type A;
                type BXBoxi32;
                type BXY;
                type CBXBoxi32;

                fn a
                (val0:<Self as Api>::BXY, val1:<Self as Api>::CBXBoxi32) -> <Self as Api>::A;
                fn bx_boxi_32
                (val0:<Self as Api>::X, val1:Box<i32 <> >) -> <Self as Api>::BXBoxi32;
                fn bxy
                (val0:<Self as Api>::X, val1:<Self as Api>::Y) -> <Self as Api>::BXY;
                fn cbx_boxi_32
                (val0:<Self as Api>::BXBoxi32) -> <Self as Api>::CBXBoxi32;
            }
        };
        assert_eq!(source.api().to_string(), expected.to_string())
    }

    #[test]
    fn test_rust() {
        let source = Collector::new(parse!{
            struct A(B<X,Y>, C<B<X,Box<i32>>>);
            struct B<X,Y>(X,Y);
            struct C<T>(T);
        });
        let expected = quote! {
            pub struct Rust;

            impl Api for Rust {
                type A         = A<>;
                type BXBoxi32  = B< <Self as Api>::X, Box<i32 <> > >;
                type BXY       = B< <Self as Api>::X, <Self as Api>::Y>;
                type CBXBoxi32 = C< <Self as Api>::BXBoxi32>;
                fn a
                (val0:<Self as Api>::BXY, val1:<Self as Api>::CBXBoxi32) -> <Self as Api>::A { A{val0, val1} }
                fn bx_boxi_32
                (val0:<Self as Api>::X, val1:Box<i32 <> >) -> <Self as Api>::BXBoxi32 { B{val0, val1} }
                fn bxy
                (val0:<Self as Api>::X, val1:<Self as Api>::Y) -> <Self as Api>::BXY { B{val0, val1} }
                fn cbx_boxi_32
                (val0:<Self as Api>::BXBoxi32) -> <Self as Api>::CBXBoxi32 { C{val0} }
            }
        };
        assert_eq!(source.rust().to_string(), expected.to_string())
    }

    #[test]
    fn test_scalaa() {
        let source = Collector::new(parse!{
            struct A(B<X,Y>, C<B<X,Box<i32>>>);
            struct B<X,Y>(X,Y);
            struct C<T>(T);
        });
        let expected = quote! {
            pub struct Rust;

            impl Api for Rust {
                type A         = A<>;
                type BXBoxi32  = B< <Self as Api>::X, Box<i32 <> > >;
                type BXY       = B< <Self as Api>::X, <Self as Api>::Y>;
                type CBXBoxi32 = C< <Self as Api>::BXBoxi32>;
                fn a
                (val0:<Self as Api>::BXY, val1:<Self as Api>::CBXBoxi32) -> <Self as Api>::A { A{val0, val1} }
                fn bx_boxi_32
                (val0:<Self as Api>::X, val1:Box<i32 <> >) -> <Self as Api>::BXBoxi32 { B{val0, val1} }
                fn bxy
                (val0:<Self as Api>::X, val1:<Self as Api>::Y) -> <Self as Api>::BXY { B{val0, val1} }
                fn cbx_boxi_32
                (val0:<Self as Api>::BXBoxi32) -> <Self as Api>::CBXBoxi32 { C{val0} }
            }
        };
        assert_eq!(source.scala().to_string(), expected.to_string())
    }
}
