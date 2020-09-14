//! This module exports jni bindings generator.

use crate::ast::*;
use crate::generator::Generator;
use crate::types;

use std::collections::BTreeMap as Map;
use std::collections::BTreeSet as Set;
use proc_macro2::TokenStream;
use quote::ToTokens;
use quote::quote;
use itertools::Itertools;


// ===========================
// === File Representation ===
// ===========================


/// Class with monomorphized type arguments.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct MonomorphizedClass {
    pub class   : Class,
    pub args    : Vec<Type>,
    pub extends : Option<Name>,
}

impl MonomorphizedClass {
    /// Construct a new instance.
    pub fn new(class:Class, args:Vec<Type>, extends:Option<Name>) -> Self {
        Self{class,args,extends}
    }
}

/// A representation of rust file with additional lookup tables.
#[derive(Debug,Clone,Default)]
pub struct PreprocessedFile {
    /// File name.
    pub file_name : String,
    /// Package name.
    pub package : String,
    /// Currently active module.
    pub module : Vec<Name>,
    /// Set of all class names.
    pub class_names : Set<Name>,
    /// Vector of all available classes and their arguments.
    pub classes : Vec<MonomorphizedClass>,
    /// Set of generic parameters a type is used with.
    pub generics : Map<Name,Set<Type>>,
}

impl PreprocessedFile {
    /// Add a class to the list of classes and monomorphize the types of it's arguments.
    pub fn add_class(&mut self, mut class:Class, extends:Option<Name>) {
        let args = class.args.iter().map(|Field{name,typ}|
            Field{name:name.clone(), typ:self.monomorphize(&typ).typ}
        ).collect();
        let arg_types  = class.args.into_iter().map(|field| field.typ);
        class.typ.path = self.module[1..].into();
        let class      = Class::new(class.doc,class.typ,args,class.named);
        self.classes.push(MonomorphizedClass::new(class, arg_types.collect(), extends));
    }

    /// Return a monomorphized name and the original type with monomorphized arguments.
    pub fn monomorphize(&mut self, typ:&Type) -> UniqueType {
        let mut uid  = typ.name.str.clone();
        let mut args = vec![];
        let     path = vec![];
        for arg in typ.args.iter() {
            let typ = self.monomorphize(&arg);
            uid.push_str(&typ.unique_name.str);
            args.push(typ.typ);
        }
        if args.is_empty() || types::builtin(&typ.name).is_some() {
            return UniqueType::new(Name(uid), Type::new(typ.name.clone(), path, args));
        }
        let alias = Type::new(Name(&uid), path, args);
        self.generics.entry(typ.name.clone()).or_insert_with(Set::new).insert(alias);
        UniqueType::new(Name(&uid), Type::from(Name(&uid)))
    }
}

/// Representation of type with an additional unique name.
#[derive(Debug,Clone)]
pub struct UniqueType {
    /// Unique name of this type.
    unique_name: Name,
    /// The type itself.
    typ : Type,
}

impl UniqueType {
    /// Return a new UniqueType instance.
    pub fn new(unique_name:Name, typ:Type) -> Self {
        Self{unique_name,typ}
    }
}

// === Generator Impls ===

impl Generator<PreprocessedFile> for &File {
    fn write(self, source:&mut PreprocessedFile) {
        source.package   = self.package.clone();
        source.file_name = self.name.clone().str;
        Module::new(self.name.clone(),&self.content.items[..]).write(source);
    }
}

impl<'a> Generator<PreprocessedFile> for &Module<'a> {
    fn write(self, source:&mut PreprocessedFile) {
        source.module.push(self.name.clone());
        for item in self.lines {
            match item {
                syn::Item::Mod   (val) => Module::from(val).write(source),
                syn::Item::Type  (val) => TypeAlias::from(val).write(source),
                syn::Item::Struct(val) => Class::from(val).write(source),
                syn::Item::Enum  (val) => Enum::from(val).write(source),
                _                      => (),
            }
        }
        source.module.pop();
    }
}

impl Generator<PreprocessedFile> for &TypeAlias {
    fn write(self, source:&mut PreprocessedFile) {
        if self.typ.args.is_empty() {
            source.monomorphize(&self.typ);
        }
    }
}

impl Generator<PreprocessedFile> for &Class {
    fn write(self, source:&mut PreprocessedFile) {
        source.add_class(self.clone(), None);
        source.class_names.insert(self.typ.name.clone());
        if self.typ.args.is_empty() {
            source.generics.entry(self.typ.name.clone()).or_default().insert(self.typ.clone());
        }
    }
}

impl Generator<PreprocessedFile> for &Enum {
    fn write(self, source:&mut PreprocessedFile) {
        for Variant{class,..} in self.variants.iter().cloned() {
            source.add_class(class,Some(self.typ.name.clone()));
        }
        source.class_names.insert(self.typ.name.clone());
        if self.typ.args.is_empty() {
            source.generics.entry(self.typ.name.clone()).or_default().insert(self.typ.clone());
        }
    }
}


// === ToTokens Impls ===

impl ToTokens for Name {
    fn to_tokens(&self, tokens:&mut TokenStream) {
        syn::Ident::new(&self.str,proc_macro2::Span::call_site()).to_tokens(tokens)
    }
}



// ======================
// === Rust Generator ===
// ======================

/// An associated type in a trait with a monorphized signature.
#[derive(Debug,Clone)]
pub struct AssociatedType {
    /// Name of the associated type.
    pub name : Name,
    /// Name of the enum variant if any.
    pub variant : Option<Name>,
    /// The class the associated type represents.
    pub class : Class,
}

impl AssociatedType {
    /// Creates a new AssociatedType instance.
    pub fn new(name:Name, variant:Option<Name>, class:Class) -> Self {
        AssociatedType{name,variant,class}
    }

    /// Generate a type signature of a type in trait.
    ///
    /// For builtin types this returns `#name<typ(#(#args),*>`, for anything else `Api::#name`.
    pub fn typ(typ:&Type) -> TokenStream {
        let args = typ.args.iter().map(Self::typ);
        let name = &typ.name;
        if types::builtin(&name).is_none() {
            quote!(<Self as Api>::#name)
        } else if !typ.args.is_empty() {
            quote!(#name<#(#args),*>)
        } else {
            quote!(#name)
        }
    }

    /// Generate a function signature of associated type constructor.
    ///
    /// For example
    /// ```
    /// use ast_generation::api::AssociatedType;
    /// use ast_generation::ast::*;
    /// use quote::quote;
    ///
    /// let args  = vec![("x", "i64").into(), ("y", "Y").into()];
    /// let class = Class::new("".into(), "".into(), args, true);
    /// let typ   = AssociatedType::new(Name("Name"), None, class);
    ///
    /// assert_eq!(typ.fun(), quote!(name(x:i64, y:<Self as Api>::Y) -> <Self as Api>::Name))
    /// ```
    pub fn fun(&self) -> TokenStream {
        let typ  = &self.name;
        let name = &name::var(&if let Some(var) = &self.variant {typ.clone()+var} else {typ.clone()});
        let args = self.class.args.iter().map(|Field{ref name,ref typ}| {
            let typ = Self::typ(typ);
            quote!(#name:#typ)
        });
        quote!(fn #name(&self,#(#args),*) -> <Self as Api>::#typ)
    }

}

/// A generator of an API for constructing the AST.
#[derive(Debug,Clone)]
pub struct Source {
    /// Name of the scala package.
    pub package: String,
    /// Set of all class names.
    pub class_names: Set<Name>,
    /// Vector of all classes.
    pub classes: Vec<Class>,
    /// Vector of user defined associated types.
    pub types: Vec<AssociatedType>,
    /// Set of generic parameters a type is used with.
    pub generics: Map<Name,Set<Type>>,
}

impl Source {
    /// Generate API definition for the given AST definition.
    pub fn api(ast:&str) -> String {
        let input = syn::parse_str(ast).unwrap();
        let file  = File::new("Ast","org.enso.ast",input);
        let api   = Self::new(file.into()).ast_api();
        api.to_string()
    }

    /// Create a new instance from `Collector`.
    pub fn new(file:PreprocessedFile) -> Self {
        fn apply(typ:&Type, vars:&Map<&Name,&Type>) -> Type {
            if let Some(&typ) = vars.get(&typ.name) { typ.clone() } else {
                let args = typ.args.iter().map(|typ| apply(typ,&vars)).collect();
                Type::new(typ.name.clone(),Vec::default(),args)
            }
        }
        let     package = file.package.replace('.',"/") + "/" + &file.file_name;
        let mut classes = Vec::default();
        let mut types   = Vec::default();
        for MonomorphizedClass{class,args,extends} in file.classes {
            let (name,variant) = if let Some(name) = extends {
                ( name.clone()          , Some(class.typ.name.clone()) )
            } else {
                ( class.typ.name.clone(), None                         )
            };
            for mut typ in file.generics.get(&name).cloned().unwrap_or_default() {
                let name = std::mem::replace(&mut typ.name,name.clone());
                let vars = class.typ.args.iter().map(|t| &t.name).zip(&typ.args).collect();
                let args = class.args.iter().map(|Field{name,typ}|
                    Field{name:name.clone(),typ:apply(&typ,&vars)}
                ).collect();
                let class = Class::new(class.doc.clone(),typ,args,class.named);
                types.push(AssociatedType::new(name,variant.clone(),class));
            }
            let args = args.into_iter().map(|typ| Field{name:Name(""),typ}).collect();
            if variant.is_none() || class.named {
                classes.push(Class::new("".to_string(),class.typ,args,class.named))
            }
        }
        Self{class_names:file.class_names,classes,package,types,generics:file.generics}
    }

    /// Generate the AST trait.
    pub fn ast_trait(&self) -> TokenStream {
        let typs      = self.generics.iter().flat_map(|(_,typ)| typ.iter().map(|typ| &typ.name));
        let funs      = self.types.iter().map(|obj| obj.fun());
        let typ_docs  = self.generics.iter().flat_map(|(typ,args)| args.iter().map(move |arg| {
            let args  = arg.args.iter().map(AssociatedType::typ);
            let typ   = if arg.args.is_empty() { quote!(#typ) } else {
                quote!(#typ<#(#args),*>)
            };
            format!("The AST Node {}.",typ.to_string().replace(" ",""))
        })).collect_vec();
        let fun_docs  = self.types.iter().map(|obj| {
            format!("Construct Self::{}.{}",obj.name.str,obj.class.doc)
        });
        quote! {
            pub trait Api {
                #( #[doc=#typ_docs] type #typs );*;
                #( #[doc=#fun_docs]      #funs );*;
            }
        }
    }

    /// Generate the Rust struct that is used to construct the Rust AST.
    pub fn rust_struct(&self) -> TokenStream {
        quote!{
            #[derive(Debug,Clone,Copy,Default)]
            pub struct Rust;
        }
    }

    /// Generate an implementation of the AST trait for the Rust AST.
    pub fn rust_impl(&self) -> TokenStream {
        let types    = self.generics.iter().flat_map(|(typ,args)| args.iter().map(move |arg| {
            let args = arg.args.iter().map(AssociatedType::typ);
            let name = &arg.name;
            quote!(type #name = #typ<#(#args),*>)
        }));
        let funs     = self.types.iter().map(|obj| {
            let fun  = obj.fun();
            let args = obj.class.args.iter().map(|Field{name,..}| name);
            let args = quote!(#(#args),*);
            let name = &obj.class.typ.name;
            let name = if let Some(variant) = &obj.variant { quote!(#name::#variant) } else {
                quote!(#name)
            };
            if obj.class.named {
                quote!(#fun{ #name{#args} })
            } else {
                quote!(#fun{ #name(#args) })
            }
        });
        quote! {
            impl Api for Rust {
                #(#types);*;

                #(#funs)*
            }
        }
    }

    /// Generate the Scala struct that is used to construct the Scala AST.
    pub fn scala_struct(&self) -> TokenStream {
        let fields  = self.classes.iter().map(|obj| name::var(&obj.typ.name)).collect_vec();
        let objects = self.classes.iter().map(|obj| {
            let     name = types::jni_name("".to_string(),&obj.typ,self.package.as_str());
            let mut args = String::from("(");
            for Field{typ,..} in &obj.args {
                args = types::jni_arg(args,&typ,self.package.as_str(),&self.class_names);
            }
            args += ")V";
            quote!(Object::new(&env,#name,#args))
        });
        quote! {
            use crate::ffi::Object;
            use crate::ffi::StdLib;
            use jni::JNIEnv;

            #[allow(missing_debug_implementations)]
            #[derive(Clone)]
            pub struct Scala<'a> {
                pub env:&'a JNIEnv<'a>,
                pub lib:StdLib<'a>,
                #(pub #fields:Object<'a>),*
            }

            impl<'a> Scala<'a> {
                pub fn new(env:&'a JNIEnv<'a>) -> Self {
                    Self { env,lib:StdLib::new(env),#(#fields:#objects),* }
                }
            }
        }
    }

    /// Generate the implementation of AST trait for the Scala AST.
    pub fn scala_impl(&self) -> TokenStream {
        let types    = self.generics.iter().flat_map(|(_,typ)| typ.iter().map(|typ|&typ.name));
        let funs     = self.types.iter().map(|obj| {
            let fun  = obj.fun();
            if obj.variant.is_some() && !obj.class.named {
                return quote!(#fun { val0 })
            }
            let typ  = &name::var(obj.variant.as_ref().unwrap_or_else(||&obj.class.typ.name));
            let args = obj.class.args.iter().map(|Field{name,..}| quote!(#name.jvalue(&self.lib)));
            quote!(#fun { self.#typ.init(&[#(#args),*]) })
        });
        quote! {
            use crate::ffi::ToJValue;
            use jni::objects::JObject;

            impl<'a> Api for Scala<'a> {
                #(type #types = JObject<'a>);*;

                #(#funs)*
            }
        }
    }

    /// Generate the AST trait and implementation for Scala and Rust AST.
    pub fn ast_api(&self) -> TokenStream {
        let rust_struct  = self.rust_struct();
        let scala_struct = self.scala_struct();
        let ast_trait    = self.ast_trait();
        let rust_impl    = self.rust_impl();
        let scala_impl   = self.scala_impl();
        quote! {
            #![allow(missing_docs)]

            use crate::ast::*;

            #rust_struct
            #scala_struct

            #ast_trait
            #rust_impl
            #scala_impl
        }
    }
}


// === From Impls ===

impl From<File> for PreprocessedFile {
    fn from(file:File) -> Self {
        let mut result = PreprocessedFile::default();
        file.write(&mut result);
        result
    }
}



// ========================
// === Name Conversions ===
// ========================

/// Module for name manipulation.
pub mod name {
    use crate::ast::Name;
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
    fn test_monomorphization() {
        let     generics = PreprocessedFile::from(File::new("","",parse!{
            struct A(B<X,Y>,C<B<X,Box<i32>>>);
        })).generics;
        let     boxi32   = Type::new(Name("Box"),vec![],vec![Name("i32").into()]);
        let mut expected = Map::new();
        let mut set      = Set::new();
        set.insert(Type::new(Name("BXY"),vec![],vec![Name("X").into(),Name("Y").into()]));
        set.insert(Type::new(Name("BXBoxi32"),vec![],vec![Name("X").into(),boxi32]));
        expected.insert(Name("B"),std::mem::replace(&mut set,Set::new()));
        set.insert(Type::new(Name("CBXBoxi32"),vec![],vec![Name("BXBoxi32").into()]));
        expected.insert(Name("C"),std::mem::replace(&mut set,Set::new()));
        set.insert(Name("A").into());
        expected.insert(Name("A"),std::mem::replace(&mut set,Set::new()));
        assert_eq!(generics,expected);
    }

    #[test]
    fn test_trait_generics() {
        let source = Source::new(File::new("Ast","ast",parse!{
            /// Generic wrapper.
            struct A<T>(T);
            /// Wrapper around A<i32>.
            struct B(A<i32>);
        }).into());
        let expected = quote! {
            pub trait Api {
                #[doc="The AST Node A<i32>."]
                type Ai32;
                #[doc="The AST Node B."]
                type B;

                #[doc="Construct Self::Ai32. Generic wrapper."]
                fn ai_32(&self,val0:i32              ) -> <Self as Api>::Ai32;
                #[doc="Construct Self::B. Wrapper around A<i32>."]
                fn b    (&self,val0:<Self as Api>::Ai32) -> <Self as Api>::B;
            }
        };
        assert_eq!(source.ast_trait().to_string(),expected.to_string())
    }

    #[test]
    fn test_trait_nested_generics() {
        let source = Source::new(File::new("","",parse!{
            struct A(B<X,Y>, C<B<X,Box<i32>>>);
            struct B<X,Y>(X,Y);
            struct C<T>(T);
        }).into());
        let expected = quote! {
            pub trait Api {
                type A;
                type BXBoxi32;
                type BXY;
                type CBXBoxi32;

                fn a
                (&self, val0:<Self as Api>::BXY, val1:<Self as Api>::CBXBoxi32) -> <Self as Api>::A;
                fn bx_boxi_32
                (&self, val0:<Self as Api>::X, val1:Box<i32 <> >) -> <Self as Api>::BXBoxi32;
                fn bxy
                (&self, val0:<Self as Api>::X, val1:<Self as Api>::Y) -> <Self as Api>::BXY;
                fn cbx_boxi_32
                (&self, val0:<Self as Api>::BXBoxi32) -> <Self as Api>::CBXBoxi32;
            }
        };
        assert_eq!(source.ast_trait().to_string(),expected.to_string())
    }

    #[test]
    fn test_rust_nested_generics() {
        let source = Source::new(File::new("","",parse!{
            struct A(B<X,Y>, C<B<X,Box<i32>>>);
            struct B<X,Y>(X,Y);
            struct C<T>(T);
        }).into());
        let expected = quote! {
            impl Api for Rust {
                type A         = A<>;
                type BXBoxi32  = B< <Self as Api>::X, Box<i32> >;
                type BXY       = B< <Self as Api>::X, <Self as Api>::Y>;
                type CBXBoxi32 = C< <Self as Api>::BXBoxi32>;

                fn a(&self, val0:<Self as Api>::BXY, val1:<Self as Api>::CBXBoxi32) -> <Self as Api>::A {
                    A(val0, val1)
                }
                fn bx_boxi_32(&self, val0:<Self as Api>::X, val1:Box<i32>) -> <Self as Api>::BXBoxi32 {
                    B(val0, val1)
                }
                fn bxy(&self, val0:<Self as Api>::X, val1:<Self as Api>::Y) -> <Self as Api>::BXY {
                    B(val0, val1)
                }
                fn cbx_boxi_32(&self,val0:<Self as Api>::BXBoxi32) -> <Self as Api>::CBXBoxi32 {
                    C(val0)
                }
            }
        };
        assert_eq!(source.rust_impl().to_string(),expected.to_string())
    }

    #[test]
    fn test_scala_nested_generics_struct() {
        let source = Source::new(File::new("Ast","ast",parse!{
            struct A(B<i8,u8>, C<B<i32,Vec<i64>>>);
            struct B<X,Y>(X,Y);
            struct C<T>(T);
        }).into());
        let expected = quote! {
            use crate::ffi::Object;
            use crate::ffi::StdLib;
            use jni::JNIEnv;

            #[allow(missing_debug_implementations)]
            #[derive(Clone)]
            pub struct Scala<'a> {
                pub env: &'a JNIEnv<'a>,
                pub lib: StdLib<'a>,
                pub a: Object<'a>,
                pub b: Object<'a>,
                pub c: Object<'a>
            }

            impl<'a> Scala<'a> {
                pub fn new(env: &'a JNIEnv<'a>) -> Self {
                    Self {
                        env,
                        lib: StdLib::new(env),
                        a: Object::new(&env, "ast/Ast$A", "(Last/Ast$B;Last/Ast$C;)V"),
                        b: Object::new(&env, "ast/Ast$B", "(Ljava/lang/Object;Ljava/lang/Object;)V"),
                        c: Object::new(&env, "ast/Ast$C", "(Ljava/lang/Object;)V")
                    }
                }
            }
        };
        assert_eq!(source.scala_struct().to_string(),expected.to_string())
    }

    #[test]
    fn test_scala_nested_generics_impl() {
        let source = Source::new(File::new("","",parse!{
            struct A(B<X,Y>, C<B<X,Box<i32>>>);
            struct B<X,Y>(X,Y);
            struct C<T>(T);
        }).into());
        let expected = quote! {
            use crate::ffi::ToJValue;
            use jni::objects::JObject;

            impl<'a> Api for Scala<'a> {
                type A         = JObject<'a>;
                type BXBoxi32  = JObject<'a>;
                type BXY       = JObject<'a>;
                type CBXBoxi32 = JObject<'a>;

                fn a(&self, val0:<Self as Api>::BXY, val1:<Self as Api>::CBXBoxi32) -> <Self as Api>::A {
                    self.a.init(&[val0.jvalue(&self.lib), val1.jvalue(&self.lib)])
                }
                fn bx_boxi_32(&self, val0:<Self as Api>::X, val1:Box<i32>) -> <Self as Api>::BXBoxi32 {
                    self.b.init(&[val0.jvalue(&self.lib), val1.jvalue(&self.lib)])
                }
                fn bxy(&self, val0:<Self as Api>::X, val1:<Self as Api>::Y) -> <Self as Api>::BXY {
                    self.b.init(&[val0.jvalue(&self.lib), val1.jvalue(&self.lib)])
                }
                fn cbx_boxi_32(&self,val0:<Self as Api>::BXBoxi32) -> <Self as Api>::CBXBoxi32 {
                    self.c.init(&[val0.jvalue(&self.lib)])
                }
            }
        };
        assert_eq!(source.scala_impl().to_string(),expected.to_string())
    }

    #[test]
    fn test_rust_unnamed_enum_impl() {
        let source = Source::new(File::new("Ast","ast",parse!{
            enum Enum { A(a::AA), B(a::AA) }
        }).into());
        let expected = quote! {
            impl Api for Rust {
                type Enum = Enum<>;
                fn enum_a(&self, val0:<Self as Api>::AA) -> <Self as Api>::Enum { Enum::A(val0) }
                fn enum_b(&self, val0:<Self as Api>::AA) -> <Self as Api>::Enum { Enum::B(val0) }
            }
        };
        assert_eq!(source.rust_impl().to_string(),expected.to_string())
    }

    #[test]
    fn test_scala_unnamed_enum_impl() {
        let source = Source::new(File::new("Ast","ast",parse!{
            enum Enum { A(a::AA), B(a::AA) }
        }).into());
        let expected = quote! {
            use crate::ffi::ToJValue;
            use jni::objects::JObject;

            impl<'a> Api for Scala<'a> {
                type Enum = JObject<'a>;
                fn enum_a(&self, val0:<Self as Api>::AA) -> <Self as Api>::Enum { val0 }
                fn enum_b(&self, val0:<Self as Api>::AA) -> <Self as Api>::Enum { val0 }
            }
        };
        assert_eq!(source.scala_impl().to_string(),expected.to_string())
    }

    #[test]
    fn test_scala_unnamed_enum_struct() {
        let source = Source::new(File::new("Ast","ast",parse!{
            enum Enum { A(a::AA), B(a::AA) }
        }).into());
        let expected = quote! {
            use crate::ffi::Object;
            use crate::ffi::StdLib;
            use jni::JNIEnv;

            #[allow(missing_debug_implementations)]
            #[derive(Clone)]
            pub struct Scala<'a> { pub env: &'a JNIEnv<'a>, pub lib: StdLib<'a>, }

            impl<'a> Scala<'a> {
                pub fn new(env: &'a JNIEnv<'a>) -> Self {
                    Self { env, lib: StdLib::new(env), }
                }
            }
        };
        assert_eq!(source.scala_struct().to_string(),expected.to_string())
    }

    #[test]
    fn test_scala_box() {
        let source = Source::new(File::new("Ast","ast",parse!{
            struct A(Box<i32>);
        }).into());
        let expected = quote! {
            use crate::ffi::Object;
            use crate::ffi::StdLib;
            use jni::JNIEnv;

            #[allow(missing_debug_implementations)]
            #[derive(Clone)]
            pub struct Scala<'a> {
                pub env: &'a JNIEnv<'a>,
                pub lib: StdLib<'a>,
                pub a: Object<'a>
            }

            impl<'a> Scala<'a> {
                pub fn new(env: &'a JNIEnv<'a>) -> Self {
                    Self {
                        env,
                        lib: StdLib::new(env),
                        a: Object::new(&env, "ast/Ast$A", "(I)V")
                    }
                }
            }
        };
        assert_eq!(source.scala_struct().to_string(),expected.to_string())
    }
}
