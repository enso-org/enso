//! This module exports generator ast.

use itertools::Itertools;
use syn;
use syn::Ident;
use quote::quote;



// =================
// === Scala AST ===
// =================

/// Represents a single source file `$name.scala`.
#[derive(Debug,Clone)]
pub struct File {
    pub lib     : Stdlib,
    pub package : String,
    pub name    : Name,
    pub content : syn::File,
}

impl File {
    /// Creates a Scala source file.
    ///
    /// Note that the content of the file is going to be wrapped in an extra object
    /// `object FileName { content.. }`, because Scala (compared to Rust) doesn't support
    /// top level type aliases.
    pub fn new(name:&str, package:String, content:syn::File) -> Self {
        Self{lib:Stdlib(), package, name:name.into(), content}
    }
}

/// Represents a modified standard library.
#[derive(Debug,Clone,Copy,Default)]
pub struct Stdlib();

/// Represents an object `object Name { line; ... }`.
#[derive(Debug,Clone)]
pub struct Object {
    pub name  : Name,
    pub lines : Vec<syn::Item>,
}

/// Represents valid top level terms.
#[derive(Debug,Clone)]
pub enum Term {
    Object(Object),
    Type(TypeAlias),
    Class(Class),
    Enum(Enum),
}

impl Object {
    pub fn lines(self) -> impl Iterator<Item=Term> {
        self.lines.into_iter().flat_map(|item| Some(match item {
            syn::Item::Mod(val) => {
                let (_, lines) = val.content.unwrap_or_default();
                Term::Object(Object{name: Name(&val.ident), lines})
            }
            syn::Item::Struct(ref val) => Term::Class(val.into()),
            syn::Item::Enum(ref val) => Term::Enum(val.into()),
            syn::Item::Type(ref val) => Term::Type(val.into()),
            _                        => None?,
        }))
    }
}

/// Represents a class `class Name[Generics](field:Field, ...)`
#[derive(Debug,Clone,Ord,PartialOrd,Eq,PartialEq)]
pub struct Class {
    pub typ  : Type,
    pub args : Vec<(Name,Type)>,
}

/// Represents a set of classes extending a sealed trait.
#[derive(Debug,Clone)]
pub struct Enum {
    pub typ      : Type,
    pub variants : Vec<(Option<Name>, Class)>,
}

/// Represents the `extends Name` statement.
#[derive(Debug,Clone,Copy)]
pub struct Extends<'a> { pub name:&'a Name }

/// Constructs a new `extends Name` statement.
pub fn extends(name:&Name) -> Extends {
    Extends{name}
}

/// Represents a qualified type `Foo.Foo[Bar[Baz], ..]`
#[derive(Debug,Clone,Hash,PartialEq,Eq,PartialOrd,Ord)] // TODO Eq is too slow
pub struct Type { pub name: Name, pub args: Vec<Type> }

/// Represents a type alias `type Foo = Bar[Baz]`
#[derive(Debug,Clone)]
pub struct TypeAlias {
    pub typ: Type,
    pub val: Type,
}

/// Represents a type name or a variable name.
#[derive(Debug,Clone,Hash,PartialEq,Eq,PartialOrd,Ord)]
pub struct Name { pub str:String }

impl Name {
    pub fn add(mut self, other:&Name) -> Self {
        self.str.push_str(&other.str);
        self
    }
}


// === Constructors ===

#[allow(non_snake_case)]
pub fn Name<T:Into<Name>>(t:T) -> Name {
    t.into()
}

#[allow(non_snake_case)]
pub fn Class(name:Type, fields:&syn::Fields) -> Class {
    let args = fields.iter().enumerate().map(|(i,arg)| {
       let name = arg.ident.as_ref().map_or(Name(format!("val{}",i)),Name);
       (name, Type::from(&arg.ty))
    }).collect();

    Class{ typ: name,args}
}

#[allow(non_snake_case)]
pub fn Type(name:impl Into<Name>, generics:&syn::Generics) -> Type {
    let mut args = vec![];
    for arg in generics.params.iter() {
        if let syn::GenericParam::Type(typ) = arg {
            args.push(Type{name:Name(&typ.ident), args:vec![]})
        }
    }
    Type{name:name.into(), args}
}


// == From Impls ==

impl From<&str> for Name {
    fn from(name:&str) -> Self {
        Name{ str:name.into()}
    }
}

impl From<&String> for Name {
    fn from(name:&String) -> Self {
        Name{ str:name.into()}
    }
}

impl From<String> for Name {
    fn from(name:String) -> Self {
        Name{ str: name }
    }
}

impl From<&Ident> for Name {
    fn from(name:&Ident) -> Self {
        quote!(#name).to_string().into()
    }
}

impl From<&syn::ItemType> for TypeAlias {
    fn from(ty:&syn::ItemType) -> Self {
        Self{ typ: Type(&ty.ident, &ty.generics), val:Type::from(ty.ty.as_ref())}
    }
}

impl From<&syn::ItemStruct> for Class {
    fn from(val:&syn::ItemStruct) -> Self {
        Class(Type(&val.ident, &val.generics), &val.fields)
    }
}

impl From<&syn::ItemEnum> for Enum {
    fn from(val:&syn::ItemEnum) -> Self {
        let     generics = &val.generics;
        let mut variants = vec![];
        for var in val.variants.iter() {
            match &var.fields {
                syn::Fields::Named(_) =>
                    variants.push((None, Class(Type(&var.ident, generics), &var.fields))),
                syn::Fields::Unnamed(fields) => {
                    if let syn::Type::Path(path) = fields.unnamed.first().unwrap().ty.clone() {
                        let segments             = path.path.segments.iter().rev().take(2);
                        let (name, object)       = segments.map(|s| Name(&s.ident)).collect_tuple().unwrap();
                        let args                 = vec![(Name("variant"), Type{name,args:vec![]})];
                        variants.push((Some(object), Class{ typ: Type(&var.ident, generics), args}));
                    }
                }
                _ => (),
            }
        }
        Self { typ: Type(&val.ident, generics), variants}
    }
}

impl From<Name> for Type {
    fn from(name:Name) -> Self {
        Type{name, args:vec![]}
    }
}

impl From<&syn::Type> for Type {
    fn from(typ:&syn::Type) -> Self {
        let mut name = Name("");
        let mut args = vec![];
        if let syn::Type::Path(path) = typ {
            let typ  = path.path.segments.last().unwrap();
            if let syn::PathArguments::AngleBracketed(params) = &typ.arguments {
                for typ in params.args.iter() {
                    if let syn::GenericArgument::Type(typ) = typ {
                        args.push(Type::from(typ));
                    }
                }
            }
            name = Name(&typ.ident);
        }
        Self{name, args}
    }
}
