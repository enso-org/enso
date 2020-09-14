//! This module exports an internal AST of Rust source code.

use itertools::Itertools;
use quote::quote;
use std::ops::Add;
use syn;
use syn::Ident;
use syn::ItemMod;



// =================
// === Scala AST ===
// =================

/// Represents a single source file `$name.scala`.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct File {
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
    pub fn new(name:&str, package:&str, content:syn::File) -> Self {
        Self{package:package.into(),name:name.into(),content}
    }
}

/// Represents an object `object Name { line; ... }`.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Module<'a> {
    pub name  : Name,
    pub lines : &'a [syn::Item],
}

/// Represents a class `class Name[Generics](field:Field, ...)`.
///
/// This can either correspond to a Rust struct or a single enum variant.
#[allow(missing_docs)]
#[derive(Debug,Clone,Ord,PartialOrd,Eq,PartialEq)]
pub struct Class {
    pub doc   : String,
    pub typ   : Type,
    pub args  : Vec<Field>,
    /// The class fields have user defined names.
    pub named : bool,
}

/// A single class field.
#[allow(missing_docs)]
#[derive(Debug,Clone,Ord,PartialOrd,Eq,PartialEq)]
pub struct Field {
    pub name : Name,
    pub typ  : Type,
}

/// Represents a set of classes extending a sealed trait.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Enum {
    pub typ      : Type,
    pub variants : Vec<Variant>,
}

/// A single enum variant.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Variant {
    pub module : Option<Name>,
    pub class  : Class,
}

/// Represents the `extends Name` statement.
#[allow(missing_docs)]
#[derive(Debug,Clone,Copy)]
pub struct Extends<'a> { pub name:&'a Name }

/// Constructs a new `extends Name` statement.
pub fn extends(name:&Name) -> Extends {
    Extends{name}
}

/// Represents a qualified type `Path.Name[Bar[Baz], ..]`
#[allow(missing_docs)]
#[derive(Debug,Clone,Hash,PartialEq,Eq,PartialOrd,Ord)]
pub struct Type {
    pub name : Name,
    pub path : Vec<Name>,
    pub args : Vec<Type>
}

/// Represents a type alias `type Foo = Bar[Baz]`
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct TypeAlias {
    pub name : Type,
    pub typ  : Type,
}

/// Represents a type name or a variable name.
#[allow(missing_docs)]
#[derive(Debug,Clone,Default,Hash,PartialEq,Eq,PartialOrd,Ord)]
pub struct Name { pub str:String }

impl Add<&Name> for Name {
    type Output = Name;

    fn add(mut self, other:&Name) -> Self {
        self.str.push_str(&other.str);
        self
    }
}


// === Constructors ===

impl<'a> Module<'a> {
    /// Creates a new Module instance.
    pub fn new(name:Name, lines:&'a [syn::Item]) -> Self {
        Module{name,lines}
    }
}

/// Name constructor.
#[allow(non_snake_case)]
pub fn Name<T:Into<Name>>(t:T) -> Name {
    t.into()
}

impl Class {
    /// Create a new Class instance.
    pub fn new(doc:String, typ:Type, args:Vec<Field>, named:bool) -> Self {
        Self{doc,typ,args,named}
    }
}

/// Class constructor.
#[allow(non_snake_case)]
pub fn Class(name:Type, fields:&syn::Fields, attrs:&Vec<syn::Attribute>) -> Class {
    let named = if let syn::Fields::Named{..} = fields {true} else {false};
    let args  = fields.iter().enumerate().map(|(i,arg)| {
        let name = arg.ident.as_ref().map_or(Name(format!("val{}",i)),Name);
        Field{name,typ:Type::from(&arg.ty)}
    }).collect();
    let doc_atr = attrs.iter().find(|atr| match atr.path.get_ident() {
        Some(ident) => ident.to_string().as_str() == "doc",
        None        => false,
    });
    let doc = if let Some(doc_atr) = doc_atr {
        let tokens = doc_atr.tokens.to_string();
        tokens.chars().skip(4).take(tokens.len()-5).collect()
    } else { "".to_string() };
    Class::new(doc,name,args,named)
}

impl Type {
    /// Create a new Type instance.
    pub fn new(name:Name, path:Vec<Name>, args:Vec<Type>) -> Self {
        Self{name,path,args}
    }
}

/// Type constructor.
#[allow(non_snake_case)]
pub fn Type(name:impl Into<Name>,  generics:&syn::Generics) -> Type {
    let mut args = Vec::default();
    for arg in generics.params.iter() {
        if let syn::GenericParam::Type(typ) = arg {
            args.push(Type::from(Name(&typ.ident)))
        }
    }
    Type::new(name.into(),Default::default(),args)
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
        Self{ name:Type(&ty.ident, &ty.generics), typ:Type::from(ty.ty.as_ref())}
    }
}

impl From<&syn::ItemStruct> for Class {
    fn from(val:&syn::ItemStruct) -> Self {
        Class(Type(&val.ident, &val.generics), &val.fields, &val.attrs)
    }
}


impl<'a> From<&'a syn::ItemMod> for Module<'a> {
    fn from(val:&'a ItemMod) -> Self {
        let lines = if let Some((_,lines)) = &val.content {&lines[..]} else {&[]};
        Module{name:Name(&val.ident),lines}
    }
}

impl From<&syn::ItemEnum> for Enum {
    fn from(val:&syn::ItemEnum) -> Self {
        let     generics = &val.generics;
        let mut variants = Vec::default();
        for var in val.variants.iter() {
            let (module,typ) = match &var.fields {
                syn::Fields::Unnamed(fields) => {
                    if let syn::Type::Path(val) = fields.unnamed.first().unwrap().ty.clone() {
                        let err      = "Unnamed fields in enum must be qualified.";
                        let path     = val.path.segments.iter().rev().take(2);
                        let (_,name) = path.map(|s|Name(&s.ident)).collect_tuple().expect(err);
                        (Some(name),Type(&var.ident,generics))
                    } else {
                        (None,Type(&var.ident,generics))
                    }
                }
                _ => (None,Type(&var.ident,generics)),
            };
            variants.push(Variant{module,class:Class(typ,&var.fields,&var.attrs)});
        }
        Self{typ:Type(&val.ident,generics),variants}
    }
}

impl From<Name> for Type {
    fn from(name:Name) -> Self {
        Type{name, path:Default::default(), args:Default::default()}
    }
}

impl From<&str> for Type {
    fn from(name:&str) -> Self {
        Type::from(Name::from(name))
    }
}

impl From<&syn::Type> for Type {
    fn from(typ:&syn::Type) -> Self {
        let mut name = Name::default();
        let mut path = Vec::default();
        let mut args = Vec::default();
        if let syn::Type::Path(typ) = typ {
            path    = typ.path.segments.iter().dropping_back(1).map(|s|Name(&s.ident)).collect();
            let typ = typ.path.segments.last().unwrap();
            name = Name(&typ.ident);
            if let syn::PathArguments::AngleBracketed(params) = &typ.arguments {
                for typ in params.args.iter() {
                    if let syn::GenericArgument::Type(typ) = typ {
                        args.push(Type::from(typ));
                    }
                }
            }
        }
        Self{name,path,args}
    }
}
