//! This module exports generator ast.

use itertools::Itertools;
use proc_macro2::Span;
use syn;
use syn::Ident;
use inflector::Inflector;



// =================
// === Scala AST ===
// =================

/// Represents a single source file `$name.scala`.
#[derive(Debug,Clone)]
pub struct File {
    pub lib     : Stdlib,
    pub package : String,
    pub name    : Name,
    pub content : syn::File ,
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
pub struct Object<'a> {
    pub name  : Name,
    pub lines : &'a [syn::Item],
}

/// Represents valid top level terms.
#[derive(Debug,Clone)]
pub enum Term<'a> {
    Object(Object<'a>),
    Type(TypeAlias<'a>),
    Class(Class<'a>),
    Enum(Enum<'a>),
}

impl<'a> Object<'a> {
    pub fn lines(&self) -> impl Iterator<Item=Term> {
        self.lines.iter().flat_map(|item| Some(match &item {
            syn::Item::Struct(syn::ItemStruct{ident,generics,fields,..}) =>
                if let syn::Fields::Named(fields) = fields {
                    Term::Class(Class{name:Name::typ(&ident), generics:generics.into(), fields})
                } else { None? }
            syn::Item::Mod(val) => {
                let (_, lines) = val.content.as_ref()?;
                Term::Object(Object{name:Name::typ(&val.ident), lines:&lines[..]})
            }
            syn::Item::Enum(val) => Term::Enum(val.into()),
            syn::Item::Type(val) => Term::Type(val.into()),
            _                    => None?,
        }))
    }
}

/// Represents a class `class Name[Generics](field:Field, ...)`
#[derive(Debug,Clone)]
pub struct Class<'a> {
    pub name     : Name,
    pub generics : Generics<'a>,
    pub fields   : &'a syn::FieldsNamed,
}

impl<'a> Class<'a> {
    pub fn fields(&self) -> impl Iterator<Item=(Name,Type)> {
        self.fields.named.iter().flat_map(|field| {
            Some((Name::var(field.ident.as_ref()?), Type{typ:&field.ty}))
        })
    }
}
/// Represents a set of classes extending a sealed trait.
#[derive(Debug,Clone)]
pub struct Enum<'a> {
    pub name : Name,
    pub val  : &'a syn::ItemEnum
}

/// Represents an enum variant.
#[derive(Debug,Clone)]
pub enum Variant<'a> {
    Named   { name :Name, fields:&'a syn::FieldsNamed },
    Unnamed { class:Name, object:Name }
}

impl<'a> Enum<'a> {
    pub fn variants(&self) -> impl Iterator<Item=Variant> {
        self.val.variants.iter().flat_map(|variant| Some(match &variant.fields {
            syn::Fields::Named  (fields) => Variant::Named{name:Name::typ(&variant.ident), fields},
            syn::Fields::Unnamed(fields) => {
                if let syn::Type::Path(path) = fields.unnamed.first()?.ty.clone() {
                    let segments             = path.path.segments.iter().rev().take(2);
                    let (class, object)      = segments.map(|s|Name::typ(&s.ident)).collect_tuple()?;
                    Variant::Unnamed{class, object}
                } else { None? }
            }
            _ => None?,
        }))
    }
}

/// Represents the `extends Name` statement.
#[derive(Debug,Clone)]
pub struct Extends<'a> { pub name:&'a Name }

/// Constructs a new `extends Name` statement.
pub fn extends(name:&Name) -> Extends {
    Extends{name}
}

/// Represents a set of type parameters `[Generic1, Generic2, ...]`.
#[derive(Debug,Clone)]
pub struct Generics<'a> { pub generics:&'a syn::Generics }

/// Represents a qualified type `Foo.Foo[Bar[Baz], ..]`
#[derive(Debug,Clone)]
pub struct Type<'a> { pub typ:&'a syn::Type }

/// Represents a type alias `type Foo = Bar[Baz]`
#[derive(Debug,Clone)]
pub struct TypeAlias<'a> {
    pub name     : Name,
    pub generics : Generics<'a>,
    pub typ      : Type<'a>,
}

/// Represents a type name or a variable name.
#[derive(Debug,Clone,Hash,PartialEq,Eq)]
pub struct Name { pub name:Ident }

impl Name {
    /// Creates a Scala variable name (camel case).
    ///
    /// `Foo_bar` => `fooBar`
    pub fn var(name:&Ident) -> Self {
        Self::from(name.to_string().to_camel_case().as_str())
    }

    /// Creates a Scala type name.
    ///
    /// The following Rust types are automatically converted to Scala types:
    /// ```code
    /// u32   | i32   | u16 | i16 | i8 => Int,
    /// usize | isize | u64 | i64      => Long,
    /// u8                             => Byte,
    /// char                           => Char,
    /// Vec                            => Vector,
    /// Uuid                           => UUID,
    /// ```
    pub fn typ(name:&Ident) -> Self {
        let mut string;
        let name = match name.to_string().as_str() {
            "u32"   | "i32"   | "u16" | "i16" | "i8" => "Int",
            "usize" | "isize" | "u64" | "i64"        => "Long",
            "u8"                                     => "Byte",
            "char"                                   => "Char",
            "Vec"                                    => "Vector",
            "Uuid"                                   => "UUID",
            name                                     => {
                string = name.to_camel_case();
                string[0..1].make_ascii_uppercase();
                &string
            }
        };
        name.into()
    }
}


// == From Impls ==

impl From<&str> for Name {
    fn from(val:&str) -> Self {
        Name{name:Ident::new(val, Span::call_site())}
    }
}

impl<'a> From<&'a syn::Type> for Type<'a> {
    fn from(typ:&'a syn::Type) -> Self {
        Type{typ}
    }
}

impl<'a> From<&'a syn::Generics> for Generics<'a> {
    fn from(generics:&'a syn::Generics) -> Self {
        Generics{generics}
    }
}

impl<'a> From<&'a syn::ItemType> for TypeAlias<'a> {
    fn from(ty:&'a syn::ItemType) -> Self {
        Self{name:Name::typ(&ty.ident), generics:(&ty.generics).into(), typ:ty.ty.as_ref().into()}
    }
}

impl<'a> From<&'a syn::ItemEnum> for Enum<'a> {
    fn from(val:&'a syn::ItemEnum) -> Self {
        Self{name:Name::typ(&val.ident), val}
    }
}
