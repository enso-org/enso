//! This module exports scala ast generator.

#![allow(unused_must_use)]

use itertools::Itertools;
use proc_macro2::Span;
use std::collections::HashMap;
use std::fs;
use syn;
use syn::Ident;
use inflector::Inflector;
use std::io::Read;

macro_rules! push {
    ($source:expr, $($args:expr),*) => { $($source.push($args));* }
}

// =======================
// === Scala Generator ===
// =======================

/// A state of Scala source code.
#[derive(Debug,Clone,Default)]
pub struct ScalaSource {
    /// The content of the file.
    code:String,
    /// Current indentation.
    indent:usize,
    /// Inheritance hierarchy.
    extends:HashMap<Name,Name>
}

impl ScalaSource {
    /// Write scala source code into buffer.
    pub fn push(&mut self, ast:impl ScalaGenerator) {
        ast.write(self)
    }

    pub fn ast() -> std::io::Result<String> {
        let mut content = String::new();
        let mut file    = fs::File::open("lib/rust/ast/src/ast.rs")?;
        file.read_to_string(&mut content);

        let pkg  = String::from("org.enso.ast");
        let file = syn::parse_file(content.as_str()).unwrap();

        Ok(File::new("Ast", pkg, file).to_string())
    }
}

/// A Scala ast generator.
pub trait ScalaGenerator :Sized {
    /// Write Scala source code into buffer.
    fn write(self, source:&mut ScalaSource);

    /// Get string representation of self.
    fn to_string(self) -> String {
        let mut source = ScalaSource::default();
        self.write(&mut source);
        source.code
    }
}

/// Writes an amount of spaces corresponding to the current indent.
#[derive(Debug,Clone,Copy,Default)]
pub struct Tab();

/// Tab generator.
pub const TAB:Tab = Tab();

/// A trait for wrapping a value in Option.
pub trait When :Sized {
    /// Returns Some(self) iff some is true.
    fn when(self, some:bool) -> Option<Self> {
        if some { Some(self) } else { None }
    }
}

// == Trait Impls ==

impl<T> When for T {}

impl ScalaGenerator for &str {
    fn write(self, source:&mut ScalaSource) {
        source.code.push_str(self)
    }
}

impl ScalaGenerator for Tab {
    fn write(self, source:&mut ScalaSource) {
        for _ in 0..source.indent { source.code.push(' ') }
    }
}

impl<T:ScalaGenerator> ScalaGenerator for Option<T> {
    fn write(self, source:&mut ScalaSource) {
        if let Some(t) = self { t.write(source) }
    }
}



// =================
// === Scala AST ===
// =================

/// Represents a single source file `$name.scala`.
#[derive(Debug,Clone)]
pub struct File { lib:Stdlib, package:String, content:Object }

impl File {
    /// Creates a Scala source file.
    ///
    /// Note that the content of the file is going to be wrapped in an extra object
    /// `object FileName { content.. }`, because Scala (compared to Rust) doesn't support
    /// top level type aliases.
    pub fn new(name:&str, package:String, file:syn::File) -> Self {
        let lib     = Stdlib();
        let content = Object{name:Name::new(name), lines:file.items};
        Self{lib, package, content}
    }
}

/// Represents a modified standard library.
#[derive(Debug,Clone,Copy,Default)]
pub struct Stdlib();

/// Represents an object `object Name { line; ... }`.
#[derive(Debug,Clone)]
pub struct Object { name:Name, lines:Vec<syn::Item> }

/// Represents valid top level terms.
#[derive(Debug,Clone)]
pub enum Term {
    Object(Object),
    Type(TypeAlias),
    Class(Class),
    Enum(Enum),
}

impl Object {
    pub fn lines(lines:Vec<syn::Item>) -> impl Iterator<Item=Term> {
        lines.into_iter().flat_map(|item| Some(match item {
            syn::Item::Struct(syn::ItemStruct{ident,generics,fields,..}) =>
            if let syn::Fields::Named(fields) = fields {
                Term::Class(Class{name:Name::typ(&ident), generics:generics.into(), fields})
            } else { None? }
            syn::Item::Mod(val) => {
                let (_, lines) = val.content?;
                Term::Object(Object{name:Name::typ(&val.ident), lines})
            }
            syn::Item::Enum(val) => Term::Enum(val.into()),
            syn::Item::Type(val) => Term::Type(val.into()),
            _                    => None?,
        }))
    }
}

/// Represents a class `class Name[Generics](field:Field, ...)`
#[derive(Debug,Clone)]
pub struct Class { name:Name, generics:Generics, fields:syn::FieldsNamed  }

impl Class {
    pub fn fields(fields:syn::FieldsNamed) -> impl Iterator<Item=(Name,Type)> {
        fields.named.into_iter().flat_map(|field| {
            Some((Name::var(&field.ident?), Type{typ:field.ty}))
        })
    }
}
/// Represents a set of classes extending a sealed trait.
#[derive(Debug,Clone)]
pub struct Enum { name:Name, val:syn::ItemEnum }

/// Represents an enum variant.
#[derive(Debug,Clone)]
pub enum Variant {
    Named { name:Name, fields:syn::FieldsNamed },
    Unnamed { class:Name, object:Name }
}

impl Enum {
    pub fn variants(variants:impl Iterator<Item=syn::Variant>) -> impl Iterator<Item=Variant> {
        variants.flat_map(|variant| Some(match variant.fields {
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
pub struct Extends<'a> { name:&'a Name }

/// Constructs a new `extends Name` statement.
pub fn extends(name:&Name) -> Extends {
    Extends{name}
}

/// Represents a set of type parameters `[Generic1, Generic2, ...]`.
#[derive(Debug,Clone)]
pub struct Generics { generics:syn::Generics }

/// Represents a qualified type `Foo.Foo[Bar[Baz], ..]`
#[derive(Debug,Clone)]
pub struct Type { typ:syn::Type }

/// Represents a type alias `type Foo = Bar[Baz]`
#[derive(Debug,Clone)]
pub struct TypeAlias { name:Name, generics:Generics, typ:Type }

/// Represents a type name or a variable name.
#[derive(Debug,Clone,Hash,PartialEq,Eq)]
pub struct Name { name:Ident }

impl Name {
    /// Creates a new name.
    fn new(name:&str) -> Self {
        Name{name:Ident::new(name, Span::call_site())}
    }

    /// Creates a Scala variable name (camel case).
    ///
    /// `Foo_bar` => `fooBar`
    fn var(name:&Ident) -> Self {
        Self::new(&name.to_string().to_camel_case())
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
    fn typ(name:&Ident) -> Self {
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
        Self::new(name)
    }
}


// == Trait Impls ==

impl From<syn::Type> for Type {
    fn from(typ:syn::Type) -> Self {
        Type{typ}
    }
}

impl From<syn::Generics> for Generics {
    fn from(generics:syn::Generics) -> Self {
        Generics{generics}
    }
}

impl From<syn::ItemType> for TypeAlias {
    fn from(ty:syn::ItemType) -> Self {
        Self{name:Name::typ(&ty.ident), generics:ty.generics.into(), typ:(*ty.ty).into()}
    }
}

impl From<syn::ItemEnum> for Enum {
    fn from(val:syn::ItemEnum) -> Self {
        Self{name:Name::typ(&val.ident), val}
    }
}

impl ScalaGenerator for File {
    fn write(self, source:&mut ScalaSource) {
        push!(source, "package ", self.package.as_str(), "\n\n", self.lib, "\n\n\n\n");
        push!(source, self.content, "\n");
    }
}

impl ScalaGenerator for Stdlib {
    fn write(self, source:&mut ScalaSource) {
        push!(source, "import java.util.UUID");
    }
}


impl ScalaGenerator for Object {
    fn write(self, source:&mut ScalaSource) {
        push!(source, TAB, "object ", &self.name, " {\n");

        source.indent += 2;

        if source.extends.contains_key(&self.name) {
            push!(source, TAB, "sealed trait ", &self.name, extends(&self.name));
        }
        for item in Self::lines(self.lines) {
            source.push("\n");
            match item {
                Term::Object(val) => source.push(val),
                Term::Type  (val) => source.push(val),
                Term::Class (val) => source.push(val),
                Term::Enum  (val) => source.push(val),
            }
        }

        source.indent -= 2;

        push!(source, TAB, "}\n");
    }
}

impl ScalaGenerator for TypeAlias {
    fn write(self, source:&mut ScalaSource) {
        push!(source, TAB, "type ", &self.name, &self.generics, " = ", self.typ, "\n");
    }
}

impl ScalaGenerator for Class {
    fn write(self, source:&mut ScalaSource) {
        push!(source, TAB, "case class ", &self.name, &self.generics, "(");

        for (i, (name, typ)) in Self::fields(self.fields).enumerate() {
            push!(source, ", ".when(i!=0), &name, ":", typ);
        }

        push!(source, ")", extends(&self.name));
    }
}

impl ScalaGenerator for Enum {
    fn write(self, source:&mut ScalaSource) {
        let generics = Generics::from(self.val.generics);

        push!(source, TAB, "sealed trait ", &self.name, &generics, extends(&self.name));
        for variant in Self::variants(self.val.variants.into_iter()) {
            match variant {
                Variant::Named{name, fields} => {
                    source.extends.insert(name.clone(), self.name.clone());
                    push!(source, Class{name, generics:generics.clone(), fields});
                }
                Variant::Unnamed{class, object} => {
                    source.extends.insert(object.clone(), self.name.clone());
                    source.extends.insert(class, object);
                }
            }
        }
    }
}

impl<'a> ScalaGenerator for Extends<'a> {
    fn write(self, source:&mut ScalaSource) {
        if let Some(name) = source.extends.get(self.name) {
            source.code.push_str(" extends ");
            source.code.push_str(name.name.to_string().as_str());
        }
        source.code.push('\n');
    }
}

impl ScalaGenerator for &Generics {
    fn write(self, source:&mut ScalaSource) {
        if self.generics.params.is_empty() {return}
        source.push("[");
        for (i, param) in self.generics.params.iter().enumerate() {
            if let syn::GenericParam::Type(typ) = param {
                push!(source, ", ".when(i!=0), &Name::typ(&typ.ident));
            }
        }
        source.push("]");
    }
}

impl ScalaGenerator for Type {
    fn write(self, source:&mut ScalaSource) {
        if let syn::Type::Path(path) = self.typ {
            for (i, typ) in path.path.segments.into_iter().enumerate() {
                let boxed = typ.ident.to_string().as_str() == "Box";
                if i!=0   { push!(source, ".") }
                if !boxed { push!(source, &Name::typ(&typ.ident)) }
                if let syn::PathArguments::AngleBracketed(typ) = typ.arguments {
                    if !boxed { push!(source, "[") }
                    for (i, typ) in typ.args.into_iter().enumerate() {
                        if let syn::GenericArgument::Type(typ) = typ {
                            push!(source, ", ".when(i!=0), Type::from(typ));
                        }
                    }
                    if !boxed { push!(source, "]") }
                }
            }
        }
    }
}

impl ScalaGenerator for &Name {
    fn write(self, source:&mut ScalaSource) {
        push!(source, self.name.to_string().as_str())
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
        let rust = syn::parse_quote! {
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

        let scala = "\
package org.enso.ast

import java.util.UUID



object Ast {

  type A[X] = B[X, Y]

  sealed trait FooBarBaz

  object A {
    sealed trait A extends FooBarBaz

    case class Foo() extends A

    case class Bar(x:Long, y:Byte, z:B.Type) extends A
  }

  object B {
    sealed trait B extends FooBarBaz

    type Type = Baz

    sealed trait Baz extends B
    case class Baz1() extends Baz
    case class Baz2(fooBar:Vector[Int]) extends Baz
  }
}

";
        assert_eq!(File::new("Ast", "org.enso.ast".into(), rust).to_string(), scala);
    }
}
