//! This module exports scala ast generator.

#![allow(unused_must_use)]

use itertools::Itertools;
use proc_macro2::Span;
use std::collections::HashMap;
use std::fmt::Write;
use std::fs::File;
use std::io::prelude::*;
use syn;
use syn::Ident;



// =======================
// === Scala Generator ===
// =======================

/// A Scala ast generator.
#[derive(Debug, Clone, Default)]
pub struct ScalaGenerator {
    /// The content of the file.
    code:    String,
    /// Current indentation.
    indent:  usize,
    /// Inheritance hierarchy.
    extends: HashMap<Ident, Ident>,
}

impl ScalaGenerator {
    /// Generates a Scala ast from `lib/rust/ast/src/lib.rs`.
    pub fn ast() -> std::io::Result<String> {
        let mut content = String::new();
        let mut file = File::open("lib/rust/ast/src/ast.rs")?;
        file.read_to_string(&mut content);

        Ok(Self::file("ast", syn::parse_file(content.as_str()).unwrap()))
    }

    /// Generates a Scala ast definition from a parsed Rust ast definition.
    pub fn file(name: &str, file: syn::File) -> String {
        let mut this = Self::default();
        writeln!(this.code, "package org.enso.ast\n");
        writeln!(this.code, "import java.util.UUID\n\n");
        this.block(&Ident::new(name, Span::call_site()), &file.items[..]);
        this.code
    }

    /// Generates a block of Scala code.
    fn block(&mut self, ident: &Ident, lines: &[syn::Item]) {
        write!(self.code, "\n{:i$}object ", "", i = self.indent);
        self.typ_name(ident);
        writeln!(self.code, " {{");
        self.indent += 2;
        if self.extends.contains_key(ident) {
            write!(self.code, "{:i$}sealed trait ", "", i = self.indent);
            self.typ_name(ident);
            self.extends(ident);
        }

        for item in lines {
            match item {
                syn::Item::Enum(val) => self.adt(val),
                syn::Item::Type(val) => {
                    write!(self.code, "\n{:i$}type ", "", i = self.indent);
                    self.typ_name(&val.ident);
                    self.generics(&val.generics);
                    write!(self.code, " = ");
                    self.typ(val.ty.as_ref());
                    writeln!(self.code);
                }
                syn::Item::Struct(val) =>
                    if let syn::Fields::Named(fields) = &val.fields {
                        self.class(&val.ident, &val.generics, fields);
                    } else {
                        panic!("All struct fields must be named!");
                    },
                syn::Item::Mod(val) => {
                    if let Some(content) = &val.content {
                        self.block(&val.ident, &content.1[..]);
                    };
                }
                _ => (),
            }
        }

        self.indent -= 2;
        writeln!(self.code, "{:i$}}}", "", i = self.indent);
    }

    /// Generates a Scala case class.
    ///
    /// `struct Foo { bar:Bar, baz:Baz }` => `case class Foo(bar:Bar, baz:Baz)`
    fn class(&mut self, ident: &Ident, generics: &syn::Generics, fields: &syn::FieldsNamed) {
        write!(self.code, "{:i$}case class ", "", i = self.indent);
        self.typ_name(ident);
        self.generics(generics);
        write!(self.code, "(");
        for (i, field) in fields.named.iter().enumerate() {
            if i != 0 {
                write!(self.code, ", ");
            }
            if let Some(ident) = &field.ident {
                self.var_name(ident);
            }
            write!(self.code, ": ");
            self.typ(&field.ty);
        }
        write!(self.code, ")");
        self.extends(ident);
    }

    /// Generates Scala ADT - case classes extending a sealed trait.
    ///
    /// There are two modes of conversion:
    ///
    /// 1) When the Rust enum variant has named fields:
    /// ```
    /// enum Foo {
    ///     Bar { x: isize },
    ///     Baz { y: isize },
    /// }
    /// ```
    /// ===>
    /// ```scala
    /// sealed trait Foo
    /// case class Bar(x:Int) extends Foo
    /// case class Baz(y:Int) extends Foo
    /// ```
    ///
    /// 2) When the Rust enum variant has one unnamed field with qualified type:
    /// ```
    /// enum Foo {
    ///     Bar(barz::Bar),
    ///     Baz(barz::Baz),
    /// }
    /// mod barz {
    ///     pub struct Bar {}
    ///     pub struct Baz {
    ///         y: isize,
    ///     }
    /// }
    /// ```
    /// ===>
    /// ```scala
    /// sealed trait Foo
    /// object barz {
    ///     sealed trait Barz extends Foo
    ///     case class Bar() extends Barz
    ///     case class Baz(y:size) extends Barz
    /// }
    /// ```
    fn adt(&mut self, adt: &syn::ItemEnum) {
        write!(self.code, "\n{:i$}sealed trait {}", "", adt.ident, i = self.indent);
        self.generics(&adt.generics);
        self.extends(&adt.ident);
        for variant in &adt.variants {
            match &variant.fields {
                syn::Fields::Named(fields) => {
                    self.extends.insert(variant.ident.clone(), adt.ident.clone());
                    self.class(&variant.ident, &adt.generics, fields);
                }
                syn::Fields::Unnamed(fields) => {
                    if let Some(syn::Type::Path(path)) = fields.unnamed.first().map(|f| &f.ty) {
                        let path = path.path.segments.iter().rev().take(2).collect_tuple();
                        if let Some((class, object)) = path {
                            self.extends.insert(object.ident.clone(), adt.ident.clone());
                            self.extends.insert(class.ident.clone(), object.ident.clone());
                        }
                    }
                }
                _ => (),
            }
        }
    }

    /// Generates Scala class extension.
    ///
    /// `foo` => `extends Foo`
    fn extends(&mut self, ident: &Ident) {
        if let Some(name) = self.extends.get(ident).cloned() {
            write!(self.code, " extends ");
            self.typ_name(&name);
        }
        writeln!(self.code);
    }

    /// Generates Scala type parameters.
    ///
    /// `<Foo, Bar>` = `[Foo, Bar]`
    fn generics(&mut self, generics: &syn::Generics) {
        if generics.params.is_empty() {
            return;
        }
        write!(self.code, "[");
        for (i, param) in generics.params.iter().enumerate() {
            if i != 0 {
                write!(self.code, ", ");
            }
            if let syn::GenericParam::Type(typ) = param {
                self.typ_name(&typ.ident)
            }
        }
        write!(self.code, "]");
    }

    /// Generates a qualified scala type with type arguments.
    ///
    /// `foo::Bar<Baz>` => `Foo.Bar[Baz]`
    fn typ(&mut self, typ: &syn::Type) {
        if let syn::Type::Path(path) = typ {
            for (i, typ) in path.path.segments.iter().enumerate() {
                if i != 0 {
                    write!(self.code, ".");
                }
                self.typ_segment(typ);
            }
        }
    }

    /// Generates a Scala type with type arguments.
    ///
    /// `Foo<Bar<Baz>>` => `Foo[Bar[Baz]]`
    fn typ_segment(&mut self, typ: &syn::PathSegment) {
        let boxed = typ.ident.to_string().as_str() == "Box";
        if !boxed {
            self.typ_name(&typ.ident);
        }
        if let syn::PathArguments::AngleBracketed(typ) = &typ.arguments {
            if !boxed {
                write!(self.code, "[");
            }
            for (i, typ) in typ.args.iter().enumerate() {
                if i != 0 {
                    write!(self.code, ", ");
                }
                if let syn::GenericArgument::Type(typ) = typ {
                    self.typ(typ);
                }
            }
            if !boxed {
                write!(self.code, "]");
            }
        }
    }

    /// Generates a Scala variable name (camel case).
    ///
    /// `foo_bar` => `fooBar`
    fn var_name(&mut self, ident: &Ident) {
        let mut underscore = false;
        for char in ident.to_string().chars() {
            if char == '_' {
                underscore = true;
            } else if underscore {
                underscore = false;
                for char in char.to_uppercase() {
                    self.code.push(char)
                }
            } else {
                self.code.push(char);
            }
        }
    }

    /// Generates a Scala type name.
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
    fn typ_name(&mut self, ident: &Ident) {
        let name = match ident.to_string().as_str() {
            "u32" | "i32" | "u16" | "i16" | "i8" => "Int",
            "usize" | "isize" | "u64" | "i64" => "Long",
            "u8" => "Byte",
            "char" => "Char",
            "Vec" => "Vector",
            "Uuid" => "UUID",
            name => {
                let mut chars = name.chars();
                if let Some(char) = chars.next() {
                    write!(self.code, "{}", char.to_uppercase().to_string() + chars.as_str());
                }
                ""
            }
        };
        write!(self.code, "{}", name);
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
    case class Bar(x: Long, y: Byte, z: B.Type) extends A
  }

  object B {
    sealed trait B extends FooBarBaz

    type Type = Baz

    sealed trait Baz extends B
    case class Baz1() extends Baz
    case class Baz2(fooBar: Vector[Int]) extends Baz
  }
}
";
        assert_eq!(ScalaGenerator::file("ast", rust), scala);
    }
}
