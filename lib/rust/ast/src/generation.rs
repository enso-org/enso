//! This module exports scala ast generator.

use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::io::prelude::*;
use itertools::Itertools;
use syn;
use syn::Ident;



// =======================
// === Scala Generator ===
// =======================

/// A builder of a scala file.
#[derive(Debug,Clone,Default)]
pub struct ScalaBuilder {
    /// The content of the file.
    code: String,
    /// Current indentation.
    indent: usize,
    /// Inheritance hierarchy.
    extends: HashMap<Ident,Ident>
}

impl ScalaBuilder {

    /// Generates a scala ast.
    pub fn generate_scala_ast() -> String {
        let mut content = String::new();
        let mut file = File::open("lib/rust/ast/impl/src/lib.rs").unwrap();
        file.read_to_string(&mut content);

        ScalaBuilder::file(syn::parse_file(content.as_str()).unwrap())
    }

    /// Generates a content of scala file from given content of parsed rust file.
    pub fn file(file:syn::File) -> String {
        let mut this = Self::default();
        this.block(&file.items[..]);
        this.code
    }

    /// Generates a block of scala code.
    fn block(&mut self, lines:&[syn::Item]) {
        for item in lines {
            match item {
                syn::Item::Enum  (val) => self.adt(&val),
                syn::Item::Type  (val) => {
                    write!(self.code, "{:i$}type ", "", i=self.indent);
                    self.typ_name(&val.ident);
                    self.generics(&val.generics);
                    write!(self.code, " = ");
                    self.typ(val.ty.as_ref());
                    write!(self.code, "\n");
                }
                syn::Item::Struct(val) => {
                    if let syn::Fields::Named(fields) = &val.fields {
                        self.class(&val.ident, &val.generics, fields);
                    } else {
                        panic!("All struct fields must be named!");
                    }
                }
                syn::Item::Mod(val) => {
                    write!(self.code, "{:i$}object {} {{\n" , "", val.ident, i=self.indent);
                    self.indent += 2;
                    write!(self.code, "{:i$}sealed trait ", "", i=self.indent);
                    self.typ_name(&val.ident);
                    self.extends(&val.ident);
                    write!(self.code, "\n");
                    if let Some(content) = &val.content {
                        self.block(&content.1[..]);
                    };
                    self.indent -= 2;
                    write!(self.code, "{:i$}}}\n", "", i=self.indent);
                }
                _ => (),
            }
        }
    }

    /// Generates a scala case class.
    fn class(&mut self, ident:&Ident, generics:&syn::Generics, fields:&syn::FieldsNamed) {
        write!(self.code, "{:i$}case class ", "", i=self.indent);
        self.typ_name(ident);
        self.generics(generics);
        write!(self.code, "(");
        for (i, field) in fields.named.iter().enumerate()  {
            if i != 0 { write!(self.code, ", "); }
            if let Some(ident) = &field.ident {
                self.var_name(&ident);
            }
            write!(self.code, ": ");
            self.typ(&field.ty);
        }
        write!(self.code, ")");
        self.extends(ident);
        write!(self.code, "\n");
    }

    /// Generates scala ADT - case classes extending a sealed trait.
    ///
    /// There are two modes of conversion:
    ///
    /// 1) When the rust enum variant has named fields:
    /// ```
    /// enum Foo { Bar{x:X}, Baz{y:Y} }
    /// ```
    /// ===>
    /// ```
    /// sealed trait Foo
    /// case class Bar(x:X) extends Foo
    /// case class Baz(y:Y) extends Foo
    /// ```
    ///
    /// 2) When the rust enum variant has one unnamed field with qualified type:
    /// ```
    /// enum Foo { Bar(barz::Bar), Baz(barz::Baz) }
    /// mod barz {
    ///     struct Bar {x:X}
    ///     struct Baz {y:Y}
    /// }
    /// ```
    /// ===>
    /// ```
    /// sealed trait Foo
    /// object barz {
    ///     sealed trait Barz extends Foo
    ///     case class Bar(x:X) extends Barz
    ///     case class Baz(y:Y) extends Barz
    /// }
    /// ```
    fn adt(&mut self, adt:&syn::ItemEnum) {
        write!(self.code, "{:i$}sealed trait {}", "", adt.ident, i=self.indent);
        self.generics(&adt.generics);
        self.extends(&adt.ident);
        write!(self.code, "\n");
        for variant in &adt.variants {
            match &variant.fields {
                syn::Fields::Named  (fields) => {
                    self.extends.insert(variant.ident.clone(), adt.ident.clone());
                    self.class(&variant.ident, &adt.generics, fields);
                },
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

    /// Generates scala class extension.
    ///
    /// `foo` => `extends Foo`
    fn extends(&mut self, ident:&Ident) {
        if let Some(name) = self.extends.get(&ident) {
            write!(self.code, " extends ");
            self.typ_name(&name.clone());
        }
    }

    /// Generates scala type parameters.
    ///
    /// `<Foo, Bar>` = `[Foo, Bar]`
    fn generics(&mut self, generics:&syn::Generics) {
        if generics.params.is_empty() {return}
        write!(self.code, "[");
        for (i, param) in generics.params.iter().enumerate() {
            if i != 0 { write!(self.code, ", "); }
            match param {
                syn::GenericParam::Type(typ) => self.typ_name(&typ.ident),
                _ => (),
            }
        }
        write!(self.code, "]");
    }

    /// Generates a qualified scala type with type arguments.
    ///
    /// `foo::Bar<Baz>` => `Foo.Bar[Baz]`
    fn typ(&mut self, typ:&syn::Type) {
        match typ {
            syn::Type::Path(path) => {
                for (i, typ) in path.path.segments.iter().enumerate() {
                    if i != 0 { write!(self.code, "."); }
                    self.typ_segment(typ);
                }
            }
            _ => (),
        }
    }

    /// Generates a scala type with type arguments.
    ///
    /// `Foo<Bar<Baz>>` => `Foo[Bar[Baz]]`
    fn typ_segment(&mut self, typ:&syn::PathSegment) {
        self.typ_name(&typ.ident);
        match &typ.arguments {
            syn::PathArguments::AngleBracketed(typ) => {
                write!(self.code, "[");
                for (i, typ) in typ.args.iter().enumerate() {
                    if i != 0 { write!(self.code, ", "); }
                    match typ {
                        syn::GenericArgument::Type(typ) => self.typ(typ),
                        _ => (),
                    }
                }
                write!(self.code, "]");
            }
            _ => (),
        }
    }

    /// Generates a scala variable name (camel case).
    ///
    /// `foo_bar` => `fooBar`
    fn var_name(&mut self, ident:&Ident) {
        let mut underscore = false;
        for char in ident.to_string().chars() {
            if char == '_' {
                underscore = true ;
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

    /// Generates a scala type name.
    ///
    /// Converts rust primitives into scala primitives i.e. `usize` => `Int`
    fn typ_name(&mut self, ident:&Ident) {
        let name = match ident.to_string().as_str() {
            "usize" | "isize" | "u64" | "u32" | "u16" | "i64" | "i32" | "i16" | "i8" => "Int",
            "u8"   => "byte",
            "char" => "Char",
            "Vec"  => "Vector",
            name   => {
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
