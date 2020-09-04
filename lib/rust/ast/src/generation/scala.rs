//! This module exports scala ast generator.

use std::collections::HashMap;
use std::fs;
use syn;
use std::io::Read;
use super::Generator;
use super::ast::*;
use super::Tab;
use super::TAB;
use super::When;



// =======================
// === Scala Generator ===
// =======================

/// A state of Scala source code.
#[derive(Debug,Clone,Default)]
pub struct Source {
    /// The content of the file.
    code:String,
    /// Current indentation.
    indent:usize,
    /// Inheritance hierarchy.
    extends:HashMap<Name,Name>,
}

impl Source {
    /// Write scala source code into buffer.
    pub fn write(&mut self, ast:impl Generator<Source>) {
        ast.write(self)
    }

    pub fn ast() -> std::io::Result<String> {
        let mut content = String::new();
        let mut file    = fs::File::open("lib/rust/ast/src/ast.rs")?;
        file.read_to_string(&mut content)?;

        let file      = syn::parse_file(content.as_str()).unwrap();
        let this:Self = File::new("Ast", "org.enso.ast", file).source();

        Ok(this.code)
    }
}

// == Trait Impls ==

impl Generator<Source> for &str {
    fn write(self, source:&mut Source) {
        source.code.push_str(self)
    }
}

impl Generator<Source> for Tab {
    fn write(self, source:&mut Source) {
        for _ in 0..source.indent { source.code.push(' ') }
    }
}

// == ScalaGenerator Impls ==

impl Generator<Source> for &File {
    fn write(self, source:&mut Source) {
        let content = Module {name:self.name.clone(), lines:&self.content.items[..]};
        write!(source, "package ", self.package.as_str(), "\n\n", self.lib, "\n\n\n\n");
        write!(source, &content, "\n");
    }
}

impl Generator<Source> for Stdlib {
    fn write(self, source:&mut Source) {
        write!(source, "import java.util.UUID");
    }
}

impl<'a> Generator<Source> for &Module<'a> {
    fn write(self, source:&mut Source) {
        let name = name::typ(&self.name);

        write!(source, TAB, "object ", &name, " {\n");

        source.indent += 2;

        if source.extends.contains_key(&name) {
            write!(source, TAB, "sealed trait ", &name, extends(&name));
        }
        for item in self.lines {
            source.write("\n");
            match item {
                syn::Item::Mod   (val) => Module::from(val).write(source),
                syn::Item::Type  (val) => TypeAlias::from(val).write(source),
                syn::Item::Struct(val) => Class::from(val).write(source),
                syn::Item::Enum  (val) => Enum::from(val).write(source),
                _                      => (),
            }
        }

        source.indent -= 2;

        write!(source, TAB, "}\n");
    }
}

impl Generator<Source> for &TypeAlias {
    fn write(self, source:&mut Source) {
        write!(source, TAB, "type ", &self.val, " = ", &self.val, "\n");
    }
}

impl Generator<Source> for &Class {
    fn write(self, source:&mut Source) {
        write!(source, TAB, "case class ", &self.typ, "(");

        for (i, (name,typ)) in self.args.iter().enumerate() {
            write!(source, ", ".when(i!=0), &name::var(name), ":", typ);
        }

        write!(source, ")", extends(&self.typ.name));
    }
}

impl Generator<Source> for &Enum {
    fn write(self, source:&mut Source) {
        write!(source, TAB, "sealed trait ", &self.typ, extends(&self.typ.name));
        for (object, class) in self.variants.iter() {
            let name = class.typ.name.clone();
            match object {
                Some(object) => {
                    source.extends.insert(name::typ(&object), self.typ.name.clone());
                    source.extends.insert(name, name::typ(&object));
                }
                None => {
                    source.extends.insert(name, self.typ.name.clone());
                    write!(source, class);
                }
            }
        }
    }
}

impl<'a> Generator<Source> for Extends<'a> {
    fn write(self, source:&mut Source) {
        if let Some(name) = source.extends.get(self.name) {
            source.code.push_str(" extends ");
            source.code.push_str(name.str.to_string().as_str());
        }
        source.code.push('\n');
    }
}

impl Generator<Source> for &Type {
    fn write(self, source:&mut Source) {
        let name  = name::typ(&self.name);
        let valid = !name.str.is_empty();
        for name in &self.path {
            write!(source, name, ".");
        }
        if valid { write!(source, &name) }
        if !self.args.is_empty() {
            if valid { write!(source, "[") }
            for (i,typ) in self.args.iter().enumerate() {
                write!(source, ", ".when(i!=0), typ);
            }
            if valid { write!(source, "]") }
        }
    }
}

impl Generator<Source> for &Name {
    fn write(self, source:&mut Source) {
        write!(source, self.str.to_string().as_str())
    }
}



// ========================
// === Name Conversions ===
// ========================

pub mod name {
    use crate::generation::ast::Name;
    use crate::generation::types;
    use inflector::Inflector;


    /// Creates a Scala type name `foo_bar => FooBar`
    pub fn typ(name:&Name) -> Name {
        if let Some(name) = types::builtin(name) { return name.scala.into() }

        let mut string = name.str.to_camel_case();
        string[0..1].make_ascii_uppercase();
        string.into()
    }

    /// Creates a Scala variable name `Foo_bar => fooBar`
    pub fn var(name:&Name) -> Name {
        let mut name = name.str.to_camel_case();
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
        // TODO B.Type?
        let scala = "\
package org.enso.ast

import java.util.UUID



object Ast {

  type A[X] = B[X, Y]

  sealed trait FooBarBaz

  object A {
    sealed trait A extends FooBarBaz

    case class Foo() extends A

    case class Bar(x:Long, y:Byte, z:Type) extends A
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
        let src: Source = File::new("Ast", "org.enso.ast".into(), rust).source();

        assert_eq!(src.code, scala);
    }
}
