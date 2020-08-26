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
    extends:HashMap<Name,Name>
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

        let pkg       = String::from("org.enso.ast");
        let file      = syn::parse_file(content.as_str()).unwrap();
        let this:Self = File::new("Ast", pkg, file).source();

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

impl Generator<Source> for File {
    fn write(self, source:&mut Source) {
        let content = Object {name:self.name.clone(),lines:&self.content.items[..]};
        write!(source, "package ", self.package.as_str(), "\n\n", self.lib, "\n\n\n\n");
        write!(source, content, "\n");
    }
}

impl Generator<Source> for Stdlib {
    fn write(self, source:&mut Source) {
        write!(source, "import java.util.UUID");
    }
}

impl<'a> Generator<Source> for Object<'a> {
    fn write(self, source:&mut Source) {
        write!(source, TAB, "object ", &self.name, " {\n");

        source.indent += 2;

        if source.extends.contains_key(&self.name) {
            write!(source, TAB, "sealed trait ", &self.name, extends(&self.name));
        }
        for item in self.lines() {
            source.write("\n");
            match item {
                Term::Object(val) => source.write(val),
                Term::Type  (val) => source.write(val),
                Term::Class (val) => source.write(val),
                Term::Enum  (val) => source.write(val),
            }
        }

        source.indent -= 2;

        write!(source, TAB, "}\n");
    }
}

impl<'a> Generator<Source> for TypeAlias<'a> {
    fn write(self, source:&mut Source) {
        write!(source, TAB, "type ", &self.name, self.generics, " = ", self.typ, "\n");
    }
}

impl<'a> Generator<Source> for Class<'a> {
    fn write(self, source:&mut Source) {
        write!(source, TAB, "case class ", &self.name, self.generics, "(");

        for (i, (name, typ)) in self.fields().enumerate() {
            write!(source, ", ".when(i!=0), &name, ":", typ);
        }

        write!(source, ")", extends(&self.name));
    }
}

impl<'a> Generator<Source> for Enum<'a> {
    fn write(self, source:&mut Source) {
        let generics = Generics::from(&self.val.generics);

        write!(source, TAB, "sealed trait ", &self.name, generics, extends(&self.name));
        for variant in self.variants() {
            match variant {
                Variant::Named{name, fields} => {
                    source.extends.insert(name.clone(), self.name.clone());
                    write!(source, Class{name, generics:generics.clone(), fields});
                }
                Variant::Unnamed{class, object} => {
                    source.extends.insert(object.clone(), self.name.clone());
                    source.extends.insert(class, object);
                }
            }
        }
    }
}

impl<'a> Generator<Source> for Extends<'a> {
    fn write(self, source:&mut Source) {
        if let Some(name) = source.extends.get(self.name) {
            source.code.push_str(" extends ");
            source.code.push_str(name.name.to_string().as_str());
        }
        source.code.push('\n');
    }
}

impl<'a> Generator<Source> for Generics<'a> {
    fn write(self, source:&mut Source) {
        if self.generics.params.is_empty() {return}
        source.write("[");
        for (i, param) in self.generics.params.iter().enumerate() {
            if let syn::GenericParam::Type(typ) = param {
                write!(source, ", ".when(i!=0), &Name::typ(&typ.ident));
            }
        }
        source.write("]");
    }
}

impl<'a> Generator<Source> for Type<'a> {
    fn write(self, source:&mut Source) {
        if let syn::Type::Path(path) = self.typ {
            for (i, typ) in path.path.segments.iter().enumerate() {
                let boxed = typ.ident.to_string().as_str() == "Box";
                if i!=0   { write!(source, ".") }
                if !boxed { write!(source, &Name::typ(&typ.ident)) }
                if let syn::PathArguments::AngleBracketed(typ) = &typ.arguments {
                    if !boxed { write!(source, "[") }
                    for (i, typ) in typ.args.iter().enumerate() {
                        if let syn::GenericArgument::Type(typ) = typ {
                            write!(source, ", ".when(i!=0), Type::from(typ));
                        }
                    }
                    if !boxed { write!(source, "]") }
                }
            }
        }
    }
}

impl Generator<Source> for &Name {
    fn write(self, source:&mut Source) {
        write!(source, self.name.to_string().as_str())
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
        let src: Source = File::new("Ast", "org.enso.ast".into(), rust).source();

        assert_eq!(src.code, scala);
    }
}
