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
    pub fn push(&mut self, ast:impl Generator<Source=Self>) {
        ast.write(self)
    }

    pub fn ast() -> std::io::Result<String> {
        let mut content = String::new();
        let mut file    = fs::File::open("lib/rust/ast/src/ast.rs")?;
        file.read_to_string(&mut content)?;

        let pkg  = String::from("org.enso.ast");
        let file = syn::parse_file(content.as_str()).unwrap();

        Ok(File::new("Ast", pkg, file).source().code)
    }
}

// == Trait Impls ==

impl Generator for &str {
    type Source = Source;

    fn write(self, source:&mut Source) {
        source.code.push_str(self)
    }
}

impl Generator for Tab {
    type Source = Source;

    fn write(self, source:&mut Source) {
        for _ in 0..source.indent { source.code.push(' ') }
    }
}

// == ScalaGenerator Impls ==

impl Generator for File {
    type Source = Source;

    fn write(self, source:&mut Source) {
        let content = Object {name:self.name.clone(),lines:&self.content.items[..]};
        each!(source.push, "package ", self.package.as_str(), "\n\n", self.lib, "\n\n\n\n");
        each!(source.push, content, "\n");
    }
}

impl Generator for Stdlib {
    type Source = Source;

    fn write(self, source:&mut Source) {
        each!(source.push, "import java.util.UUID");
    }
}

impl<'a> Generator for Object<'a> {
    type Source = Source;

    fn write(self, source:&mut Source) {
        each!(source.push, TAB, "object ", &self.name, " {\n");

        source.indent += 2;

        if source.extends.contains_key(&self.name) {
            each!(source.push, TAB, "sealed trait ", &self.name, extends(&self.name));
        }
        for item in self.lines() {
            source.push("\n");
            match item {
                Term::Object(val) => source.push(val),
                Term::Type  (val) => source.push(val),
                Term::Class (val) => source.push(val),
                Term::Enum  (val) => source.push(val),
            }
        }

        source.indent -= 2;

        each!(source.push, TAB, "}\n");
    }
}

impl<'a> Generator for TypeAlias<'a> {
    type Source = Source;

    fn write(self, source:&mut Source) {
        each!(source.push, TAB, "type ", &self.name, &self.generics, " = ", self.typ, "\n");
    }
}

impl<'a> Generator for Class<'a> {
    type Source = Source;

    fn write(self, source:&mut Source) {
        each!(source.push, TAB, "case class ", &self.name, &self.generics, "(");

        for (i, (name, typ)) in self.fields().enumerate() {
            each!(source.push, ", ".when(i!=0), &name, ":", typ);
        }

        each!(source.push, ")", extends(&self.name));
    }
}

impl<'a> Generator for Enum<'a> {
    type Source = Source;

    fn write(self, source:&mut Source) {
        let generics = Generics::from(&self.val.generics);

        each!(source.push, TAB, "sealed trait ", &self.name, &generics, extends(&self.name));
        for variant in self.variants() {
            match variant {
                Variant::Named{name, fields} => {
                    source.extends.insert(name.clone(), self.name.clone());
                    each!(source.push, Class{name, generics:generics.clone(), fields});
                }
                Variant::Unnamed{class, object} => {
                    source.extends.insert(object.clone(), self.name.clone());
                    source.extends.insert(class, object);
                }
            }
        }
    }
}

impl<'a> Generator for Extends<'a> {
    type Source = Source;

    fn write(self, source:&mut Source) {
        if let Some(name) = source.extends.get(self.name) {
            source.code.push_str(" extends ");
            source.code.push_str(name.name.to_string().as_str());
        }
        source.code.push('\n');
    }
}

impl<'a> Generator for &Generics<'a> {
    type Source = Source;

    fn write(self, source:&mut Source) {
        if self.generics.params.is_empty() {return}
        source.push("[");
        for (i, param) in self.generics.params.iter().enumerate() {
            if let syn::GenericParam::Type(typ) = param {
                each!(source.push, ", ".when(i!=0), &Name::typ(&typ.ident));
            }
        }
        source.push("]");
    }
}

impl<'a> Generator for Type<'a> {
    type Source = Source;

    fn write(self, source:&mut Source) {
        if let syn::Type::Path(path) = self.typ {
            for (i, typ) in path.path.segments.iter().enumerate() {
                let boxed = typ.ident.to_string().as_str() == "Box";
                if i!=0   { each!(source.push, ".") }
                if !boxed { each!(source.push, &Name::typ(&typ.ident)) }
                if let syn::PathArguments::AngleBracketed(typ) = &typ.arguments {
                    if !boxed { each!(source.push, "[") }
                    for (i, typ) in typ.args.iter().enumerate() {
                        if let syn::GenericArgument::Type(typ) = typ {
                            each!(source.push, ", ".when(i!=0), Type::from(typ));
                        }
                    }
                    if !boxed { each!(source.push, "]") }
                }
            }
        }
    }
}

impl Generator for &Name {
    type Source = Source;

    fn write(self, source:&mut Source) {
        each!(source.push, self.name.to_string().as_str())
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
        assert_eq!(File::new("Ast", "org.enso.ast".into(), rust).source().code, scala);
    }
}
