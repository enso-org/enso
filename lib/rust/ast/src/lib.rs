#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports the implementation of the enso abstract syntax tree.

use uuid::Uuid;



// ===================================
// === Abstract Syntax Tree (Stub) ===
// ===================================

/// An ast node of unknown shape.
pub type AnyAst = Ast<Shape>;

/// An ast node with an unique id and length.
#[derive(Debug,Clone)]
pub struct Ast<Shape> {
    /// An unique identifier.
    uid: Option<Uuid>,
    /// Length in number of chars of this ast node.
    len: usize,
    /// The number of trailing spaces.
    off: usize,
    /// The ast node itself.
    ast: Shape,
}

// The set of all ast nodes.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub enum Shape {
    Unrecognized(Unrecognized),
    Blank(Blank),
    Var(Var),
    Cons(Cons),
    Opr(Opr),
    Number(Number),
    Text(Text),
    Prefix(Prefix),
    Infix(Infix),
    Module(Module),
    Block(Block),
}


// === Unrecognized ===

/// Unrecognized token.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Unrecognized { pub str: String }


// === Identifiers ===

/// The ast node for the underscore `_`.
#[allow(missing_docs)]
#[derive(Debug,Clone,Copy)]
pub struct Blank {}

/// The ast node for a variable.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Var { pub name: String }

/// The ast node for a constructor.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Cons { pub name: String }

/// The ast node for an operator.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Opr { pub name: String }


// === Number ===

/// The ast node for a number
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Number { pub number: String }


// === Text ===

/// The ast node for a string of text.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Text { pub text: String }


// === Application ===

/// The ast node for application.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Prefix { pub func: Box<AnyAst>, pub arg: Box<AnyAst> }

/// The ast node for an infix operator application.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Infix { pub larg: Box<AnyAst>, pub opr: Box<Ast<Opr>>, pub rarg: Box<AnyAst> }


// === Module ===

/// The ast node for a module that represents the file's root block.
///
/// The module consists of a sequence of possibly empty lines with no leading indentation.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Module { pub lines: Vec<Option<AnyAst>> }


// === Block ===

/// The ast node for a block that represents a sequence of equally indented lines.
///
/// Lines may contain some child ast or be empty. Block is used for all code blocks except for
/// the root one, which uses `Module`.
#[derive(Debug,Clone)]
pub struct Block {
    /// Absolute's block indent, counting from the module's root.
    pub indent: usize,
    /// Leading empty lines. Each line is represented by absolute count of spaces
    /// it contains, counting from the root.
    pub empty_lines: Vec<usize>,
    /// First line with non-empty item.
    pub first_line: Box<AnyAst>,
    /// Rest of lines, each of them optionally having contents.
    pub lines: Vec<Option<AnyAst>>,
}

/// The ast node of a line in block or module.
#[derive(Debug,Clone)]
pub struct Line<T> {
    /// The AST stored in the line.
    pub ast: T,
    /// The trailing whitespace in the line after the `ast`.
    pub off: usize
}


// === Definition ===

pub struct MethodDefinition { name:Ast<Var>, args:AnyAst, body:AnyAst }

// === Into<Shape> ===

impl From<Unrecognized> for Shape { fn from(val:Unrecognized) -> Self { Self::Unrecognized(val) } }
impl From<Blank>        for Shape { fn from(val:Blank)        -> Self { Self::Blank       (val) } }
impl From<Var>          for Shape { fn from(val:Var)          -> Self { Self::Var         (val) } }
impl From<Cons>         for Shape { fn from(val:Cons)         -> Self { Self::Cons        (val) } }
impl From<Opr>          for Shape { fn from(val:Opr)          -> Self { Self::Opr         (val) } }
impl From<Number>       for Shape { fn from(val:Number)       -> Self { Self::Number      (val) } }
impl From<Text>         for Shape { fn from(val:Text)         -> Self { Self::Text        (val) } }
impl From<Prefix>       for Shape { fn from(val:Prefix)       -> Self { Self::Prefix      (val) } }
impl From<Infix>        for Shape { fn from(val:Infix)        -> Self { Self::Infix       (val) } }
impl From<Module>       for Shape { fn from(val:Module)       -> Self { Self::Module      (val) } }
impl From<Block>        for Shape { fn from(val:Block)        -> Self { Self::Block       (val) } }



// ====================
// === Constructors ===
// ====================

impl AnyAst {
    /// Creates a new ast node with random `Uuid` from `Shape`.
    pub fn new(ast:impl Into<Shape>) -> Self {
        Self::new_with_id(ast, Some(Uuid::new_v4()))
    }
    /// Creates a new ast node with given `Uuid` from given `Shape`.
    pub fn new_with_id(ast:impl Into<Shape>, uid:Option<Uuid>) -> Self {
        Self {uid, len:0, ast:ast.into()}
    }

    /// Creates a new ast node with `Shape::Unrecognized`.
    pub fn unrecognized(str:String) -> Self {
        Self::new(Unrecognized{str})
    }

    /// Creates a new ast node with `Shape::Blank`.
    pub fn blank() -> Self {
        Self::new(Blank{})
    }

    /// Creates a new ast node with `Shape::Var`.
    pub fn var(name:String) -> Self {
        Self::new(Var{name})
    }

    /// Creates a new ast node with `Shape::Cons`.
    pub fn cons(name:String) -> Self {
        Self::new(Cons{name})
    }

    /// Creates a new ast node with `Shape::Opr`.
    pub fn opr(name:String) -> Self {
        Self::new(Opr{name})
    }

    /// Creates a new ast node with `Shape::Number`.
    pub fn num(number:i64) -> Self {
        Self::new(Number{number:number.to_string()})
    }

    /// Creates a new ast node with `Shape::Text`.
    pub fn text(text:String) -> Self {
        Self::new(Text{text})
    }
}

impl Prefix {
    /// Creates an `Prefix` shape with spacing=0.
    pub fn from_vars(func:AnyAst, arg:AnyAst) -> Self {
        Self {func:Box::new(func), off:0, arg:Box::new(arg)}
    }
}

impl Infix {
    /// Creates an `Infix` shape with spacing=0.
    pub fn from_vars(larg:AnyAst, opr:AnyAst, rarg:AnyAst) -> Infix {
        Infix {larg:Box::new(larg), loff:0, opr:Box::new(opr), roff:1, rarg:Box::new(rarg)}
    }
}


impl Module {
    /// Creates a `Module` shape with lines storing given ASTs.
    pub fn from_lines(lines:&[Option<AnyAst>]) -> Self {
        Self {lines: lines.iter().cloned().map(Line::new).collect()}
    }
}

impl Block {
    /// Creates a `Block` shape with lines storing given ASTs.
    pub fn new(indent:usize, first_line:AnyAst, tail_lines:&[Option<AnyAst>]) -> Self {
        let empty_lines = Vec::new();
        let first_line  = Box::new(Line::new(first_line));
        let lines       = tail_lines.iter().cloned().map(Line::new).collect();

        Self {indent,empty_lines,first_line,lines}
    }
}

impl<T> Line<T> {
    /// Creates a `Line` shape with zero offset from the given ast.
    pub fn new(ast:T) -> Self {
        Self {ast, off:0}
    }
}
