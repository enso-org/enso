#![feature(type_ascription)]
#![feature(unboxed_closures)]
#![cfg_attr(test, allow(dead_code))]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(proc_macro_hygiene)]
#![feature(specialization)]
#![feature(weak_into_raw)]
#![feature(associated_type_defaults)]
#![feature(set_stdio)]
#![feature(overlapping_marker_traits)]
//#![warn(missing_docs)]

// Lints. To be refactored after this gets resolved: https://github.com/rust-lang/cargo/issues/5034
#![allow(clippy::option_map_unit_fn)]

#![feature(generators, generator_trait)]


use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

pub use basegl_prelude as prelude;
use wasm_bindgen::prelude::*;
use prelude::*;
use structology::*;

use serde::{Serialize, Deserialize};
use serde_json;

#[derive(Iterator)]
pub struct Demo<T> (T, u32, T);
// pub struct Demo<T> { t: T, x:u32, z:T }

// struct Demo<'a, T: ?Sized> {
//     a: Box<T>,
//     b: u8,
//     c: &'a str,
//     d: String,
// }

pub type Stream<T> = Vec<T>;


// ============
// === Tree ===
// ============

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Tree<K,V> where K: Eq + Hash {
    pub value    : Option<V>,
    pub branches : HashMap<K, Tree<K,V>>,
}

// ===============
// === Shifted ===
// ===============

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize, Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Shifted<T> {
    #[shrinkwrap(main_field)]
    pub wrapped : T,
    pub off     : usize,
}

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct ShiftedVec1<T> {
    pub head: T,
    pub tail: Vec<Shifted<T>>
}


// ===============
// === Layered ===
// ===============

pub trait Layer<T> {
    fn layered(t: T) -> Self;
}

#[derive(Debug)]
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Layered<T>(pub T);

impl<T> Layer<T> for Layered<T> {
    fn layered(t: T) -> Self { Layered(t) }
}


// ===========
// === AST ===
// ===========

#[derive(Eq, PartialEq, Debug, Shrinkwrap, Serialize, Deserialize)]
#[shrinkwrap(mutable)]
pub struct Ast {
    #[serde(flatten)]
    pub wrapped: Rc<WithID<WithSpan<Shape<Ast>>>>
}


impl Ast {
    fn iter(&self) -> Rc<dyn Iterator<Item = &'_ Shape<Ast>> + '_> {
        Rc::new(IterGen(move || {
            let curr = &self.wrapped.wrapped.wrapped;
            yield curr;

            if let Shape::App(t) = curr {
                for elem in (*t.func).iter() { yield elem }
                for elem in (*t.arg).iter() { yield elem }
            }
        }))
    }
}

impl<T: Spanned + Into<Shape<Ast>>>
From<T> for Ast {
    fn from(t: T) -> Self {
        let span      = t.span();
        let shape     = t.into();
        let with_span = WithSpan { wrapped: shape, span };
        let with_id   = WithID   { wrapped: with_span, id: None };
        Ast { wrapped: Rc::new(with_id) }
    }
}

// =============
// === Shape ===
// =============

#[ast(flat)] pub enum Shape<T> {

    // === Identifiers ===
    Blank     { },
    Var       { name : String },
    Cons      { name : String },
    Opr       { name : String },
    Mod       { name : String },

    // === Literals ===
    Number    { base: Option<String>, int: String },
    Text      (Text<T>),

    // === Expressions ===
    App       { func : T   , off  : usize , arg: T                            },
    Infix     { larg : T   , loff : usize , opr: Opr , roff: usize , rarg: T  },
    SectLeft  { arg  : T   , off  : usize , opr: Opr                          },
    SectRight { opr  : Opr , off  : usize , arg: T                            },
    SectSides { opr  : Opr                                                    },

    Invalid   (Invalid<T>),
    Block     (Block<T>),
    Module    (Module<T>),
    Macro     (Macro<T>),
    Comment   (Comment),
    Import    (Import<T>),
    Mixfix    (Mixfix<T>),
    Group     (Group<T>),
    Def       (Def<T>),
    Foreign   (Foreign),
}

// ===============
// === Invalid ===
// ===============

#[ast] pub enum Invalid<T> {
    Unrecognized  { input : String },
    Unexpected    { msg   : String, stream: Stream<T> },
    InvalidSuffex { elem  : T, suffix: String },
    // DanglingBase
}


// ============
// === Text ===
// ============

#[ast] pub enum Text<T> {
    Raw { body: TextBody<TextRawSegment> },
    Fmt { body: TextBody<TextFmtSegment<T>> }
}

#[ast] pub struct TextRawSegment {
    pub value: String
}

#[ast] pub enum TextFmtSegment<T> {
    Plain  { value  : String },
    Expr   { value  : Option<T> },
    Escape { escape : TextEscape },
}

pub type TextBlock<T> = Vec<TextLine<T>>;
#[ast] pub struct TextLine<T> { off: usize, elems: Vec<T> }
#[ast] pub struct TextBody<T> { quote: TextQuote, block: TextBlock<T> }
#[ast] pub enum   TextQuote   { Single, Triple }
#[ast] pub struct TextEscape {} // TODO


// =============
// === Block ===
// =============

#[ast] pub struct Block<T> {
    ty          : BlockType,
    ident       : usize,
    empty_lines : usize,
    first_line  : BlockLine<T>,
    lines       : Vec<BlockLine<Option<T>>>,
    is_orphan   : bool,
}

#[ast] pub enum   BlockType     { Continuous, Discontinuous }
#[ast] pub struct BlockLine <T> { elem: T, off: usize }

// ==============
// === Module ===
// ==============

#[ast] pub struct Module<T> { lines: Vec<BlockLine<Option<T>>> }

// =============
// === Macro ===
// =============

#[ast] pub enum Macro<T> {
    Match     (Match<T>),
    Ambiguous (Ambiguous),
}

#[ast] pub struct MacroMatch<T> {
    pub pfx      : Option<MacroPatternMatch<Ast>>,
    pub segs     : ShiftedVec1<MacroMatchSegment<T>>,
    pub resolved : Ast
}

#[ast] pub struct MacroAmbiguous {
    pub segs  : ShiftedVec1<MacroAmbiguousSegment>,
//    pub paths : Tree<Ast, ()>, // FIXME
}

#[ast] pub struct MacroMatchSegment<T> {
    pub head : Ast,
    pub body : MacroPatternMatch<Shifted<T>>
}

#[ast] pub struct MacroAmbiguousSegment {
    pub head: Ast,
    pub body: Option<Shifted<Ast>>
}

pub type MacroPattern = Rc<MacroPatternRaw>;
#[ast] pub enum MacroPatternRaw {

    // === Boundary Patterns ===
    Begin   ,
    End     ,

    // === Structural Patterns ===
    Nothing ,
    Seq     { pat1 : MacroPattern , pat2    : MacroPattern                    },
    Or      { pat1 : MacroPattern , pat2    : MacroPattern                    },
    Many    { pat  : MacroPattern                                             },
    Except  { not  : MacroPattern, pat      : MacroPattern                    },

    // === Meta Patterns ===
    Build   { pat  : MacroPattern                                             },
    Err     { msg  : String       , pat     : MacroPattern                    },
    Tag     { tag  : String       , pat     : MacroPattern                    },
    Cls     { cls  : PatternClass , pat     : MacroPattern                    },

    // === Token Patterns ===
    Tok     { spaced : Spaced     , ast     : Ast                             },
    Blank   { spaced : Spaced                                                 },
    Var     { spaced : Spaced                                                 },
    Cons    { spaced : Spaced                                                 },
    Opr     { spaced : Spaced     , max_prec : Option<usize>                  },
    Mod     { spaced : Spaced                                                 },
    Num     { spaced : Spaced                                                 },
    Text    { spaced : Spaced                                                 },
    Block   { spaced : Spaced                                                 },
    Macro   { spaced : Spaced                                                 },
    Invalid { spaced : Spaced                                                 },
}

#[ast] pub enum PatternClass { Normal, Pattern }
pub type Spaced = Option<bool>;

#[derive(Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub enum Either<L,R> { Left(L), Right(R) }
pub type Switch<T> = Either<T,T>;

pub type MacroPatternMatch<T> = Rc<FooRaw<T>>;
#[ast] pub enum FooRaw<T> {

    // === Boundary Matches ===
    Begin   { pat: MacroPatternRawBegin },
    End     { pat: MacroPatternRawEnd   },

    // === Structural Matches ===
    Nothing { pat: MacroPatternRawNothing                                     },
    Seq     { pat: MacroPatternRawSeq     , elem: (MacroPatternMatch<T>, MacroPatternMatch<T>) },
    Or      { pat: MacroPatternRawOr      , elem: Switch<MacroPatternMatch<T>>},
    Many    { pat: MacroPatternRawMany    , elem: Vec<MacroPatternMatch<T>>   },
    Except  { pat: MacroPatternRawExcept  , elem: MacroPatternMatch<T>        },

    // === Meta Matches ===
    Build   { pat: MacroPatternRawBuild   , elem: T                           },
    Err     { pat: MacroPatternRawErr     , elem: T                           },
    Tag     { pat: MacroPatternRawTag     , elem: MacroPatternMatch<T>        },
    Cls     { pat: MacroPatternRawCls     , elem: MacroPatternMatch<T>        },

    // === Token Matches ===
    Tok     { pat: MacroPatternRawTok     , elem: T                           },
    Blank   { pat: MacroPatternRawBlank   , elem: T                           },
    Var     { pat: MacroPatternRawVar     , elem: T                           },
    Cons    { pat: MacroPatternRawCons    , elem: T                           },
    Opr     { pat: MacroPatternRawOpr     , elem: T                           },
    Mod     { pat: MacroPatternRawMod     , elem: T                           },
    Num     { pat: MacroPatternRawNum     , elem: T                           },
    Text    { pat: MacroPatternRawText    , elem: T                           },
    Block   { pat: MacroPatternRawBlock   , elem: T                           },
    Macro   { pat: MacroPatternRawMacro   , elem: T                           },
    Invalid { pat: MacroPatternRawInvalid , elem: T                           },

}


// =============================================================================
// === Spaceless AST ===========================================================
// =============================================================================

#[ast] pub struct Comment {
    pub lines: Vec<String>
}

#[ast] pub struct Import<T> {
    pub lines: Vec<T>
}

#[ast] pub struct Mixfix<T> {
    pub name: Vec<T>,
    pub args: Vec<T>,
}

#[ast] pub struct Group<T> {
    pub body: Option<T>,
}

#[ast] pub struct Def<T> {
    pub name: Cons,
    pub args: Vec<T>,
    pub body: Option<T>
}

#[ast] pub struct Foreign {
    pub indent : usize,
    pub lang   : String,
    pub code   : Vec<String>
}


// =============================================================================
// === Spaceless AST ===========================================================
// =============================================================================



impl Var {
    pub fn new(name: String) -> Ast {
        Ast::from (Var { name })
    }
}

impl Spanned for Var {}

impl<T> Shape<T> {
    pub fn iter(&self) -> Box<dyn Iterator<Item = &'_ T> + '_> {
        Box::new(IterGen(move || {
            match self {
                Shape::App(t) => { yield &t.func; yield &t.arg; }
                _ => {}
            }
        }))
    }
}

pub struct Visitor<'t> {
    pub to_be_visited: Vec<&'t mut Ast>,
}

impl<'t> Visitor<'t> {
    pub fn visit(&mut self, t: &'t mut Ast) {
        self.to_be_visited.push(t);
    }
}

pub struct InstancedVisitor<'t, Instance> {
    pub visitor  : Visitor<'t>,
    pub instance : Instance
}

impl<'t, Instance> InstancedVisitor<'t, Instance>
where Instance: VisitorFor<'t, App<Ast>> {

    pub fn run(&mut self) {
        // loop {
        //     match self.visitor.to_be_visited.pop() {
        //         None    => break,
        //         Some(t) => self.step(t)
        //     }
        // }
    }

    pub fn step(&mut self, t: &'t mut Shape<Ast>) {
        match t {
            Shape::App(t) => self.instance.visit(&mut self.visitor, t),
            _ => {}
        }
    }
}


pub trait VisitorFor<'t, T: 't>
where &'t mut T: IntoIterator<Item = &'t mut Ast> {
    fn visit(&mut self, visitor: &mut Visitor<'t>, t: &'t mut T) {
        t.into_iter().for_each(|s| visitor.visit(s))
    }
}

// ===========
// === AST ===
// ===========

pub trait Spanned {
    fn span(&self) -> usize { 0 }
}




// === WithID ===

#[derive(Eq, PartialEq, Debug, Shrinkwrap, Serialize, Deserialize)]
#[shrinkwrap(mutable)]
pub struct WithID<T> {
    #[shrinkwrap(main_field)]
    #[serde(flatten)]
    pub wrapped: T,
    pub id: Option<i32>
}

impl<T, S:Layer<T>>
Layer<T> for WithID<S> {
    fn layered(t: T) -> Self {
        WithID { wrapped: Layer::layered(t), id: None }
    }
}

// === WithSpan ===

#[derive(Eq, PartialEq, Debug, Shrinkwrap, Serialize, Deserialize)]
#[shrinkwrap(mutable)]
pub struct WithSpan<T> {
    #[shrinkwrap(main_field)]
    #[serde(flatten)]
    pub wrapped: T,
    pub span: usize
}

impl<T> Spanned for WithSpan<T> {
    fn span(&self) -> usize { self.span }
}

impl<T:Spanned, S:Layer<T>>
Layer<T> for WithSpan<S> {
    fn layered(t: T) -> Self {
        let span = t.span();
        WithSpan { wrapped: Layer::layered(t), span }
    }
}

// ==========================




impl<T> Spanned for Shape<T>{}

// ==== boilerplate



pub struct IterGen<G: Generator>(pub G);

impl<G> Iterator for IterGen<G>
where G: Generator<Return = ()> + Unpin {
    type Item = G::Yield;
    fn next(&mut self) -> Option<Self::Item> {
        match { Pin::new(&mut self.0).resume() } {
            GeneratorState::Yielded(element) => Some(element),
            _ => None,
        }
    }
}






// =============
// === Utils ===
// =============

use console_error_panic_hook;

fn main() {
    let a = Var { name: "foo".to_string() };
    let b: Shape<Ast> = Shape::from(a);
    let c: Ast = Ast::from(b);
    let serialized = serde_json::to_string(&c).unwrap();
    let d: Ast = serde_json::from_str(&serialized).unwrap();
    println!("serialized = {}", serialized);
    println!("deserialized = {:?}", d);
}

#[wasm_bindgen(start)]
pub fn start() {
    console_error_panic_hook::set_once();
    set_stdout();
    main();
}


////////////////////////////////////////////////
////////////////////////////////////////////////

type PrintFn = fn(&str) -> std::io::Result<()>;

struct Printer {
    printfn: PrintFn,
    buffer: String,
    is_buffered: bool,
}

impl Printer {
    fn new(printfn: PrintFn, is_buffered: bool) -> Printer {
        Printer {
            buffer: String::new(),
            printfn,
            is_buffered,
        }
    }
}

impl std::io::Write for Printer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buffer.push_str(&String::from_utf8_lossy(buf));

        if !self.is_buffered {
            (self.printfn)(&self.buffer)?;
            self.buffer.clear();

            return Ok(buf.len());
        }

        if let Some(i) = self.buffer.rfind('\n') {
            let buffered = {
                let (first, last) = self.buffer.split_at(i);
                (self.printfn)(first)?;

                String::from(&last[1..])
            };

            self.buffer.clear();
            self.buffer.push_str(&buffered);
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        (self.printfn)(&self.buffer)?;
        self.buffer.clear();

        Ok(())
    }
}

fn _print(msg: &str) -> std::io::Result<()> {
    web_sys::console::info_1(&msg.to_string().into());
    Ok(())
}


pub fn set_stdout() {
    let printer = Printer::new(_print, true);
    std::io::set_print(Some(Box::new(printer)));
}

pub fn set_stdout_unbuffered() {
    let printer = Printer::new(_print, false);
    std::io::set_print(Some(Box::new(printer)));
}