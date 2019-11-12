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

// =============
// === Shape ===
// =============

#[derive(Debug)] pub struct Var           { name: String }
#[derive(Debug)] pub struct Opr           { name: String }
#[derive(Debug)] pub struct App       <T> { func: T, off: i32, arg: T }
#[derive(Debug)] pub struct SectLeft  <T> { arg:  T, off: i32, opr: Opr }
#[derive(Debug)] pub struct SectRight <T> { opr:  Opr, off: i32, arg: T }
#[derive(Debug)] pub struct SectSides     { opr:  Opr }

#[derive(Debug)]
pub enum Shape<T> {
    Var(Var),
    App(App<T>),
}


type AppIterator<'t, T> = impl Iterator<Item = &'t T>;
pub fn app_iterator<'t, T>(t: &'t App<T>) -> AppIterator<'t, T> {
    IterGen(move || {
        yield &t.func; 
        yield &t.arg;
    })
}

type AppIteratorMut<'t, T> = impl Iterator<Item = &'t mut T>;
pub fn app_iterator_mut<'t, T>(t: &'t mut App<T>) -> AppIteratorMut<'t, T> {
    IterGen(move || {
        yield &mut t.func; 
        yield &mut t.arg;
    })
}

impl<T> App<T> {
    pub fn iter(&self) -> Box<dyn Iterator<Item = &'_ T> + '_> {
        Box::new(IterGen(move || { 
            yield &self.func;        
            yield &self.arg;       
        }))
    }
}

impl<'t, T> IntoIterator for &'t App<T> {
    type Item     = &'t T;
    type IntoIter = AppIterator<'t, T>;
    fn into_iter(self) -> AppIterator<'t, T> { app_iterator(self) }
}

impl<'t, T> IntoIterator for &'t mut App<T> {
    type Item     = &'t mut T;
    type IntoIter = AppIteratorMut<'t, T>;
    fn into_iter(self) -> AppIteratorMut<'t, T> { app_iterator_mut(self) }
}


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
        loop {
            match self.visitor.to_be_visited.pop() {
                None    => break,
                Some(t) => self.step(t) 
            }
        }
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

#[derive(Debug)]
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct WithID<T> {
    #[shrinkwrap(main_field)]
    pub wrapped: T,
    pub id: Option<i32>
}

#[derive(Debug)]
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct WithSpan<T> {
    #[shrinkwrap(main_field)]
    pub wrapped: T,
    pub span: i32
}

#[derive(Debug)]
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Ast { 
    pub wrapped: Box<WithID<WithSpan<Shape<Ast>>>> 
}


impl Ast {
    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Shape<Ast>> + '_> {
        Box::new(IterGen(move || {
            let curr = &self.wrapped.wrapped.wrapped;
            yield curr;
            
            if let Shape::App(t) = curr {
                for elem in t.func.iter() { yield elem }
                for elem in t.arg.iter() { yield elem }
            }
        }))
    }
}


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


fn main() {
    let node = |ast_of| Ast { wrapped: Box::new(WithID { wrapped: ast_of, id: None })};

    // let ast = node(
    //     Shape::App { 
    //         func: node(
    //             Shape::App { 
    //                 func: node(Shape::Var { name: "foo".to_owned() }),
    //                 off: 0,
    //                 arg: node(Shape::Var { name: "bar".to_owned() })
    //             }
    //         ), 
    //         off: 0,
    //         arg: node(Shape::Var { name: "baz".to_owned() })
    //     }
    // );
    
    // let ast = mk_nodes(100000);
    
    // let mut i: i32 = 0;
    // for node in ast.iter() {
    //     // dbg!(node);
    //     i += 1;
    // }
    // dbg!(i);
}

// fn mk_nodes(depth: i32) -> Ast {
//     let node = |ast_of| Ast { wrapped: Box::new(WithID { elem: ast_of, id: None })};
    
//     let mut ast = node(Shape::App { 
//         func: node(Shape::Var { name: "foo".to_owned() }),
//         off: 0,
//         arg: node(Shape::Var { name: "bar".to_owned() })
//     });
    
//     for x in 0..depth {
//         ast = node( Shape::App { 
//             func: node(Shape::Var { name: "foo".to_owned() }),
//             off: 0,
//             arg: ast
//         })
//     };

//     ast
    
// }








// ==== boilerplate




// =================================
// === Module Structure Reexport ===
// =================================





use console_error_panic_hook;

#[wasm_bindgen(start)]
pub fn start() {
    console_error_panic_hook::set_once();
    set_stdout();
    println!("----");

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