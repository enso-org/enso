#![cfg_attr(test, allow(dead_code))]
#![feature(unboxed_closures)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(specialization)]
#![feature(associated_type_defaults)]
#![feature(set_stdio)]
//#![warn(missing_docs)]

// Lints. To be refactored after this gets resolved:
// https://github.com/rust-lang/cargo/issues/5034
#![allow(clippy::option_map_unit_fn)]
//#![feature(trace_macros)]
//#![recursion_limit="256"]
//trace_macros!(true);


// =================================
// === Module Structure Reexport ===
// =================================

pub mod control;
pub mod data;
pub mod math;
pub mod dirty;
pub mod display;
pub use basegl_prelude as prelude;
pub mod backend {
    pub use basegl_backend_webgl as webgl;
}
pub mod system {
    pub use basegl_system_web as web;
}
pub mod tp;


// ============
// === Main ===
// ============

use console_error_panic_hook;
use display::world::*;
use nalgebra;
use nalgebra::Vector2;
use nalgebra::Vector3;
use wasm_bindgen::prelude::*;

type Position = SharedBuffer<Vector2<f32>>;
type Color    = SharedBuffer<Vector3<f32>>;

#[wasm_bindgen(start)]
pub fn start() {
    console_error_panic_hook::set_once();
    set_stdout();
    init(&mut World::new().borrow_mut());
}

#[derive(Debug)]
pub struct Rect {
    position : Var<Vector2<f32>>,
    color    : Var<Vector3<f32>>,
}

fn init(world: &mut World) {
    let wspace_id : WorkspaceID    = world.add(Workspace::build("canvas"));
    let workspace : &mut Workspace = &mut world[wspace_id];
    let mesh_id   : MeshID         = workspace.new_mesh();
    let mesh      : &mut Mesh      = &mut workspace[mesh_id];
    let geo       : &mut Geometry  = &mut mesh.geometry;
    let scopes    : &mut Scopes    = &mut geo.scopes;
    let pt_scope  : &mut VarScope  = &mut scopes.point;
    let pos       : Position       = pt_scope.add_buffer("position");
    let color     : Color          = pt_scope.add_buffer("color");

    let inst_ix = pt_scope.add_instance();

    let rect = Rect {
        position : pos.get(inst_ix),
        color    : color.get(inst_ix)
    };

    world.on_frame(move |_| on_frame(&rect)).forget();
}

pub fn on_frame(rect: &Rect) {
     rect.position.modify(|p| p.x += 1.0)
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