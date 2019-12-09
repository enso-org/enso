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
pub mod text;
pub use basegl_prelude as prelude;
pub mod backend {
    pub use basegl_backend_webgl as webgl;
}
pub mod system {
    pub use basegl_system_web as web;
}
pub mod tp;
pub mod utils;

// ==================
// === Example 01 ===
// ==================

mod example_01 {
    use crate::set_stdout;
    use crate::display::world::*;
    use crate::prelude::*;
    use nalgebra::{Vector2, Vector3};
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    #[allow(dead_code)]
    pub fn run_01_example() {
        console_error_panic_hook::set_once();
        set_stdout();
        init(&mut World::new().borrow_mut());
    }

    type Position = SharedBuffer<Vector2<f32>>;
    type Color    = SharedBuffer<Vector3<f32>>;

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
}

// ==================
// === Example 03 ===
// ==================

mod example_03 {
    use wasm_bindgen::prelude::*;

    use crate::utils;
    use crate::display::world::{World,Workspace,Add};
    use crate::text::font::FontRenderInfo;
    use crate::Color;

    use crate::dirty::traits::SharedSetter1;
    use basegl_core_embedded_fonts::EmbeddedFonts;
    use itertools::iproduct;

    const FONT_NAMES : &[&str] = &
    [ "DejaVuSans"
    , "DejaVuSansMono"
    , "DejaVuSansMono-Bold"
    , "DejaVuSerif"
    ];

    const SIZES : &[f64] = &[0.024, 0.032, 0.048];

    #[wasm_bindgen]
    #[allow(dead_code)]
    pub fn run_03_text() {
        utils::set_panic_hook();
        basegl_core_msdf_sys::run_once_initialized(|| {
            let mut world_ref = World::new();
            let workspace_id  = world_ref.add(Workspace::build("canvas"));
            let world         = &mut world_ref.borrow_mut();
            let workspace     = &mut world[workspace_id];
            let font_base     = EmbeddedFonts::create_and_fill();
            let font_creator  = |name:&&'static str| FontRenderInfo::from_embedded(&font_base,name);
            let fonts_iter    = FONT_NAMES.iter().map(font_creator);
            let mut fonts     = fonts_iter.collect::<Box<[FontRenderInfo]>>();

            let all_cases     = iproduct!(0..fonts.len(), 0..SIZES.len());

            for (font, size) in all_cases {

                let x = -0.95 + 0.6 * (size as f64);
                let y = 0.90 - 0.45 * (font as f64);
                let line_position = nalgebra::Vector2::new(x,y);
                let text_compnent = crate::text::TextComponentBuilder {
                    text : "To be, or not to be, that is the question:\n\
                        Whether 'tis nobler in the mind to suffer\n\
                        The slings and arrows of outrageous fortune,\n\
                        Or to take arms against a sea of troubles\n\
                        And by opposing end them."
                        .to_string(),
                    font     : &mut fonts[font],
                    position : line_position,
                    size     : SIZES[size],
                    color    : Color {r: 1.0, g: 1.0, b: 1.0, a: 1.0},
                }.build(workspace);
                workspace.text_components.push(text_compnent);
            }
            world.workspace_dirty.set(workspace_id);
        });
    }
}


// =================
// === Utilities ===
// =================

#[derive(Debug)]
pub struct Color<T> {
    pub r : T,
    pub g : T,
    pub b : T,
    pub a : T
}


// ===============
// === Printer ===
// ===============

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
