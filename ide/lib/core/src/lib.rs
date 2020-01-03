#![cfg_attr(test, allow(dead_code))]
#![feature(drain_filter)]
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

/// Uncomment the following code to enable macro debugging.
//#![feature(trace_macros)]
//#![recursion_limit="256"]
//trace_macros!(true);


// =================================
// === Module Structure Reexport ===
// =================================

pub mod animation;
pub mod control;
pub mod data;
pub mod debug;
pub mod display;
pub mod traits;

pub use basegl_prelude as prelude;
pub mod system {
    pub use basegl_system_web as web;
}



// ==================
// === Example 01 ===
// ==================

mod example_01 {
    use super::*;
    use crate::set_stdout;
    use crate::display::world::*;
    use crate::prelude::*;
    use nalgebra::{Vector2, Vector3, Matrix4};
    use wasm_bindgen::prelude::*;
    use basegl_system_web::{Logger, get_performance};
    use web_sys::Performance;
    use crate::display::object::DisplayObjectData;


    #[wasm_bindgen]
    #[allow(dead_code)]
    pub fn run_example_basic_objects_management() {
        set_panic_hook();
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
        let sym_id    : SymbolId       = workspace.new_symbol();
        let symbol    : &mut Symbol    = &mut workspace[sym_id];
        let mesh      : &mut Mesh      = &mut symbol.surface;
        let scopes    : &mut Scopes    = &mut mesh.scopes;
        let pt_scope  : &mut VarScope  = &mut scopes.point;
        let inst_scope: &mut VarScope  = &mut scopes.instance;
        let transform : Buffer<Matrix4<f32>> = inst_scope.add_buffer("transform");
        let uv        : Buffer<Vector2<f32>> = pt_scope.add_buffer("uv");
        let bbox      : Buffer<Vector2<f32>> = pt_scope.add_buffer("bbox");

        let p1_ix = pt_scope.add_instance();
        let p2_ix = pt_scope.add_instance();
        let p3_ix = pt_scope.add_instance();
        let p4_ix = pt_scope.add_instance();

        let inst_1_ix = inst_scope.add_instance();
//        let inst_2_ix = inst_scope.add_instance();
//
        let transform1 = transform.get(inst_1_ix);
//        let transform2 = transform.get(inst_2_ix);

//        transform1.modify(|t| {t.append_translation_mut(&Vector3::new( 1.0,  100.0, 0.0));});
//        transform2.modify(|t| {t.append_translation_mut(&Vector3::new( 1.0,  200.0, 0.0));});



        let uv1 = uv.get(p1_ix);
        let uv2 = uv.get(p2_ix);
        let uv3 = uv.get(p3_ix);
        let uv4 = uv.get(p4_ix);

        uv1.set(Vector2::new(0.0, 0.0));
        uv2.set(Vector2::new(0.0, 1.0));
        uv3.set(Vector2::new(1.0, 0.0));
        uv4.set(Vector2::new(1.0, 1.0));

        let bbox1 = bbox.get(p1_ix);
        let bbox2 = bbox.get(p2_ix);
        let bbox3 = bbox.get(p3_ix);
        let bbox4 = bbox.get(p4_ix);

        bbox1.set(Vector2::new(2.0, 2.0));
        bbox2.set(Vector2::new(2.0, 2.0));
        bbox3.set(Vector2::new(2.0, 2.0));
        bbox4.set(Vector2::new(2.0, 2.0));


//        let mm1 = model_matrix.get(p1_ix);
//        let mm2 = model_matrix.get(p2_ix);
//        let mm3 = model_matrix.get(p3_ix);
//        let mm4 = model_matrix.get(p4_ix);
//
//        mm1.modify(|t| {t.append_translation_mut(&Vector3::new( 1.0,  100.0, 0.0));});
//        mm2.modify(|t| {t.append_translation_mut(&Vector3::new( 1.0,  100.0, 0.0));});
//        mm3.modify(|t| {t.append_translation_mut(&Vector3::new( 1.0,  100.0, 0.0));});
//        mm4.modify(|t| {t.append_translation_mut(&Vector3::new( 1.0,  100.0, 0.0));});
//    mm5.modify(|t| {t.append_translation_mut(&Vector3::new(-1.0,  1.0, 0.0));});
//    mm6.modify(|t| {t.append_translation_mut(&Vector3::new(-1.0, -1.0, 0.0));});
//
//    mm1.set(Matrix4::new( 0.0,  0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0));
//    mm2.set(Matrix4::new( 0.0,  0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0));
//    mm3.set(Matrix4::new( 0.0,  0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0));
//
//    mm4.set(Matrix4::new( 0.0,  0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0));
//    mm5.set(Matrix4::new( 0.0,  0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0));
//    mm6.set(Matrix4::new( 0.0,  0.0, 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0));


//    println!("{:?}",pos);
//    println!("{:?}",pos.borrow().as_prim());

//        let camera = workspace.scene.camera.clone();


        let make_widget = |scope: &mut VarScope| {
            let inst_1_ix = scope.add_instance();
            let transform1 = transform.get(inst_1_ix);
            Widget::new(Logger::new("widget"),transform1)
        };



        let w1 = Widget::new(Logger::new("widget1"),transform1);




        let mut widgets: Vec<Widget> = default();
        let count = 100;
        for _ in 0 .. count {
            let widget = make_widget(inst_scope);
            widgets.push(widget);
        }

        let performance = get_performance().unwrap();


        let mut i:i32 = 0;
        world.on_frame(move |w| on_frame(w,&mut i,&w1, &mut widgets,&performance,wspace_id,sym_id,&transform)).forget();


    }

    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::many_single_char_names)]
    pub fn on_frame(world:&mut World, ii:&mut i32, w1:&Widget, widgets:&mut Vec<Widget>, performance:&Performance, wspace_id:WorkspaceID, sym_id:SymbolId, transform : &Buffer<Matrix4<f32>>) {
//        camera.mod_position(|p| {
//            p.x -= 0.1;
//            p.z += 1.0
//        });

        let workspace : &mut Workspace = &mut world[wspace_id];
        let symbol    : &mut Symbol    = &mut workspace[sym_id];
        let mesh      : &mut Mesh      = &mut symbol.surface;
        let scopes    : &mut Scopes    = &mut mesh.scopes;
        let inst_scope: &mut VarScope  = &mut scopes.instance;

        let make_widget = |scope: &mut VarScope| {
            let inst_1_ix = scope.add_instance();
            let transform1 = transform.get(inst_1_ix);
            Widget::new(Logger::new("widget"),transform1)
        };



        w1.transform.mod_position(|p| p.y += 0.5);
        w1.transform.update();

        *ii += 1;

        if *ii < 200i32 {
            let count = 100;
            if widgets.len() < 100_000 {
                for _ in 0..count {
                    let widget = make_widget(inst_scope);
                    widgets.push(widget);
                }
            }

            let t = (performance.now() / 1000.0) as f32;
            let length = widgets.len() as f32;
            for (i, object) in widgets.iter_mut().enumerate() {
                let i = i as f32;
                let d = (i / length - 0.5) * 2.0;

                let mut y = d;
                let r = (1.0 - y * y).sqrt();
                let mut x = (y * 100.0 + t).cos() * r;
                let mut z = (y * 100.0 + t).sin() * r;

                x += (y * 1.25 + t * 2.50).cos() * 0.5;
                y += (z * 1.25 + t * 2.00).cos() * 0.5;
                z += (x * 1.25 + t * 3.25).cos() * 0.5;
                object.transform.set_position(Vector3::new(x * 50.0 + 200.0, y * 50.0 + 100.0, z * 50.0));
//            object.transform.set_position(Vector3::new(0.0, 0.0, 0.0));
                object.transform.update();

//            let faster_t = t * 100.0;
//            let r = (i +   0.0 + faster_t) as u8 % 255;
//            let g = (i +  85.0 + faster_t) as u8 % 255;
//            let b = (i + 170.0 + faster_t) as u8 % 255;
//            set_gradient_bg(&object.dom, &r.into(), &g.into(), &b.into());
            }
        }



    }


    pub struct Widget {
        pub transform : DisplayObjectData,
        pub mm        : Var<Matrix4<f32>>,
    }

    impl Widget {
        pub fn new(logger:Logger, mm:Var<Matrix4<f32>>) -> Self {
            let transform = DisplayObjectData::new(logger);
            let mm_cp = mm.clone();
            transform.set_on_updated(move |t| {
                mm_cp.set(t.matrix().clone());
            });
            Self {transform,mm}
        }
    }
}

// ==================
// === Example 02 ===
// ==================

mod example_02 {
    use super::*;
    use wasm_bindgen::prelude::*;

    use crate::display::world::World;
    use crate::display::world::Workspace;
    use crate::display::world::Add;
    use crate::display::world::WorkspaceID;
    use crate::Color;
    use crate::data::dirty::traits::*;

    use nalgebra::Point2;
    use nalgebra::Vector2;
    use crate::display::shape::text::content::CharPosition;
    use crate::display::shape::text::content::TextChange;
    use crate::display::shape::text::TextComponentBuilder;
    use crate::display::shape::text::TextComponentProperties;

    #[wasm_bindgen]
    #[allow(dead_code)]
    pub fn run_example_text() {
        set_panic_hook();
        basegl_core_msdf_sys::run_once_initialized(|| {
            let mut world_ref     = World::new();
            let workspace_id      = world_ref.add(Workspace::build("canvas"));
            let world :&mut World = &mut world_ref.borrow_mut();
            let workspace         = &mut world.workspaces[workspace_id];
            let fonts             = &mut world.fonts;
            let font_id           = fonts.load_embedded_font("DejaVuSansMono").unwrap();

            let mut text_component = TextComponentBuilder{workspace,fonts,font_id,
                text       : "".to_string(),
                properties : TextComponentProperties {
                    position  : Point2::new(-0.95,-0.9),
                    size      : Vector2::new(1.8,1.6),
                    text_size : 0.032,
                    color     : Color {r: 0.0, g: 0.8, b: 0.0, a: 1.0},
                }
            }.build();
            text_component.cursors.add_cursor(CharPosition{line:0,column:0});
            workspace.text_components.push(text_component);
            world.workspace_dirty.set(workspace_id);

            let now             = js_sys::Date::now();
            let animation_start = now + 3000.0;
            let start_scrolling = animation_start + 10000.0;
            let mut chars       = typed_character_list(animation_start,include_str!("lib.rs"));
            world.on_frame(move |w| {
                animate_text_component(w,workspace_id,&mut chars,start_scrolling)
            }).forget();
        });
    }

    struct CharToPush {
        time   : f64,
        a_char: char,
    }

    const ONE_CHAR_TYPING_DURATION_MS : f64 = 50.0;

    fn typed_character_list(start_time:f64, text:&'static str) -> Vec<CharToPush> {
        text.char_indices().map(|(i,a_char)| {
            let time = start_time + ONE_CHAR_TYPING_DURATION_MS * i as f64;
            CharToPush {time,a_char}
        }).collect()
    }

    fn animate_text_component
    ( world:&mut World
    , workspace_id:WorkspaceID
    , typed_chars:&mut Vec<CharToPush>
    , start_scrolling:f64) {
        let workspace   = &mut world.workspaces[workspace_id];
        let editor      = workspace.text_components.first_mut().unwrap();
        let now         = js_sys::Date::now();

        let to_type_now = typed_chars.drain_filter(|ch| ch.time <= now);
        for ch in to_type_now {
            let cursor = editor.cursors.cursors.first_mut().unwrap();
            let string = ch.a_char.to_string();
            let change = TextChange::insert(cursor.position, string.as_str());
            editor.content.make_change(change);
            let new_cursor_position = CharPosition {
                line: editor.content.lines.len()-1,
                column : editor.content.lines.last().unwrap().len(),
            };
            cursor.position = new_cursor_position;
            editor.cursors.dirty_cursors.insert(0);
        }
        if start_scrolling <= js_sys::Date::now() {
            editor.scroll(Vector2::new(0.0, -0.01));
        }
        world.workspace_dirty.set(workspace_id);
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
    pub a : T,
}

#[derive(Debug)]
pub struct Area<T> {
    pub left   : T,
    pub right  : T,
    pub top    : T,
    pub bottom : T,
}

impl<T:std::ops::Sub+Clone> Area<T> {
    pub fn width(&self) -> T::Output {
        self.right.clone() - self.left.clone()
    }

    pub fn height(&self) -> T::Output {
        self.top.clone() - self.bottom.clone()
    }
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

pub fn set_panic_hook() {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    #[cfg(feature = "console_error_panic_hook")]
        console_error_panic_hook::set_once();
}
