//! An example showing usage of Text Text.

#![recursion_limit = "1024"]
// === Features ===
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::system::web;
use ensogl_core::system::web::Closure;
use ensogl_core::system::web::JsCast;
use ensogl_core::system::web::JsValue;
use ensogl_text::buffer;
use ensogl_text::formatting;
use ensogl_text::Text;
use ensogl_text_msdf::run_once_initialized;
use wasm_bindgen::prelude::wasm_bindgen;



// ==============
// === Shapes ===
// ==============

const DEBUG_FRAME_HOLD: usize = 3;
const BORDER_PADDING: f32 = 2.0;
const BORDER_WIDTH: f32 = 1.0;

const RED: Vector4 = Vector4::new(0.85, 0.2, 0.2, 1.0);
const GREEN: Vector4 = Vector4::new(0.2, 0.85, 0.2, 1.0);



// ===============
// === Borders ===
// ===============

mod h_line {
    use super::*;
    ensogl_core::define_shape_system! {
        (color_rgba:Vector4<f32>) {
            let fill_color = Var::<color::Rgba>::from(color_rgba);
            let height = Var::<Pixels>::from("input_size.y");
            let shape = Rect((BORDER_WIDTH.px(), height));
            let shape = shape.fill(fill_color);
            shape.into()
        }
    }
}

mod v_line {
    use super::*;
    ensogl_core::define_shape_system! {
        (color_rgba:Vector4<f32>) {
            let fill_color = Var::<color::Rgba>::from(color_rgba);
            let width = Var::<Pixels>::from("input_size.x");
            let shape = Rect((width, BORDER_WIDTH.px()));
            let shape = shape.fill(fill_color);
            shape.into()
        }
    }
}

ensogl_core::define_endpoints_2! {}

#[derive(Debug, Clone, CloneRef, Default)]
struct Borders {
    left: h_line::View,
    right: h_line::View,
    bottom: v_line::View,
    top: v_line::View,
    right_changed_frame_hold: Rc<Cell<usize>>,
    bottom_changed_frame_hold: Rc<Cell<usize>>,
}

impl Borders {
    pub fn show(&self, app: &Application, area: &Text) {
        let frp = Frp::new();
        let network = frp.network();
        let borders = self;
        app.display.add_child(&borders.left);
        app.display.add_child(&borders.right);
        app.display.add_child(&borders.top);
        app.display.add_child(&borders.bottom);
        borders.left.color_rgba.set(GREEN);
        borders.top.color_rgba.set(GREEN);

        ensogl_core::frp::extend! {network
            eval_ app.display.after_rendering([borders]{
                if borders.right_changed_frame_hold.get() != 0 {
                    borders.right_changed_frame_hold.modify(|t| *t -= 1);
                } else {
                    borders.right.color_rgba.set(GREEN);
                }

                if borders.bottom_changed_frame_hold.get() != 0 {
                    borders.bottom_changed_frame_hold.modify(|t| *t -= 1);
                } else {
                    borders.bottom.color_rgba.set(GREEN);
                }
            });
            eval area.height ([borders](h) {
                borders.right.size.set(Vector2(BORDER_WIDTH + BORDER_PADDING * 2.0, *h));
                borders.left.size.set(Vector2(BORDER_WIDTH + BORDER_PADDING * 2.0, *h));
                borders.right.set_position_y(-h/2.0);
                borders.left.set_position_y(-h/2.0);

                borders.bottom_changed_frame_hold.set(DEBUG_FRAME_HOLD);
                borders.bottom.color_rgba.set(RED);
                borders.bottom.set_position_y(-*h);
            });
            eval area.width ([borders](w) {
                borders.top.size.set(Vector2(*w, BORDER_WIDTH + BORDER_PADDING * 2.0));
                borders.bottom.size.set(Vector2(*w, BORDER_WIDTH + BORDER_PADDING * 2.0));
                borders.top.set_position_x(w/2.0);
                borders.bottom.set_position_x(w/2.0);

                borders.right_changed_frame_hold.set(DEBUG_FRAME_HOLD);
                borders.right.color_rgba.set(RED);
                borders.right.set_position_x(*w);
            });
        }
        mem::forget(frp);
    }
}



// ===================
// === Entry Point ===
// ===================

/// Main example runner.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    run_once_initialized(|| {
        init(Application::new("root"));
    });
}


fn init(app: Application) {
    let area = app.new_view::<Text>();
    let quote = "Et Eärello Endorenna utúlien.\nSinome maruvan ar Hildinyar tenn' Ambar-metta\n";
    let snowman = "\u{2603}";
    let zalgo = "Z̮̞̠͙͔ͅḀ̗̞͈̻̗Ḷ͙͎̯̹̞͓G̻O̭̗̮";
    let _text = quote.to_string() + snowman + zalgo;
    let _text = "test".to_string();
    // area.set_content("aஓbc🧑🏾de\nfghij\nklmno\npqrst\n01234\n56789");
    area.set_content("abcdefg");
    // area.set_font("default");
    area.focus();
    area.hover();

    let borders = Borders::default();
    borders.show(&app, &area);

    // FIXME: fix forward_panic_hook_to_console
    // FIXME: fix test in monitor

    // TODO: Task na unit testyy do textow
    // TODO: next PR - > Text area to gui component.


    let scene = &app.display.default_scene;
    let navigator = Navigator::new(scene, &scene.camera());
    app.display.default_scene.add_child(&area);

    let area = Rc::new(RefCell::new(Some(area)));
    init_debug_hotkeys(&area);

    mem::forget(navigator);
    mem::forget(app);
}

fn init_debug_hotkeys(area: &Rc<RefCell<Option<Text>>>) {
    let area = area.clone_ref();
    let closure: Closure<dyn Fn(JsValue)> = Closure::new(move |val: JsValue| {
        let event = val.unchecked_into::<web::KeyboardEvent>();
        if event.ctrl_key() {
            let key = event.code();
            if key == "Slash" {
                debug!("Removing the text area.");
                *area.borrow_mut() = None;
            }
        }
        if let Some(area) = &*area.borrow() {
            if event.ctrl_key() {
                let key = event.code();
                warn!("{:?}", key);
                if key == "Digit1" {
                    if event.shift_key() {
                        area.set_property_default(color::Rgba::black());
                    } else {
                        area.set_property(buffer::RangeLike::Selections, color::Rgba::black());
                    }
                } else if key == "Digit2" {
                    if event.shift_key() {
                        area.set_property_default(color::Rgba::red());
                    } else {
                        area.set_property(buffer::RangeLike::Selections, color::Rgba::red());
                    }
                } else if key == "Digit3" {
                    if event.shift_key() {
                        area.set_property_default(color::Rgba::green());
                    } else {
                        area.set_property(buffer::RangeLike::Selections, color::Rgba::green());
                    }
                } else if key == "Digit4" {
                    if event.shift_key() {
                        area.set_property_default(color::Rgba::blue());
                    } else {
                        area.set_property(buffer::RangeLike::Selections, color::Rgba::blue());
                    }
                } else if key == "Digit0" {
                    area.set_property(
                        buffer::RangeLike::Selections,
                        formatting::Property::Color(None),
                    );
                } else if key == "KeyB" {
                    if event.shift_key() {
                        area.set_property_default(formatting::Weight::Bold);
                    } else if event.alt_key() {
                        area.set_property(
                            buffer::RangeLike::Selections,
                            formatting::Property::Weight(None),
                        );
                    } else {
                        area.set_property(buffer::RangeLike::Selections, formatting::Weight::Bold);
                    }
                } else if key == "KeyH" {
                    if event.shift_key() {
                        area.set_property_default(formatting::SdfWeight(0.02));
                    } else {
                        area.set_property(
                            buffer::RangeLike::Selections,
                            formatting::SdfWeight(0.02),
                        );
                    }
                } else if key == "KeyI" {
                    if event.shift_key() {
                        area.set_property_default(formatting::Style::Italic);
                    } else {
                        area.set_property(buffer::RangeLike::Selections, formatting::Style::Italic);
                    }
                } else if key == "Equal" {
                    if event.shift_key() {
                        area.set_property_default(formatting::Size(16.0));
                    } else {
                        area.mod_property(buffer::RangeLike::Selections, formatting::SizeDiff(2.0));
                    }
                } else if key == "Minus" {
                    if event.shift_key() {
                        area.set_property_default(formatting::Size(16.0));
                    } else {
                        area.mod_property(
                            buffer::RangeLike::Selections,
                            formatting::SizeDiff(-2.0),
                        );
                    }
                } else if key == "ArrowUp" {
                    area.mod_first_view_line(-1);
                } else if key == "ArrowDown" {
                    area.mod_first_view_line(1);
                } else if key == "KeyW" {
                    area.set_long_text_truncation_mode(true);
                    area.set_view_width(60.0);
                }
            }
        }
    });
    let handle = web::add_event_listener_with_bool(&web::window, "keydown", closure, true);
    mem::forget(handle);
}
