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
#![allow(clippy::bool_to_int_with_if)]
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
use ensogl_core::display::Scene;
use ensogl_core::frp::io::timer::DocumentOps;
use ensogl_core::frp::io::timer::HtmlElementOps;
use ensogl_core::system::web;
use ensogl_core::system::web::Closure;
use ensogl_core::system::web::JsCast;
use ensogl_core::system::web::JsValue;
use ensogl_text::buffer;
use ensogl_text::formatting;
use ensogl_text::Text;
use ensogl_text_msdf::run_once_initialized;



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
    ensogl_core::shape! {
        (style: Style, color_rgba: Vector4<f32>) {
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
    ensogl_core::shape! {
        (style: Style, color_rgba: Vector4<f32>) {
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
                borders.right.set_size(Vector2(BORDER_WIDTH + BORDER_PADDING * 2.0, *h));
                borders.left.set_size(Vector2(BORDER_WIDTH + BORDER_PADDING * 2.0, *h));
                borders.right.set_y(-h/2.0);
                borders.left.set_y(-h/2.0);

                borders.bottom_changed_frame_hold.set(DEBUG_FRAME_HOLD);
                borders.bottom.color_rgba.set(RED);
                borders.bottom.set_y(-*h);
            });
            eval area.width ([borders](w) {
                borders.top.set_size(Vector2(*w, BORDER_WIDTH + BORDER_PADDING * 2.0));
                borders.bottom.set_size(Vector2(*w, BORDER_WIDTH + BORDER_PADDING * 2.0));
                borders.top.set_x(w/2.0);
                borders.bottom.set_x(w/2.0);

                borders.right_changed_frame_hold.set(DEBUG_FRAME_HOLD);
                borders.right.color_rgba.set(RED);
                borders.right.set_x(*w);
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
    let content = "abcdefghijk";
    // This is a testing string left here for convenience.
    // area.set_content("aஓbc🧑🏾de\nfghij\nklmno\npqrst\n01234\n56789");
    area.set_content(content);
    area.set_font("mplus1p");
    area.set_property_default(color::Rgba::black());
    area.deprecated_focus();
    area.hover();

    let borders = Borders::default();
    borders.show(&app, &area);

    let scene = &app.display.default_scene;
    let navigator = Navigator::new(scene, &scene.camera());
    app.display.default_scene.add_child(&area);

    let area = Rc::new(RefCell::new(Some(area)));

    // Initialization of HTML div displaying the same text. It allows for switching between
    // WebGL and HTML versions to compare them.
    let style = web::document.create_element_or_panic("style");
    let css = web::document.create_text_node("@import url('https://fonts.googleapis.com/css2?family=M+PLUS+1p:wght@400;700&display=swap');");
    style.append_child(&css).unwrap();
    web::document.head().unwrap().append_child(&style).unwrap();
    let div = web::document.create_div_or_panic();
    div.set_style_or_warn("width", "100px");
    div.set_style_or_warn("height", "100px");
    div.set_style_or_warn("position", "absolute");
    div.set_style_or_warn("z-index", "100");
    div.set_style_or_warn("font-family", "'M PLUS 1p'");
    div.set_style_or_warn("font-size", "12px");
    div.set_style_or_warn("display", "none");
    div.set_inner_text(content);
    web::document.body().unwrap().append_child(&div).unwrap();

    init_debug_hotkeys(&app.display.default_scene, &area, &div);

    let scene = scene.clone_ref();
    let handler = app.display.on.before_frame.add(move |_time| {
        let shape = scene.dom.shape();
        div.set_style_or_warn("left", format!("{}px", shape.width / 2.0));
        div.set_style_or_warn("top", format!("{}px", shape.height / 2.0 - 0.5));
    });

    mem::forget(handler);
    mem::forget(navigator);
    mem::forget(app);
}

fn init_debug_hotkeys(scene: &Scene, area: &Rc<RefCell<Option<Text>>>, div: &web::HtmlDivElement) {
    let html_version = Rc::new(Cell::new(false));
    let scene = scene.clone_ref();
    let area = area.clone_ref();
    let div = div.clone();
    let mut fonts_cycle = ["dejavusans", "dejavusansmono", "mplus1p"].iter().cycle();
    let closure: Closure<dyn FnMut(JsValue)> = Closure::new(move |val: JsValue| {
        let event = val.unchecked_into::<web::KeyboardEvent>();
        if event.ctrl_key() {
            let key = event.code();
            if key == "Slash" {
                debug!("Removing the text area.");
                *area.borrow_mut() = None;
            }
        }
        if let Some(area) = &*area.borrow() {
            div.set_inner_text(&area.content.value().to_string());
            if event.ctrl_key() {
                let key = event.code();
                warn!("{:?}", key);
                if key == "KeyH" {
                    html_version.set(!html_version.get());
                    if html_version.get() {
                        warn!("Showing the HTML version.");
                        area.unset_parent();
                        div.set_style_or_warn("display", "block");
                    } else {
                        warn!("Showing the WebGL version.");
                        scene.add_child(&area);
                        div.set_style_or_warn("display", "none");
                    }
                } else if key == "Digit1" {
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
                } else if key == "KeyN" {
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
                } else if key == "KeyF" {
                    let font = fonts_cycle.next().unwrap();
                    warn!("Switching to font '{}'.", font);
                    area.set_font(font);
                } else if key == "Equal" {
                    if event.shift_key() {
                        area.set_property_default(formatting::Size(16.0));
                    } else {
                        area.mod_property(
                            buffer::RangeLike::Selections,
                            formatting::FontSizeDiff(2.0),
                        );
                    }
                } else if key == "Minus" {
                    if event.shift_key() {
                        area.set_property_default(formatting::Size(16.0));
                    } else {
                        area.mod_property(
                            buffer::RangeLike::Selections,
                            formatting::FontSizeDiff(-2.0),
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
