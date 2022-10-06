//! EasingAnimator examples.

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

use ensogl_core::animation::easing::*;
use ensogl_core::prelude::*;
use ensogl_core::system::web::traits::*;

use ensogl_core::animation;
use ensogl_core::system::web;
use js_sys::Math;
use nalgebra::Vector2;
use std::ops::Add;
use std::ops::Mul;
use std::rc::Rc;
use wasm_bindgen::prelude::wasm_bindgen;
use web::CanvasRenderingContext2d;
use web::HtmlCanvasElement;



// ==================
// === SpriteData ===
// ==================

/// Look and feel properties of sprite objects.
#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(missing_docs)]
pub struct SpriteData {
    pub position: Vector2<f32>,
    pub size:     f64,
}

impl SpriteData {
    /// Constructor.
    pub fn new(position: Vector2<f32>, size: f64) -> Self {
        Self { position, size }
    }

    /// Creates random SpriteData.
    pub fn random() -> Self {
        let x = ((Math::random() - 0.5) * 2.0) as f32;
        let y = ((Math::random() - 0.5) * 2.0) as f32;
        let position = Vector2::new(x, y);
        let size = Math::random() * 100.0;
        Self::new(position, size)
    }
}

impl Mul<f32> for SpriteData {
    type Output = Self;
    fn mul(self, rhs: f32) -> Self {
        let position = self.position * rhs;
        let size = self.size * rhs as f64;
        SpriteData { position, size }
    }
}

impl Add<SpriteData> for SpriteData {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        let position = self.position + rhs.position;
        let size = self.size + rhs.size;
        SpriteData { position, size }
    }
}



// ==============
// === Canvas ===
// ==============

/// A simplified Canvas object used in the EasingAnimator example.
#[derive(Clone, Debug)]
pub struct Canvas {
    canvas:  HtmlCanvasElement,
    context: CanvasRenderingContext2d,
}

impl Canvas {
    /// Constructor.
    pub fn new(container_id: &str) -> Self {
        let canvas = web::document.create_canvas_or_panic();
        canvas.set_style_or_warn("border", "1px solid black");
        canvas.set_width(256);
        canvas.set_height(256);

        let context = canvas.get_context("2d").unwrap().unwrap();
        let context: CanvasRenderingContext2d = context.dyn_into().unwrap();

        let app = web::document.get_html_element_by_id(container_id).unwrap();
        app.append_or_warn(&canvas);

        Self { canvas, context }
    }

    /// Clears the canvas.
    pub fn clear(&self) {
        self.context.clear_rect(0.0, 0.0, self.width(), self.height())
    }

    /// Gets Canvas' width.
    pub fn width(&self) -> f64 {
        self.canvas.width() as f64
    }

    /// Gets Canvas` height.
    pub fn height(&self) -> f64 {
        self.canvas.height() as f64
    }

    /// Draw sprite of the provided properties.
    pub fn draw_sprite(&self, data: SpriteData, color: &str) {
        let size = (20.0 + data.size) / self.height();
        let point = data.position;
        self.context.save();
        self.context.set_fill_style(&color.into());
        self.context.scale(self.width() / 2.0, self.height() / 2.0).ok();
        self.context.set_line_width(2.0 / self.height());
        self.context.translate(1.0, 1.0).ok();
        self.context.fill_rect(
            point.x as f64 - size / 2.0,
            point.y as f64 - size / 2.0,
            size,
            size,
        );
        self.context.restore();
    }

    /// Draw a 2D graph of the provided easing function.
    pub fn draw_graph<F: Fn(f32) -> f32>(&self, f: F, color: &str, time_ms: f32) {
        let time_ms = time_ms as f64;
        let width = self.width() - 1.0;
        let height = self.height();

        self.context.set_stroke_style(&color.into());
        self.context.begin_path();
        self.context.save();
        self.context.scale(width, height / 2.0).ok();
        self.context.translate(0.0, 0.5).ok();
        self.context.set_line_width(1.0 / height);
        self.context.move_to(0.0, f(0.0) as f64);
        for x in 1..self.canvas.width() {
            let x = x as f64 / width;
            let y = f(x as f32) as f64;
            self.context.line_to(x, y);
        }
        self.context.stroke();

        self.context.set_fill_style(&color.into());
        let width = 8.0 / width;
        let height = 16.0 / height;
        let time_s = time_ms / 1000.0;
        let x = time_s / 2.0;
        let y = f(x as f32) as f64;
        self.context.fill_rect(x - width / 2.0, y - height / 2.0, width, height);
        self.context.restore();
    }
}

#[allow(clippy::type_complexity)]
struct Sampler {
    color:           &'static str,
    time:            f32,
    left_canvas:     Canvas,
    right_canvas:    Canvas,
    easing_animator: Animator<SpriteData, Box<dyn Fn(f32) -> f32>, Box<dyn Fn(SpriteData)>>,
    properties:      Rc<Cell<SpriteData>>,
    easing_function: Box<dyn Fn(f32) -> f32>,
}

impl Sampler {
    #[allow(trivial_casts)]
    fn new<F>(color: &'static str, left_canvas: &Canvas, right_canvas: &Canvas, f: F) -> Self
    where F: CloneableFnEasing {
        let left_canvas = left_canvas.clone();
        let right_canvas = right_canvas.clone();
        let properties = Rc::new(Cell::new(SpriteData::new(Vector2::new(0.0, 0.0), 1.0)));
        let start = SpriteData::random();
        let end = SpriteData::random();
        let prop = properties.clone();
        let easing_function = Box::new(f.clone()) as Box<dyn Fn(f32) -> f32>;
        let easing_function2 = Box::new(f) as Box<dyn Fn(f32) -> f32>;
        let animation_cb = Box::new(move |t| prop.set(t)) as Box<dyn Fn(SpriteData)>;
        let easing_animator = Animator::new(start, end, easing_function2, animation_cb, ());
        let time = 0.0;
        easing_animator.set_duration(2.0.s());
        Self {
            color,
            time,
            left_canvas,
            right_canvas,
            easing_animator,
            properties,
            easing_function,
        }
    }

    fn render(&mut self, time_diff: f32) {
        self.time += time_diff;
        if self.time > 3000.0 {
            self.time = 0.0;
            let animator = &self.easing_animator;
            animator.set_start_value_no_restart(animator.target_value());
            animator.set_target_value_no_restart(SpriteData::random());
            animator.reset();
        }
        self.left_canvas.draw_graph(&self.easing_function, self.color, self.time);
        self.right_canvas.draw_sprite(self.properties.get(), self.color);
    }
}



// ===============
// === Example ===
// ===============

struct Example {
    _animator: animation::Loop,
}

impl Example {
    #[allow(trivial_casts)]
    pub fn new(
        name: &str,
        ease_in: impl CloneableFnEasing,
        ease_out: impl CloneableFnEasing,
        ease_in_out: impl CloneableFnEasing,
    ) -> Self {
        let example = web::document.create_div_or_panic();
        example.set_attribute_or_warn("id", name);
        example.set_style_or_warn("margin", "10px");
        let container = web::document.get_html_element_by_id("examples").unwrap();
        let header = web::document.create_div_or_panic();
        header.set_attribute_or_warn("id", "center");
        header.set_style_or_warn("background-color", "black");
        header.set_style_or_warn("color", "white");
        header.set_inner_html(name);
        example.append_or_warn(&header);
        container.append_or_warn(&example);
        let left_canvas = Canvas::new(name);
        let right_canvas = Canvas::new(name);
        let mut sampler1 = Sampler::new("green", &left_canvas, &right_canvas, ease_in);
        let mut sampler2 = Sampler::new("blue", &left_canvas, &right_canvas, ease_out);
        let mut sampler3 = Sampler::new("red", &left_canvas, &right_canvas, ease_in_out);

        let _animator = animation::Loop::new(Box::new(move |time_info: animation::TimeInfo| {
            left_canvas.clear();
            right_canvas.clear();
            sampler1.render(time_info.previous_frame.unchecked_raw());
            sampler2.render(time_info.previous_frame.unchecked_raw());
            sampler3.render(time_info.previous_frame.unchecked_raw());
        }) as Box<dyn FnMut(animation::TimeInfo)>);
        Self { _animator }
    }
}

macro_rules! examples {
    ($($name:ident),*) => {$(
        std::mem::forget(Example::new(
            stringify!($name),
            paste!{[<$name _in>]()},
            paste!{[<$name _out>]()},
            paste!{[<$name _in_out>]()},
        ));
    )*};
}

/// Runs EasingAnimator example.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let container = web::document.create_div_or_panic();
    container.set_attribute_or_warn("id", "examples");
    container.set_style_or_warn("display", "flex");
    container.set_style_or_warn("flex-wrap", "wrap");
    container.set_style_or_warn("position", "absolute");
    container.set_style_or_warn("top", "0px");
    web::document.body_or_panic().append_or_warn(&container);
    examples![expo, bounce, circ, quad, cubic, quart, quint, sine, back, elastic];
}
