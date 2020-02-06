//! EasingAnimator examples.

use wasm_bindgen::prelude::*;
use crate::animation::easing::*;
use crate::animation::animator::easing::EasingAnimator;
use crate::system::web::create_element;
use crate::system::web::NodeInserter;
use crate::system::web::AttributeSetter;
use crate::system::web::StyleSetter;

use nalgebra::Vector2;
use nalgebra::zero;

use web_sys::HtmlElement;
use web_sys::HtmlCanvasElement;
use web_sys::CanvasRenderingContext2d;
use wasm_bindgen::JsCast;
use crate::system::web::get_element_by_id;
use crate::animation::animator::continuous::ContinuousAnimator;
use crate::animation::animator::fixed_step::FixedStepAnimator;
use js_sys::Math;

use std::rc::Rc;
use std::cell::RefCell;

/// A simplified Canvas object used in the EasingAnimator example.
#[derive(Clone,Debug)]
pub struct Canvas {
    canvas  : HtmlCanvasElement,
    context : CanvasRenderingContext2d
}

/// Interpolable properties for our example code.
#[derive(Clone,Copy,Debug)]
pub struct Properties {
    /// Position property.
    pub position : Vector2<f32>,

    /// Size property.
    pub size : f64
}

impl Properties {
    /// Creates new Properties instance.
    pub fn new(position:Vector2<f32>, size:f64) -> Self {
        Self {position,size}
    }

    /// Creates random Properties.
    pub fn random() -> Self {
        Self::new(vector2_random(), Math::random() * 100.0)
    }
}

use std::ops::Mul;
use std::ops::Add;

impl Mul<f32> for Properties {
    type Output = Self;
    fn mul(self, rhs:f32) -> Self {
        let position = self.position * rhs;
        let size     = self.size     * rhs as f64;
        Properties {position,size}
    }
}

impl Add<Properties> for Properties {
    type Output = Self;
    fn add(self, rhs:Self) -> Self {
        let position = self.position + rhs.position;
        let size     = self.size     + rhs.size;
        Properties {position,size}
    }
}

impl Canvas {
    /// Creates a Canvas element inside the identified container.
    pub fn new(container_id:&str) -> Self {
        let canvas = create_element("canvas").unwrap();
        let canvas: HtmlCanvasElement = canvas.dyn_into().unwrap();
        canvas.set_style_or_panic("border", "1px solid black");

        canvas.set_width (256);
        canvas.set_height(256);

        let context = canvas.get_context("2d").unwrap().unwrap();
        let context : CanvasRenderingContext2d = context.dyn_into().unwrap();

        let app : HtmlElement = get_element_by_id(container_id).unwrap().dyn_into().unwrap();
        app.append_or_panic(&canvas);

        Self {canvas,context}
    }

    /// Clears the canvas.
    pub fn clear(&self) {
        self.context.clear_rect(0.0, 0.0, self.canvas.width() as f64, self.canvas.height() as f64)
    }

    /// Gets Canvas' width.
    pub fn width(&self) -> f64 { self.canvas.width() as f64 }

    /// Gets Canvas` height.
    pub fn height(&self) -> f64 { self.canvas.height() as f64 }

    //create properties with {position, opacity}

    /// Draw a point
    pub fn point(&self, properties:Properties, color:&str) {
        let size = (20.0 + properties.size) / self.height();
        let point = properties.position;
        self.context.save();
        self.context.set_fill_style(&color.into());
        self.context.scale(self.width() / 2.0, self.height() / 2.0).ok();
        self.context.set_line_width(2.0 / self.height());
        self.context.translate(1.0, 1.0).ok();
        self.context.fill_rect(point.x as f64 - size / 2.0,point.y as f64 - size / 2.0,size,size);
        self.context.restore();
    }

    /// Draw a 2D graph of the provided FnEasing function.
    pub fn graph<F:FnEasing>(&self, f:F, color:&str, time_ms:f64) {
        let width  = self.width() - 1.0;
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
        let width  = 8.0  / width;
        let height = 16.0 / height;
        let time_seconds = time_ms / 1000.0;
        let x      = (time_seconds % 3.0) / 2.0;
        let y      = f(x as f32) as f64;
        self.context.fill_rect(x - width / 2.0, y - height / 2.0, width, height);
        self.context.restore();
    }
}

/// Creates a Vector3<f32> with random components from -1 to 1.
fn vector2_random() -> Vector2<f32> {
    let x = ((Math::random() - 0.5) * 2.0) as f32;
    let y = ((Math::random() - 0.5) * 2.0) as f32;
    Vector2::new(x, y)
}

struct SharedData {
    graph_canvas     : Canvas,
    animation_canvas : Canvas,
    easing_animator  : Option<EasingAnimator<Properties>>,
    properties       : Properties,
    easing_function  : &'static dyn FnEasing
}

#[derive(Clone)]
struct SubExample {
    data : Rc<RefCell<SharedData>>
}

impl SubExample {
    fn new<F>
    ( graph_canvas     : Canvas
    , animation_canvas : Canvas
    , f                : &'static F
    , initial_value    : Properties
    , final_value      : Properties) -> Self
    where F:FnEasing {
        let properties       = Properties::new(Vector2::new(0.0, 0.0), 1.0);
        let easing_animator  = None;
        let easing_function = f;
        let data = SharedData {
            properties,
            easing_function,
            graph_canvas,
            animation_canvas,
            easing_animator
        };
        let data     = Rc::new(RefCell::new(data));
        let weak     = Rc::downgrade(&data);
        let duration = 2.0;
        let easing_animator = EasingAnimator::new(
            move |value| { weak.upgrade().map(|data| data.borrow_mut().properties = value); },
            f,
            initial_value,
            final_value,
            duration
        );
        data.borrow_mut().easing_animator = Some(easing_animator);
        Self {data}
    }

    fn set_properties(&mut self, value:Properties) {
        let mut data      = self.data.borrow_mut();
        let initial_value = data.properties;
        data.easing_animator.as_mut().map(
            |easing| easing.animate(initial_value, value, 2.0)
        );
    }

    fn render(&self, color:&str, time_ms:f64) {
        let data = self.data.borrow();
        data.graph_canvas.graph(data.easing_function, color, time_ms);
        data.animation_canvas.point(data.properties, color);
    }
}

struct Example {
    _animator : ContinuousAnimator
}

impl Example {
    pub fn new<F1, F2, F3>
    ( name        : &str
    , ease_in     : &'static F1
    , ease_out    : &'static F2
    , ease_in_out : &'static F3) -> Self
    where F1:FnEasing, F2:FnEasing, F3:FnEasing {
        let example : HtmlElement = create_element("div").unwrap().dyn_into().unwrap();
        example.set_attribute_or_panic("id", name);
        example.set_style_or_panic("margin", "10px");
        let container : HtmlElement = get_element_by_id("examples").unwrap().dyn_into().unwrap();
        let header    : HtmlElement = create_element("center").unwrap().dyn_into().unwrap();
        header.set_style_or_panic("background-color", "black");
        header.set_style_or_panic("color", "white");
        header.set_inner_html(name);
        example.append_or_panic(&header);
        container.append_or_panic(&example);
        let graph_canvas     = Canvas::new(name);
        let animation_canvas = Canvas::new(name);

        let initial_value    = Properties::new(zero(), 1.0);
        let final_value      = Properties::random();

        let mut easing_in = SubExample::new(
            graph_canvas.clone(),
            animation_canvas.clone(),
            ease_in,
            initial_value,
            final_value
        );
        let easing_in_clone = easing_in.clone();

        let mut easing_out = SubExample::new(
            graph_canvas.clone(),
            animation_canvas.clone(),
            ease_out,
            initial_value,
            final_value
        );
        let easing_out_clone = easing_out.clone();

        let mut easing_in_out = SubExample::new(
            graph_canvas.clone(),
            animation_canvas.clone(),
            ease_in_out,
            initial_value,
            final_value
        );
        let easing_in_out_clone = easing_in_out.clone();

        let _fixed_step = FixedStepAnimator::new(1.0 / 3.0, move |_| {
            let properties = Properties::random();
            easing_in.set_properties(properties);
            easing_out.set_properties(properties);
            easing_in_out.set_properties(properties);
        });

        let _animator = ContinuousAnimator::new(move |time_ms| {
            let _keep_alive = &_fixed_step;
            graph_canvas.clear();
            animation_canvas.clear();
            easing_out_clone.render("green", time_ms);
            easing_in_out_clone.render("blue", time_ms);
            easing_in_clone.render("red", time_ms);
        });
        Self {_animator}
    }
}

macro_rules! example {
    ($name:ident) => {
        std::mem::forget(Example::new(
            stringify!($name),
            &paste::expr!{[<$name _in>]},
            &paste::expr!{[<$name _out>]},
            &paste::expr!{[<$name _in_out>]},
        ))
    };
}

#[wasm_bindgen]
#[allow(dead_code)]
/// Runs EasingAnimator example.
pub fn run_example_easing_animator() {
    let container : HtmlElement = create_element("div").unwrap().dyn_into().unwrap();
    container.set_attribute_or_panic("id", "examples");
    container.set_style_or_panic("display", "flex");
    container.set_style_or_panic("flex-wrap", "wrap");
    container.set_style_or_panic("position", "absolute");
    container.set_style_or_panic("top", "0px");
    get_element_by_id("app").unwrap().append_or_panic(&container);
    example!(expo);
    example!(bounce);
    example!(circ);
    example!(quad);
    example!(cubic);
    example!(quart);
    example!(quint);
    example!(sine);
    example!(back);
    example!(elastic);
}
