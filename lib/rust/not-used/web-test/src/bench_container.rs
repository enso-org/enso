use crate::prelude::*;

use super::Container;
use crate::system::web;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;
use wasm_bindgen::JsCast;



// ======================
// === BenchContainer ===
// ======================

/// Html container displaying benchmark results.
#[derive(Shrinkwrap, Debug)]
pub struct BenchContainer {
    #[shrinkwrap(main_field)]
    container:       Container,
    pub measurement: web::HtmlDivElement,
    pub time:        web::HtmlElement,
    pub iter:        web::HtmlElement,
    pub button:      web::HtmlElement,
}

impl BenchContainer {
    /// Creates an identificable container with provided dimensions.
    pub fn new(name: &str, width: f32, height: f32) -> Self {
        let div = web::create_div();
        div.set_style_or_panic("margin", "0px 2px");
        div.set_style_or_panic("height", "24px");
        div.set_style_or_panic("bottom-border", "1px solid black");
        div.set_style_or_panic("display", "flex");
        div.set_style_or_panic("justify-content", "space-between");
        div.set_style_or_panic("align-items", "center");

        div.set_inner_html(
            "<div>00.00ms</div>\
                            <div>0 iterations</div>\
                            <button>Toggle</button>",
        );

        let children = div.children();
        let time = children.item(0).expect("time div");
        let iter = children.item(1).expect("iter div");
        let button = children.item(2).expect("button div");
        let time: web::HtmlElement = time.dyn_into().expect("time HtmlElement");
        let iter: web::HtmlElement = iter.dyn_into().expect("iter HtmlElement");
        let button: web::HtmlElement = button.dyn_into().expect("buttn HtmlElement");

        let container = Container::new("Benchmarks", name, width, height);
        let header_height = 17.0;
        let height = format!("{}px", height + header_height + 25.0);

        container.div.set_style_or_panic("height", height);
        container.div.insert_before_or_panic(&div, &container.container);

        let measurement = div;
        Self { container, measurement, time, iter, button }
    }
}
