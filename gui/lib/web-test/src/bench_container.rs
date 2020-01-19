use crate::prelude::*;

use super::Container;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;

use web_sys::HtmlElement;


// ======================
// === BenchContainer ===
// ======================

/// Html container displaying benchmark results.
#[derive(Shrinkwrap)]
pub struct BenchContainer {
    #[shrinkwrap(main_field)]
    container       : Container,
    pub measurement : HtmlElement,
    pub time        : HtmlElement,
    pub iter        : HtmlElement,
    pub button      : HtmlElement
}

impl BenchContainer {
    /// Creates an identificable container with provided dimensions.
    pub fn new(name:&str, width:f32, height:f32) -> Self {
        let div = create_element("div").expect("div");
        let div : HtmlElement = dyn_into(div).expect("HtmlElement");

        div.set_property_or_panic("margin"         , "0px 2px");
        div.set_property_or_panic("height"         , "24px");
        div.set_property_or_panic("bottom-border"  , "1px solid black");
        div.set_property_or_panic("display"        , "flex");
        div.set_property_or_panic("justify-content", "space-between");
        div.set_property_or_panic("align-items"    , "center");

        div.set_inner_html("<div>00.00ms</div>\
                            <div>0 iterations</div>\
                            <button>Toggle</button>");

        let children             = div.children();
        let time                 = children.item(0).expect("time div");
        let iter                 = children.item(1).expect("iter div");
        let button               = children.item(2).expect("button div");
        let time   : HtmlElement = dyn_into(time).expect("time HtmlElement");
        let iter   : HtmlElement = dyn_into(iter).expect("iter HtmlElement");
        let button : HtmlElement = dyn_into(button).expect("buttn HtmlElement");

        let container       = Container::new("Benchmarks", name, width, height);
        let header_height   = 17.0;
        let height          = format!("{}px", height + header_height + 25.0);

        container.div.set_property_or_panic("height", height);
        container.div.insert_before_or_panic(&div, &container.container);

        let measurement = div;
        Self {container,measurement,time,iter,button}
    }
}
