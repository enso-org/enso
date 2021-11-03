use super::Group;
use crate::system::web;
use crate::system::web::AttributeSetter;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;
use wasm_bindgen::JsCast;



// =================
// === Container ===
// =================

/// A container to hold tests in `wasm-pack test`.
#[derive(Clone, Debug)]
pub struct Container {
    pub div:       web::HtmlDivElement,
    pub header:    web::HtmlElement,
    pub container: web::HtmlElement,
}

impl Container {
    /// Creates an identificable container with provided dimensions.
    pub fn new(group: &str, name: &str, width: f32, height: f32) -> Self {
        let div = web::create_div();
        let width = format!("{}px", width);
        let header = web::create_element("center");
        let header: web::HtmlElement = header.dyn_into().expect("HtmlElement");

        div.set_style_or_panic("width", &width);
        div.set_style_or_panic("height", format!("{}px", height + 17.0));
        div.set_style_or_panic("border", "1px solid black");
        div.set_style_or_panic("position", "relative");
        div.set_style_or_panic("margin", "10px");
        header.set_inner_html(name);
        header.set_style_or_panic("width", &width);
        header.set_style_or_panic("height", format!("{}px", 16.0));
        header.set_style_or_panic("border-bottom", "1px solid black");
        header.set_style_or_panic("position", "relative");
        div.append_or_panic(&header);

        let container = web::create_div();
        let container: web::HtmlElement = container.dyn_into().expect("HtmlElement");

        container.set_style_or_panic("width", width);
        container.set_style_or_panic("height", format!("{}px", height));
        container.set_attribute_or_panic("id", name);
        container.set_style_or_panic("position", "relative");

        div.append_or_panic(&container);

        Group::new(group).div.append_or_panic(&div);
        Self { div, header, container }
    }
}
