use crate::system::web;
use crate::system::web::AttributeSetter;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;
use wasm_bindgen::JsCast;


// =============
// === Group ===
// =============

/// Helper to group test containers
#[derive(Clone, Debug)]
pub struct Group {
    pub div: web::HtmlDivElement,
}

impl Group {
    pub fn new(name: &str) -> Self {
        let div: web::HtmlDivElement = match web::get_element_by_id(name) {
            // If id=name exists, we use it.
            Ok(div) => div.dyn_into().expect("div should be a HtmlElement"),
            // If it doesn't exist, we create a new element.
            Err(_) => {
                let div = web::create_div();
                div.set_attribute_or_panic("id", name);
                div.set_style_or_panic("display", "flex");
                div.set_style_or_panic("flex-wrap", "wrap");
                div.set_style_or_panic("border", "1px solid black");
                div.set_style_or_panic("margin-bottom", "10px");

                let header = web::create_element("center");
                let header = header.dyn_into();
                let header: web::HtmlElement = header.expect("HtmlElement");
                let border = "1px solid black";

                header.set_inner_html(name);
                header.set_style_or_panic("border-bottom", border);
                header.set_style_or_panic("width", "100%");
                div.append_or_panic(&header);
                web::body().append_or_panic(&div);
                div
            }
        };
        Self { div }
    }
}
