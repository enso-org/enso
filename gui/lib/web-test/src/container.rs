use super::Group;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::AttributeSetter;
use crate::system::web::StyleSetter;
use crate::system::web::NodeInserter;

use web_sys::HtmlElement;


// =================
// === Container ===
// =================

/// A container to hold tests in `wasm-pack test`.
pub struct Container {
    pub div       : HtmlElement,
    pub header    : HtmlElement,
    pub container : HtmlElement
}

impl Container {
    /// Creates an identificable container with provided dimensions.
    pub fn new(group:&str, name:&str, width:f32, height:f32) -> Self {
        let div    = create_element("div").expect("div");
        let div    = dyn_into::<_, HtmlElement>(div).expect("HtmlElement");
        let width  = format!("{}px", width);
        let header = create_element("center").expect("div");
        let header = dyn_into::<_, HtmlElement>(header).expect("HtmlElement");

        div.set_property_or_panic("width"   , &width);
        div.set_property_or_panic("height"  , format!("{}px", height + 17.0));
        div.set_property_or_panic("border"  , "1px solid black");
        div.set_property_or_panic("position", "relative");
        div.set_property_or_panic("margin"  , "10px");
        header.set_inner_html(name);
        header.set_property_or_panic("width" , &width);
        header.set_property_or_panic("height", format!("{}px", 16.0));
        header.set_property_or_panic("border-bottom", "1px solid black");
        header.set_property_or_panic("position", "relative");
        div.append_or_panic(&header);

        let container               = create_element("div").expect("div");
        let container : HtmlElement = dyn_into(container).expect("HtmlElement");

        container.set_property_or_panic("width" , width);
        container.set_property_or_panic("height", format!("{}px", height));
        container.set_attribute_or_panic("id", name);
        container.set_property_or_panic("position", "relative");

        div.append_or_panic(&container);

        Group::new(group).div.append_or_panic(&div);
        Self { div, header, container }
    }
}
