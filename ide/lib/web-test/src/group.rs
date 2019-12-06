use crate::system::web::document;
use crate::system::web::dyn_into;
use crate::system::web::create_element;
use crate::system::web::get_element_by_id;
use crate::system::web::AttributeSetter;
use crate::system::web::StyleSetter;
use crate::system::web::NodeInserter;

use web_sys::HtmlElement;


// =============
// === Group ===
// =============

/// Helper to group test containers
pub struct Group {
    pub div : HtmlElement,
}

impl Group {
    pub fn new(name:&str) -> Self {
        let div:HtmlElement = match get_element_by_id(name) {
            // If id=name exists, we use it.
            Ok(div) => dyn_into(div).expect("div should be a HtmlElement"),
            // If it doesn't exist, we create a new element.
            Err(_) => {
                let div = create_element("div");
                let div = div.expect("TestGroup failed to create div");
                let div = dyn_into::<_, HtmlElement>(div).expect("HtmlElement");

                div.set_attribute_or_panic("id"           , name);
                div.set_property_or_panic ("display"      , "flex");
                div.set_property_or_panic ("flex-wrap"    , "wrap");
                div.set_property_or_panic ("border"       , "1px solid black");
                div.set_property_or_panic ("margin-bottom", "10px");

                let header = create_element("center");
                let header = header.expect("TestGroup failed to create header");
                let header = dyn_into::<_, HtmlElement>(header);
                let header = header.expect("HtmlElement");
                let border = "1px solid black";

                header.set_inner_html(name);
                header.set_property_or_panic("border-bottom", border);
                header.set_property_or_panic("width"        , "100%");
                div.append_or_panic(&header);

                let document = document().expect("Document is not present");
                let body     = document.body().expect("Body is not present");
                body.append_or_panic(&div);
                div
            },
        };
        Self { div }
    }
}
