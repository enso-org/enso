#![allow(missing_docs)]

use crate::prelude::*;

use crate::system::web::dom::Object;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::Result;
use crate::system::web::Error;
use crate::system::web::StyleSetter;
use crate::animation::position::HasPosition;

use nalgebra::{Vector2, Vector3};
use web_sys::HtmlElement;



// ==================
// === HTMLObject ===
// ==================

/// A structure for representing a 3D HTMLElement in a `HTMLScene`.
#[derive(Shrinkwrap, Debug, Clone)]
#[shrinkwrap(mutable)]
pub struct HTMLObject {
    #[shrinkwrap(main_field)]
    pub object     : Object,
    pub dom        : HtmlElement,
    dimensions     : Vector2<f32>,
}

impl HasPosition for HTMLObject {
    fn position(&self) -> Vector3<f32> {
        self.object.position()
    }

    fn set_position(&mut self, position:Vector3<f32>) {
        self.object.set_position(position)
    }
}

impl HTMLObject {
    /// Creates a HTMLObject from element name.
    pub fn new(dom_name: &str) -> Result<Self> {
        let dom = dyn_into(create_element(dom_name)?)?;
        Ok(Self::from_element(dom))
    }

    /// Creates a HTMLObject from a web_sys::HtmlElement.
    pub fn from_element(element: HtmlElement) -> Self {
        element.set_property_or_panic("position", "absolute");
        element.set_property_or_panic("width"   , "0px");
        element.set_property_or_panic("height"  , "0px");
        let dom = element;
        let object = default();
        let dimensions = Vector2::new(0.0, 0.0);
        Self { object, dom, dimensions }
    }

    /// Creates a HTMLObject from a HTML string.
    pub fn from_html_string<T>(html_string: T) -> Result<Self>
        where T : AsRef<str> {
        let element = create_element("div")?;
        element.set_inner_html(html_string.as_ref());
        match element.first_element_child() {
            Some(element) => Ok(Self::from_element(dyn_into(element)?)),
            None          => Err(Error::missing("valid HTML")),
        }
    }

    /// Sets the underlying HtmlElement dimension.
    pub fn set_dimensions(&mut self, width: f32, height: f32) {
        self.dimensions = Vector2::new(width, height);
        self.dom.set_property_or_panic("width",  format!("{}px", width));
        self.dom.set_property_or_panic("height", format!("{}px", height));
    }

    /// Gets the underlying HtmlElement dimension.
    pub fn dimensions(&self) -> &Vector2<f32> {
        &self.dimensions
    }
}
