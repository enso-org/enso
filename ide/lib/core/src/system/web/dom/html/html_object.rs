//! This module contains the implementation of HTMLObject, a struct used to represent CSS3D
//! elements.

use crate::prelude::*;

use crate::display::object::DisplayObjectData;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::Result;
use crate::system::web::Error;
use crate::system::web::StyleSetter;

use nalgebra::Vector2;
use web_sys::HtmlElement;



// ==================
// === HtmlObject ===
// ==================

/// A structure for representing a 3D HTMLElement in a `HTMLScene`.
#[derive(Shrinkwrap, Debug, Clone)]
#[shrinkwrap(mutable)]
pub struct HtmlObject {
    #[shrinkwrap(main_field)]
    /// HTMLObject's hierarchical transforms.
    pub display_object : DisplayObjectData,

    /// The DOM to be rendered with CSS3D.
    pub dom            : HtmlElement,

    dimensions         : Vector2<f32>,
}

impl HtmlObject {
    /// Creates a HTMLObject from element name.
    pub fn new<L:Into<Logger>>(logger:L, dom_name:&str) -> Result<Self> {
        let dom = dyn_into(create_element(dom_name)?)?;
        Ok(Self::from_element(logger,dom))
    }

    /// Creates a HTMLObject from a web_sys::HtmlElement.
    pub fn from_element<L:Into<Logger>>(logger:L, element:HtmlElement) -> Self {
        element.set_property_or_panic("position", "absolute");
        element.set_property_or_panic("width"   , "0px");
        element.set_property_or_panic("height"  , "0px");
        let dom            = element;
        let display_object = DisplayObjectData::new(logger.into());
        let dimensions     = Vector2::new(0.0, 0.0);
        Self {display_object,dom,dimensions}
    }

    /// Creates a HTMLObject from a HTML string.
    pub fn from_html_string<L:Into<Logger>, T:AsRef<str>>(logger:L, html_string:T) -> Result<Self> {
        let element = create_element("div")?;
        element.set_inner_html(html_string.as_ref());
        match element.first_element_child() {
            Some(element) => Ok(Self::from_element(logger,dyn_into(element)?)),
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
