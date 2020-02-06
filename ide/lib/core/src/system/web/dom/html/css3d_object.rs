//! This module contains the implementation of Css3dObject, a struct used to represent CSS3D
//! elements.

use crate::prelude::*;

use crate::display::object::DisplayObjectData;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::Result;
use crate::system::web::Error;
use crate::system::web::StyleSetter;
use crate::system::web::NodeRemover;

use nalgebra::Vector2;
use nalgebra::Vector3;
use web_sys::HtmlElement;



// ==================
// === Css3dOrder ===
// ==================

/// This enumeration is used for moving the object from front to back of the surrounding HtmlElement
/// (usually a WebGL canvas).
#[allow(missing_docs)]
#[derive(Debug,Clone,Copy)]
pub enum Css3dOrder {
    Front,
    Back
}

impl Default for Css3dOrder {
    fn default() -> Self {
        Css3dOrder::Front
    }
}



// =============================
// === Css3dObjectProperties ===
// =============================

#[derive(Debug)]
struct Css3dObjectProperties {
    display_object : DisplayObjectData,
    dom            : HtmlElement,
    dimensions     : Vector2<f32>,
    css3d_order    : Css3dOrder
}

impl Drop for Css3dObjectProperties {
    fn drop(&mut self) {
        self.display_object.with_logger(|logger| {
            self.dom.remove_from_parent_or_warn(logger);
        });
        self.display_object.unset_parent();
    }
}



// =======================
// === Css3dObjectData ===
// =======================

#[derive(Clone,Debug)]
pub(super) struct Css3dObjectData {
    properties : Rc<RefCell<Css3dObjectProperties>>
}

impl Css3dObjectData {
    fn new
    ( display_object : DisplayObjectData
    , dom            : HtmlElement
    , dimensions     : Vector2<f32>
    , css3d_order    : Css3dOrder) -> Self {
        let properties = Css3dObjectProperties {display_object,dom,dimensions,css3d_order};
        let properties = Rc::new(RefCell::new(properties));
        Self {properties}
    }

    fn set_css3d_order(&self, css3d_order: Css3dOrder) {
        self.properties.borrow_mut().css3d_order = css3d_order
    }

    fn css3d_order(&self) -> Css3dOrder {
        self.properties.borrow().css3d_order
    }

    fn position(&self) -> Vector3<f32> {
        self.properties.borrow().display_object.position()
    }

    fn set_dimensions(&self, dimensions:Vector2<f32>) {
        let mut properties = self.properties.borrow_mut();
        properties.dimensions = dimensions;
        properties.display_object.with_logger(|logger| {
            properties.dom.set_style_or_warn("width",  format!("{}px", dimensions.x), logger);
            properties.dom.set_style_or_warn("height", format!("{}px", dimensions.y), logger);
        });
    }

    fn dimensions(&self) -> Vector2<f32> {
        self.properties.borrow().dimensions
    }

    fn dom(&self) -> HtmlElement {
        self.properties.borrow().dom.clone()
    }

    fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        let mut position = self.position();
        f(&mut position);
        self.properties.borrow().display_object.set_position(position);
    }

    fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.properties.borrow().display_object.mod_scale(f);
    }
}



// ===================
// === Css3dObject ===
// ===================

/// A structure for representing a HtmlElement in the 3d world.
#[derive(Debug,Clone)]
pub struct Css3dObject {
    pub(super) data : Css3dObjectData
}

impl Css3dObject {
    /// Creates a Css3dObject from element name.
    pub(super) fn new<L:Into<Logger>,S:Str>(logger:L, dom_name:S) -> Result<Self> {
        let dom = dyn_into(create_element(dom_name.as_ref())?)?;
        Ok(Self::from_element(logger,dom))
    }

    /// Creates a Css3dObject from a web_sys::HtmlElement.
    pub(super) fn from_element<L:Into<Logger>>(logger:L, element:HtmlElement) -> Self {
        let logger = logger.into();
        element.set_style_or_warn("position", "absolute", &logger);
        element.set_style_or_warn("width"   , "0px"     , &logger);
        element.set_style_or_warn("height"  , "0px"     , &logger);
        let dom            = element;
        let display_object = DisplayObjectData::new(logger);
        let dimensions     = Vector2::new(0.0, 0.0);
        let css3d_order    = default();
        let data = Css3dObjectData::new(
            display_object,
            dom,
            dimensions,
            css3d_order
        );
        Self {data}
    }

    /// Creates a Css3dObject from a HTML string.
    pub(super) fn from_html_string<L:Into<Logger>,T:Str>(logger:L, html_string:T) -> Result<Self> {
        let element = create_element("div")?;
        element.set_inner_html(html_string.as_ref());
        match element.first_element_child() {
            Some(element) => {
                let element = dyn_into(element)?;
                Ok(Self::from_element(logger,element))
            },
            None => Err(Error::missing("valid HTML")),
        }
    }

    /// Sets Css3dOrder.
    pub fn set_css3d_order(&mut self, css3d_order: Css3dOrder) {
        self.data.set_css3d_order(css3d_order)
    }

    /// Gets Css3dOrder.
    pub fn css3d_order(&self) -> Css3dOrder {
        self.data.css3d_order()
    }

    /// Sets the underlying HtmlElement dimension.
    pub fn set_dimensions(&mut self, dimensions:Vector2<f32>) {
        self.data.set_dimensions(dimensions)
    }

    /// Gets the underlying HtmlElement dimension.
    pub fn dimensions(&self) -> Vector2<f32> {
        self.data.dimensions()
    }

    /// Gets Css3dObject's dom.
    pub fn dom(&self) -> HtmlElement {
        self.data.dom()
    }

    /// Gets object's position.
    pub fn position(&self) -> Vector3<f32> {
        self.data.position()
    }

    /// Modifies the position of the object.
    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.data.mod_position(f);
    }

    /// Modifies the scale of the object.
    pub fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.data.mod_scale(f);
    }
}

impl From<&Css3dObject> for DisplayObjectData {
    fn from(t:&Css3dObject) -> Self {
        t.data.properties.borrow().display_object.clone_ref()
    }
}
