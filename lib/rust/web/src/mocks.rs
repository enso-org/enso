use crate::prelude::*;

use crate::AttributeSetter;
use crate::NodeInserter;
use crate::NodeRemover;
use crate::Result;
use crate::StyleSetter;
use enso_logger::WarningLogger as Logger;



// ===================
// === EventTarget ===
// ===================

#[derive(Clone, Debug)]
pub struct EventTarget {}

impl EventTarget {
    pub fn new() -> Self {
        Self {}
    }

    pub fn add_event_listener_with_callback(&self, s: &str, _callback: &JsValue) -> Result<()> {
        Ok(())
    }

    pub fn add_event_listener_with_callback_and_bool(
        &self,
        s: &str,
        _callback: &JsValue,
        b: bool,
    ) -> Result<()> {
        Ok(())
    }

    pub fn remove_event_listener_with_callback(&self, s: &str, _callback: &JsValue) -> Result<()> {
        Ok(())
    }

    pub fn add_event_listener_with_callback_and_add_event_listener_options<U>(
        &self,
        s: &str,
        _callback: &JsValue,
        options: U,
    ) -> Result<()> {
        Ok(())
    }
}

impl From<HtmlElement> for EventTarget {
    fn from(_: HtmlElement) -> Self {
        Self::new()
    }
}

impl From<HtmlDivElement> for EventTarget {
    fn from(_: HtmlDivElement) -> Self {
        Self::new()
    }
}

// ============
// === Node ===
// ============

#[derive(Clone, Debug)]
pub struct Node {
    event_target: EventTarget,
}

impl Node {
    pub fn new() -> Self {
        Self { event_target: EventTarget::new() }
    }
}

impl Deref for Node {
    type Target = EventTarget;

    fn deref(&self) -> &Self::Target {
        &self.event_target
    }
}

impl NodeInserter for Node {
    fn append_or_panic(&self, node: &Node) {}

    fn append_or_warn(&self, node: &Node, logger: &Logger) {}

    fn prepend_or_panic(&self, node: &Node) {}

    fn prepend_or_warn(&self, node: &Node, logger: &Logger) {}

    fn insert_before_or_panic(&self, node: &Node, ref_node: &Node) {}

    fn insert_before_or_warn(&self, node: &Node, ref_node: &Node, logger: &Logger) {}
}

impl NodeRemover for Node {
    fn remove_from_parent_or_panic(&self) {}

    fn remove_from_parent_or_warn(&self, logger: &Logger) {}

    fn remove_child_or_panic(&self, node: &Node) {}

    fn remove_child_or_warn(&self, node: &Node, logger: &Logger) {}
}



// ===============
// === Element ===
// ===============

#[derive(Clone, Debug)]
pub struct Element {
    node:     Node,
    js_value: JsValue,
}

impl Element {
    pub fn new() -> Self {
        Self { node: Node::new(), js_value: 0.into() }
    }

    pub fn set_class_name(&self, value: &str) {}

    pub fn children(&self) -> HtmlCollection {
        HtmlCollection::new()
    }

    pub fn get_bounding_client_rect(&self) -> DomRect {
        DomRect::new()
    }

    pub fn set_inner_html(&self, text: &str) {}

    // TODO: better abstraction for JsCast?
    pub fn dyn_into<T: From<Self>>(self) -> Result<T> {
        Ok(T::from(self))
    }
}

impl Deref for Element {
    type Target = Node;
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl AsRef<JsValue> for Element {
    fn as_ref(&self) -> &JsValue {
        &self.js_value
    }
}

impl AttributeSetter for Element {
    fn set_attribute_or_panic<T: Str, U: Str>(&self, name: T, value: U) {}

    fn set_attribute_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &Logger) {}
}



// ===================
// === HtmlElement ===
// ===================

#[derive(Clone, Debug)]
pub struct HtmlElement {
    element: Element,
}

impl From<Element> for HtmlElement {
    fn from(element: Element) -> HtmlElement {
        HtmlElement { element }
    }
}

impl HtmlElement {
    pub fn new() -> Self {
        Self { element: Element::new() }
    }
}

impl AsRef<JsValue> for HtmlElement {
    fn as_ref(&self) -> &JsValue {
        self.element.as_ref()
    }
}

impl Deref for HtmlElement {
    type Target = Element;
    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl StyleSetter for HtmlElement {
    fn set_style_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &WarningLogger) {}

    fn set_style_or_panic<T: Str, U: Str>(&self, name: T, value: U) {}
}

// ======================
// === HtmlDivElement ===
// ======================

#[derive(Clone, Debug)]
pub struct HtmlDivElement {
    element: HtmlElement,
}

impl Deref for HtmlDivElement {
    type Target = HtmlElement;
    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl AsRef<JsValue> for HtmlDivElement {
    fn as_ref(&self) -> &JsValue {
        self.element.as_ref()
    }
}

impl From<HtmlDivElement> for HtmlElement {
    fn from(element: HtmlDivElement) -> Self {
        Self::new()
    }
}

impl HtmlDivElement {
    pub fn new() -> Self {
        Self { element: HtmlElement::new() }
    }
}



// =========================
// === HtmlCanvasElement ===
// =========================

#[derive(Clone, Debug)]
pub struct HtmlCanvasElement {
    element: HtmlElement,
}

impl Deref for HtmlCanvasElement {
    type Target = HtmlElement;
    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl HtmlCanvasElement {
    pub fn new() -> Self {
        Self { element: HtmlElement::new() }
    }
}



// ==============
// === Window ===
// ==============

#[derive(Clone, Debug)]
pub struct Window {
    event_target: EventTarget,
}

impl Window {
    pub fn new() -> Self {
        Self { event_target: EventTarget::new() }
    }
    pub fn device_pixel_ratio(&self) -> f64 {
        1.0
    }
    pub fn open_with_url_and_target(&self, url: &str, target: &str) -> Result<()> {
        Ok(())
    }
}

impl Deref for Window {
    type Target = EventTarget;
    fn deref(&self) -> &Self::Target {
        &self.event_target
    }
}



// ==============
// === Others ===
// ==============

// === Document ===

#[derive(Clone, Debug)]
pub struct Document {}
impl Document {
    pub fn new() -> Self {
        Self {}
    }
}

// === Performance ===

#[derive(Clone, Debug)]
pub struct Performance {}
impl Performance {
    pub fn new() -> Self {
        Self {}
    }
    pub fn now(&self) -> f64 {
        0.0
    }
}


// === WebGl2RenderingContext ===

#[derive(Clone, Debug)]
pub struct WebGl2RenderingContext {}
impl WebGl2RenderingContext {
    pub fn new() -> Self {
        Self {}
    }
}

// === HtmlCollection ===

#[derive(Clone, Debug)]
pub struct HtmlCollection {}
impl HtmlCollection {
    pub fn new() -> Self {
        Self {}
    }
    pub fn length(&self) -> u32 {
        0
    }
}

// === DomRect ===

#[derive(Clone, Debug)]
pub struct DomRect {}
impl DomRect {
    pub fn new() -> Self {
        Self {}
    }
    pub fn x(&self) -> f64 {
        0.0
    }
    pub fn y(&self) -> f64 {
        0.0
    }
    pub fn width(&self) -> f64 {
        0.0
    }
    pub fn height(&self) -> f64 {
        0.0
    }
}
