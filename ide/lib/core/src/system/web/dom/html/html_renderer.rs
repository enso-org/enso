//! This module contains the HTMLRenderer, a struct used to render CSS3D elements.

use crate::prelude::*;

use crate::display::camera::Camera2d;
use crate::display::camera::camera2d::Projection;
use crate::system::web::dom::html::HtmlScene;
use crate::system::gpu::data::JsBufferView;
use crate::system::web::Result;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;
use crate::system::web::dom::DomContainer;
use crate::system::web::dom::ResizeCallback;

use js_sys::Object;
use nalgebra::Vector2;
use nalgebra::Matrix4;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;
use web_sys::HtmlElement;



// ===================
// === Js Bindings ===
// ===================

mod js {
    use super::*;
    #[wasm_bindgen(module = "/src/system/web/dom/html/snippets.js")]
    extern "C" {
        pub fn set_object_transform(dom:&JsValue, matrix_array:&Object);
        pub fn setup_perspective(dom: &JsValue, znear: &JsValue);
        pub fn setup_camera_orthographic(dom:&JsValue, matrix_array:&JsValue);
        pub fn setup_camera_perspective
        ( dom          : &JsValue
        , near         : &JsValue
        , matrix_array : &JsValue
        );
    }
}

/// eps is used to round very small values to 0.0 for numerical stability
pub fn eps(value: f32) -> f32 {
    if value.abs() < 1e-10 { 0.0 } else { value }
}

/// Inverts Matrix Y coordinates.
/// It's equivalent to scaling by (1.0, -1.0, 1.0).
pub fn invert_y(mut m: Matrix4<f32>) -> Matrix4<f32> {
    // Negating the second column to invert Y.
    m.row_part_mut(1, 4).iter_mut().for_each(|a| *a = -*a);
    m
}

fn set_object_transform(dom: &JsValue, matrix: &Matrix4<f32>) {
    // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array =  matrix.js_buffer_view();
        js::set_object_transform(&dom, &matrix_array);
    }
}

fn setup_camera_perspective
(dom:&JsValue, near:f32, matrix:&Matrix4<f32>) { // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array = matrix.js_buffer_view();
        js::setup_camera_perspective(
            &dom,
            &near.into(),
            &matrix_array
        )
    }
}

fn setup_camera_orthographic(dom:&JsValue, matrix:&Matrix4<f32>) {
    // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array = matrix.js_buffer_view();
        js::setup_camera_orthographic(&dom, &matrix_array)
    }
}



// ========================
// === HTMLRendererData ===
// ========================

#[derive(Debug)]
pub struct HTMLRendererData {
    pub dom    : HtmlElement,
    pub camera : HtmlElement
}

impl HTMLRendererData {
    pub fn new(dom:HtmlElement, camera:HtmlElement) -> Self {
        Self {dom,camera}
    }

    pub fn set_dimensions(&self, dimensions : Vector2<f32>) {
        let width  = format!("{}px", dimensions.x);
        let height = format!("{}px", dimensions.y);
        self.dom.set_property_or_panic("width", &width);
        self.dom.set_property_or_panic("height", &height);
        self.camera.set_property_or_panic("width" , &width);
        self.camera.set_property_or_panic("height", &height);
    }
}

// ====================
// === HTMLRenderer ===
// ====================

/// A renderer for `HTMLObject`s.
#[derive(Debug)]
pub struct HtmlRenderer {
    container : DomContainer,
    data      : Rc<HTMLRendererData>
}

impl HtmlRenderer {
    /// Creates a HTMLRenderer.
    pub fn new(dom_id: &str) -> Result<Self> {
        let container            = DomContainer::from_id(dom_id)?;
        let dom: HtmlElement     = dyn_into(create_element("div")?)?;
        let camera : HtmlElement = dyn_into(create_element("div")?)?;

        dom.set_property_or_panic("position", "absolute");
        dom.set_property_or_panic("top"     , "0px");
        dom.set_property_or_panic("overflow", "hidden");
        dom.set_property_or_panic("overflow", "hidden");
        dom.set_property_or_panic("width"   , "100%");
        dom.set_property_or_panic("height"  , "100%");
        camera.set_property_or_panic("width"          , "100%");
        camera.set_property_or_panic("height"         , "100%");
        camera.set_property_or_panic("transform-style", "preserve-3d");

        container.dom.append_or_panic(&dom);
        dom.append_or_panic(&camera);

        let data             = Rc::new(HTMLRendererData::new(dom,camera));
        let mut htmlrenderer = Self {container,data};

        htmlrenderer.init_listeners();
        Ok(htmlrenderer)
    }

    fn init_listeners(&mut self) {
        let dimensions = self.dimensions();
        let data       = self.data.clone();
        self.add_resize_callback(move |dimensions:&Vector2<f32>| {
            data.set_dimensions(*dimensions);
        });
        self.set_dimensions(dimensions);
    }

    /// Renders the `Scene` from `Camera`'s point of view.
    pub fn render(&self, camera: &mut Camera2d, scene: &HtmlScene) {
        camera.update();
        let trans_cam  = camera.transform().matrix().try_inverse();
        let trans_cam  = trans_cam.expect("Camera's matrix is not invertible.");
        let trans_cam  = trans_cam.map(eps);
        let trans_cam  = invert_y(trans_cam);
        let half_dim   = self.container.dimensions() / 2.0;
        let fovy_slope = camera.half_fovy_slope();
        let near       = half_dim.y / fovy_slope;

        match camera.projection() {
            Projection::Perspective{..} => {
                js::setup_perspective(&self.data.dom, &near.into());
                setup_camera_perspective(
                    &self.data.camera,
                    near,
                    &trans_cam
                );
            },
            Projection::Orthographic => {
                setup_camera_orthographic(&self.data.camera, &trans_cam);
            }
        }

        let scene : &HtmlScene = &scene;
        for object in &mut scene.into_iter() {
            object.update();
            let mut transform = object.matrix();
            transform.iter_mut().for_each(|a| *a = eps(*a));

            let parent_node  = object.dom.parent_node();
            if !self.data.camera.is_same_node(parent_node.as_ref()) {
                self.data.camera.append_or_panic(&object.dom);
            }

            set_object_transform(&object.dom, &transform);
        }
    }

    /// Adds a ResizeCallback.
    pub fn add_resize_callback<T:ResizeCallback>(&mut self, callback : T) {
        self.container.add_resize_callback(callback);
    }

    /// Sets HTMLRenderer's container dimensions.
    pub fn set_dimensions(&mut self, dimensions : Vector2<f32>) {
        self.container.set_dimensions(dimensions);
        self.data.set_dimensions(dimensions);
    }
}


// === Getters ===

impl HtmlRenderer {
    /// Gets HTMLRenderer's container.
    pub fn container(&self) -> &DomContainer {
        &self.container
    }

    /// Gets HTMLRenderer's DOM.
    pub fn dom(&self) -> &HtmlElement {
        &self.data.dom
    }

    /// Gets the Scene Renderer's dimensions.
    pub fn dimensions(&self) -> Vector2<f32> {
        self.container.dimensions()
    }
}
