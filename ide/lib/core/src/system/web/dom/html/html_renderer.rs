#![allow(missing_docs)]

use crate::prelude::*;

use crate::system::web::dom::GraphicsRenderer;
use crate::system::web::dom::Scene;
use crate::system::web::dom::Camera;
use crate::system::web::dom::CameraType;
use crate::system::web::dom::html::HTMLObject;
use crate::system::gpu::data::JsBufferView;
use crate::system::web::Result;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;

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
        , y_scale      : &JsValue
        , half_width   : &JsValue
        , half_height  : &JsValue
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
(dom:&JsValue, near:f32, half_width:f32, half_height:f32, matrix:&Matrix4<f32>) { // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array = matrix.js_buffer_view();
        js::setup_camera_perspective(
            &dom,
            &near.into(),
            &half_width.into(),
            &half_height.into(),
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
    pub div    : HtmlElement,
    pub camera : HtmlElement
}

impl HTMLRendererData {
    pub fn new(div : HtmlElement, camera : HtmlElement) -> Self {
        Self {div,camera}
    }

    pub fn set_dimensions(&self, dimensions : Vector2<f32>) {
        let width  = format!("{}px", dimensions.x);
        let height = format!("{}px", dimensions.y);
        self.div   .set_property_or_panic("width" , &width);
        self.div   .set_property_or_panic("height", &height);
        self.camera.set_property_or_panic("width" , &width);
        self.camera.set_property_or_panic("height", &height);
    }
}

// ====================
// === HTMLRenderer ===
// ====================

/// A renderer for `HTMLObject`s.
#[derive(Shrinkwrap, Debug)]
pub struct HTMLRenderer {
    #[shrinkwrap(main_field)]
    pub renderer : GraphicsRenderer,
    pub data     : Rc<HTMLRendererData>
}

impl HTMLRenderer {
    /// Creates a HTMLRenderer.
    pub fn new(dom_id: &str) -> Result<Self> {
        let renderer             = GraphicsRenderer::new(dom_id)?;
        let div    : HtmlElement = dyn_into(create_element("div")?)?;
        let camera : HtmlElement = dyn_into(create_element("div")?)?;

        div.set_property_or_panic("position", "absolute");
        div.set_property_or_panic("top", "0px");
        div.set_property_or_panic("overflow", "hidden");
        div   .set_property_or_panic("overflow"       , "hidden");
        div   .set_property_or_panic("width"          , "100%");
        div   .set_property_or_panic("height"         , "100%");
        camera.set_property_or_panic("width"          , "100%");
        camera.set_property_or_panic("height"         , "100%");
        camera.set_property_or_panic("transform-style", "preserve-3d");

        renderer.container.dom.append_or_panic(&div);
        div                   .append_or_panic(&camera);

        let data       = Rc::new(HTMLRendererData::new(div, camera));
        let mut htmlrenderer = Self {renderer,data};

        htmlrenderer.init_listeners();
        Ok(htmlrenderer)
    }

    fn init_listeners(&mut self) {
        let dimensions = self.renderer.dimensions();
        let data = self.data.clone();
        self.renderer.add_resize_callback(move |dimensions:&Vector2<f32>| {
            data.set_dimensions(*dimensions);
        });
        self.set_dimensions(dimensions);
    }

    /// Renders the `Scene` from `Camera`'s point of view.
    pub fn render(&self, camera: &mut Camera, scene: &Scene<HTMLObject>) {
        let trans_cam    = camera.transform().to_homogeneous().try_inverse();
        let trans_cam    = trans_cam.expect("Camera's matrix is not invertible.");
        let trans_cam    = trans_cam.map(eps);
        let trans_cam    = invert_y(trans_cam);

        let half_dim     = self.renderer.container.dimensions() / 2.0;
        let y_scale      = camera.get_y_scale();
        let y_scale      = y_scale * half_dim.y;

        match camera.camera_type() {
            CameraType::Perspective(_) => {
                js::setup_perspective(&self.data.div, &y_scale.into());
                setup_camera_perspective(
                    &self.data.camera,
                    y_scale,
                    half_dim.x,
                    half_dim.y,
                    &trans_cam
                );
            },
            CameraType::Orthographic(_) => {
                setup_camera_orthographic(&self.data.camera, &trans_cam);
            }
        }

        let scene : &Scene<HTMLObject> = &scene;
        for object in &mut scene.into_iter() {
            let mut transform = object.transform().to_homogeneous();
            transform.iter_mut().for_each(|a| *a = eps(*a));

            let parent_node  = object.dom.parent_node();
            if !self.data.camera.is_same_node(parent_node.as_ref()) {
                self.data.camera.append_or_panic(&object.dom);
            }

            set_object_transform(&object.dom, &transform);
        }
    }

    pub fn set_dimensions(&mut self, dimensions : Vector2<f32>) {
        self.renderer.set_dimensions(dimensions);
        self.data.set_dimensions(dimensions);
    }
}


// === Getters ===

impl HTMLRenderer {
    pub fn div(&self) -> &HtmlElement {
        &self.data.div
    }
}
