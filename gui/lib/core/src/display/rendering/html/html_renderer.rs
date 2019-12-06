use crate::prelude::*;

use crate::display::rendering::GraphicsRenderer;
use crate::display::rendering::Scene;
use crate::display::rendering::Camera;
use crate::display::rendering::html::HTMLObject;
use crate::math::utils::IntoFloat32ArrayView;
use crate::math::utils::eps;
use crate::math::utils::invert_y;
use crate::system::web::Result;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;

use nalgebra::Vector2;
use nalgebra::Matrix4;

use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;
use web_sys::HtmlElement;

mod js {
    use super::*;
    #[wasm_bindgen(module = "/js/html_renderer.js")]
    extern "C" {
        pub fn set_object_transform(dom: &JsValue, matrix_array: &JsValue);
        pub fn setup_perspective(dom: &JsValue, znear: &JsValue);
        pub fn setup_camera_transform
        ( dom          : &JsValue
        , znear        : &JsValue
        , half_width   : &JsValue
        , half_height  : &JsValue
        , matrix_array : &JsValue
        );
    }
}

fn set_object_transform(dom: &JsValue, matrix: &Matrix4<f32>) {
    // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array =  matrix.into_float32_array_view();
        js::set_object_transform(&dom, &matrix_array);
    }
}

fn setup_camera_transform
( dom         : &JsValue
, near        : f32
, half_width  : f32
, half_height : f32
, matrix      : &Matrix4<f32>
) {

    // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array = matrix.into_float32_array_view();
        js::setup_camera_transform(
            &dom,
            &near.into(),
            &half_width.into(),
            &half_height.into(),
            &matrix_array
        )
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
        Self { div, camera }
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

        div   .set_property_or_panic("width"          , "100%");
        div   .set_property_or_panic("height"         , "100%");
        camera.set_property_or_panic("width"          , "100%");
        camera.set_property_or_panic("height"         , "100%");
        camera.set_property_or_panic("transform-style", "preserve-3d");

        renderer.container.dom.append_or_panic(&div);
        div                   .append_or_panic(&camera);

        let data       = Rc::new(HTMLRendererData::new(div, camera));
        let mut htmlrenderer = Self { renderer, data };

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
        let trans_cam    = camera.transform.to_homogeneous().try_inverse();
        let trans_cam    = trans_cam.expect("Camera's matrix is not invertible.");
        let trans_cam    = trans_cam.map(eps);
        let trans_cam    = invert_y(trans_cam);

        // Note [znear from projection matrix]
        let half_dim     = self.renderer.container.dimensions() / 2.0;
        let y_scale      = camera.get_y_scale();
        let near         = y_scale * half_dim.y;

        js::setup_perspective(&self.data.div, &near.into());
        setup_camera_transform(
            &self.data.camera,
            near,
            half_dim.x,
            half_dim.y,
            &trans_cam
        );

        let scene : &Scene<HTMLObject> = &scene;
        for object in &mut scene.into_iter() {
            let mut transform = object.transform.to_homogeneous();
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

// Note [znear from projection matrix]
// ===================================
// https://github.com/mrdoob/three.js/blob/22ed6755399fa180ede84bf18ff6cea0ad66f6c0/examples/js/renderers/CSS3DRenderer.js#L275
