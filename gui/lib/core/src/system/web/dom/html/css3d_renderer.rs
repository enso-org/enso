//! This module contains the Css3dRenderer, a struct used to render CSS3D elements.

use crate::prelude::*;

use crate::display::object::DisplayObjectData;
use crate::display::camera::Camera2d;
use crate::display::camera::camera2d::Projection;
use crate::system::web::dom::html::{Css3dObject, Css3dSystem};
use crate::system::gpu::data::JsBufferView;
use crate::system::web::Result;
use crate::system::web::create_element;
use crate::system::web::dyn_into;
use crate::system::web::NodeInserter;
use crate::system::web::NodeRemover;
use crate::system::web::StyleSetter;
use crate::system::web::dom::DomContainer;
use crate::system::web::dom::ResizeCallback;
use crate::system::web::get_element_by_id;
use super::css3d_object::Css3dOrder;

use nalgebra::Vector2;
use nalgebra::Matrix4;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;
use web_sys::HtmlElement;
use js_sys::Object;


// ===================
// === Js Bindings ===
// ===================

mod js {
    use super::*;
    #[wasm_bindgen(inline_js = "
        function arr_to_css_matrix3d(a) {
            return 'matrix3d(' + a.join(',') + ')'
        }

        export function set_object_transform(dom, matrix_array) {
            let css = arr_to_css_matrix3d(matrix_array);
            dom.style.transform = 'translate(-50%, -50%)' + css;
        }

        export function setup_perspective(dom, perspective) {
            dom.style.perspective = perspective + 'px';
        }

        export function setup_camera_orthographic(dom, matrix_array) {
            dom.style.transform = arr_to_css_matrix3d(matrix_array);
        }

        export function setup_camera_perspective
        (dom, near, matrix_array) {
            let translateZ  = 'translateZ(' + near + 'px)';
            let matrix3d    = arr_to_css_matrix3d(matrix_array);
            let transform   = translateZ + matrix3d;
            dom.style.transform = transform;
        }
    ")]
    extern "C" {
        /// Setup perspective CSS 3D projection on DOM.
        #[allow(unsafe_code)]
        pub fn setup_perspective(dom: &JsValue, znear: &JsValue);

        /// Setup Camera orthographic projection on DOM.
        #[allow(unsafe_code)]
        pub fn setup_camera_orthographic(dom:&JsValue, matrix_array:&JsValue);

        /// Setup Camera perspective projection on DOM.
        #[allow(unsafe_code)]
        pub fn setup_camera_perspective(dom:&JsValue, near:&JsValue, matrix_array:&JsValue);

        /// Sets object's CSS 3D transform.
        #[allow(unsafe_code)]
        pub fn set_object_transform(dom:&JsValue, matrix_array:&Object);
    }
}

#[allow(unsafe_code)]
fn set_object_transform(dom:&JsValue, matrix:&Matrix4<f32>) {
    // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array = matrix.js_buffer_view();
        js::set_object_transform(&dom,&matrix_array);
    }
}


#[allow(unsafe_code)]
fn setup_camera_perspective(dom:&JsValue, near:f32, matrix:&Matrix4<f32>) {
    // Views to WASM memory are only valid as long the backing buffer isn't
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

#[allow(unsafe_code)]
fn setup_camera_orthographic(dom:&JsValue, matrix:&Matrix4<f32>) {
    // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array = matrix.js_buffer_view();
        js::setup_camera_orthographic(&dom, &matrix_array)
    }
}



// =============
// === Utils ===
// =============

/// Inverts Matrix Y coordinates. It's equivalent to scaling by (1.0, -1.0, 1.0).
pub fn invert_y(mut m: Matrix4<f32>) -> Matrix4<f32> {
    // Negating the second column to invert Y.
    m.row_part_mut(1, 4).iter_mut().for_each(|a| *a = -*a);
    m
}



// =========================
// === Css3dRendererData ===
// =========================

#[derive(Debug)]
struct Css3dRendererData {
    pub front_dom                 : HtmlElement,
    pub back_dom                  : HtmlElement,
    pub front_dom_view_projection : HtmlElement,
    pub back_dom_view_projection  : HtmlElement,
    logger                        : Logger
}

impl Css3dRendererData {
    pub fn new
    ( front_dom                 : HtmlElement
    , back_dom                  : HtmlElement
    , front_dom_view_projection : HtmlElement
    , back_dom_view_projection  : HtmlElement
    , logger                    : Logger) -> Self {
        Self {logger,front_dom,back_dom, front_dom_view_projection, back_dom_view_projection }
    }

    fn set_dimensions(&self, dimensions:Vector2<f32>) {
        let width  = format!("{}px", dimensions.x);
        let height = format!("{}px", dimensions.y);
        let doms   = vec![&self.front_dom, &self.back_dom, &self.front_dom_view_projection, &self.back_dom_view_projection];
        for dom in doms {
            dom.set_style_or_warn("width" , &width, &self.logger);
            dom.set_style_or_warn("height", &height, &self.logger);
        }
    }
}



// =====================
// === Css3dRenderer ===
// =====================

/// `Css3dRenderer` is a renderer for `Css3dObject`s. It integrates with other rendering contexts,
/// such as WebGL, by placing two HtmlElements in front and behind of the Canvas element,
/// allowing the move `Css3dObject`s between these two layers, mimicking z-index ordering.
///
/// To make use of its functionalities, the API user can create a `Css3dSystem` by using
/// the `Css3dRenderer::new_system` method which creates and manages instances of
/// `Css3dObject`s.
#[derive(Clone,Debug)]
pub struct Css3dRenderer {
    container : DomContainer,
    data      : Rc<Css3dRendererData>
}

impl Css3dRenderer {
    /// Creates a Css3dRenderer inside an element.
    pub fn from_element_or_panic(logger:&Logger, element:HtmlElement) -> Self {
        let logger       = logger.sub("Css3dRenderer");
        let container    = DomContainer::from_element(element);
        let front_dom    = create_div();
        let back_dom     = create_div();
        let front_camera_dom = create_div();
        let back_camera_dom = create_div();

        front_dom.set_style_or_warn("position","absolute",&logger);
        front_dom.set_style_or_warn("top","0px",&logger);
        front_dom.set_style_or_warn("overflow","hidden",&logger);
        front_dom.set_style_or_warn("overflow","hidden",&logger);
        front_dom.set_style_or_warn("width","100%",&logger);
        front_dom.set_style_or_warn("height","100%",&logger);
        front_dom.set_style_or_warn("pointer-events","none",&logger);
        back_dom.set_style_or_warn("position","absolute",&logger);
        back_dom.set_style_or_warn("top","0px",&logger);
        back_dom.set_style_or_warn("overflow","hidden",&logger);
        back_dom.set_style_or_warn("overflow","hidden",&logger);
        back_dom.set_style_or_warn("width","100%",&logger);
        back_dom.set_style_or_warn("height","100%",&logger);
        back_dom.set_style_or_warn("pointer-events","none",&logger);
        back_dom.set_style_or_warn("z-index","-1",&logger);
        front_camera_dom.set_style_or_warn("width", "100%", &logger);
        front_camera_dom.set_style_or_warn("height", "100%", &logger);
        front_camera_dom.set_style_or_warn("transform-style", "preserve-3d", &logger);
        back_camera_dom.set_style_or_warn("width", "100%", &logger);
        back_camera_dom.set_style_or_warn("height", "100%", &logger);
        back_camera_dom.set_style_or_warn("transform-style", "preserve-3d", &logger);

        container.dom.append_or_warn(&front_dom,&logger);
        container.dom.append_or_warn(&back_dom,&logger);
        front_dom.append_or_warn(&front_camera_dom, &logger);
        back_dom.append_or_warn(&back_camera_dom, &logger);

        let data = Css3dRendererData::new(
            front_dom,
            back_dom,
            front_camera_dom,
            back_camera_dom,
            logger);
        let data = Rc::new(data);
        Self{container,data}.init()
    }

    /// Creates a Css3dRenderer.
    pub fn new(logger:&Logger, dom_id:&str) -> Result<Self> {
        Ok(Self::from_element_or_panic(logger,dyn_into(get_element_by_id(dom_id)?)?))
    }

    pub(super) fn new_system(&self) -> Css3dSystem {
        let css3d_renderer = self.clone();
        let logger         = self.data.logger.sub("Css3dSystem");
        let display_object = DisplayObjectData::new(&logger);
        Css3dSystem {display_object,css3d_renderer,logger}
    }

    fn init(mut self) -> Self {
        let dimensions = self.dimensions();
        self.set_dimensions(dimensions);
        let data = self.data.clone();
        self.add_resize_callback(move |dimensions:&Vector2<f32>| {
            data.set_dimensions(*dimensions);
        });
        self
    }

    /// Creates a new instance of Css3dObject and adds it to parent.
    pub(super) fn new_instance<S:Str>
    (&self, dom_name:S, parent:DisplayObjectData) -> Result<Css3dObject> {
        let front_camera = self.data.front_dom_view_projection.clone();
        let back_camera  = self.data.back_dom_view_projection.clone();
        let logger       = self.data.logger.sub("object");
        let object       = Css3dObject::new(logger,dom_name);
        object.as_ref().map(|object| {
            parent.add_child(object);
            let display_object : DisplayObjectData = object.into();
            display_object.set_on_render(enclose!((object,display_object) move || {
                let object_dom    = object.dom();
                let mut transform = display_object.matrix();
                transform.iter_mut().for_each(|a| *a = eps(*a));

                let camera_node = match object.css3d_order() {
                    Css3dOrder::Front => &front_camera,
                    Css3dOrder::Back  => &back_camera
                };

                let parent_node = object.dom().parent_node();
                if !camera_node.is_same_node(parent_node.as_ref()) {
                    display_object.with_logger(|logger| {
                        object_dom.remove_from_parent_or_warn(logger);
                        camera_node.append_or_warn(&object_dom,logger);
                    });
                }

                set_object_transform(&object_dom, &transform);
            }));
        }).ok();
        object
    }

    /// Renders `Camera`'s point of view.
    pub fn render(&self, camera:&Camera2d) {
        let trans_cam  = camera.transform().matrix().try_inverse();
        let trans_cam  = trans_cam.expect("Camera's matrix is not invertible.");
        let trans_cam  = trans_cam.map(eps);
        let trans_cam  = invert_y(trans_cam);
        let half_dim   = self.container.dimensions() / 2.0;
        let fovy_slope = camera.half_fovy_slope();
        let near       = half_dim.y / fovy_slope;

        match camera.projection() {
            Projection::Perspective{..} => {
                js::setup_perspective(&self.data.front_dom, &near.into());
                js::setup_perspective(&self.data.back_dom, &near.into());
                setup_camera_perspective(&self.data.front_dom_view_projection, near, &trans_cam);
                setup_camera_perspective(&self.data.back_dom_view_projection, near, &trans_cam);
            },
            Projection::Orthographic => {
                setup_camera_orthographic(&self.data.front_dom_view_projection, &trans_cam);
                setup_camera_orthographic(&self.data.back_dom_view_projection, &trans_cam);
            }
        }
    }

    /// Adds a ResizeCallback.
    pub fn add_resize_callback<T:ResizeCallback>(&mut self, callback:T) {
        self.container.add_resize_callback(callback);
    }

    /// Sets Css3dRenderer's container dimensions.
    pub fn set_dimensions(&mut self, dimensions:Vector2<f32>) {
        self.data.set_dimensions(dimensions);
        self.container.set_dimensions(dimensions);
    }
}


// === Getters ===

impl Css3dRenderer {
    /// Gets Css3dRenderer's container.
    pub fn container(&self) -> &DomContainer {
        &self.container
    }

    /// Gets Css3dRenderer's DOM.
    pub fn dom(&self) -> &HtmlElement {
        &self.data.front_dom
    }

    /// Gets the Css3dRenderer's dimensions.
    pub fn dimensions(&self) -> Vector2<f32> {
        self.container.dimensions()
    }
}



// =============
// === Utils ===
// =============

fn create_div() -> HtmlElement {
    let element = create_element("div").expect("Couldn't create element");
    dyn_into(element).expect("Couldn't cast to HtmlElement")
}

/// eps is used to round very small values to 0.0 for numerical stability
pub fn eps(value: f32) -> f32 {
    if value.abs() < 1e-10 { 0.0 } else { value }
}
