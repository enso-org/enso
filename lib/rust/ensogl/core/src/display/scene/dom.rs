//! This module defines a DOM management utilities.

use crate::prelude::*;

use crate::display::camera::camera2d::Projection;
use crate::display::camera::Camera2d;
use crate::display::object::traits::*;
use crate::display::symbol::dom::eps;
use crate::display::symbol::dom::inverse_y_translation;
use crate::display::symbol::DomSymbol;
use crate::system::gpu::data::JsBufferView;
use crate::system::web;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;

use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::HtmlDivElement;



// ===================
// === Js Bindings ===
// ===================

mod js {
    use super::*;
    #[wasm_bindgen(inline_js = "
        function arr_to_css_matrix3d(a) {
            return `matrix3d(${a.join(',')})`
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
            let transform   = translateZ + matrix3d + 'translate(50%,50%)';
            dom.style.transform = transform;
        }
    ")]
    extern "C" {
        /// Setup perspective CSS 3D projection on DOM.
        #[allow(unsafe_code)]
        pub fn setup_perspective(dom: &web::JsValue, znear: &web::JsValue);

        /// Setup Camera orthographic projection on DOM.
        #[allow(unsafe_code)]
        pub fn setup_camera_orthographic(dom: &web::JsValue, matrix_array: &web::JsValue);

        /// Setup Camera perspective projection on DOM.
        #[allow(unsafe_code)]
        pub fn setup_camera_perspective(
            dom: &web::JsValue,
            near: &web::JsValue,
            matrix_array: &web::JsValue,
        );
    }
}

#[allow(unsafe_code)]
fn setup_camera_perspective(dom: &web::JsValue, near: f32, matrix: &Matrix4<f32>) {
    // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array = matrix.js_buffer_view();
        js::setup_camera_perspective(dom, &near.into(), &matrix_array)
    }
}

#[allow(unsafe_code)]
fn setup_camera_orthographic(dom: &web::JsValue, matrix: &Matrix4<f32>) {
    // Views to WASM memory are only valid as long the backing buffer isn't
    // resized. Check documentation of IntoFloat32ArrayView trait for more
    // details.
    unsafe {
        let matrix_array = matrix.js_buffer_view();
        js::setup_camera_orthographic(dom, &matrix_array)
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



// ====================
// === DomSceneData ===
// ====================

/// Internal representation for `DomScene`.
#[derive(Clone, Debug)]
pub struct DomSceneData {
    /// The root dom element of this scene.
    pub dom:                 HtmlDivElement,
    /// The child div of the `dom` element with view-projection Css 3D transformations applied.
    pub view_projection_dom: HtmlDivElement,
    logger:                  Logger,
}

impl DomSceneData {
    /// Constructor.
    pub fn new(dom: HtmlDivElement, view_projection_dom: HtmlDivElement, logger: Logger) -> Self {
        Self { dom, view_projection_dom, logger }
    }
}



// ================
// === DomScene ===
// ================

/// `DomScene` is a renderer for `DomSymbol`s. It integrates with other rendering contexts,
/// such as WebGL, by placing two HtmlElements in front and behind of the Canvas element,
/// allowing the move `DomSymbol`s between these two layers, mimicking z-index ordering.
///
/// To make use of its functionalities, the API user can create a `Css3dSystem` by using
/// the `DomScene::new_system` method which creates and manages instances of
/// `DomSymbol`s.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct DomScene {
    data: Rc<DomSceneData>,
}

impl DomScene {
    /// Constructor.
    pub fn new(logger: impl AnyLogger) -> Self {
        let logger = Logger::new_sub(logger, "DomScene");
        let dom = web::create_div();
        let view_projection_dom = web::create_div();

        dom.set_class_name("dom-scene-layer");
        dom.set_style_or_warn("position", "absolute", &logger);
        dom.set_style_or_warn("top", "0px", &logger);
        dom.set_style_or_warn("overflow", "hidden", &logger);
        dom.set_style_or_warn("overflow", "hidden", &logger);
        dom.set_style_or_warn("width", "100%", &logger);
        dom.set_style_or_warn("height", "100%", &logger);
        dom.set_style_or_warn("pointer-events", "none", &logger);

        view_projection_dom.set_class_name("view_projection");
        view_projection_dom.set_style_or_warn("width", "100%", &logger);
        view_projection_dom.set_style_or_warn("height", "100%", &logger);
        view_projection_dom.set_style_or_warn("transform-style", "preserve-3d", &logger);

        dom.append_or_warn(&view_projection_dom, &logger);

        let data = DomSceneData::new(dom, view_projection_dom, logger);
        let data = Rc::new(data);
        Self { data }
    }

    /// Gets the number of children DomSymbols.
    pub fn children_number(&self) -> u32 {
        self.data.dom.children().length()
    }

    /// Sets the z-index of this DOM element.
    pub fn set_z_index(&self, z: i32) {
        self.data.dom.set_style_or_warn("z-index", z.to_string(), &self.logger);
    }

    /// Sets the CSS property `filter: grayscale({value})` on this element. A value of 0.0 displays
    /// the element normally. A value of 1.0 will make the element completely gray.
    pub fn filter_grayscale(&self, value: f32) {
        self.data.dom.set_style_or_warn("filter", format!("grayscale({})", value), &self.logger);
    }

    /// Creates a new instance of DomSymbol and adds it to parent.
    pub fn manage(&self, object: &DomSymbol) {
        let dom = object.dom();
        let data = &self.data;
        if object.is_visible() {
            self.view_projection_dom.append_or_panic(dom);
        }
        object.display_object().set_on_hide(f_!(dom.remove()));
        object.display_object().set_on_show(f__!([data,dom] {
            data.view_projection_dom.append_or_panic(&dom)
        }));
    }

    /// Update the objects to match the new camera's point of view. This function should be called
    /// only after camera position change.
    pub fn update_view_projection(&self, camera: &Camera2d) {
        if self.children_number() == 0 {
            return;
        }

        let trans_cam = camera.transform_matrix().try_inverse();
        let trans_cam = trans_cam.expect("Camera's matrix is not invertible.");
        let trans_cam = trans_cam.map(eps);
        let trans_cam = inverse_y_translation(trans_cam);
        let half_dim = camera.screen().height / 2.0;
        let fovy_slope = camera.half_fovy_slope();
        let near = half_dim / fovy_slope;

        match camera.projection() {
            Projection::Perspective { .. } => {
                js::setup_perspective(&self.data.dom, &near.into());
                setup_camera_perspective(&self.data.view_projection_dom, near, &trans_cam);
            }
            Projection::Orthographic => {
                setup_camera_orthographic(&self.data.view_projection_dom, &trans_cam);
            }
        }
    }
}
