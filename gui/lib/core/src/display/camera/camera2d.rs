#![allow(missing_docs)]

//! Camera implementation which is specialized for 2D view (it computes some additional parameters,
//! like the zoom to the canvas).

use crate::prelude::*;

use crate::data::dirty;
use crate::display::object::DisplayObjectData;
use nalgebra::{Vector3, Matrix4, Perspective3};
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::data::dirty::traits::*;



// =================
// === Alignment ===
// =================

/// Camera alignment. It describes where the origin of the camera should be aligned to.
#[derive(Clone,Debug)]
pub struct Alignment {
    pub horizontal : HorizontalAlignment,
    pub vertical   : VerticalAlignment,
}

#[derive(Clone,Debug)]
pub enum HorizontalAlignment {Left,Center,Right}

#[derive(Clone,Debug)]
pub enum VerticalAlignment {Top,Center,Bottom}

impl Default for HorizontalAlignment { fn default() -> Self { Self::Center } }
impl Default for VerticalAlignment   { fn default() -> Self { Self::Center } }
impl Default for Alignment {
    fn default() -> Self {
        let horizontal = default();
        let vertical   = default();
        Self {horizontal,vertical}
    }
}



// ==============
// === Screen ===
// ==============

/// Camera's frustum screen dimensions.
#[derive(Clone,Debug)]
pub struct Screen {
    pub width  : f32,
    pub height : f32,
}

impl Default for Screen {
    fn default() -> Self {
        let width  = 100.0;
        let height = 100.0;
        Self {width,height}
    }
}



// ==================
// === Projection ===
// ==================

/// Camera's projection type.
#[derive(Clone,Debug)]
pub enum Projection {
    Perspective {fov:f32},
    Orthographic
}

impl Default for Projection {
    fn default() -> Self {
        Self::Perspective {fov:45.0f32.to_radians()}
    }
}



// ================
// === Clipping ===
// ================

/// Camera's frustum clipping range.
#[derive(Clone,Debug)]
pub struct Clipping {
    pub near : f32,
    pub far  : f32
}

impl Default for Clipping {
    fn default() -> Self {
        let near = 0.0;
        let far  = 1000.0;
        Self {near,far}
    }
}



// ====================
// === Camera2dData ===
// ====================

/// Internal `Camera2d` representation. Please see `Camera2d` for full documentation.
#[derive(Clone,Debug)]
pub struct Camera2dData {
    pub transform          : DisplayObjectData,
    screen                 : Screen,
    zoom                   : f32,
    zoom_uniform           : Uniform<f32>,
    native_z               : f32,
    alignment              : Alignment,
    projection             : Projection,
    clipping               : Clipping,
    view_matrix            : Matrix4<f32>,
    projection_matrix      : Matrix4<f32>,
    view_projection_matrix : Matrix4<f32>,
    projection_dirty       : ProjectionDirty,
    transform_dirty        : TransformDirty2
}

type ProjectionDirty = dirty::SharedBool<()>;
type TransformDirty2 = dirty::SharedBool<()>;

impl Camera2dData {
    pub fn new(logger:Logger, globals:&UniformScope) -> Self {
        let screen                 = default();
        let projection             = default();
        let clipping               = default();
        let alignment              = default();
        let zoom                   = 1.0;
        let native_z               = 1.0;
        let view_matrix            = Matrix4::identity();
        let projection_matrix      = Matrix4::identity();
        let view_projection_matrix = Matrix4::identity();
        let projection_dirty       = ProjectionDirty::new(logger.sub("projection_dirty"),());
        let transform_dirty        = TransformDirty2::new(logger.sub("transform_dirty"),());
        let transform_dirty_copy   = transform_dirty.clone();
        let transform              = DisplayObjectData::new(logger);
        let zoom_uniform           = globals.add_or_panic("zoom",1.0);
        transform.set_on_updated(move |_| { transform_dirty_copy.set(); });
        transform.mod_position(|p| p.z = 1.0);
        projection_dirty.set();
        Self {transform,screen,projection,clipping,alignment,zoom,zoom_uniform,native_z,view_matrix
             ,projection_matrix,view_projection_matrix,projection_dirty,transform_dirty}
    }

    pub fn recompute_view_matrix(&mut self) {
        // TODO: Handle all alignments.
        let mut transform       = self.transform.matrix();
        let div                 = 2.0 * self.zoom;
        let alignment_transform = Vector3::new(self.screen.width/div, self.screen.height/div, 0.0);
        transform.append_translation_mut(&alignment_transform);
        self.view_matrix = transform.try_inverse().unwrap()
    }

    pub fn recompute_projection_matrix(&mut self) {
        self.projection_matrix = match &self.projection {
            Projection::Perspective {fov} => {
                let aspect = self.screen.width / self.screen.height;
                let near   = self.clipping.near;
                let far    = self.clipping.far;
                *Perspective3::new(aspect,*fov,near,far).as_matrix()
            }
            _ => unimplemented!()
        };
    }

    // https://github.com/rust-lang/rust-clippy/issues/4914
    #[allow(clippy::useless_let_if_seq)]
    pub fn update(&mut self) -> bool {
        self.transform.update();
        let mut changed = false;
        if self.transform_dirty.check() {
            self.recompute_view_matrix();
            self.transform_dirty.unset();
            changed = true;
        }
        if self.projection_dirty.check() {
            self.recompute_projection_matrix();
            self.projection_dirty.unset();
            changed = true;
        }
        if changed {
            self.view_projection_matrix = self.projection_matrix * self.view_matrix;
            self.zoom_uniform.set(self.zoom);
        }
        changed
    }
}


// === Getters ===

impl Camera2dData {
    pub fn zoom(&self) -> f32 {
        self.zoom
    }

    pub fn view_projection_matrix (&self) -> &Matrix4<f32> {
        &self.view_projection_matrix
    }
}


// === Setters ===

impl Camera2dData {
    pub fn projection_mut(&mut self) -> &mut Projection {
        self.projection_dirty.set();
        &mut self.projection
    }

    pub fn clipping_mut(&mut self) -> &mut Clipping {
        self.projection_dirty.set();
        &mut self.clipping
    }

    pub fn set_screen(&mut self, width:f32, height:f32) {
        self.screen.width  = width;
        self.screen.height = height;
        self.projection_dirty.set();

        match &self.projection {
            Projection::Perspective {fov} => {
                let zoom       = self.zoom;
                let alpha      = fov / 2.0;
                let native_z  = height / (2.0 * alpha.tan());
                self.native_z = native_z;
                self.mod_position_keep_zoom(|t| t.z = native_z / zoom);
            }
            _ => unimplemented!()
        };
    }
}


// === Transform Setters ===

impl Camera2dData {
    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.mod_position_keep_zoom(f);
        self.zoom = self.native_z / self.transform.position().z;
    }

    pub fn set_position(&mut self, value:Vector3<f32>) {
        self.mod_position(|p| *p = value);
    }
}


// === Private Transform Setters ===

impl Camera2dData {
    fn mod_position_keep_zoom<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.transform.mod_position(f)
    }
}



// ================
// === Camera2d ===
// ================

/// Camera definition for 2D objects.
///
/// Although this camera implementation is defined in terms of 3D transformations under the hood,
/// it has several properties which make sense only in the context of a 2D projection:
/// - The `zoom` factor which correlates to the final image zoom. When the `zoom` parameter is set
///   to `1.0`, the units correspond 1:1 to pixels on the screen.
/// - The `native_z` value describes the z-axis distance at which the `zoom` value is `1.0`.
/// - When a new screen dimensions are provided, the camera automatically recomputes the z-axis
///   position to keep the `zoom` unchanged.
/// - The `alignment` describes where the origin is placed in the camera frustum. It is used for
///   drawing elements and scaling the view. By default, the `alignment` is set to center, which
///   defines the origin center at the center of the screen. When scaling the view, objects placed
///   in the center of the view will not move visually. If you set the alignment to bottom-left
///   corner, you will get a view which behaves like a window in window-based GUIs. When scaling
///   the window, the left-bottom corner will stay in place.
#[derive(Clone,Debug)]
pub struct Camera2d {
    rc: Rc<RefCell<Camera2dData>>
}

impl Camera2d {
    /// Creates new Camera instance.
    pub fn new(logger:Logger, globals:&UniformScope) -> Self {
        let data = Camera2dData::new(logger,globals);
        let rc   = Rc::new(RefCell::new(data));
        Self {rc}
    }
}


// === Modifiers ===

impl Camera2d {
    /// Sets screen dimensions.
    pub fn set_screen(&self, width:f32, height:f32) {
        self.rc.borrow_mut().set_screen(width,height)
    }

    /// Update all diry camera parameters and compute updated view-projection matrix.
    pub fn update(&self) -> bool {
        self.rc.borrow_mut().update()
    }
}


// === Getters ===

impl Camera2d {
    pub fn zoom(&self) -> f32 {
        self.rc.borrow().zoom()
    }

    pub fn view_projection_matrix(&self) -> Matrix4<f32> {
        *self.rc.borrow().view_projection_matrix()
    }
}


// === Setters ===

impl Camera2d {
    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow_mut().mod_position(f)
    }

    pub fn set_position(&self, value:Vector3<f32>) {
        self.rc.borrow_mut().set_position(value)
    }
}
