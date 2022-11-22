//! Camera implementation which is specialized for 2D view (it computes some additional parameters,
//! like the zoom to the canvas).

use crate::control::callback::traits::*;
use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::control::callback;
use crate::data::dirty;
use crate::display;
use crate::display::scene::Scene;
use crate::frp;

use nalgebra::Perspective3;



// ==============
// === Screen ===
// ==============

/// Camera's frustum screen dimensions.
#[derive(Clone, Copy, Debug, Default)]
#[allow(missing_docs)]
pub struct Screen {
    pub width:  f32,
    pub height: f32,
}

impl Screen {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Get Screen's aspect ratio.
    pub fn aspect(self) -> f32 {
        self.width / self.height
    }

    /// Check whether the screen size is zero or negative.
    pub fn is_degenerated(self) -> bool {
        self.width < std::f32::EPSILON || self.height < std::f32::EPSILON
    }
}

impl From<Screen> for Vector2<f32> {
    fn from(screen: Screen) -> Vector2<f32> {
        Vector2(screen.width, screen.height)
    }
}



// ==================
// === Projection ===
// ==================

/// Camera's projection type.
#[derive(Clone, Copy, Debug)]
pub enum Projection {
    /// Perspective projection.
    Perspective {
        /// Field of view.
        fov: f32,
    },

    /// Orthographic projection.
    Orthographic,
}

impl Default for Projection {
    fn default() -> Self {
        let fov = 45.0f32.to_radians();
        Self::Perspective { fov }
    }
}



// ================
// === Clipping ===
// ================

/// Camera's frustum clipping range.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct Clipping {
    pub near: f32,
    pub far:  f32,
}

impl Default for Clipping {
    fn default() -> Self {
        let near = 1.0;
        //FIXME: Bigger screens needs bigger far values, which means that this value has to be
        // updated when the screen is resized.
        let far = 10000.0;
        Self { near, far }
    }
}



// =============
// === Dirty ===
// =============

/// Dirty status of camera properties.
#[derive(Clone, CloneRef, Debug)]
pub struct Dirty {
    projection: ProjectionDirty,
    transform:  TransformDirty,
}

impl Dirty {
    fn new() -> Self {
        let projection = ProjectionDirty::new(());
        let transform = TransformDirty::new(());
        Self { projection, transform }
    }
}



// ================
// === Matrixes ===
// ================

/// Camera matrix properties.
#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct Matrix {
    view:            Matrix4<f32>,
    view_inversed:   Matrix4<f32>,
    projection:      Matrix4<f32>,
    view_projection: Matrix4<f32>,
}

impl Matrix {
    fn new() -> Self {
        let view = Matrix4::identity();
        let view_inversed = Matrix4::identity();
        let projection = Matrix4::identity();
        let view_projection = Matrix4::identity();
        Self { view, view_inversed, projection, view_projection }
    }
}

impl Default for Matrix {
    fn default() -> Self {
        Self::new()
    }
}



// ====================
// === Camera2dData ===
// ====================

/// Frp outputs of the Camera2d.
#[derive(Debug, Clone, CloneRef)]
pub struct Frp {
    network:      frp::Network,
    /// Camera position.
    pub position: frp::Source<Vector3<f32>>,
    /// Camera zoom factor.
    pub zoom:     frp::Source<f32>,
    /// Camera's frustum screen dimensions.
    pub screen:   frp::Source<Screen>,
}

/// Function used to return the updated screen dimensions.
pub trait ScreenUpdateFn = Fn(Vector2<f32>) + 'static;

/// Function used to return the updated `Camera2d`'s zoom.
pub trait ZoomUpdateFn = Fn(f32) + 'static;

/// Internal `Camera2d` representation. Please see `Camera2d` for full documentation.
#[derive(Debug)]
struct Camera2dData {
    display_object:         display::object::Instance,
    screen:                 Screen,
    zoom:                   f32,
    z_zoom_1:               f32,
    projection:             Projection,
    clipping:               Clipping,
    matrix:                 Matrix,
    dirty:                  Dirty,
    zoom_update_registry:   callback::registry::Copy1<f32>,
    screen_update_registry: callback::registry::Copy1<Vector2<f32>>,
    frp:                    Frp,
}

type ProjectionDirty = dirty::SharedBool<()>;
type TransformDirty = dirty::SharedBool<()>;

impl Camera2dData {
    fn new(display_object: &display::object::Instance) -> Self {
        let screen = Screen::new();
        let projection = default();
        let clipping = default();
        let zoom = 1.0;
        let z_zoom_1 = 1.0;
        let matrix = default();
        let dirty = Dirty::new();
        let display_object = display_object.clone_ref();
        let zoom_update_registry = default();
        let screen_update_registry = default();
        display_object.set_on_updated(f_!(dirty.transform.set()));
        display_object.mod_position(|p| p.z = 1.0);
        dirty.projection.set();
        let network = frp::Network::new("Camera2d");
        frp::extend! { network
            frp_position <- source();
            frp_zoom <- source();
            frp_screen <- source();
        }
        let frp = Frp { network, position: frp_position, zoom: frp_zoom, screen: frp_screen };
        Self {
            frp,
            display_object,
            screen,
            zoom,
            z_zoom_1,
            projection,
            clipping,
            matrix,
            dirty,
            zoom_update_registry,
            screen_update_registry,
        }
        .init()
    }

    fn init(mut self) -> Self {
        self.set_screen(self.screen.width, self.screen.height);
        self
    }

    fn add_zoom_update_callback<F: ZoomUpdateFn>(&mut self, f: F) -> callback::Handle {
        self.zoom_update_registry.add(f)
    }

    fn add_screen_update_callback<F: ScreenUpdateFn>(&mut self, f: F) -> callback::Handle {
        self.screen_update_registry.add(f)
    }

    fn recompute_view_matrix(&mut self) {
        let transform = self.display_object.matrix();
        self.matrix.view_inversed = transform;
        self.matrix.view = transform.try_inverse().unwrap()
    }

    fn recompute_projection_matrix(&mut self) {
        self.matrix.projection = match &self.projection {
            Projection::Perspective { fov } => {
                let aspect = self.screen.aspect();
                let near = self.clipping.near;
                let far = self.clipping.far;
                *Perspective3::new(aspect, *fov, near, far).as_matrix()
            }
            _ => unimplemented!(),
        };
    }

    fn inversed_projection_matrix(&self) -> Matrix4<f32> {
        match &self.projection {
            Projection::Perspective { .. } =>
                Perspective3::from_matrix_unchecked(self.matrix.projection).inverse(),
            _ => unimplemented!(),
        }
    }

    fn inversed_view_projection_matrix(&self) -> Matrix4<f32> {
        self.matrix.view_inversed * self.inversed_projection_matrix()
    }

    // https://github.com/rust-lang/rust-clippy/issues/4914
    #[allow(clippy::useless_let_if_seq)]
    fn update(&mut self, scene: &Scene) -> bool {
        self.display_object.update(scene);
        let mut changed = false;
        if self.dirty.transform.check() {
            self.recompute_view_matrix();
            self.dirty.transform.unset();
            changed = true;
        }
        if self.dirty.projection.check() {
            self.recompute_projection_matrix();
            self.dirty.projection.unset();
            changed = true;
        }
        if changed {
            self.matrix.view_projection = self.matrix.projection * self.matrix.view;
            let zoom = self.zoom;
            self.zoom_update_registry.run_all(zoom);
        }
        changed
    }
}


// === Setters ===

impl Camera2dData {
    fn projection_mut(&mut self) -> &mut Projection {
        self.dirty.projection.set();
        &mut self.projection
    }

    fn clipping_mut(&mut self) -> &mut Clipping {
        self.dirty.projection.set();
        &mut self.clipping
    }

    fn set_screen(&mut self, width: f32, height: f32) {
        if self.screen.is_degenerated() {
            self.zoom = 1.0;
        }
        self.screen.width = width;
        self.screen.height = height;
        self.dirty.projection.set();

        match &self.projection {
            Projection::Perspective { fov } => {
                let zoom = self.zoom;
                let alpha = fov / 2.0;
                let z_zoom_1 = height / (2.0 * alpha.tan());
                self.z_zoom_1 = z_zoom_1;
                self.mod_position_keep_zoom(|t| t.z = z_zoom_1 / zoom);
            }
            _ => unimplemented!(),
        };
        let dimensions = Vector2::new(width, height);
        self.screen_update_registry.run_all(dimensions);
    }

    fn reset_zoom(&mut self) {
        self.zoom = 1.0;
        self.set_screen(self.screen.width, self.screen.height);
    }

    /// Check whether the screen size is zero or negative.
    fn is_degenerated(&self) -> bool {
        self.screen.is_degenerated()
    }
}


// === Transform Setters ===

impl Camera2dData {
    fn mod_position<F: FnOnce(&mut Vector3<f32>)>(&mut self, f: F) {
        self.mod_position_keep_zoom(f);
        let z = self.display_object.position().z.abs();
        self.zoom = if z < std::f32::EPSILON { std::f32::INFINITY } else { self.z_zoom_1 / z };
    }

    fn set_position(&mut self, value: Vector3<f32>) {
        self.mod_position(|p| *p = value);
    }

    fn set_rotation(&mut self, yaw: f32, pitch: f32, roll: f32) {
        self.display_object.mod_rotation(|r| *r = Vector3::new(yaw, pitch, roll))
    }

    fn mod_position_keep_zoom<F: FnOnce(&mut Vector3<f32>)>(&mut self, f: F) {
        self.display_object.mod_position(f)
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
/// - The `z_zoom_1` value describes the z-axis distance at which the `zoom` value is `1.0`.
/// - When a new screen dimensions are provided, the camera automatically recomputes the z-axis
///   position to keep the `zoom` unchanged.
/// - The `alignment` describes where the origin is placed in the camera frustum. It is used for
///   drawing elements and scaling the view. By default, the `alignment` is set to center, which
///   defines the origin center at the center of the screen. When scaling the view, objects placed
///   in the center of the view will not move visually. If you set the alignment to bottom-left
///   corner, you will get a view which behaves like a window in window-based GUIs. When scaling the
///   window, the left-bottom corner will stay in place.
#[derive(Clone, CloneRef, Debug)]
pub struct Camera2d {
    display_object: display::object::Instance,
    data:           Rc<RefCell<Camera2dData>>,
}

impl Camera2d {
    /// Creates new [`Camera2d`] instance. Please note that the camera will be of zero-size and in
    /// order for it to work properly, you have to initialize it by using the `set_screen` method.
    pub fn new() -> Self {
        let display_object = display::object::Instance::new();
        let data = Camera2dData::new(&display_object);
        let data = Rc::new(RefCell::new(data));
        Self { display_object, data }
    }
}

impl Default for Camera2d {
    fn default() -> Self {
        Self::new()
    }
}


// === Modifiers ===

impl Camera2d {
    /// Sets screen dimensions.
    pub fn set_screen(&self, width: f32, height: f32) {
        let (endpoint, screen) = {
            let mut borrowed = self.data.borrow_mut();
            borrowed.set_screen(width, height);
            (borrowed.frp.screen.clone_ref(), borrowed.screen)
        };
        endpoint.emit(screen);
    }

    /// Resets the zoom of the camera to the 1.0 value.
    pub fn reset_zoom(&self) {
        self.data.borrow_mut().reset_zoom()
    }

    /// Update all dirty camera parameters and compute updated view-projection matrix.
    pub fn update(&self, scene: &Scene) -> bool {
        let (is_updated, frp, zoom) = {
            let mut borrowed = self.data.borrow_mut();
            let is_updated = borrowed.update(scene);
            let frp = borrowed.frp.clone_ref();
            let zoom = borrowed.zoom;
            (is_updated, frp, zoom)
        };
        if is_updated {
            frp.position.emit(self.display_object.position());
            frp.zoom.emit(zoom);
        }
        is_updated
    }

    // FIXME: This can fail, for example, when during calling the callback another callback is
    //        being registered.
    /// Adds a callback to notify when `zoom` is updated.
    pub fn add_zoom_update_callback<F: ZoomUpdateFn>(&self, f: F) -> callback::Handle {
        self.data.borrow_mut().add_zoom_update_callback(f)
    }

    /// Adds a callback to notify when `screen` is updated.
    pub fn add_screen_update_callback<F: ScreenUpdateFn>(&self, f: F) -> callback::Handle {
        self.data.borrow_mut().add_screen_update_callback(f)
    }
}


// === Getters ===

#[allow(missing_docs)]
impl Camera2d {
    pub fn frp(&self) -> Frp {
        self.data.borrow().frp.clone_ref()
    }

    pub fn clipping(&self) -> Clipping {
        self.data.borrow().clipping
    }

    pub fn screen(&self) -> Screen {
        self.data.borrow().screen
    }

    pub fn zoom(&self) -> f32 {
        self.data.borrow().zoom
    }

    pub fn projection(&self) -> Projection {
        self.data.borrow().projection
    }

    pub fn fovy(&self) -> f32 {
        (1.0 / self.projection_matrix()[(1, 1)]).atan() * 2.0
    }

    pub fn half_fovy_slope(&self) -> f32 {
        (self.fovy() / 2.0).tan()
    }

    pub fn z_zoom_1(&self) -> f32 {
        self.data.borrow().z_zoom_1
    }

    pub fn view_matrix(&self) -> Matrix4<f32> {
        self.data.borrow().matrix.view
    }

    pub fn projection_matrix(&self) -> Matrix4<f32> {
        self.data.borrow().matrix.projection
    }

    pub fn inversed_view_matrix(&self) -> Matrix4<f32> {
        self.data.borrow().matrix.view_inversed
    }

    pub fn inversed_projection_matrix(&self) -> Matrix4<f32> {
        self.data.borrow().inversed_projection_matrix()
    }

    pub fn inversed_view_projection_matrix(&self) -> Matrix4<f32> {
        self.data.borrow().inversed_view_projection_matrix()
    }

    pub fn view_projection_matrix(&self) -> Matrix4<f32> {
        self.data.borrow().matrix.view_projection
    }
}


// === Setters ===

#[allow(missing_docs)]
impl Camera2d {
    pub fn mod_position<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.data.borrow_mut().mod_position(f)
    }

    pub fn set_position(&self, value: Vector3<f32>) {
        self.data.borrow_mut().set_position(value)
    }
}


// === Conversions ===

impl display::Object for Camera2d {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    /// A regression test checks whether handling the camera's FRP events does not cause panics
    /// at runtime.
    ///
    /// If the events are emitted with the camera's internal [`RefCell`] lock held, the usage of
    /// camera API methods in event handlers can cause a panic. We need to check methods that use
    /// both immutable and mutable borrows because, depending on the implementation of
    /// the camera, it can hold either lock variant while emitting the event.
    #[test]
    fn test_frp_endpoints_are_not_causing_refcell_locks() {
        let app = crate::application::Application::new("root");
        let camera = app.display.default_scene.camera();
        let frp = camera.frp();
        let network = frp::Network::new("TestCamera2d");
        frp::extend! { network
            // `zoom()` method uses immutable borrow under the hood.
            dummy <- frp.position.map(f_!(camera.zoom()));
            // `set_position` method uses mutable borrow.
            eval_ dummy(camera.set_position(default()));
            // `screen` output is fired from the method that uses mutable borrow.
            eval_ frp.screen(camera.zoom());
            eval_ frp.screen(camera.set_position(default()));
        }
        camera.set_position(Vector3(1.0, 2.0, 3.0));
        camera.update(&app.display.default_scene);
    }
}
