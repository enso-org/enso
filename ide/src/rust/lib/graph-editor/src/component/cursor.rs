//! Definition of the Cursor (known as well as mouse pointer) component.

use crate::prelude::*;

use enso_frp as frp;
use enso_frp::frp;
use enso_frp;
use ensogl::control::callback;
use ensogl::data::color::Srgba;
use ensogl::display::Buffer;
use ensogl::display::layout::alignment;
use ensogl::display::scene::{Scene,ShapeRegistry};
use ensogl::display::scene;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display::{Sprite, Attribute};
use ensogl::display;
use ensogl::gui::component::animation;
use ensogl::gui::component;
use ensogl::system::web;



// ==================
// === CursorView ===
// ==================

/// Canvas shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (position:Vector2<f32>, selection_size:Vector2<f32>, press:f32) {
            let radius = 8.px() - 2.px() * press;
            let side   = &radius * 2.0;
            let width  = Var::<Distance<Pixels>>::from("input_selection_size.x * input_press");
            let height = Var::<Distance<Pixels>>::from("input_selection_size.y * input_press");
            let cursor = Rect((&side + width.abs(),&side + height.abs()))
                .corners_radius(radius)
                .translate((-&width/2.0, -&height/2.0))
                .translate(("input_position.x","input_position.y"))
                .fill(Srgba::new(0.0,0.0,0.0,0.3));
            cursor.into()
        }
    }
}

/// Shape view for Cursor.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct CursorView {
    pub scene_view    : scene::View,
    pub resize_handle : callback::Handle,
}

impl component::ShapeViewDefinition for CursorView {
    type Shape = shape::Shape;
    fn new(shape:&Self::Shape, scene:&Scene, shape_registry:&ShapeRegistry) -> Self {
        let scene_shape = scene.shape();
        shape.sprite.size().set(Vector2::new(scene_shape.width(),scene_shape.height()));

        let resize_handle = scene.on_resize(enclose!((shape) move |scene_shape:&web::dom::ShapeData| {
            shape.sprite.size().set(Vector2::new(scene_shape.width(),scene_shape.height()));
        }));

        let shape_system = shape_registry.shape_system(PhantomData::<shape::Shape>);
        shape_system.shape_system.set_alignment(alignment::HorizontalAlignment::Left, alignment::VerticalAlignment::Bottom);

        let scene_view = scene.views.new();
        scene.views.main.remove(&shape_system.shape_system.symbol);
        scene_view.add(&shape_system.shape_system.symbol);
        Self {scene_view,resize_handle}
    }
}



// ==============
// === Events ===
// ==============

/// Cursor events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Events {
    pub press   : frp::Dynamic<()>,
    pub release : frp::Dynamic<()>,
}

impl Default for Events {
    fn default() -> Self {
        frp! {
            press   = source::<()> ();
            release = source::<()> ();
        }
        Self {press,release}
    }
}



// ==============
// === Cursor ===
// ==============

/// Cursor (mouse pointer) definition.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct Cursor {
    data : Rc<CursorData>
}

/// Weak version of `Cursor`.
#[derive(Clone,CloneRef,Debug)]
pub struct WeakCursor {
    data : Weak<CursorData>
}

/// Internal data for `Cursor`.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct CursorData {
    pub logger : Logger,
    pub events : Events,
    pub view   : component::ShapeView<CursorView>,
}

impl Cursor {
    /// Constructor.
    pub fn new() -> Self {
        let logger = Logger::new("cursor");
        let view   = component::ShapeView::new(&logger);
        let events = Events::default();
        let data   = CursorData {logger,events,view};
        let data   = Rc::new(data);
        Cursor {data} . init()
    }

    fn init(self) -> Self {
        // FIXME: This is needed now because frp leaks memory.
        let weak_view_data = Rc::downgrade(&self.view.data);
        let press = animation(move |value| {
            weak_view_data.upgrade().for_each(|view_data| {
                view_data.borrow().as_ref().for_each(|t| t.shape.press.set(value))
            })
        });

        self.events.press.map("press", enclose!((press) move |_| {
            press.set_target_position(1.0);
        }));

        self.events.release.map("release", enclose!((press) move |_| {
            press.set_target_position(0.0);
        }));

        self
    }

    /// Position setter.
    pub fn set_position(&self, pos:Vector2<f32>) {
        self.view.data.borrow().as_ref().for_each(|view| {
            view.shape.position.set(pos);
        })
    }

    /// Selection size setter.
    pub fn set_selection_size(&self, pos:Vector2<f32>) {
        self.view.data.borrow().as_ref().for_each(|view| {
            view.shape.selection_size.set(pos);
        })
    }
}

impl Default for Cursor {
    fn default() -> Self {
        Cursor::new()
    }
}

impl StrongRef for Cursor {
    type WeakRef = WeakCursor;
    fn downgrade(&self) -> WeakCursor {
        WeakCursor {data:Rc::downgrade(&self.data)}
    }
}

impl WeakRef for WeakCursor {
    type StrongRef = Cursor;
    fn upgrade(&self) -> Option<Cursor> {
        self.data.upgrade().map(|data| Cursor{data})
    }
}

impl<'t> From<&'t Cursor> for &'t display::object::Node {
    fn from(t:&'t Cursor) -> Self {
        &t.view.display_object
    }
}
