use crate::prelude::*;

use ensogl::data::color;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::gui::component;
use ensogl::display;

use crate::Type;
use crate::node::input::area;



// =================
// === Constants ===
// =================

/// The horizontal padding of ports. It affects how the port hover should extend the target text
/// boundary on both sides.
pub const PADDING_X : f32  = 4.0;



// ===================
// === Hover Shape ===
// ===================

/// Port hover shape definition.
pub mod hover {
    use super::*;
    ensogl::define_shape_system! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let shape  = Rect((&width,&height));
            if !area::DEBUG {
                let color = Var::<color::Rgba>::from("srgba(1.0,1.0,1.0,0.00001)");
                shape.fill(color).into()
            } else {
                let shape = shape.corners_radius(6.px());
                let color = Var::<color::Rgba>::from("srgba(1.0,0.0,0.0,0.1)");
                shape.fill(color).into()
            }
        }
    }
}

/// Function used to hack depth sorting. To be removed when it will be implemented in core engine.
pub fn depth_sort_hack(scene:&Scene) {
    let logger = Logger::new("hack");
    component::ShapeView::<hover::Shape>::new(&logger,scene);
}



// =============
// === Shape ===
// =============

/// Port shape definition.
pub mod viz {
    use super::*;
    ensogl::define_shape_system! {
        (style:Style, color:Vector4) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let shape  = Rect((&width,&height)).corners_radius(&height / 2.0);
            shape.fill("srgba(input_color)").into()
        }
    }
}



// =============
// === Shape ===
// =============

/// Shapes the port is build from. It consist of the `hover_shape`, which represents a hover area of
/// a full node height, and the `viz_shape`, which is a nice, visual highlight representation.
/// Both shapes are children of the `root` display object:
///
/// ```ignore
///     hover_shape
///      ◄──────►
/// ╭───┬────────┬──┄
/// │   │╭──────╮│▼ viz_shape
/// │   │╰──────╯│▲ (appears after mouse_hover)
/// ╰───┴────────┴──┄
/// ```
#[derive(Clone,CloneRef,Debug)]
pub struct Shape {
    pub root  : display::object::Instance,
    pub hover : component::ShapeView<hover::Shape>,
    pub viz   : component::ShapeView<viz::Shape>,
}

impl Shape {
    pub fn new(logger:&Logger, scene:&Scene, size:Vector2, hover_height:f32) -> Self {
        let root  = display::object::Instance::new(logger);
        let hover = component::ShapeView::<hover::Shape>::new(logger,scene);
        let viz   = component::ShapeView::<viz::Shape>::new(logger,scene);

        let width_padded = size.x + 2.0 * PADDING_X;
        hover.shape.sprite.size.set(Vector2::new(width_padded,hover_height));
        viz.shape.sprite.size.set(Vector2::new(width_padded,size.y));
        hover.shape.mod_position(|t| t.x = size.x/2.0);
        viz.shape.mod_position(|t| t.x = size.x/2.0);
        viz.shape.color.set(color::Rgba::transparent().into());

        root.add_child(&hover);
        root.add_child(&viz);
        let viz_shape_system = scene.shapes.shape_system(PhantomData::<viz::Shape>);
        viz_shape_system.shape_system.set_pointer_events(false);

        Self {root,hover,viz}
    }
}

impl display::Object for Shape {
    fn display_object(&self) -> &display::object::Instance {
        &self.root.display_object()
    }
}



// =============
// === Model ===
// =============

ensogl::define_endpoints! {
    Input {
        set_optional         (bool),
        set_disabled         (bool),
        set_active           (bool),
        set_hover            (bool),
        set_connected        (bool),
        set_parent_connected (bool),
        set_usage_type       (Option<Type>),
    }

    Output {
        text_color   (color::Lcha),
        select_color (color::Lcha),
        final_type   (Option<Type>),
    }
}

/// Input port model. Please note that this is not a component model. It is a `SpanTree` payload
/// model.
#[derive(Clone,Debug,Default)]
pub struct Model {
    pub frp             : Frp,
    pub tp              : Rc<RefCell<Option<Type>>>,
    pub shape           : Option<Shape>,
    pub name            : Option<String>,
    pub index           : usize,
    pub local_index     : usize,
    pub length          : usize,
    pub highlight_color : color::Lcha,
}

impl Deref for Model {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Model {
    pub fn new() -> Self {
        default()
    }

    /// Shape initialization. Please note that not all port models get their shapes initialized,
    /// as some are skipped. For example, given the expression `(((foo)))`, the inner parentheses
    /// will be skipped, as there is no point in making them ports. The skip algorithm is
    /// implemented as part of the port are initialization.
    pub fn init_shape
    (&mut self, logger:impl AnyLogger, scene:&Scene, size:Vector2, hover_height:f32) -> Shape {
        let logger_name = format!("port({},{})",self.index,self.length);
        let logger      = Logger::sub(logger,logger_name);
        let shape       = Shape::new(&logger,scene,size,hover_height);
        self.shape      = Some(shape);
        self.shape.as_ref().unwrap().clone_ref()
    }
}
