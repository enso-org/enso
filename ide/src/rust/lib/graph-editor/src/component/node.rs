//! Definition of the Node component.

#![allow(missing_docs)]
// WARNING! UNDER HEAVY DEVELOPMENT. EXPECT DRASTIC CHANGES.

pub mod port;

use crate::prelude::*;

//use crate::component::node::port::Registry;

use enso_frp;
use enso_frp as frp;
use ensogl::data::color;
use ensogl::display::Attribute;
use ensogl::display::Buffer;
use ensogl::display::Sprite;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display;
use ensogl::gui::component::animation;
use ensogl::gui::component::animation2;
use ensogl::gui::component;
use ensogl::display::shape::text::glyph::font::FontRegistry;
use ensogl::display::shape::text::glyph::system::GlyphSystem;

use super::connection::Connection;
use crate::component::visualization;


/// Icons definitions.
pub mod icons {
    use super::*;

    /// History icon.
    pub fn history() -> AnyShape {
        let radius_diff    = 0.5.px();
        let corners_radius = 2.0.px();
        let width_diff     = &corners_radius * 3.0;
        let offset         = 2.px();
        let width          = 32.px();
        let height         = 16.px();
        let persp_diff1    = 6.px();

        let width2          = &width  - &width_diff;
        let width3          = &width2 - &width_diff;
        let corners_radius2 = &corners_radius  - &radius_diff;
        let corners_radius3 = &corners_radius2 - &radius_diff;
        let persp_diff2     = &persp_diff1 * 2.0;

        let rect1 = Rect((&width ,&height)).corners_radius(&corners_radius);
        let rect2 = Rect((&width2,&height)).corners_radius(&corners_radius2).translate_y(&persp_diff1);
        let rect3 = Rect((&width3,&height)).corners_radius(&corners_radius3).translate_y(&persp_diff2);

        let rect3 = rect3 - rect2.translate_y(&offset);
        let rect2 = rect2 - rect1.translate_y(&offset);

        let rect1 = rect1.fill(color::Rgba::new(0.26, 0.69, 0.99, 1.00));
        let rect2 = rect2.fill(color::Rgba::new(0.26, 0.69, 0.99, 0.6));
        let rect3 = rect3.fill(color::Rgba::new(0.26, 0.69, 0.99, 0.4));

        let icon = (rect3 + rect2 + rect1).translate_y(-persp_diff2/2.0);
        icon.into()
    }
}

/// Ring angle shape definition.
pub fn ring_angle<R,W,A>(inner_radius:R, width:W, angle:A) -> AnyShape
    where R : Into<Var<Distance<Pixels>>>,
          W : Into<Var<Distance<Pixels>>>,
          A : Into<Var<Angle<Radians>>> {
    let inner_radius = inner_radius.into();
    let width        = width.into();
    let angle        = angle.into();

    let angle2  = &angle / 2.0;
    let radius  = &width / 2.0;
    let inner   = Circle(&inner_radius);
    let outer   = Circle(&inner_radius + &width);
    let section = Plane().cut_angle(&angle);
    let corner1 = Circle(&radius).translate_y(inner_radius + radius);
    let corner2 = corner1.rotate(&angle2);
    let corner1 = corner1.rotate(-&angle2);
    let ring    = &outer - &inner;
    let pie     = &ring * &section;
    let out     = &pie + &corner1 + &corner2;
    let out     = out.fill(color::Rgba::new(0.9,0.9,0.9,1.0));
    out.into()
}


const NODE_SHAPE_PADDING : f32 = 40.0;


// ============
// === Node ===
// ============

/// Canvas node shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style, selection:f32) {
            let bg_color = style.get("graph_editor.node.background.color").color().unwrap_or_else(|| color::Rgba::new(1.0,0.0,0.0,1.0).into());
            let selection_color = style.get("graph_editor.node.selection.color").color().unwrap_or_else(|| color::Rgba::new(1.0,0.0,0.0,1.0).into());
            let selection_size  = style.get("graph_editor.node.selection.size").number().unwrap_or(8.0);

            let border_size_f = 16.0;

            let width  : Var<Distance<Pixels>> = "input_size.x".into();
            let height : Var<Distance<Pixels>> = "input_size.y".into();
            let width  = width  - NODE_SHAPE_PADDING.px() * 2.0;
            let height = height - NODE_SHAPE_PADDING.px() * 2.0;
            let radius = 14.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            let shape  = shape.fill(color::Rgba::from(bg_color));


            // === Shadow ===

            let shadow_size   = 14.px();
            let shadow_width  = &width  + &shadow_size * 2.0;
            let shadow_height = &height + &shadow_size * 2.0;
            let shadow_radius = &shadow_height / 2.0;
            let shadow        = Rect((shadow_width,shadow_height)).corners_radius(shadow_radius);
            let shadow_color  = color::LinearGradient::new()
                .add(0.0,color::Rgba::new(0.0,0.0,0.0,0.0).into_linear())
                .add(1.0,color::Rgba::new(0.0,0.0,0.0,0.20).into_linear());
            let shadow_color  = color::SdfSampler::new(shadow_color).max_distance(border_size_f).slope(color::Slope::Exponent(4.0));
            let shadow        = shadow.fill(shadow_color);


            // === Selection ===

            let selection_size = selection_size.px();
            let select_width   = &width  - 2.px() + &selection_size * 2.0 * &selection;
            let select_height  = &height - 2.px() + &selection_size * 2.0 * &selection;
            let select_radius  = &select_height / 2.0;
            let select         = Rect((select_width,select_height)).corners_radius(select_radius);
            let select         = select.fill(color::Rgba::from(selection_color));

            let out = select + shadow + shape;
            out.into()
        }
    }
}

/// Canvas node shape definition.
pub mod output_area {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style, grow:f32) {
            let width  : Var<Distance<Pixels>> = "input_size.x".into();
            let height : Var<Distance<Pixels>> = "input_size.y".into();
            let width  = width  - NODE_SHAPE_PADDING.px() * 2.0;
            let height = height - NODE_SHAPE_PADDING.px() * 2.0;

            let hover_area_size   = 20.0.px();
            let hover_area_width  = &width  + &hover_area_size * 2.0;
            let hover_area_height = &height / 2.0 + &hover_area_size;
            let hover_area        = Rect((&hover_area_width,&hover_area_height));
            let hover_area        = hover_area.translate_y(-hover_area_height/2.0);
            let hover_area        = hover_area.fill(color::Rgba::new(0.0,0.0,0.0,0.000_001));

            let shrink           = 1.px() - 1.px() * &grow;
            let radius           = 14.px();
            let port_area_size   = 4.0.px() * &grow;
            let port_area_width  = &width  + (&port_area_size - &shrink) * 2.0;
            let port_area_height = &height + (&port_area_size - &shrink) * 2.0;
            let bottom_radius    = &radius + &port_area_size;
            let port_area        = Rect((&port_area_width,&port_area_height));
            let port_area        = port_area.corners_radius(&bottom_radius);
            let port_area        = port_area - BottomHalfPlane();
            let corner_radius    = &port_area_size / 2.0;
            let corner_offset    = &port_area_width / 2.0 - &corner_radius;
            let corner           = Circle(&corner_radius);
            let left_corner      = corner.translate_x(-&corner_offset);
            let right_corner     = corner.translate_x(&corner_offset);
            let port_area        = port_area + left_corner + right_corner;
            let port_area        = port_area.fill(color::Rgba::from(color::Lcha::new(0.6,0.5,0.76,1.0)));

            let out = hover_area + port_area;
            out.into()
        }
    }
}


/// Canvas node shape definition.
pub mod drag_area {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let width  : Var<Distance<Pixels>> = "input_size.x".into();
            let height : Var<Distance<Pixels>> = "input_size.y".into();
            let width  = width  - NODE_SHAPE_PADDING.px() * 2.0;
            let height = height - NODE_SHAPE_PADDING.px() * 2.0;
            let radius = 14.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            let shape  = shape.fill(color::Rgba::new(0.0,0.0,0.0,0.000_001));

            let out = shape;
            out.into()
        }
    }
}


pub mod label {
    use super::*;

    #[derive(Clone,CloneRef,Debug)]
    #[allow(missing_docs)]
    pub struct Shape {
        pub lines : Rc<RefCell<Vec<ensogl::display::shape::text::glyph::system::Line>>>,
        pub obj   : display::object::Instance,

    }
    impl ensogl::display::shape::system::Shape for Shape {
        type System = ShapeSystem;
        fn sprites(&self) -> Vec<&Sprite> {
            vec![]
        }
    }
    impl display::Object for Shape {
        fn display_object(&self) -> &display::object::Instance {
            &self.obj
        }
    }
    #[derive(Clone, CloneRef, Debug)]
    #[allow(missing_docs)]
    pub struct ShapeSystem {
        pub fonts : Rc<FontRegistry>,
        pub glyph_system: GlyphSystem,
        style_manager: StyleWatch,

    }
    impl ShapeSystemInstance for ShapeSystem {
        type Shape = Shape;

        fn new(scene: &Scene) -> Self {
            let style_manager = StyleWatch::new(&scene.style_sheet);
            let mut fonts     = FontRegistry::new();
            let font          = fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap();
            let glyph_system  = GlyphSystem::new(scene,font);
            let fonts         = Rc::new(fonts);

            let symbol = &glyph_system.sprite_system().symbol;
            scene.views.main.remove(symbol);
            scene.views.label.add(symbol);

            Self { fonts, glyph_system, style_manager } // .init_refresh_on_style_change()
        }

        fn new_instance(&self) -> Self::Shape {
            let color = color::Rgba::new(1.0, 1.0, 1.0, 0.7);
            let obj   = display::object::Instance::new(Logger::new("test"));
            let line1 = self.glyph_system.new_line();
            line1.set_font_size(12.0);
            line1.set_font_color(color);
            line1.set_text("draw_maps size (distribution normal)");
            obj.add_child(&line1);

            // !!! println!(">>> {:?}", line1.font().get_glyph_info('a').advance * 12.0);


//            let color = color::Rgba::new(0.18, 0.173, 0.165, 1.0);
//            let line2 = self.glyph_system.new_line();
//            line2.set_font_size(12.0);
//            line2.set_font_color(color);
//            line2.set_text("size");
//            obj.add_child(&line2);
//            line2.mod_position(|t| t.x += 72.0);

//            let lines = Rc::new(RefCell::new(vec![line1,line2]));
            let lines = Rc::new(RefCell::new(vec![line1]));

            Shape { lines,obj }
        }
    }
    impl ShapeSystem {
//        fn init_refresh_on_style_change(self) -> Self {
//            let shape_system = self.shape_system.clone_ref();
//            let style_manager = self.style_manager.clone_ref();
//            self.style_manager.set_on_style_change(move || {
//                shape_system.set_shape(&Self::shape_def(&style_manager));
//            });
//            self
//        }


//        pub fn shape_def(__style_watch__: &StyleWatch) -> AnyShape {
//            use ensogl::display::style::data::DataMatch;
//
//            Circle(10.px()).fill(color::Rgba::new(0.97,0.96,0.95)).into()
//        }
    }
}



// ==============
// === Events ===
// ==============

/// Node events.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct InputEvents {
    pub network           : frp::Network,
    pub select            : frp::Source,
    pub deselect          : frp::Source,
    pub set_visualization : frp::Source<Option<visualization::Visualization>>,
}

impl InputEvents {
    pub fn new() -> Self {
        frp::new_network! { network
            def select            = source();
            def deselect          = source();
            def set_visualization = source();
        }
        Self {network,select,deselect,set_visualization}
    }
}

impl Default for InputEvents {
    fn default() -> Self {
        Self::new()
    }
}


#[derive(Clone,CloneRef,Debug,Deref)]
#[allow(missing_docs)]
pub struct OutputPortsEvents {
    pub shape_view_events : component::ShapeViewEvents,
}


#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Events {
    pub input        : InputEvents,
    pub output_ports : OutputPortsEvents
}

impl Deref for Events {
    type Target = InputEvents;
    fn deref(&self) -> &Self::Target {
        &self.input
    }
}



// ============
// === Node ===
// ============

/// Node definition.
#[derive(AsRef,Clone,CloneRef,Debug,Deref)]
pub struct Node {
    data : Rc<NodeData>,
}

impl AsRef<Node> for Node {
    fn as_ref(&self) -> &Self {
        self
    }
}

/// Weak version of `Node`.
#[derive(Clone,CloneRef,Debug)]
pub struct WeakNode {
    data : Weak<NodeData>
}

impl WeakElement for WeakNode {
    type Strong = Node;

    fn new(view: &Self::Strong) -> Self {
        view.downgrade()
    }

    fn view(&self) -> Option<Self::Strong> {
        self.upgrade()
    }
}

impl WeakKey for WeakNode {
    type Key = display::object::Id;

    fn with_key<F, R>(view: &Self::Strong, f: F) -> R where F: FnOnce(&Self::Key) -> R {
        f(&view.id())
    }
}


/// Internal data of `Node`
#[derive(Debug)]
#[allow(missing_docs)]
pub struct NodeData {
    pub scene                   : Scene,
    pub display_object          : display::object::Instance,
    pub logger                  : Logger,
    pub frp                     : Events,
    pub label_area              : component::ShapeView<label::Shape>,
    pub main_area               : component::ShapeView<shape::Shape>,
    pub drag_area               : component::ShapeView<drag_area::Shape>,
    pub output_area             : component::ShapeView<output_area::Shape>,
    pub ports                   : port::Manager,
    pub visualization_container : visualization::Container,
}

pub const NODE_WIDTH : f32 = 284.0;
pub const NODE_HEIGHT : f32 = 28.0;
pub const TEXT_OFF : f32 = 12.0;

impl Node {
    /// Constructor.
    pub fn new(scene:&Scene) -> Self {


        let logger  = Logger::new("node");
        let _connection = Connection::new(scene); // FIXME hack for sorting

        let output_area = component::ShapeView::<output_area::Shape>::new(&logger,scene);
        let main_area    = component::ShapeView::<shape::Shape>::new(&logger,scene);
        let drag_area   = component::ShapeView::<drag_area::Shape>::new(&logger,scene);
        port::sort_hack(scene); // FIXME hack for sorting
        let label_area    = component::ShapeView::<label::Shape>::new(&logger,scene);
        let display_object  = display::object::Instance::new(&logger);
        display_object.add_child(&drag_area);
        display_object.add_child(&output_area);
        display_object.add_child(&main_area);
        display_object.add_child(&label_area);

        // FIXME: maybe we can expose shape system from shape?
        let shape_system = scene.shapes.shape_system(PhantomData::<shape::Shape>);
        shape_system.shape_system.set_pointer_events(false);

        let width = NODE_WIDTH;
        let height = 28.0;

        let size = Vector2::new(width+NODE_SHAPE_PADDING*2.0, height+NODE_SHAPE_PADDING*2.0);
        main_area.shape.sprite.size().set(size);
        drag_area.shape.sprite.size().set(size);
        output_area.shape.sprite.size().set(size);
        main_area.mod_position(|t| t.x += width/2.0);
        main_area.mod_position(|t| t.y += height/2.0);
        drag_area.mod_position(|t| t.x += width/2.0);
        drag_area.mod_position(|t| t.y += height/2.0);
        output_area.mod_position(|t| t.x += width/2.0);
        output_area.mod_position(|t| t.y += height/2.0);

        label_area.mod_position(|t| t.x += TEXT_OFF);
        label_area.mod_position(|t| t.y += 4.0 + 6.0);

        let ports = port::Manager::new(&logger,scene);
        let scene = scene.clone_ref();

        let input = InputEvents::new();



        let network = &input.network;


        let visualization_container = visualization::Container::new();
        visualization_container.mod_position(|t| {
            t.x = 60.0;
            t.y = -120.0;
        });

        display_object.add_child(&visualization_container);

        let view_data = main_area.shape.clone_ref();
        let selection = animation(network, move |value| {
            view_data.selection.set(value)
        });

        let (output_area_size_setter, output_area_size) = animation2(network);


        frp::extend! { network
            let selection_ref = selection.clone_ref();
            def _select = input.select.map(move |_| {
                selection_ref.set_target_position(1.0);
            });

            let selection_ref = selection.clone_ref();
            def _deselect = input.deselect.map(move |_| {
                selection_ref.set_target_position(0.0);
            });

            def _output_area_size = output_area_size.map(f!((size)
                output_area.shape.grow.set(*size)
            ));

            def _output_show = output_area.events.mouse_over.map(f_!(
                output_area_size_setter.set_target_position(1.0)
            ));

            def _output_hide = output_area.events.mouse_out.map(f_!(
                output_area_size_setter.set_target_position(0.0)
            ));

            def _f_set_vis = input.set_visualization.map(f!((content)
                visualization_container.frp.set_visualization.emit(content)
            ));
        }


        //////////////////////////////////////////////////////

        ports.mod_position(|p| {
            p.x = TEXT_OFF;
            p.y = NODE_HEIGHT/2.0;
        });
        display_object.add_child(&ports);


        let output_ports = OutputPortsEvents { shape_view_events:output_area.events.clone_ref() };

        let frp = Events{input,output_ports};



        let data = Rc::new(NodeData {scene,display_object,logger,frp,main_area,drag_area
            ,output_area,label_area,ports,visualization_container});
        Self {data}
    }
}

impl StrongRef for Node {
    type WeakRef = WeakNode;
    fn downgrade(&self) -> WeakNode {
        WeakNode {data:Rc::downgrade(&self.data)}
    }
}

impl WeakRef for WeakNode {
    type StrongRef = Node;
    fn upgrade(&self) -> Option<Node> {
        self.data.upgrade().map(|data| Node{data})
    }
}

impl display::Object for Node {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl display::WeakObject for WeakNode {
    fn try_display_object(&self) -> Option<display::object::Instance> {
        self.upgrade().map(|ref t| t.display_object().clone_ref())
    }
}
