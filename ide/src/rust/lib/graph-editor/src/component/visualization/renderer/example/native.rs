//! Examples of defining visualization in Rust using web_sys or ensogl.

use crate::prelude::*;

use crate::component::visualization::*;

use ensogl::data::color::Rgba;
use ensogl::display::DomSymbol;
use ensogl::display::layout::alignment;
use ensogl::display::scene::Scene;
use ensogl::display;
use ensogl::gui::component;
use ensogl::system::web;
use ensogl::display::object::ObjectOps;


// ==========================
// === Native BubbleChart ===
// ==========================

/// Bubble shape definition.
pub mod shape {
    use super::*;
    use ensogl::display::shape::*;
    use ensogl::display::scene::Scene;
    use ensogl::display::Sprite;
    use ensogl::display::Buffer;
    use ensogl::display::Attribute;

    ensogl::define_shape_system! {
        (position:Vector2<f32>,radius:f32) {
            let node = Circle(radius);
            let node = node.fill(Rgba::new(0.17,0.46,0.15,1.0));
            let node = node.translate(("input_position.x","input_position.y"));
            node.into()
        }
    }
}

/// Sample implementation of a Bubble Chart using the ensogl shape system.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct BubbleChart {
    pub display_object : display::object::Instance,
    pub scene          : Scene,
        frp            : DataRendererFrp,
        views          : RefCell<Vec<component::ShapeView<shape::Shape>>>,
        logger         : Logger,
        size           : Cell<Vector2<f32>>,
}

#[allow(missing_docs)]
impl BubbleChart {
    pub fn new(scene:&Scene) -> Self {
        let logger         = Logger::new("bubble");
        let display_object = display::object::Instance::new(&logger);
        let views          = RefCell::new(vec![]);
        let frp            = default();
        let size           = Cell::new(Vector2::zero());
        let scene          = scene.clone_ref();

        BubbleChart { display_object,views,logger,frp,size,scene }
    }
}

impl DataRenderer for BubbleChart {

    fn receive_data(&self, data:Data) -> Result<(),DataError> {
        let data_inner: Rc<Vec<Vector3<f32>>> = data.as_binary()?;

        // Avoid re-creating views, if we have already created some before.
        let mut views = self.views.borrow_mut();
        views.resize_with(data_inner.len(),|| component::ShapeView::new(&self.logger,&self.scene));

        // TODO[mm] this is somewhat inefficient, as the canvas for each bubble is too large.
        // But this ensures that we can get a cropped view area and avoids an issue with the data
        // and position not matching up.
        views.iter().zip(data_inner.iter()).for_each(|(view,item)| {

            let shape_system = self.scene.shapes.shape_system(PhantomData::<shape::Shape>);
            shape_system.shape_system.set_alignment(
                alignment::HorizontalAlignment::Left, alignment::VerticalAlignment::Bottom);

            view.display_object.set_parent(&self.display_object);
            view.shape.sprite.size().set(self.size.get());
            view.shape.radius.set(item.z);
            view.shape.position.set(Vector2::new(item.x,item.y));
        });
        Ok(())
    }

    fn set_size(&self, size:Vector2<f32>) {
        self.size.set(size);
    }

    fn frp(&self) -> &DataRendererFrp {
        &self.frp
    }
}

impl display::Object for BubbleChart {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object.display_object()
    }
}



// ===============================
// === Native RawText Renderer ===
// ===============================

/// Sample visualization that renders the given data as text. Useful for debugging and testing.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct RawText {
    scene     : Scene,
    root_node : DomSymbol,
    size      : Cell<Vector2<f32>>,
    frp       : DataRendererFrp,
    logger    : Logger,
}

impl RawText {
    /// Constructor.
    pub fn new(scene:&Scene) -> Self {
        let logger    = Logger::new("RawText");
        let div       = web::create_div();
        let root_node = DomSymbol::new(&div);
        let frp       = default();
        let size      = Cell::new(Vector2::zero());
        let scene     = scene.clone_ref();

        // FIXME It seems by default the text here is mirrored.
        // FIXME This should be fixed in the DOMSymbol directly and removed here.
        root_node.set_rotation(Vector3::new(180.0_f32.to_radians(), 0.0, 0.0));
        scene.dom.layers.front.manage(&root_node);

        RawText{root_node,logger,frp,size,scene}.init()
    }

    fn init(self) -> Self {
        self.update_style();
        self
    }

    fn update_style(&self) {
        let mut style = "white-space:pre;".to_string();
        style += "overflow-y:auto;";
        style += "overflow-x:auto;";
        // TODO: Integrate with the global style system and replace constant color.
        style += "color:white;";
        style += &format!("height:{}px;", self.size.get().x);
        style += &format!("width:{}px;", self.size.get().y);
        style += "pointer-events:auto";
        self.root_node.dom().set_attribute("style",&style).unwrap();
    }
}

impl display::Object for RawText {
    fn display_object(&self) -> &display::object::Instance {
        &self.root_node.display_object()
    }
}

impl DataRenderer for RawText {
    fn receive_data(&self, data:Data) -> Result<(),DataError> {
        let data_inner = data.as_json()?;
        let data_str   = serde_json::to_string_pretty(&data_inner);
        let data_str   = data_str.unwrap_or_else(|e| format!("<Cannot render data: {}>", e));
        self.root_node.dom().set_inner_text(&data_str);
        Ok(())
    }

    fn set_size(&self, size:Vector2<f32>) {
        self.size.set(size);
        self.update_style();
    }

    fn frp(&self) -> &DataRendererFrp {
        &self.frp
    }
}
