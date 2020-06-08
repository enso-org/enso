//! Examples of defining visualization in Rust using web_sys or ensogl.

use crate::prelude::*;

use crate::component::visualization::*;
use crate::component::visualization;

use ensogl::data::color::Rgba;
use ensogl::display::DomSymbol;
use ensogl::display::scene::Scene;
use ensogl::display;
use ensogl::gui::component;
use ensogl::system::web;
use ensogl::system::web::StyleSetter;
use crate::frp;



// FIXME: move to separate file.

// ===================
// === BubbleChart ===
// ===================

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

/// Sample implementation of a Bubble Chart.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct BubbleChart {
    pub display_object : display::object::Instance,
    pub scene          : Scene,
        signature      : Signature,
        frp            : visualization::instance::Frp,
        views          : RefCell<Vec<component::ShapeView<shape::Shape>>>,
        logger         : Logger,
        size           : Rc<Cell<V2>>,
}

#[allow(missing_docs)]
impl BubbleChart {
    pub fn definition() -> Definition {
        Definition::new(
            Signature::new_for_any_type(Path::builtin("[Demo] Bubble Visualization")),
            |scene| { Ok(Self::new(scene).into()) }
        )
    }

    pub fn new(scene:&Scene) -> Self {
        let logger         = Logger::new("bubble");
        let display_object = display::object::Instance::new(&logger);
        let views          = RefCell::new(vec![]);
        let frp            = visualization::instance::Frp::new();
        let size           = default();
        let scene          = scene.clone_ref();
        let signature      = Signature::new_for_any_type(Path::builtin("[Demo] Bubble Chart"));
        BubbleChart {display_object,views,logger,frp,size,scene,signature} . init()
    }

    fn init(self) -> Self {
        let network = &self.frp.network;
        let size    = &self.size;
        frp::extend! { network
            eval self.frp.set_size ((s) size.set(*s));
            eval self.frp.send_data ([](_data) {
            // FIXME: uncomment and update.
//                let data_inner: Rc<Vec<Vector3<f32>>> = data.as_binary()?;
//                // Avoid re-creating views, if we have already created some before.
//                let mut views = self.views.borrow_mut();
//                views.resize_with(data_inner.len(),|| component::ShapeView::new(&self.logger,&self.scene));
//
//                // TODO[mm] this is somewhat inefficient, as the canvas for each bubble is too large.
//                // But this ensures that we can get a cropped view area and avoids an issue with the data
//                // and position not matching up.
//                views.iter().zip(data_inner.iter()).for_each(|(view,item)| {
//                    let size : Vector2<f32> = self.size.get().into();
//                    view.display_object.set_parent(&self.display_object);
//                    view.shape.sprite.size().set(size);
//                    view.shape.radius.set(item.z);
//                    view.shape.position.set(Vector2::new(item.x,item.y) - size / 2.0);
//                });
//                Ok(())
            });
        }
        self
    }
}

impl From<BubbleChart> for Instance {
    fn from(t:BubbleChart) -> Self {
        Self::new(&t,&t.frp)
    }
}

impl display::Object for BubbleChart {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object.display_object()
    }
}



// FIXME: move to separate file.

// ===============
// === RawText ===
// ===============

/// Sample visualization that renders the given data as text. Useful for debugging and testing.
#[derive(Debug,Shrinkwrap)]
#[allow(missing_docs)]
pub struct RawText {
    #[shrinkwrap(main_field)]
    model : RawTextModel,
    frp   : visualization::instance::Frp,
}

impl RawText {
    /// Definition of this visualization.
    pub fn definition() -> Definition {
        Definition::new(
            Signature::new_for_any_type(Path::builtin("Raw Text Visualization (native)")),
            |scene| { Ok(Self::new(scene).into()) }
        )
    }

    /// Constructor.
    pub fn new(scene:&Scene) -> Self {
        let frp   = visualization::instance::Frp::new();
        let model = RawTextModel::new(scene);
        Self {model,frp} . init()
    }

    fn init(self) -> Self {
        let network = &self.frp.network;
        let model   = &self.model;
        frp::extend! { network
            eval self.frp.set_size  ((size) model.set_size(*size));
            eval self.frp.send_data ((data) model.receive_data(data).unwrap()); // FIXME : on error emit it via FRP
        }
        self
    }
}

#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct RawTextModel {
    logger : Logger,
    dom    : DomSymbol,
    size   : Rc<Cell<V2>>,
}

impl RawTextModel {
    /// Constructor.
    fn new(scene:&Scene) -> Self {
        let logger  = Logger::new("RawText");
        let div     = web::create_div();
        let dom     = DomSymbol::new(&div);
        let size    = Rc::new(Cell::new(V2(200.0,200.0)));

        dom.dom().set_style_or_warn("white-space"   ,"pre"                  ,&logger);
        dom.dom().set_style_or_warn("overflow-y"    ,"auto"                 ,&logger);
        dom.dom().set_style_or_warn("overflow-x"    ,"auto"                 ,&logger);
        dom.dom().set_style_or_warn("font-family"   ,"dejavuSansMono"       ,&logger);
        dom.dom().set_style_or_warn("font-size"     ,"11px"                 ,&logger);
        dom.dom().set_style_or_warn("margin-left"   ,"12px"                 ,&logger);
        dom.dom().set_style_or_warn("color"         ,"rgba(255,255,255,0.7)",&logger);
        dom.dom().set_style_or_warn("pointer-events","auto"                 ,&logger);

        scene.dom.layers.main.manage(&dom);
        RawTextModel{dom,logger,size}.init()
    }

    fn init(self) -> Self {
        self.reload_style();
        self
    }

    fn set_size(&self, size:V2) {
        self.size.set(size);
        self.reload_style();
    }

    fn receive_data(&self, data:&Data) -> Result<(),DataError> {
        let data_inner = match data {
            Data::Json {content} => content,
            _ => todo!() // FIXME
        };
        let data_str = serde_json::to_string_pretty(&**data_inner);
        let data_str = data_str.unwrap_or_else(|e| format!("<Cannot render data: {}>", e));
        let data_str = format!("\n{}",data_str);
        self.dom.dom().set_inner_text(&data_str);
        Ok(())
    }

    fn reload_style(&self) {
        let size = self.size.get();
        self.dom.set_size(Vector2::new(size.x,size.y));
    }
}

impl From<RawText> for Instance {
    fn from(t:RawText) -> Self {
        Self::new(&t,&t.frp)
    }
}

impl display::Object for RawText {
    fn display_object(&self) -> &display::object::Instance {
        &self.dom.display_object()
    }
}
