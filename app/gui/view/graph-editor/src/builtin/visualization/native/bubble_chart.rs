//! Bubble Chart visualisation implemented using the native shape system.

use crate::component::visualization::*;
use crate::prelude::*;

use crate::component::visualization;

use enso_frp as frp;
use ensogl::data::color::Rgba;
use ensogl::display;
use ensogl::display::scene::Scene;



// =============
// === Shape ===
// =============

/// Bubble shape definition.
pub mod shape {
    use super::*;
    use ensogl::display::shape::*;

    ensogl::define_shape_system! {
        (position:Vector2<f32>,radius:f32) {
            let node = Circle(radius);
            let node = node.fill(Rgba::new(0.17,0.46,0.15,1.0));
            let node = node.translate(("input_position.x","input_position.y"));
            node.into()
        }
    }
}



// ========================
// === BubbleChartModel ===
// ========================

/// Sample implementation of a Bubble Chart.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct BubbleChartModel {
    pub display_object: display::object::Instance,
    pub scene:          Scene,
    signature:          Signature,
    views:              Rc<RefCell<Vec<shape::View>>>,
    logger:             Logger,
    size:               Rc<Cell<Vector2>>,
}

impl BubbleChartModel {
    #[allow(clippy::question_mark)]
    fn receive_data(&self, data: &Data) -> Result<(), DataError> {
        let data_inner = match data {
            Data::Json { content } => content,
            _ => return Err(DataError::BinaryNotSupported),
        };
        let data_inner: &serde_json::Value = data_inner;
        let data_inner: Rc<Vec<Vector3<f32>>> =
            if let Ok(result) = serde_json::from_value(data_inner.clone()) {
                result
            } else {
                return Err(DataError::InvalidJsonText);
            };

        // Avoid re-creating views, if we have already created some before.
        let mut views = self.views.borrow_mut();
        views.resize_with(data_inner.len(), shape::View::new);

        // TODO[mm] this is somewhat inefficient, as the canvas for each bubble is too large.
        // But this ensures that we can get a cropped view area and avoids an issue with the data
        // and position not matching up.
        views.iter().zip(data_inner.iter()).for_each(|(view, item)| {
            let size = self.size.get();
            self.display_object.add_child(&view);
            view.size.set(size);
            view.radius.set(item.z);
            view.position.set(Vector2(item.x, item.y) - size / 2.0);
        });
        Ok(())
    }
}



// ===================
// === BubbleChart ===
// ===================

/// Sample implementation of a Bubble Chart.
#[derive(Debug, Shrinkwrap)]
#[allow(missing_docs)]
pub struct BubbleChart {
    #[shrinkwrap(main_field)]
    model:   BubbleChartModel,
    network: frp::Network,
    frp:     visualization::instance::Frp,
}

#[allow(missing_docs)]
impl BubbleChart {
    pub fn definition() -> Definition {
        Definition::new(Self::signature(), |scene| Ok(Self::new(scene).into()))
    }

    pub fn new(scene: &Scene) -> Self {
        let logger = Logger::new("bubble");
        let display_object = display::object::Instance::new();
        let views = Rc::new(RefCell::new(vec![]));
        let network = frp::Network::new("bubble_chart");
        let frp = visualization::instance::Frp::new(&network);
        let size = default();
        let scene = scene.clone_ref();
        let signature = Self::signature();
        let model = BubbleChartModel { display_object, scene, signature, views, logger, size };
        BubbleChart { model, network, frp }.init()
    }

    fn init(self) -> Self {
        let network = &self.network;
        let frp = self.frp.clone_ref();
        let model = self.model.clone_ref();

        frp::extend! { network
            eval frp.set_size ((s) model.size.set(*s));
            eval frp.send_data ([frp,model](data) {
                if let Err(e) = model.receive_data(data) {
                    frp.data_receive_error.emit(Some(e));
                }
             });
        }
        self
    }

    fn signature() -> Signature {
        Signature::new_for_any_type(Path::builtin("Bubbles (WebGL)"), Format::Json)
    }
}

impl From<BubbleChart> for Instance {
    fn from(t: BubbleChart) -> Self {
        Self::new(&t, &t.frp, &t.network, None)
    }
}

impl display::Object for BubbleChart {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object.display_object()
    }
}
