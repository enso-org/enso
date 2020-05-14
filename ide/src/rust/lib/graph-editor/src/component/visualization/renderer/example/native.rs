//! Examples of defining visualisation in Rust using web_sys or ensogl.

use crate::prelude::*;

use crate::component::visualization::*;

use ensogl::data::color::Rgba;
use ensogl::display::layout::alignment;
use ensogl::display::scene::Scene;
use ensogl::display;
use ensogl::gui::component;



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
