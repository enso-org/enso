use ensogl_core::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::display;

pub mod data;
pub mod shape;

const ROW_HEIGHT: f64 = 20.0;
const ROW_PADDING: f64 = 5.0;

pub struct FlameGraph {
    display_object: display::object::Instance,
    blocks:         Vec<shape::Block>,
}

fn shape_from_block(block: data::Block, app: &Application) -> shape::Block {
    let component = shape::Block::new(app);

    let size = Vector2::new(block.width() as f32, ROW_HEIGHT as f32);
    let x = block.start + block.width() / 2.0;
    let y = block.row as f64 * (ROW_HEIGHT + ROW_PADDING);
    let pos = Vector2::new(x as f32, y as f32);

    component.set_content.emit(block.label);
    component.set_size.emit(size);
    component.set_position_xy(pos);

    component
}

impl FlameGraph {
    pub fn from_data(data: data::FlameGraph, app: &Application) -> Self {
        let logger = Logger::new("FlameGraph");
        let display_object = display::object::Instance::new(&logger);

        let blocks =
            data.blocks.into_iter().map(|block| shape_from_block(block, app)).collect_vec();
        blocks.iter().for_each(|item| display_object.add_child(item));
        Self { display_object, blocks }
    }

    pub fn blocks(&self) -> &[shape::Block] {
        &self.blocks
    }
}

impl display::Object for FlameGraph {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
