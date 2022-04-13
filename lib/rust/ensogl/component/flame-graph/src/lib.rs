//! An EnsoGL implementation of a basic flame graph.

#![recursion_limit = "256"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;

use enso_profiler_flame_graph as profiler_flame_graph;
use ensogl_core::application::Application;
use ensogl_core::display;



mod block;


pub use block::Block;



// ========================
// === Layout Constants ===
// ========================

const ROW_HEIGHT: f64 = 20.0;
const ROW_PADDING: f64 = 5.0;



// ===================
// === Flame Graph ===
// ===================


/// EnsoGL FlameGraph component. Consists of stacked blocks that indicate hierarchical time
/// intervals. The blocks can be hovered to reveal a label associated with the block.
#[derive(Debug)]
pub struct FlameGraph {
    display_object: display::object::Instance,
    blocks:         Vec<Block>,
}

/// Instantiate a `Block` shape for the given block data from the profiler.
fn shape_from_block(block: profiler_flame_graph::Block, app: &Application) -> Block {
    let component = app.new_view::<Block>();

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
    /// Create a `FlameGraph` EnsoGL component from the given graph data from the profiler.
    pub fn from_data(data: profiler_flame_graph::Graph, app: &Application) -> Self {
        let logger = Logger::new("FlameGraph");
        let display_object = display::object::Instance::new(&logger);

        let min_time =
            data.blocks.iter().map(|block| block.start.floor() as u32).min().unwrap_or_default();

        let blocks_zero_aligned = data.blocks.into_iter().map(|mut block| {
            block.start -= min_time as f64;
            block.end -= min_time as f64;
            block
        });

        let blocks = blocks_zero_aligned.map(|block| shape_from_block(block, app)).collect_vec();
        blocks.iter().for_each(|item| display_object.add_child(item));
        Self { display_object, blocks }
    }

    /// Return a reference to the blocks that make up the flame graph.
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }
}

impl display::Object for FlameGraph {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
