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
mod mark;

use enso_profiler_flame_graph::State;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use mark::Mark;

pub use block::Block;



// ========================
// === Layout Constants ===
// ========================

const ROW_HEIGHT: f64 = 20.0;
const ROW_PADDING: f64 = 5.0;
pub(crate) const BASE_TEXT_SIZE: f32 = 18.0;



// ===================
// === Flame Graph ===
// ===================


/// EnsoGL FlameGraph component. Consists of stacked blocks that indicate hierarchical time
/// intervals. The blocks can be hovered to reveal a label associated with the block.
#[derive(Debug)]
pub struct FlameGraph {
    display_object: display::object::Instance,
    blocks:         Vec<Block>,
    marks:          Vec<Mark>,
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

    let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);

    let color: color::Rgba = match block.state {
        State::Active => style.get_color("flame_graph_block_color_active").value(),
        State::Paused => style.get_color("flame_graph_block_color_paused").value(),
    };
    let color: color::Lcha = color.into();
    component.set_color(color);

    component
}

/// Instantiate a `Mark` shape for the given block data from the profiler.
fn shape_from_mark(mark: profiler_flame_graph::Mark, app: &Application) -> Mark {
    let component = app.new_view::<Mark>();
    let x = mark.position as f32;
    let y = 0.0;
    let pos = Vector2::new(x, y);

    let label = format!("{} ({:.1}ms)", mark.label, mark.position);

    component.set_content.emit(label);
    component.set_position_xy(pos);

    component
}

const MIN_INTERVAL_TIME_MS: f64 = 0.0;
const X_SCALE: f64 = 1.0;

impl FlameGraph {
    /// Create a `FlameGraph` EnsoGL component from the given graph data from the profiler.
    pub fn from_data(data: profiler_flame_graph::Graph, app: &Application) -> Self {
        let logger = Logger::new("FlameGraph");
        let display_object = display::object::Instance::new(&logger);

        let blocks = data.blocks.into_iter().filter(|block| block.width() > MIN_INTERVAL_TIME_MS);
        let marks = data.marks;

        let origin_x =
            blocks.clone().map(|block| block.start.floor() as u32).min().unwrap_or_default();

        let blocks_zero_aligned = blocks.clone().map(|mut block| {
            // Shift
            block.start -= origin_x as f64;
            block.end -= origin_x as f64;
            // Scale
            block.start *= X_SCALE;
            block.end *= X_SCALE;
            block
        });
        let blocks = blocks_zero_aligned.map(|block| shape_from_block(block, app)).collect_vec();
        blocks.iter().for_each(|item| display_object.add_child(item));


        let blocks_marks_aligned = marks.into_iter().map(|mut mark| {
            mark.position -= origin_x as f64;
            mark.position *= X_SCALE;
            mark
        });
        let marks: Vec<_> =
            blocks_marks_aligned.into_iter().map(|mark| shape_from_mark(mark, app)).collect();
        marks.iter().for_each(|item| display_object.add_child(item));

        Self { display_object, blocks, marks }
    }

    /// Return a reference to the blocks that make up the flame graph.
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }

    /// Return a reference to the marks that make up the flame graph.
    pub fn marks(&self) -> &[Mark] {
        &self.marks
    }
}

impl display::Object for FlameGraph {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
