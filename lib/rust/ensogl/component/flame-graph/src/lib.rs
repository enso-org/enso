//! An EnsoGL implementation of a basic flame graph.

#![recursion_limit = "256"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
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

use enso_profiler_flame_graph::Activity;
use enso_profiler_flame_graph::Performance;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::style;

pub use block::Block;
pub use mark::Mark;



// ========================
// === Layout Constants ===
// ========================

const ROW_HEIGHT: f64 = 20.0;
const ROW_PADDING: f64 = 5.0;



// ==============
// === Colors ===
// ==============

/// Theme path for the color of an activity block that is active.
pub const COLOR_BLOCK_ACTIVE: &str = "flame_graph_block_color_active";
/// Theme path for the color of an activity block that is paused.
pub const COLOR_BLOCK_PAUSED: &str = "flame_graph_block_color_paused";

/// Theme path for the color of a performance block that indicates good performance.
pub const COLOR_PERFORMANCE_GOOD: &str = "flame_graph_block_color_performance_good";
/// Theme path for the color of a performance block that indicates medium performance.
pub const COLOR_PERFORMANCE_MEDIUM: &str = "flame_graph_block_color_performance_medium";
/// Theme path for the color of a performance block that indicates bad performance..
pub const COLOR_PERFORMANCE_BAD: &str = "flame_graph_block_color_performance_bad";

/// Theme path for the color that is sued to color a mark.
pub const COLOR_MARK_DEFAULT: &str = "flame_graph_mark_color";


/// Trait that allows retrieval of a style::Path.
pub trait IntoThemePath {
    /// Return the `style::Path` associated with this object.
    fn theme_path(&self) -> style::Path;
}

impl IntoThemePath for Activity {
    fn theme_path(&self) -> style::Path {
        match self {
            Activity::Active => COLOR_BLOCK_ACTIVE,
            Activity::Paused => COLOR_BLOCK_PAUSED,
        }
        .into()
    }
}

impl IntoThemePath for Performance {
    fn theme_path(&self) -> style::Path {
        match self {
            Performance::Good => COLOR_PERFORMANCE_GOOD,
            Performance::Medium => COLOR_PERFORMANCE_MEDIUM,
            Performance::Bad => COLOR_PERFORMANCE_BAD,
        }
        .into()
    }
}

impl<BlockType: IntoThemePath> IntoThemePath for profiler_flame_graph::Block<BlockType> {
    fn theme_path(&self) -> style::Path {
        self.block_type.theme_path()
    }
}



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
    origin_x:       f64,
    app:            Application,
}

/// Instantiate a `Block` shape for the given block data from the profiler.
pub fn shape_from_block<BlockType: IntoThemePath>(
    block: profiler_flame_graph::Block<BlockType>,
    app: &Application,
) -> Block {
    let component = app.new_view::<Block>();

    let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
    let color: color::Rgba = style.get_color(block.theme_path()).value();
    let color: color::Lcha = color.into();
    component.set_color(color);

    let size = Vector2::new(block.width() as f32, ROW_HEIGHT as f32);
    let x = block.start + block.width() / 2.0;
    let y = block.row as f64 * (ROW_HEIGHT + ROW_PADDING);
    let pos = Vector2::new(x as f32, y as f32);

    component.set_content.emit(block.label);
    component.set_size.emit(size);
    component.set_position_xy(pos);

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

fn align_block<BlockType>(
    mut block: profiler_flame_graph::Block<BlockType>,
    origin_x: f64,
) -> profiler_flame_graph::Block<BlockType> {
    // Shift
    block.start -= origin_x;
    block.end -= origin_x;
    // Scale
    block.start *= X_SCALE;
    block.end *= X_SCALE;
    block
}

fn align_mark(mut mark: profiler_flame_graph::Mark, origin_x: f64) -> profiler_flame_graph::Mark {
    mark.position -= origin_x;
    mark.position *= X_SCALE;
    mark
}

impl FlameGraph {
    /// Create an empty graph,
    pub fn empty(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let blocks = default();
        let marks = default();
        let origin_x = default();
        let app = app.clone_ref();
        Self { display_object, blocks, marks, origin_x, app }
    }
    /// Create a `FlameGraph` EnsoGL component from the given graph data from the profiler.
    pub fn from_data(data: profiler_flame_graph::Graph, app: &Application) -> Self {
        let display_object = display::object::Instance::new();

        let activity_blocks =
            data.activity_blocks.into_iter().filter(|block| block.width() > MIN_INTERVAL_TIME_MS);
        let performance_blocks = data.performance_blocks.into_iter();
        let marks = data.marks;

        let origin_x = activity_blocks
            .clone()
            .map(|block| block.start.floor() as u32)
            .min()
            .unwrap_or_default() as f64;

        let activity_block_shapes =
            activity_blocks.map(|block| shape_from_block(align_block(block, origin_x), app));
        let performance_block_shapes =
            performance_blocks.map(|block| shape_from_block(align_block(block, origin_x), app));

        let blocks = activity_block_shapes.chain(performance_block_shapes).collect_vec();
        blocks.iter().for_each(|item| display_object.add_child(item));

        let marks: Vec<_> = marks
            .into_iter()
            .map(|mark| shape_from_mark(align_mark(mark, origin_x), app))
            .collect();
        marks.iter().for_each(|item| display_object.add_child(item));

        let app = app.clone_ref();
        Self { display_object, blocks, marks, origin_x, app }
    }

    /// Return a reference to the blocks that make up the flame graph.
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }

    /// Return a reference to the marks that make up the flame graph.
    pub fn marks(&self) -> &[Mark] {
        &self.marks
    }

    /// Add an additional activity block to the visualisation.
    pub fn add_block<BlockType: IntoThemePath>(
        &mut self,
        block: profiler_flame_graph::Block<BlockType>,
    ) {
        let block = align_block(block, self.origin_x);
        let shape = shape_from_block(block, &self.app);
        self.display_object.add_child(&shape);
        self.blocks.push(shape);
    }

    /// Add additional mark to the visualisation.
    pub fn add_mark(&mut self, mark: profiler_flame_graph::Mark) {
        let mark = align_mark(mark, self.origin_x);
        let shape = shape_from_mark(mark, &self.app);
        self.display_object.add_child(&shape);
        self.marks.push(shape);
    }

    /// Height of the graph as measured between the position of the highest and lowest block.
    pub fn height(&self) -> f32 {
        let min_max = self.blocks.iter().map(|block| block.position().y).minmax();
        min_max.into_option().map(|(min, max)| max - min).unwrap_or_default()
    }

    /// Origin of the time axis. This is the timestamp placed at the zero x-coordinate of the root
    /// display object.
    pub fn origin(&self) -> f64 {
        self.origin_x
    }

    /// Set the timestamp placed at the x-coordinate origin. For example, a value of 10.0, will mean
    /// that a block that starts at time 10ms, will be placed at the zero x-coordinate (the same
    /// position as the root display object).
    pub fn set_origin(&mut self, new_origin: f64) {
        let delta = (self.origin_x - new_origin) as f32;
        self.marks.iter().for_each(|mark| mark.mod_position_x(|pos| pos - delta));
        self.blocks.iter().for_each(|block| block.mod_position_x(|pos| pos - delta));
        self.origin_x = new_origin
    }
}

impl display::Object for FlameGraph {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
