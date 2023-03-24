//! Definition of default hierarchy widget. This widget expands each child of its span tree into
//! a new widget.

use crate::prelude::*;

use ensogl::display::object::Instance;

// =================
// === Hierarchy ===
// ================

#[derive(Debug, Clone, Copy, PartialEq, Default)]

/// Label widget configuration options.
pub struct Config;

/// Hierarchy widget. This widget expands each child of its span tree into a new widget.
#[derive(Clone, Debug)]
pub struct Widget {
    display_object: Instance,
    // shape:          debug_shape::View,
}

/// Width of a single space glyph
// TODO: avoid using hardcoded value. See https://www.pivotaltracker.com/story/show/183567623.
pub const SPACE_GLYPH_WIDTH: f32 = 7.224_609_4;

impl super::SpanWidget for Widget {
    type Config = Config;
    fn new(config: &Config, ctx: super::ConfigContext) -> Self {
        // TODO: add display object, so we can handle mouse events.
        let display_object = ctx.parent_instance.new_child();
        display_object.use_auto_layout();
        // display_object.set_size_y(100.pc());
        display_object.set_children_alignment_left_center();
        // let shape = debug_shape::View::new();
        // display_object.add_child(&shape);
        // shape.set_size((0, 1));


        let mut this = Self { display_object };
        this.configure(config, ctx);
        this
    }

    fn configure(&mut self, _config: &Config, ctx: super::ConfigContext) {
        self.display_object.set_parent(ctx.parent_instance);
        let offset = ctx.span_tree_node.sibling_offset.as_usize() as f32;
        self.display_object.set_padding_left(offset * SPACE_GLYPH_WIDTH);

        let preserve_depth =
            ctx.span_tree_node.is_chained() || ctx.span_tree_node.is_named_argument();
        let next_depth = if preserve_depth { ctx.depth } else { ctx.depth + 1 };

        for child in ctx.span_tree_node.children_iter() {
            ctx.builder.child_widget(&self.display_object, child, next_depth);
        }
    }
}


/// Temporary dropdown activation shape definition.
pub mod debug_shape {
    use super::*;
    ensogl::shape! {
        above = [
            crate::component::node::background,
            crate::component::node::input::port::hover
        ];
        (style:Style) {
            let color = Var::<color::Rgba>::from("srgba(1.0,0.0,0.0,0.1)");
            Rect(Var::canvas_size()).fill(color).into()
        }
    }
}
