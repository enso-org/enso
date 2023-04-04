//! Definition of default hierarchy widget. This widget expands each child of its span tree into
//! a new widget.

use crate::prelude::*;

use ensogl::application::Application;
use ensogl::display::object;

// =================
// === Hierarchy ===
// ================

#[derive(Debug, Clone, Copy, PartialEq, Default)]

/// Label widget configuration options.
pub struct Config;

/// Hierarchy widget. This widget expands each child of its span tree into a new widget.
#[derive(Clone, Debug)]
pub struct Widget {
    display_object: super::debug::InstanceWithBg,
}

/// Width of a single space glyph
// TODO: avoid using hardcoded value. See https://www.pivotaltracker.com/story/show/183567623.
pub const SPACE_GLYPH_WIDTH: f32 = 7.224_609_4;

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &object::Instance {
        &self.display_object.outer
    }

    fn new(_: &Config, _: &Application, _: &super::WidgetsFrp) -> Self {
        let display_object = super::debug::InstanceWithBg::olive();
        display_object.inner.use_auto_layout();
        display_object.inner.set_children_alignment_left_center().justify_content_center_y();
        Self { display_object }
    }

    fn configure(&mut self, _: &Config, ctx: super::ConfigContext) {
        let offset = ctx.span_tree_node.sibling_offset.as_usize() as f32;
        self.display_object.inner.set_padding_left(offset * SPACE_GLYPH_WIDTH);
        self.display_object.inner.remove_all_children();

        let preserve_depth =
            ctx.span_tree_node.is_chained() || ctx.span_tree_node.is_named_argument();
        let next_depth = if preserve_depth { ctx.depth } else { ctx.depth + 1 };

        for node in ctx.span_tree_node.children_iter() {
            let child = ctx.builder.child_widget(node, next_depth);
            self.display_object.inner.add_child(&child.root);
        }
    }
}
