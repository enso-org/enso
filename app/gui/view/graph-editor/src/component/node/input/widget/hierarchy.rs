//! Definition of default hierarchy widget. This widget expands each child of its span tree into
//! a new widget.

use crate::prelude::*;

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
    display_object: object::Instance,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &object::Instance {
        &self.display_object
    }

    fn new(_: &Config, _: &super::ConfigContext) -> Self {
        let display_object = object::Instance::new();
        display_object.use_auto_layout();
        display_object.set_children_alignment_left_center().justify_content_center_y();
        Self { display_object }
    }

    fn configure(&mut self, _: &Config, ctx: super::ConfigContext) {
        let increase_depth = ctx.span_tree_node.is_argument();
        let next_depth = if increase_depth { ctx.state.depth + 1 } else { ctx.state.depth };

        let children = ctx
            .span_tree_node
            .children_iter()
            .map(|node| ctx.builder.child_widget(node, next_depth))
            .collect_vec();
        self.display_object.replace_children(&children);
    }
}
