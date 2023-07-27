//! Definition of default hierarchy widget. This widget expands each child of its span tree into
//! a new widget.

use super::prelude::*;
use crate::prelude::*;

use span_tree::node::Kind;



// ==============
// === Widget ===
// ==============

/// Label widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Config;

/// Hierarchy widget. This widget expands each child of its span tree into a new widget.
#[derive(Debug, display::Object)]
pub struct Widget {
    display_object: object::Instance,
    /// A temporary list of display object children to insert. Reused across reconfigurations to
    /// avoid allocations.
    children_vec:   SmallVec<[object::Instance; 4]>,
}

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(ctx: &ConfigContext) -> Score {
        match ctx.span_node.children.is_empty() {
            false => Score::Good,
            true => Score::Mismatch,
        }
    }

    fn default_config(ctx: &ConfigContext) -> Configuration<Self::Config> {
        let has_port = !matches!(
            ctx.span_node.kind,
            Kind::NamedArgument | Kind::ChainedPrefix | Kind::BlockLine
        );
        Configuration::maybe_with_port(Config, has_port)
    }

    fn new(_: &Config, _: &ConfigContext) -> Self {
        let display_object = object::Instance::new_named("widget::Hierarchy");
        display_object.use_auto_layout();
        display_object.set_children_alignment_left_center().justify_content_center_y();
        Self { display_object, children_vec: default() }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        let child_level = ctx.info.nesting_level.next_if(ctx.span_node.kind.is_prefix_argument());
        let is_primary = ctx.info.nesting_level.is_primary();

        // When this is a top-level (primary) hierarchy widget, request children widgets to have
        // separators. The "separator" widget is a wrapper which will display the default argument
        // widget next to the separating line. This configuration will only be applied to nodes that
        // the separator accepts, which is limited to arguments in prefix chains.
        let separator_config = super::separator::Widget::default_config(&ctx).into_dyn();
        let child_config = is_primary.then_some(&separator_config);

        self.children_vec.clear();
        self.children_vec.extend(ctx.span_node.children_iter().map(|node| {
            ctx.builder.child_widget_of_type(node, child_level, child_config).root_object
        }));

        self.display_object.replace_children(&self.children_vec);
        self.children_vec.clear();
    }
}
