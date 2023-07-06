//! Separating line between top-level argument widgets.

use super::prelude::*;
use crate::prelude::*;

use crate::component::node::HEIGHT as NODE_HEIGHT;
use ensogl::data::color;
use ensogl::display::object;
use ensogl::display::shape::Rectangle;



// =================
// === Separator ===
// =================

/// Insertion point widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Config;


/// Insertion point widget. Displays nothing when not connected.
#[derive(Clone, Debug)]
pub struct Widget {
    separator: Rectangle,
    root:      object::Instance,
}

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(ctx: &ConfigContext) -> Score {
        let kind = &ctx.span_node.kind;
        let matches = ctx.info.nesting_level.is_primary()
            && (kind.is_expected_argument() || kind.definition_index().is_some());
        Score::only_if(matches)
    }

    fn default_config(_: &ConfigContext) -> Configuration<Self::Config> {
        Configuration::inert(default())
    }

    fn root_object(&self) -> &object::Instance {
        &self.root
    }

    fn new(_: &Config, _: &ConfigContext) -> Self {
        let root = object::Instance::new_named("widget::Separator");
        root.use_auto_layout()
            .set_row_flow()
            .set_children_alignment_left_center()
            .justify_content_center_y();
        let separator = Rectangle();
        separator.set_size((2.0, NODE_HEIGHT)).set_margin_xy((7.0, 0.0));
        separator.set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.12));
        Self { root, separator }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        ctx.builder.manage_margin();
        let child = ctx.builder.child_widget(ctx.span_node, ctx.info.nesting_level);
        self.root.replace_children(&[self.separator.display_object(), &child.root_object]);
    }
}
