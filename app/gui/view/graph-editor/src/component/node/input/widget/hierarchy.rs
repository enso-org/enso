//! Definition of default hierarchy widget. This widget expands each child of its span tree into
//! a new widget.

use crate::component::node::input::widget::prelude::*;
use crate::prelude::*;

use ensogl::display::object;



// ===============
// === Aliases ===
// ===============

/// A collection type used to collect a temporary list of node child widget roots, so that they can
/// be passed to `replace_children` method in one go. Avoids allocation for small number of
/// children, but also doesn't consume too much stack memory to avoid stack overflow in deep widget
/// hierarchies.
pub type CollectedChildren = SmallVec<[object::Instance; 4]>;



// =================
// === Hierarchy ===
// =================

/// Label widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Config;

/// Hierarchy widget. This widget expands each child of its span tree into a new widget.
#[derive(Clone, Debug)]
pub struct Widget {
    display_object: object::Instance,
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
        let has_port = !ctx.span_node.kind.is_named_argument();
        Configuration::maybe_with_port(default(), has_port)
    }

    fn root_object(&self) -> &object::Instance {
        &self.display_object
    }

    fn new(_: &Config, _: &ConfigContext) -> Self {
        let display_object = object::Instance::new_named("widget::Hierarchy");
        display_object.use_auto_layout();
        display_object.set_children_alignment_left_center().justify_content_center_y();
        Self { display_object }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        let child_level = ctx.info.nesting_level.next_if(ctx.span_node.is_argument());
        let children_iter = ctx.span_node.children_iter();
        let children =
            children_iter.map(|node| ctx.builder.child_widget(node, child_level).root_object);
        self.display_object.replace_children(&children.collect::<CollectedChildren>());
    }
}
