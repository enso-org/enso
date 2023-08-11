//! A wrapper widget for top-level arguments. Displays a separating line next to an argument widget.
//! Handles named argument nodes, creating a child widget only for the argument value.

use super::prelude::*;
use crate::prelude::*;

use crate::component::node::HEIGHT as NODE_HEIGHT;



/// =============
/// === Style ===
/// =============

#[derive(Clone, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::widget::separator"]
struct Style {
    color:  color::Rgba,
    margin: f32,
    width:  f32,
}



// ==============
// === Widget ===
// ==============

/// Separator widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Config;


/// The widget that displays a separating line between top-level argument nodes, as well as prints
/// the argument name when applicable. The name is only printed in cases where it would not be
/// repeated as a label in the child widget.
#[derive(Debug, display::Object)]
pub struct Widget {
    display_object: object::Instance,
    separator:      Rectangle,
}

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(ctx: &ConfigContext) -> Score {
        let kind = &ctx.span_node.kind;
        let matches = ctx.info.nesting_level.is_primary()
            && (kind.is_expected_argument()
                || kind.is_prefix_argument()
                || kind.is_named_argument());
        Score::allow_override_if(matches)
    }

    fn default_config(_: &ConfigContext) -> Configuration<Self::Config> {
        Configuration::inert(Config)
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        let display_object = object::Instance::new_named("widget::Separator");
        display_object
            .use_auto_layout()
            .set_row_flow()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        let separator = Rectangle();

        let network = &display_object.network;
        let style = ctx.cached_style::<Style>(network);
        frp::extend! { network
            eval style([separator] (style) {
                separator.set_size((style.width, NODE_HEIGHT));
                separator.set_margin_xy((style.margin, -NODE_HEIGHT / 2.0));
                separator.set_color(style.color);
            });
        }
        Self { display_object, separator }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        ctx.builder.manage_margin();
        ctx.builder.manage_child_margins();
        let level = ctx.info.nesting_level;

        // Request separator children widgets to have argument name displayed. This configuration
        // will only apply to nodes that need it, as the `argument_name` widget will only match
        // relevant nodes.
        let argument_name_config = super::argument_name::Widget::default_config(&ctx).into_dyn();
        let child_config = Some(&argument_name_config);

        // NOTE: In order to have proper port behavior for named arguments, the separator widget
        // must handle the named argument nodes, and create a child only for the value part. It is
        // the argument value node that receives the port, and we want that port to be displayed
        // over the argument name as well.
        let child = if ctx.span_node.kind.is_named_argument() {
            let mut child_iter = ctx.span_node.children_iter();
            let _name_node = child_iter.next();
            let _token = child_iter.next();
            let Some(arg_node) = child_iter.next() else { return };
            ctx.builder.child_widget_of_type(arg_node, level, child_config)
        } else {
            ctx.builder.child_widget_of_type(ctx.span_node, level, child_config)
        };

        let separator = self.separator.display_object();
        self.display_object.replace_children(&[separator, &child.root_object]);
    }
}
