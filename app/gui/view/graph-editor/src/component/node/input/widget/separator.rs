//! Separating line between top-level argument widgets.

use super::prelude::*;
use crate::prelude::*;

use crate::component::node::input::port::PORT_PADDING_X;
use crate::component::node::HEIGHT as NODE_HEIGHT;
use ensogl::data::color;
use ensogl::display::object;
use ensogl::display::shape::Rectangle;



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
            && (kind.is_expected_argument()
                || kind.is_prefix_argument()
                || kind.is_named_argument());
        if matches {
            Score::OnlyOverride
        } else {
            Score::Mismatch
        }
    }

    fn default_config(_: &ConfigContext) -> Configuration<Self::Config> {
        Configuration::inert(default())
    }

    fn root_object(&self) -> &object::Instance {
        &self.root
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        let root = object::Instance::new_named("widget::Separator");
        root.use_auto_layout()
            .set_row_flow()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        let separator = Rectangle();
        let network = &root.network;
        let style = ctx.cached_style::<Style>(network);
        frp::extend! { network
            eval style([separator] (style) {
                separator.set_size((style.width, NODE_HEIGHT));
                separator.set_margin_xy((style.margin + PORT_PADDING_X, -NODE_HEIGHT / 2.0));
                separator.set_color(style.color);
            });
        }
        Self { root, separator }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        ctx.builder.manage_margin();
        let child = ctx.builder.child_widget(ctx.span_node, ctx.info.nesting_level);
        self.root.replace_children(&[self.separator.display_object(), &child.root_object]);
    }
}
