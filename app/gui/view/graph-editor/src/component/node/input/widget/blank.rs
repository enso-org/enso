//! A dedicated widget for A blank ('_') token in an argument position.
//!
//! See also [`span_tree::node::InsertionPoint`].

use super::prelude::*;
use crate::prelude::*;

use ensogl::display::object;



/// =============
/// === Style ===
/// =============

#[derive(Clone, Copy, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::widget::blank"]
struct Style {
    size:          Vector2,
    margin_sides:  f32,
    margin_top:    f32,
    corner_radius: f32,
    color:         color::Rgba,
}



// ==============
// === Widget ===
// ==============

/// Blank widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Config;

/// Blank widget. Displays a stylized underscore shape.
#[derive(Clone, Debug)]
pub struct Widget {
    root:  object::Instance,
    _rect: Rectangle,
}

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(ctx: &ConfigContext) -> Score {
        let is_blank = ctx.span_node.is_argument() && ctx.span_expression() == "_";
        Score::only_if(is_blank)
    }

    fn default_config(_: &ConfigContext) -> Configuration<Self::Config> {
        Configuration::always(default())
    }

    fn root_object(&self) -> &object::Instance {
        &self.root
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        let root = object::Instance::new_named("widget::Blank");
        root.use_auto_layout();
        let rect = Rectangle();
        root.add_child(&rect);

        let network = &root.network;
        let style = ctx.cached_style::<Style>(network);

        frp::extend! { network
            eval style((style)
                rect.set_color(style.color)
                    .set_size(style.size)
                    .set_corner_radius(style.corner_radius)
                    .set_margin_trbl(style.margin_top, style.margin_sides, 0.0, style.margin_sides);
            );
        }
        Self { root, _rect: rect }
    }

    fn configure(&mut self, _: &Config, _: ConfigContext) {}
}
