//! A dedicated widget for A blank ('_') token in an argument position.
//!
//! See also [`span_tree::node::InsertionPoint`].

use super::prelude::*;
use crate::prelude::*;

use ensogl::display::object;

#[derive(Clone, Copy, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::widget::blank"]
struct Style {
    size:          Vector2,
    margin_sides:  f32,
    margin_top:    f32,
    corner_radius: f32,
    color:         color::Rgba,
}


// ======================
// === InsertionPoint ===
// ======================

/// Blank widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Config;


/// Blank widget. Displays a stylized underscore shape.
#[derive(Clone, Debug)]
pub struct Widget {
    rect: Rectangle,
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
        self.rect.display_object()
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        let rect = Rectangle();

        let network = &rect.display_object().network;
        let style = ctx.cached_style::<Style>();

        frp::extend! { network
            eval style.update ((style)
                rect.set_color(style.color)
                    .set_size(style.size)
                    .set_corner_radius(style.corner_radius)
                    .set_margin_trbl(style.margin_top, style.margin_sides, 0.0, style.margin_sides);
            );
        }
        Self { rect }
    }

    fn configure(&mut self, _: &Config, _: ConfigContext) {}
}
