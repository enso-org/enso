//! Definition of static text label widget.

use crate::prelude::*;

use crate::component::node::input::area::TEXT_SIZE;
use ensogl::data::color;
use ensogl::display::object;
use ensogl_component::text;
use ensogl_hardcoded_theme as theme;

// =============
// === Label ===
// =============

/// Label widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Config;

ensogl::define_endpoints_2! {
    Input {
        content(ImString),
        text_color(color::Lcha),
        crumbs(span_tree::Crumbs),
    }
}

/// Label widget. Always displays the span tree node's expression as text.
#[derive(Clone, Debug)]
pub struct Widget {
    frp:   Frp,
    root:  object::Instance,
    #[allow(dead_code)]
    label: text::Text,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &object::Instance {
        &self.root
    }

    fn new(_: &Config, ctx: &super::ConfigContext) -> Self {
        // Embed the label in a vertically centered fixed height container, so that the label's
        // baseline is properly aligned to center and lines up with other labels in the line.
        let app = ctx.app();
        let widgets_frp = ctx.frp();
        let layers = &ctx.app().display.default_scene.layers;
        let root = object::Instance::new();
        root.set_size_y(TEXT_SIZE);
        let label = text::Text::new(app);
        label.set_property_default(text::Size(TEXT_SIZE));
        label.set_y(TEXT_SIZE);
        layers.above_nodes_text.add(&label);
        root.add_child(&label);
        let frp = Frp::new();
        let network = &frp.network;

        let hover_color: color::Lcha = ctx.styles().get_color(theme::code::types::selected).into();

        frp::extend! { network
            color_change <- frp.text_color.on_change();
            parent_port_hovered <- widgets_frp.on_port_hover.map2(&frp.crumbs, |h, crumbs| {
                h.on().map_or(false, |h| crumbs.starts_with(h))
            });
            label_color <- color_change.all_with(&parent_port_hovered, move |color, hovered| {
                if *hovered { hover_color } else { *color }
            });

            label_color <- label_color.on_change();
            eval label_color((color) label.set_property_default(color));
            content_change <- frp.content.on_change();
            eval content_change((content) label.set_content(content));

            width <- label.width.on_change();
            eval width((w) root.set_size_x(*w); );
        }

        Self { frp, root, label }
    }

    fn configure(&mut self, _: &Config, ctx: super::ConfigContext) {
        let is_placeholder = ctx.span_tree_node.is_expected_argument();

        let content = if is_placeholder {
            ctx.span_tree_node.kind.argument_name().unwrap_or_default()
        } else {
            ctx.expression_at(ctx.span_tree_node.span())
        };

        let text_color: color::Lcha = if ctx.state.subtree_connection.is_connected() {
            ctx.styles().get_color(theme::code::types::selected).into()
        } else if ctx.state.disabled {
            ctx.styles().get_color(theme::code::syntax::disabled).into()
        } else if is_placeholder {
            ctx.styles().get_color(theme::code::syntax::expected).into()
        } else {
            let ty = ctx.state.usage_type.clone();
            let ty = ty.or_else(|| ctx.span_tree_node.kind.tp().map(|t| crate::Type(t.into())));
            crate::type_coloring::compute_for_code(ty.as_ref(), ctx.styles())
        };

        let input = &self.frp.public.input;
        input.content.emit(content);
        input.text_color.emit(text_color);
        input.crumbs.emit(ctx.span_tree_node.crumbs.clone());
    }
}
