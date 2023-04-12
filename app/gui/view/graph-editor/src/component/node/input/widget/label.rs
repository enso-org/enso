//! Definition of static text label widget.

use crate::prelude::*;

use super::debug::InstanceWithBg;
use crate::component::node::input::area::TEXT_SIZE;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display::object;
use ensogl_component::text;

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
    }
}

/// Label widget. Always displays the span tree node's expression as text.
#[derive(Clone, Debug)]
pub struct Widget {
    frp:   Frp,
    root:  InstanceWithBg,
    label: text::Text,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &object::Instance {
        &self.root.outer
    }

    fn new(_: &Config, app: &Application, _: &super::WidgetsFrp) -> Self {
        // Embed the label in a vertically centered fixed height container, so that the label's
        // baseline is properly aligned to center and lines up with other labels in the line.

        let layers = &app.display.default_scene.layers;
        let root = InstanceWithBg::magenta();
        root.inner.set_size_y(TEXT_SIZE);
        let label = text::Text::new(app);
        label.set_property_default(text::Size(TEXT_SIZE));
        label.set_y(TEXT_SIZE);
        layers.above_nodes_text.add(&label);
        root.inner.add_child(&label);
        let frp = Frp::new();
        let network = &frp.network;

        let inner = root.inner.clone_ref();

        frp::extend! { network
            color_change <- frp.text_color.on_change();
            eval color_change((color) label.set_property_default(color));
            content_change <- frp.content.on_change();
            eval content_change([label] (content) {
                label.set_content(content);
            });

            width <- label.width.on_change();
            eval width((w) { inner.set_size_x(*w); });
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


        // let text_color = self.styles.get_color(theme::graph_editor::node::text);
        let text_color = if is_placeholder {
            color::Lcha::new(0.5, 0.0, 0.0, 1.0)
        } else {
            color::Lcha::new(0.0, 0.0, 0.0, 1.0)
        };


        let input = &self.frp.public.input;
        input.content.emit(content.clone());
        input.text_color.emit(text_color);
    }
}
