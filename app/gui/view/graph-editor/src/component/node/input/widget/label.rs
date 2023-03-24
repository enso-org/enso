//! Definition of static text label widget.

use crate::prelude::*;

use crate::component::node::input::area::TEXT_SIZE;
use ensogl_component::text;


// =============
// === Label ===
// =============

#[derive(Debug, Clone, Copy, PartialEq, Default)]

/// Label widget configuration options.
pub struct Config {
    /// If true, the label will be displayed in a placeholder style.
    pub placeholder: bool,
}

/// Label widget. Always displays the span tree node's expression as text.
#[derive(Clone, Debug)]
pub struct Widget {
    label: text::Text,
}

impl super::SpanWidget for Widget {
    type Config = Config;
    fn new(config: &Config, ctx: super::ConfigContext) -> Self {
        let label = text::Text::new(ctx.app());
        label.set_property_default(text::Size(TEXT_SIZE));
        let text_layer = &ctx.app().display.default_scene.layers.above_nodes_text;
        text_layer.add(&label);

        // let label_display = label.display_object();
        // frp::new_network! { network
        //     text_size <- all(&label.width, &label.height);
        //     eval text_size((size) {
        //         label_display.set_size(*size);
        //     });
        // };

        let mut this = Self { label };
        this.configure(config, ctx);
        this
    }

    fn configure(&mut self, config: &Config, ctx: super::ConfigContext) {
        self.label.display_object().set_parent(ctx.parent_instance);

        let content = ctx.expression_at(ctx.span_tree_node.span());

        // let text_color = self.styles.get_color(theme::graph_editor::node::text);
        use ensogl::data::color;
        let text_color = if config.placeholder {
            color::Lcha::new(0.5, 0.0, 0.0, 1.0)
        } else {
            color::Lcha::new(0.0, 0.0, 0.0, 1.0)
        };
        self.label.set_property_default(text_color);
        self.label.set_content(content);
    }
}
