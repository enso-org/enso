//! Definition of static text label widget.

use super::prelude::*;
use crate::prelude::*;

use crate::component::node::input::area::TEXT_SIZE;

use ensogl::data::color;
use ensogl::display::object;
use ensogl_component::text;


/// =============
/// === Style ===
/// =============

#[derive(Clone, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::code::syntax"]
struct Style {
    base:      color::Rgba,
    selection: color::Rgba,
    disabled:  color::Rgba,
    expected:  color::Rgba,
}

// =============
// === Label ===
// =============

/// Label widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Config;

ensogl::define_endpoints_2! {
    Input {
        content(ImString),
        text_color(ColorState),
        text_weight(text::Weight),
        text_sdf_weight(f32),
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

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(_: &ConfigContext) -> Score {
        Score::Good
    }

    fn default_config(ctx: &ConfigContext) -> Configuration<Self::Config> {
        use span_tree::node::Kind;
        let has_port = !matches!(ctx.span_node.kind, Kind::Token | Kind::NamedArgument);
        Configuration::maybe_with_port(default(), has_port)
    }

    fn root_object(&self) -> &object::Instance {
        &self.root
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        // Embed the label in a vertically centered fixed height container, so that the label's
        // baseline is properly aligned to center and lines up with other labels in the line.
        let app = ctx.app();
        let widgets_frp = ctx.frp();
        let root = object::Instance::new_named("widget::Label");
        let label = text::Text::new(app);
        label.set_property_default(text::Size(TEXT_SIZE));
        root.add_child(&label);
        let frp = Frp::new();
        let network = &frp.network;

        let color_anim = color::Animation::new(network);
        let weight_anim = ensogl::Animation::new(network);
        weight_anim.precision.emit(0.001);

        let style = Style::from_theme(network, ctx.styles());
        frp::extend! { network
            let id = ctx.info.identity;
            parent_port_hovered <- widgets_frp.hovered_port_children.map(move |h| h.contains(&id));
            label_color <- frp.text_color.all_with3(&style.update, &parent_port_hovered,
                |state, style, hovered| state.to_color(*hovered, style)
            );

            color_anim.target <+ label_color.on_change();
            eval color_anim.value((color) label.set_property_default(color));

            weight_anim.target <+ frp.text_sdf_weight.on_change();
            eval weight_anim.value((weight) label.set_property_default(text::SdfWeight(*weight)));

            label_weight <- frp.text_weight.on_change();
            eval label_weight((weight) label.set_property_default(weight));

            content_change <- frp.content.on_change();
            eval content_change((content) label.set_content(content));

            width <- label.width.on_change();
            height <- label.height.on_change();
            eval width((w) root.set_size_x(*w); );
            eval height([root, label] (h) {
                root.set_size_y(*h);
                label.set_y(*h);
            });
        }

        Self { frp, root, label }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        let is_placeholder = ctx.span_node.is_expected_argument();

        let content = if is_placeholder {
            ctx.span_node.kind.argument_name().unwrap_or_default()
        } else {
            ctx.span_expression()
        };

        let is_connected = ctx.info.subtree_connection.is_some();
        let color_state = match () {
            _ if is_connected => ColorState::Connected,
            _ if ctx.info.disabled => ColorState::Disabled,
            _ if is_placeholder => ColorState::Placeholder,
            _ => ColorState::Base,
        };

        let ext = ctx.get_extension_or_default::<Extension>();
        let bold = ext.bold || is_placeholder;
        let text_weight = if bold { text::Weight::Bold } else { text::Weight::Normal };
        // let text_weight = text::Weight::Normal;
        // let sdf_weight = if ext.bold || is_placeholder { 0.03 } else { 0.0 };

        let input = &self.frp.public.input;
        input.content.emit(content);
        input.text_color.emit(color_state);
        input.text_weight(text_weight);
        // input.text_sdf_weight(sdf_weight);
    }
}



// =================
// === Extension ===
// =================

/// Label extension data that can be set by any of the parent widgets.
#[derive(Clone, Copy, Debug, Default)]
pub struct Extension {
    /// Display all descendant labels with bold text weight.
    pub bold: bool,
}



// ==================
// === ColorState ===
// ==================

/// Configured color state of a label widget.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, Default)]
pub enum ColorState {
    #[default]
    Base,
    Connected,
    Disabled,
    Placeholder,
}

impl ColorState {
    fn to_color(self, is_hovered: bool, style: &Style) -> color::Lcha {
        match self {
            _ if is_hovered => style.selection,
            ColorState::Base => style.base,
            ColorState::Connected => style.selection,
            ColorState::Disabled => style.disabled,
            ColorState::Placeholder => style.expected,
        }
        .into()
    }
}
