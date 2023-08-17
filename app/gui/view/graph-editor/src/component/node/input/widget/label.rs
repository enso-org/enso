//! Definition of static text label widget.

use super::prelude::*;
use crate::prelude::*;

use crate::component::node::input::area::TEXT_SIZE;

use ensogl_component::text;
use span_tree::node::Kind;



// =============
// === Style ===
// =============

#[derive(Clone, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::widget::label"]
struct Style {
    base_color:         color::Rgba,
    base_weight:        f32,
    connected_color:    color::Rgba,
    connected_weight:   f32,
    disabled_color:     color::Rgba,
    disabled_weight:    f32,
    placeholder_color:  color::Rgba,
    placeholder_weight: f32,
    pending_alpha:      f32,
}

// ==============
// === Widget ===
// ==============

/// Label widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Config;

ensogl::define_endpoints_2! {
    Input {
        content(ImString),
        text_color(ColorState),
        text_pending(bool),
        text_weight(Option<text::Weight>),
        text_sdf_weight(f32),
    }
}

/// Label widget. Always displays the span tree node's expression as text.
#[derive(Debug, display::Object)]
pub struct Widget {
    frp:            Frp,
    display_object: object::Instance,
    #[allow(dead_code)]
    label:          text::Text,
}

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(_: &ConfigContext) -> Score {
        Score::Good
    }

    fn default_config(ctx: &ConfigContext) -> Configuration<Self::Config> {
        let kind = &ctx.span_node.kind;
        let expr = ctx.span_expression();
        let not_port = matches!(kind, Kind::Token | Kind::NamedArgument | Kind::Access)
            || matches!(kind, Kind::Operation if expr == ".");
        Configuration::maybe_with_port(Config, !not_port)
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        // Embed the label in a vertically centered fixed height container, so that the label's
        // baseline is properly aligned to center and lines up with other labels in the line.
        let app = ctx.app();
        let widgets_frp = ctx.frp();
        let display_object = object::Instance::new_named("widget::Label");
        let label = text::Text::new(app);
        label.set_property_default(text::Size(TEXT_SIZE));
        display_object.add_child(&label);
        let frp = Frp::new();
        let network = &frp.network;

        let color_anim = color::Animation::new(network);
        let weight_anim = ensogl::Animation::new(network);
        weight_anim.precision.emit(0.001);

        let style = ctx.cached_style::<Style>(network);
        frp::extend! { network
            let id = ctx.info.identity;
            parent_port_hovered <- widgets_frp.hovered_port_children.map(move |h| h.contains(&id));
            parent_port_hovered <- parent_port_hovered.on_change();
            text_color <- frp.text_color.on_change();
            text_pending <- frp.text_pending.on_change();
            label_color <- all_with4(
                &style, &text_color, &parent_port_hovered, &text_pending,
                |style, state, hovered, text_pending| state.to_color(*hovered, style, *text_pending)
            ).debounce().on_change();
            color_anim.target <+ label_color;
            eval color_anim.value((color) label.set_property_default(color));

            weight_anim.target <+ frp.text_sdf_weight.on_change();
            eval weight_anim.value((weight) label.set_property_default(text::SdfWeight(*weight)));

            label_weight <- all_with4(
                &frp.text_color,
                &frp.text_weight,
                &style,
                &parent_port_hovered,
                |state, weight, style, hovered| state.to_weight(*hovered, *weight, style)
            ).debounce().on_change();
            eval label_weight((weight) label.set_property_default(weight));

            content_change <- frp.content.on_change();
            eval content_change((content) label.set_content(content));

            width <- label.width.on_change();
            height <- label.height.on_change();
            eval width((w) display_object.set_size_x(*w); );
            eval height([display_object, label] (h) {
                display_object.set_size_y(*h);
                label.set_y(*h);
            });
        }

        Self { frp, display_object, label }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        let is_placeholder = ctx.span_node.is_expected_argument();

        let expr = ctx.span_expression();
        let content = if is_placeholder || ctx.info.connection.is_some() {
            ctx.span_node.kind.argument_name().unwrap_or(expr)
        } else {
            expr
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
        let text_weight = bold.then_some(text::Weight::Bold);

        let input = &self.frp.public.input;
        input.content(content);
        input.text_color(color_state);
        input.text_pending(ctx.info.pending);
        input.text_weight(text_weight);
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
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum ColorState {
    #[default]
    Base,
    Connected,
    Disabled,
    Placeholder,
}

impl ColorState {
    fn to_weight(
        self,
        is_hovered: bool,
        weight_override: Option<text::Weight>,
        style: &Style,
    ) -> text::Weight {
        weight_override.unwrap_or_else(|| {
            let weight_num = match self {
                _ if is_hovered => style.connected_weight,
                ColorState::Base => style.base_weight,
                ColorState::Connected => style.connected_weight,
                ColorState::Disabled => style.disabled_weight,
                ColorState::Placeholder => style.placeholder_weight,
            };
            text::Weight::from(weight_num as u16)
        })
    }

    fn to_color(self, is_hovered: bool, style: &Style, text_pending: bool) -> color::Lcha {
        let base_color = color::Lcha::from(match self {
            _ if is_hovered => style.connected_color,
            ColorState::Base => style.base_color,
            ColorState::Connected => style.connected_color,
            ColorState::Disabled => style.disabled_color,
            ColorState::Placeholder => style.placeholder_color,
        });
        match text_pending {
            true => base_color.multiply_alpha(style.pending_alpha),
            false => base_color,
        }
    }
}
