//! Definition of static text label widget.

use crate::prelude::*;

use crate::component::node::input::area::TEXT_SIZE;

use ensogl::data::color;
use ensogl::display::object;
use ensogl::display::shape::StyleWatch;
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
        text_color(ColorState),
        text_weight(text::Weight),
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
        let root = object::Instance::new_named("widget::Label");
        let label = text::Text::new(app);
        label.set_property_default(text::Size(TEXT_SIZE));
        layers.label.add(&label);
        root.add_child(&label);
        let frp = Frp::new();
        let network = &frp.network;

        let styles = ctx.styles();
        frp::extend! { network
            parent_port_hovered <- widgets_frp.on_port_hover.map2(&frp.crumbs, |h, crumbs| {
                h.on().map_or(false, |h| crumbs.starts_with(h))
            });
            label_color <- frp.text_color.all_with4(
                &parent_port_hovered, &widgets_frp.set_view_mode, &widgets_frp.set_profiling_status,
                f!([styles](state, hovered, mode, status) {
                    state.to_color(*hovered, *mode, *status, &styles)
                })
            );

            label_color <- label_color.on_change();
            label_weight <- frp.text_weight.on_change();
            eval label_color((color) label.set_property_default(color));
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

    fn configure(&mut self, _: &Config, ctx: super::ConfigContext) {
        let is_placeholder = ctx.span_node.is_expected_argument();

        let content = if is_placeholder {
            ctx.span_node.kind.argument_name().unwrap_or_default()
        } else {
            ctx.expression_at(ctx.span_node.span())
        };

        let is_connected = ctx.info.subtree_connection.is_some();
        let color_state = match () {
            _ if is_connected => ColorState::Connected,
            _ if ctx.info.disabled => ColorState::Disabled,
            _ if is_placeholder => ColorState::Placeholder,
            _ => {
                let span_node_type = ctx.span_node.kind.tp();
                let usage_type = ctx.info.usage_type.clone();
                let ty = usage_type.or_else(|| span_node_type.map(|t| crate::Type(t.into())));
                let color = crate::type_coloring::compute_for_code(ty.as_ref(), ctx.styles());
                ColorState::FromType(color)
            }
        };

        let ext = ctx.get_extension_or_default::<Extension>();
        let text_weight = if ext.bold { text::Weight::Bold } else { text::Weight::Normal };
        let input = &self.frp.public.input;
        input.content.emit(content);
        input.text_color.emit(color_state);
        input.text_weight(text_weight);
        input.crumbs.emit(ctx.span_node.crumbs.clone());
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
    Connected,
    Disabled,
    Placeholder,
    FromType(color::Lcha),
}

impl ColorState {
    fn to_color(
        self,
        is_hovered: bool,
        view_mode: crate::view::Mode,
        status: crate::node::profiling::Status,
        styles: &StyleWatch,
    ) -> color::Lcha {
        use theme::code::syntax;
        let profiling_mode = view_mode.is_profiling();
        let profiled = profiling_mode && status.is_finished();
        let color_path = match self {
            _ if is_hovered => theme::code::types::selected,
            ColorState::Connected => theme::code::types::selected,
            ColorState::Disabled if profiled => syntax::profiling::disabled,
            ColorState::Placeholder if profiled => syntax::profiling::expected,
            ColorState::Disabled => syntax::disabled,
            ColorState::Placeholder => syntax::expected,
            ColorState::FromType(_) if profiled => syntax::profiling::base,
            ColorState::FromType(_) if profiling_mode => syntax::base,
            ColorState::FromType(typed) => return typed,
        };

        styles.get_color(color_path).into()
    }
}
