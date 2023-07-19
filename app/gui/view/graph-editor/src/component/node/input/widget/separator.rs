//! Separating line between top-level argument widgets.

use super::prelude::*;
use crate::prelude::*;

use crate::component::node::input::port::PORT_PADDING_X;
use crate::component::node::HEIGHT as NODE_HEIGHT;
use crate::component::node::input::area::TEXT_SIZE;

use ensogl::data::color;
use ensogl::display::object;
use ensogl::display::shape::Rectangle;
use ensogl_component::text;

/// =============
/// === Style ===
/// =============

#[derive(Clone, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::widget::separator"]
struct Style {
    color:  color::Rgba,
    margin: f32,
    width:  f32,
    arg_name_color: color::Rgba,
    arg_name_weight: f32,
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
    arg_label_wrapper: object::Instance,
    arg_name:  frp::Source<ImString>,
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
        
        let arg_label_wrapper = object::Instance::new_named("arg_label_wrapper");
        let arg_label = text::Text::new(ctx.app());
        arg_label.set_property_default(text::Size(TEXT_SIZE));
        arg_label_wrapper.add_child(&arg_label);

        let network = &root.network;
        let style = ctx.cached_style::<Style>(network);
        frp::extend! { network
            eval style([separator, arg_label, arg_label_wrapper] (style) {
                separator.set_size((style.width, NODE_HEIGHT));
                separator.set_margin_xy((style.margin + PORT_PADDING_X, -NODE_HEIGHT / 2.0));
                separator.set_color(style.color);
                arg_label.set_property_default(style.arg_name_color);
                arg_label.set_property_default(text::Weight::from(style.arg_name_weight as u16));
                // Argument label is not always visible, but when it is, it is drawn after the
                // separator. The left margin needs to be adjusted to cancel extra separator margin.
                arg_label_wrapper.set_margin_left(-PORT_PADDING_X);
                arg_label_wrapper.set_margin_right(style.margin + PORT_PADDING_X);
            });
            arg_name <- source();
            arg_label.set_content <+ arg_name.on_change();
            label_width <- arg_label.width.on_change();
            label_height <- arg_label.height.on_change();
            eval label_width((w) arg_label_wrapper.set_size_x(*w); );
            eval label_height([arg_label_wrapper, arg_label] (h) {
                arg_label_wrapper.set_size_y(*h);
                arg_label.set_y(*h);
            });
        }
        Self { root, separator, arg_label_wrapper, arg_name }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        ctx.builder.manage_margin();
        let level = ctx.info.nesting_level;

        let arg_name: Option<ImString>;
        let child_node;

        if ctx.span_node.kind.is_named_argument() {
            let mut child_iter = ctx.span_node.clone().children_iter();
            let name_node = child_iter.next();
            let _token = child_iter.next();
            let arg_node = child_iter.next();

            arg_name = name_node.map(|node| ctx.expression_at(node.span())).map(Into::into);
            child_node = arg_node.unwrap_or(ctx.span_node);
        } else {
            let show_arg = !ctx.span_node.kind.is_expected_argument();
            arg_name = show_arg
                .and_option_from(|| ctx.span_node.kind.argument_name())
                .map(Into::into);
            child_node = ctx.span_node;
        }

        let separator = self.separator.display_object();
        let label = self.arg_label_wrapper.display_object();
        let child = ctx.builder.child_widget(child_node, level);
        if let Some(arg_name) = arg_name {
            self.arg_name.emit(arg_name);
            self.root.replace_children(&[separator, label, &child.root_object]);
        } else {
            self.root.replace_children(&[separator, &child.root_object]);
        }
    }
}
