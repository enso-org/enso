//! A wrapper widget for top-level arguments. Displays the argument name next to the argument
//! widget. It is always requested as a child of a separator widget. This widget's node matching
//! rules determine whether or not the dedicated argument name label will be displayed next to the
//! separator.

use super::prelude::*;
use crate::prelude::*;

use crate::node::input::area::TEXT_SIZE;

use ensogl_component::text;



/// =============
/// === Style ===
/// =============

#[derive(Clone, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::widget::argument_name"]
struct Style {
    color:  color::Rgba,
    margin: f32,
    weight: f32,
}



// ==============
// === Widget ===
// ==============

/// Separator widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Config;


/// The widget that prints the argument name next to the argument value.
#[derive(Debug, display::Object)]
pub struct Widget {
    display_object:    object::Instance,
    arg_label_wrapper: object::Instance,
    arg_name:          frp::Source<ImString>,
}

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(ctx: &ConfigContext) -> Score {
        let kind = &ctx.span_node.kind;
        let matches = ctx.info.subtree_connection.is_none() && kind.is_prefix_argument();
        Score::allow_override_if(matches)
    }

    fn default_config(_: &ConfigContext) -> Configuration<Self::Config> {
        Configuration::always(Config)
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        let root = object::Instance::new_named("widget::ArgumentName");
        root.use_auto_layout()
            .set_row_flow()
            .set_children_alignment_left_center()
            .justify_content_center_y();

        let arg_label_wrapper = object::Instance::new_named("arg_label_wrapper");
        let arg_label = text::Text::new(ctx.app());
        arg_label.set_property_default(text::Size(TEXT_SIZE));
        arg_label_wrapper.add_child(&arg_label);

        let network = &root.network;
        let style = ctx.cached_style::<Style>(network);
        frp::extend! { network

            eval style([arg_label, arg_label_wrapper] (style) {
                arg_label.set_property_default(style.color);
                let weight = text::Weight::from(style.weight as u16);
                arg_label.set_property_default(weight);
                arg_label_wrapper.set_margin_right(style.margin);
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
        Self { display_object: root, arg_label_wrapper, arg_name }
    }

    fn configure(&mut self, _: &Config, ctx: ConfigContext) {
        ctx.builder.manage_margin();
        ctx.builder.manage_child_margins();

        let level = ctx.info.nesting_level;
        match ctx.span_node.kind.argument_name() {
            Some(arg_name) if !arg_name.is_empty() => {
                self.arg_name.emit(arg_name);
                let child = ctx.builder.child_widget(ctx.span_node, level);
                self.display_object
                    .replace_children(&[&self.arg_label_wrapper, &child.root_object]);
            }
            _ => {
                let child = ctx.builder.child_widget(ctx.span_node, level);
                self.display_object.replace_children(&[&child.root_object]);
            }
        }
    }
}
