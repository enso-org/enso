//! A widget representing a top-level method call. Displays node background. Instead of `self`
//! argument, it displays an icon as a first port.

use super::prelude::*;
use crate::prelude::*;

use ensogl::display;

use ide_view_component_list_panel_icons as icons;

use icons::any::View as AnyIcon;
use icons::component_icons::Id;


// =================
// === Constants ===
// =================

/// Distance between the icon and next widget.
const ICON_GAP: f32 = 20.0;



// ==============
// === Method ===
// ===============

/// Method widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Config {}

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Debug)]
#[allow(dead_code)]
pub struct Widget {
    display_object: display::object::Instance,
    icon_id:        Id,
    icon:           AnyIcon,
}

/// Extension which existence in given subtree prevents the widget from being created again.
struct MethodAlreadyInTree;

impl SpanWidget for Widget {
    type Config = Config;

    fn default_config(_: &ConfigContext) -> Configuration<Self::Config> {
        Configuration::always(Config {})
    }

    fn match_node(ctx: &ConfigContext) -> Score {
        let icon = ctx.span_node.application.as_ref().and_then(|a| a.icon_name.as_ref());
        if icon.is_some() && ctx.get_extension::<MethodAlreadyInTree>().is_none() {
            Score::Perfect
        } else {
            Score::Mismatch
        }
    }

    fn root_object(&self) -> &display::object::Instance {
        &self.display_object
    }

    fn new(_: &Config, _: &ConfigContext) -> Self {
        // ╭─display_object──────────────────╮
        // │ ╭ icon ─╮ ╭ content ──────────╮ │
        // │ │       │ │                   │ │
        // │ │       │ │                   │ │
        // │ ╰───────╯ ╰───────────────────╯ │
        // ╰─────────────────────────────────╯

        let display_object = display::object::Instance::new_named("widget::Method");
        display_object
            .use_auto_layout()
            .set_column_flow()
            .set_gap_x(ICON_GAP)
            .set_children_alignment_left_center()
            .justify_content_center_y();

        let icon_id = Id::default();
        let icon = icon_id.cached_view();
        Self { display_object, icon_id, icon }
    }

    fn configure(&mut self, _: &Config, mut ctx: ConfigContext) {
        ctx.set_extension(MethodAlreadyInTree);
        let icon_id = ctx
            .span_node
            .application
            .as_ref()
            .and_then(|app| app.icon_name.as_ref())
            .and_then(|name| name.parse::<Id>().ok())
            .unwrap_or_default();
        if icon_id != self.icon_id {
            self.icon_id = icon_id;
            self.icon = icon_id.cached_view();
        }

        let child = ctx.builder.child_widget(ctx.span_node, ctx.info.nesting_level);


        self.display_object.replace_children(&[self.icon.display_object(), &child.root_object]);
    }
}
