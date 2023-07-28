//! A widget representing a top-level method call. Displays node background. Instead of `self`
//! argument, it displays an icon as a first port.

use super::prelude::*;
use crate::prelude::*;

use icons::any::View as AnyIcon;
use icons::component_icons::Id as IconId;
use icons::SIZE;
use ide_view_component_list_panel_icons as icons;



// =================
// === Constants ===
// =================

/// Distance between the icon and next widget.
const ICON_GAP: f32 = 10.0;



// ==============
// === Method ===
// ===============

/// Method widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Config;

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Debug, display::Object)]
#[allow(dead_code)]
pub struct Widget {
    display_object: object::Instance,
    icon_wrapper:   object::Instance,
    icon_id:        IconId,
    icon:           AnyIcon,
}

/// Extension which existence in given subtree prevents the widget from being created again.
struct MethodAlreadyInTree;

impl SpanWidget for Widget {
    type Config = Config;

    fn default_config(_: &ConfigContext) -> Configuration<Self::Config> {
        Configuration::always(Config)
    }

    fn match_node(ctx: &ConfigContext) -> Score {
        let matches = ctx.span_node.application.is_some()
            && ctx.get_extension::<MethodAlreadyInTree>().is_none();
        Score::only_if(matches)
    }

    fn new(_: &Config, _: &ConfigContext) -> Self {
        // ╭─display_object──────────────────╮
        // │ ╭ icon ─╮ ╭ content ──────────╮ │
        // │ │       │ │                   │ │
        // │ │       │ │                   │ │
        // │ ╰───────╯ ╰───────────────────╯ │
        // ╰─────────────────────────────────╯

        let display_object = object::Instance::new_named("widget::Method");
        display_object
            .use_auto_layout()
            .set_row_flow()
            .set_gap_x(ICON_GAP)
            .set_children_alignment_left_center()
            .justify_content_center_y();

        let icon_wrapper = object::Instance::new_named("icon_wrapper");
        icon_wrapper.set_size((SIZE, SIZE));
        let mut this = Self { display_object, icon_wrapper, icon_id: default(), icon: default() };
        this.set_icon(IconId::default());
        this
    }

    fn configure(&mut self, _: &Config, mut ctx: ConfigContext) {
        ctx.set_extension(MethodAlreadyInTree);
        let icon_id = ctx
            .span_node
            .application
            .as_ref()
            .and_then(|app| app.icon_name.as_ref())
            .and_then(|name| name.parse::<IconId>().ok())
            .unwrap_or(IconId::Method);
        if icon_id != self.icon_id {
            self.set_icon(icon_id);
        }

        let child = ctx.builder.child_widget(ctx.span_node, ctx.info.nesting_level);
        self.display_object.replace_children(&[&self.icon_wrapper, &child.root_object]);
    }
}

impl Widget {
    fn set_icon(&mut self, icon_id: IconId) {
        self.icon_id = icon_id;
        self.icon = icon_id.cached_view();
        self.icon.set_size((SIZE, SIZE));
        self.icon.set_xy((SIZE / 2.0, SIZE / 2.0));
        self.icon.r_component.set(Vector4(1.0, 1.0, 1.0, 1.0));
        self.icon_wrapper.replace_children(&[self.icon.display_object()]);
    }
}
