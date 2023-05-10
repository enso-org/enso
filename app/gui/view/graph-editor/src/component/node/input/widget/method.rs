//! A widget representing a top-level method call. Displays node background. Instead of `self`
//! argument, it displays an icon as a first port.

use crate::prelude::*;

use ensogl::display;

use ide_view_component_list_panel_icons as icons;

use icons::any::View as AnyIcon;
use icons::component_icons::Id;


// =================
// === Constants ===
// =================

/// Distance between the icon and next widget.
const ICON_GAP: f32 = -20.0;

/// Maximum allowed size of the dropdown list. If the list needs to be longer or wider than allowed
/// by these values, it will receive a scroll bar.
const DROPDOWN_MAX_SIZE: Vector2 = Vector2(300.0, 500.0);



// ==============
// === Method ===
// ===============

/// Method widget configuration options.
#[derive(Debug, Clone, PartialEq)]
pub struct Config {
    // Chain
}

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Debug)]
#[allow(dead_code)]
pub struct Widget {
    display_object: display::object::Instance,
    icon:           AnyIcon,
}

/// An extension struct that can be accessed by any child widgets.
// #[derive(Debug, Clone)]
// pub struct MethodContext {
//     methods_with_icons: SmallVec<[ast::Id]>,
// }

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &display::object::Instance {
        &self.display_object
    }

    fn new(_: &Config, ctx: &super::ConfigContext) -> Self {
        let app = ctx.app();
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

        // let icon = Id::;
        let icon = todo!();
        Self { display_object, icon }.init(ctx)
    }

    fn configure(&mut self, config: &Config, mut ctx: super::ConfigContext) {
        let child_level = ctx.info.nesting_level;
        let config = super::Configuration::hierarchy();
        let child = ctx.builder.child_widget_of_type(ctx.span_node, child_level, Some(&config));
        self.display_object.replace_children(&[self.icon.display_object(), &child.root_object]);
    }
}

impl Widget {
    fn init(self, ctx: &super::ConfigContext) -> Self {
        self
    }
}
