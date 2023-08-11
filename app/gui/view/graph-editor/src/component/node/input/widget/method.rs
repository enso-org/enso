//! A widget representing a top-level method call. Displays node background. Instead of `self`
//! argument, it displays an icon as a first port.

use super::prelude::*;
use crate::prelude::*;

use ensogl_icons::any::View as AnyIcon;
use ensogl_icons::component_icons::Id as IconId;
use ensogl_icons::SIZE;



// =============
// === Style ===
// =============

#[derive(Clone, Debug, Default, PartialEq, FromTheme)]
#[base_path = "theme::widget::method"]
struct Style {
    /// Distance between the icon and next widget.
    icon_gap:           f32,
    /// Alpha channel value for the icon displayed on a node in the execution-pending state.
    pending_icon_alpha: f32,
}



// ==============
// === Method ===
// ===============

/// Method widget configuration options.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Config;

ensogl::define_endpoints_2! {
    Input {
        set_icon(IconId),
        set_icon_state(IconState),
    }
}

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Debug, display::Object)]
#[allow(dead_code)]
pub struct Widget {
    display_object: object::Instance,
    icon_wrapper:   object::Instance,
    icon_view:      Rc<RefCell<AnyIcon>>,
    frp:            Frp,
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

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
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
            .set_children_alignment_left_center()
            .justify_content_center_y();
        let icon_wrapper = object::Instance::new_named("icon_wrapper");
        icon_wrapper.set_size((SIZE, SIZE));
        let icon_view = Rc::new(RefCell::new(AnyIcon::default()));

        let frp = Frp::new();
        let network = &frp.network;
        let style = ctx.cached_style::<Style>(network);
        frp::extend! { network
            icon_gap <- style.on_change().map(|style| style.icon_gap);
            eval icon_gap((gap) display_object.set_gap_x(*gap).void());

            set_icon <- frp.set_icon.on_change();
            eval set_icon([icon_view, icon_wrapper] (icon_id) {
                let icon = icon_id.cached_view();
                icon.set_size((SIZE, SIZE));
                icon_wrapper.replace_children(&[icon.display_object()]);
                icon_view.replace(icon);
            });

            icon_state_dirty <- any(...);
            icon_state_dirty <+ frp.set_icon_state.on_change();
            icon_state_dirty <+ frp.set_icon_state.sample(&set_icon);
            icon_alpha <- all_with(&icon_state_dirty, &style, |state, style| state.alpha(style));
            eval icon_alpha((alpha)
                icon_view.borrow().r_component.set(Vector4(1.0, 1.0, 1.0, *alpha)));
        }
        frp.set_icon(IconId::default());
        frp.set_icon_state(IconState::Ready);
        Self {
            display_object,
            icon_wrapper,
            icon_view,
            frp,
        }
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
        self.frp.set_icon(icon_id);
        self.frp.set_icon_state(match ctx.info.pending {
            false => IconState::Ready,
            true => IconState::Pending,
        });
        let child = ctx.builder.child_widget(ctx.span_node, ctx.info.nesting_level);
        self.display_object.replace_children(&[&self.icon_wrapper, &child.root_object]);
    }
}


// === IconState ===

/// The display state of the method's icon.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub enum IconState {
    /// The normal state.
    #[default]
    Ready,
    /// The state when awaiting a pending computation.
    Pending,
}

impl IconState {
    fn alpha(self, style: &Style) -> f32 {
        match self {
            IconState::Ready => 1.0,
            IconState::Pending => style.pending_icon_alpha,
        }
    }
}
