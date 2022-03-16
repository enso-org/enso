//! The `Tooltip` shows extra information for UI components. It is pegged to the cursor location
//! and appears when it receives information to show.

use ensogl::gui::style::*;
use ensogl::prelude::*;

use enso_frp as frp;
use ensogl::animation::hysteretic::HystereticAnimation;
use ensogl::application::Application;
use ensogl::define_style;
use ensogl::display;
use ensogl::display::shape::StyleWatch;
use ensogl_component::label::Label;



// =================
// === Constants ===
// =================

const PLACEMENT_OFFSET: f32 = 5.0;



// =============
// === Style ===
// =============

define_style! {
    text      : Option<String>,
    placement : Placement
}

impl Style {
    /// Create a `TooltipUpdate` that sets the label of the tooltip.
    pub fn set_label(text: String) -> Self {
        let text = Some(StyleValue::new(Some(text)));
        Self { text, ..default() }
    }
    /// Create a `TooltipUpdate` that unsets the label of the tooltip.
    pub fn unset_label() -> Self {
        let text = Some(StyleValue::new(None));
        Self { text, ..default() }
    }

    /// Indicate whether the `Style` has content to display.
    pub fn has_content(&self) -> bool {
        if let Some(style_value) = self.text.as_ref() {
            if let Some(inner) = style_value.value.as_ref() {
                return inner.is_some();
            }
        }
        false
    }

    /// Create a `TooltipUpdate` that sets the placement of the tooltip.
    pub fn set_placement(placement: Placement) -> Self {
        let placement = Some(StyleValue::new(placement));
        Self { placement, ..default() }
    }

    /// Sets the placement of the tooltip.
    pub fn with_placement(mut self, placement: Placement) -> Self {
        self.placement = Some(StyleValue::new(placement));
        self
    }
}



// ==============
// === Offset ===
// ==============

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
/// Indicates the placement of the tooltip relative to the base position location.
pub enum Placement {
    Top,
    Bottom,
    Left,
    Right,
}

impl Default for Placement {
    fn default() -> Self {
        Placement::Top
    }
}

ensogl::define_endpoints! {
    Input {
        set_style     (Style),
        set_location  (Vector2),
        set_placement (Placement),
        set_offset    (Vector2),
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, Debug)]
struct Model {
    tooltip:   Label,
    root:      display::object::Instance,
    placement: Cell<Placement>,
}

impl Model {
    fn new(app: &Application) -> Self {
        let logger = Logger::new("TooltipModel");
        let tooltip = Label::new(app);
        let root = display::object::Instance::new(&logger);
        root.add_child(&tooltip);
        let placement = default();
        Self { tooltip, root, placement }
    }

    fn set_location(&self, position: Vector2, size: Vector2) {
        let layout_offset = match self.placement.get() {
            Placement::Top => Vector2::new(0.0, size.y * 0.5 + PLACEMENT_OFFSET),
            Placement::Bottom => Vector2::new(0.0, -size.y * 0.5 - PLACEMENT_OFFSET),
            Placement::Left => Vector2::new(-size.x / 2.0 - PLACEMENT_OFFSET, 0.0),
            Placement::Right => Vector2::new(size.x / 2.0 + PLACEMENT_OFFSET, 0.0),
        };

        let base_positions = position.xy();
        self.tooltip.set_position_xy(base_positions + layout_offset)
    }

    fn set_style(&self, update: &Style) {
        if let Some(style) = update.text.as_ref() {
            let text = style.value.clone().flatten();
            if let Some(text) = text {
                self.tooltip.frp.set_content(text)
            }
        }
        if let Some(style) = update.placement.as_ref() {
            if let Some(placement) = style.value {
                self.placement.set(placement)
            }
        }
    }

    fn set_visibility(&self, visible: bool) {
        if visible {
            self.root.add_child(&self.tooltip)
        } else {
            self.tooltip.unset_parent()
        }
    }

    fn set_opacity(&self, value: f32) {
        self.tooltip.frp.set_opacity(value);
    }
}



// ===============
// === Tooltip ===
// ===============

/// Tooltip component that can show extra information about other UI components.
#[derive(Clone, CloneRef, Debug)]
pub struct Tooltip {
    model:   Rc<Model>,
    #[allow(missing_docs)]
    pub frp: Rc<Frp>,
}

impl Tooltip {
    #[allow(missing_docs)]
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app));
        let frp = Rc::new(Frp::new());
        Self { model, frp }.init(app)
    }

    fn init(self, app: &Application) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&app.display.default_scene.style_sheet);
        let hide_delay_duration_ms = styles.get_number_or(
            ensogl_hardcoded_theme::application::tooltip::hide_delay_duration_ms,
            0.0,
        );
        let show_delay_duration_ms = styles.get_number_or(
            ensogl_hardcoded_theme::application::tooltip::show_delay_duration_ms,
            0.0,
        );

        let hysteretic_transition =
            HystereticAnimation::new(network, hide_delay_duration_ms, show_delay_duration_ms);

        frp::extend! { network


            // === Style ===

             eval frp.set_style ((t) model.set_style(t));
             show_text         <- frp.set_style.map(|c| c.has_content());
             on_has_content    <- show_text.on_true();
             on_has_no_content <- show_text.on_false();


            // === Location ===

             location_update <- all(frp.set_location,
                                    model.tooltip.frp.size,
                                    frp.set_offset);
             eval location_update ([model]((pos,size,offset)) {
                let base_position = pos+offset;
                model.set_location(base_position,*size)
             });


            // === Transition ===

             hysteretic_transition.to_start <+ on_has_content;
             hysteretic_transition.to_end   <+ on_has_no_content;

             eval hysteretic_transition.value ([model](value) {
                model.set_opacity(*value);
                if *value >= 0.0 {
                    model.set_visibility(true)
                } else if *value <= 0.0 {
                    model.set_visibility(false)
                }
             });
        }

        // Reset appearance to avoid artifacts when first shown.
        model.set_opacity(0.0);
        model.set_style(&Style::default());
        self
    }
}

impl display::Object for Tooltip {
    fn display_object(&self) -> &display::object::Instance {
        self.model.root.display_object()
    }
}
