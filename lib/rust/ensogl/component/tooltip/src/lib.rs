//! The `Tooltip` shows extra information for UI components. It is pegged to the cursor location
//! and appears when it receives information to show.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use ensogl::prelude::*;

use enso_frp as frp;
use ensogl::animation::hysteretic::HystereticAnimation;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::shape::StyleWatch;
use ensogl_core as ensogl;
use ensogl_core::application::tooltip::Placement;
use ensogl_core::application::tooltip::Style;
use ensogl_label::Label;


// ==============
// === Export ===
// ==============

pub use ensogl::application::tooltip;



// =================
// === Constants ===
// =================

const PLACEMENT_OFFSET: f32 = 5.0;



ensogl::define_endpoints! {
    Input {
        set_style     (Style),
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
        if let Some(text) = update.content() {
            self.tooltip.frp.set_content(text)
        }
        if let Some(placement) = update.placement() {
            self.placement.set(placement)
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

             cursor_pos <- app.cursor.frp.scene_position.map(|pos| pos.xy());
             location_update <- all(cursor_pos,
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
