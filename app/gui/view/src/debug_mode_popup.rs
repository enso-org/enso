//! A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.

use crate::prelude::*;

use crate::popup;

use ensogl::application::Application;
use ensogl::display;
use frp::stream::EventOutput;
use frp::HasLabel;



// =================
// === Constants ===
// =================

/// Mitigate limitations of constant strings concatenation.
macro_rules! define_debug_mode_shortcut {
    ($shortcut:literal) => {
        /// A keyboard shortcut used to enable/disable Debug Mode.
        pub const DEBUG_MODE_SHORTCUT: &str = $shortcut;
        const DEBUG_MODE_ENABLED: &str =
            concat!("Debug Mode enabled. To disable, press `", $shortcut, "`.");
    };
}
define_debug_mode_shortcut!("ctrl shift d");
const DEBUG_MODE_DISABLED: &str = "Debug Mode disabled.";
const LABEL_VISIBILITY_DELAY_MS: f32 = 3_000.0;



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        // Debug Mode was enabled.
        enabled(),
        // Debug Mode was disabled.
        disabled(),
    }
    Output {}
}



// ============
// === View ===
// ============

/// A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.
#[derive(Debug, Clone, CloneRef)]
pub struct View {
    frp:   Frp,
    popup: popup::View,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let popup = popup::View::new(app);

        popup.set_delay(LABEL_VISIBILITY_DELAY_MS);

        frp::extend! { network
            eval_ frp.enabled (popup.set_label(DEBUG_MODE_ENABLED.to_string()));
            eval_ frp.disabled (popup.set_label(DEBUG_MODE_DISABLED.to_string()));
        }

        Self { frp, popup }
    }

    /// Get the FRP node for the content of the pop-up, for testing purposes.
    pub fn content_frp_node(&self) -> impl EventOutput<Output = String> + HasLabel {
        self.popup.content_frp_node()
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        self.popup.display_object()
    }
}

impl Deref for View {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}
