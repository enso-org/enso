//! A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.

use crate::prelude::*;

use crate::notification::UpdateOptions;
use ensogl::application::Application;
use ensogl::display;
use frp::stream::EventOutput;
use frp::HasLabel;


// =================
// === Constants ===
// =================

// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        is_enabled(bool),
        set_label(ImString),
    }
    Output {}
}



// ============
// === View ===
// ============

/// A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.
#[derive(Debug, Clone, CloneRef)]
pub struct View {
    frp:          Frp,
    notification: crate::notification::Notification,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let notification = crate::notification::Notification::default();

        frp::extend! { network
            // eval_ frp.enabled (popup.set_label(DEBUG_MODE_ENABLED.to_string()));
            // eval_ frp.disabled (popup.set_label(DEBUG_MODE_DISABLED.to_string()));

            // eval frp.set_label ((label) {
            //     notification.update(|opts| opts.render = );
            // });
        }

        Self { frp, notification }
    }
}

// impl display::Object for View {
//     fn display_object(&self) -> &display::object::Instance {
//         self.popup.display_object()
//     }
// }

impl Deref for View {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}
