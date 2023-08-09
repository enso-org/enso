//! A pop-up notification that can be controlled by FRP.
//!
//! Non-FRP, foundational API is available in the [api](crate::notification::api) module.

use crate::prelude::*;

use crate::notification::logged::Notification;
use crate::notification::logged::UpdateOptions;

use ensogl::application::Application;


// ==============
// === Export ===
// ==============

pub mod api;
pub mod js;
pub mod logged;



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Toggles visibility.
        is_enabled(bool),
        /// Customize notification.
        set_options(UpdateOptions),
    }
    Output {}
}



// ============
// === View ===
// ============

/// A reusable popup notification.
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct View {
    #[deref]
    frp:          Frp,
    notification: Notification,
}

impl View {
    /// Constructor.
    pub fn new(_app: &Application) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let notification = Notification::default();

        frp::extend! { network
            eval frp.is_enabled ([notification] (enabled) {
                if *enabled {
                    notification.show()
                } else {
                    notification.dismiss()
                }
            });

            eval frp.set_options ([notification] (options) {
                notification.update(|opts| {
                    *opts = options.clone();
                })
            });
        }

        Self { frp, notification }
    }
}
