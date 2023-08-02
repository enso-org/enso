//! A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.

use crate::prelude::*;

use crate::notification::logged::Notification;
use crate::notification::logged::UpdateOptions;

use ensogl::application::Application;
use frp::stream::EventOutput;


// ==============
// === Export ===
// ==============

pub mod api;
pub mod js;
pub mod logged;
pub use js::Id;



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

/// A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.
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
                error!("Enabled: {:?}", enabled);
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
