//! A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.

use crate::prelude::*;

use crate::notification::handled::Notification;
use crate::notification::UpdateOptions;

use ensogl::application::Application;
use frp::stream::EventOutput;



// =================
// === Constants ===
// =================

// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        is_enabled(bool),
        set_options(UpdateOptions),
    }
    Output {}
}



// ============
// === View ===
// ============

/// A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.
#[derive(Debug, Clone, CloneRef)]
pub struct View {
    #[allow(missing_docs)]
    pub frp:      Frp,
    notification: Notification,
}

impl View {
    /// Constructor.
    pub fn new(_app: &Application) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let notification = crate::notification::handled::Notification::default();

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

impl Deref for View {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}
