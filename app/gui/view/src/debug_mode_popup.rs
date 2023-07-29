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
        set_options(UpdateOptions),
        // set_label(ImString),
        // set_html(ImString),
        // set_auto_close(bool),
    }
    Output {}
}



// ============
// === View ===
// ============

/// A pop-up that signals about enabling/disabling Debug Mode of Graph Editor.
#[derive(Debug, Clone, CloneRef)]
pub struct View {
    pub frp:      Frp,
    notification: crate::notification::Notification,
}

impl View {
    /// Constructor.
    pub fn new(_app: &Application) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let notification = crate::notification::Notification::default();

        frp::extend! { network
            eval frp.is_enabled ([notification] (enabled) {
                error!("Enabled: {:?}", enabled);
                if *enabled {
                    notification.show().handle_err(|err| {
                        error!("Failed to show notification: {:?}", err);
                    });
                } else {
                    notification.hide().handle_err(|err| {
                        error!("Failed to hide notification: {:?}", err);
                    });
                }
            });

            eval frp.set_options ([notification] (options) {
                notification.update(|opts| {
                    *opts = options.clone();
                });
            });

            // eval frp.set_label ((label) {
            //     notification.update(|opts| {
            //         opts.render = Some(crate::notification::Content::Text(label.to_string()))
            //     });
            // });

            // eval frp.set_html ([notification] (html) {
            //     let element = crate::notification::Content::from_html(html).handle_err(|err| {
            //         error!("Failed to create HTML element: {:?}", err);
            //     });
            //     notification.update(|opts| {
            //         opts.render = element
            //     });
            // });
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
