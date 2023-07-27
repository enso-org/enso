/// The API is often inconvenient as it returns [`Result`]s with [`JsValue`] errors. Usually we
/// want to send notifications and forget about them, so we don't want to deal with errors.
/// This wrapper allows to do that.
use crate::prelude::*;

use crate::notification;
use crate::notification::Content;
use crate::notification::Options;
use crate::notification::Type;
use crate::notification::UpdateOptions;

/// The unique identifier of a toast.
#[derive(Clone, Debug)]
pub struct Id(notification::Id);

impl From<notification::Id> for Id {
    fn from(id: notification::Id) -> Self {
        Self(id)
    }
}

impl From<&str> for Id {
    fn from(id: &str) -> Self {
        Self(notification::Id::from(id))
    }
}

impl Id {
    /// Close the notification.
    pub fn dismiss(&self) {
        self.0.dismiss().handle_err(|e| {
            error!("Failed to dismiss notification {self:?}: {e:?}");
        });
    }

    /// Completes the controlled progress bar.
    pub fn done(&self) {
        self.0.done().handle_err(|e| {
            error!("Failed to complete notification {self:?}: {e:?}");
        });
    }

    /// Check if a toast is displayed or not.
    pub fn is_active(&self) -> bool {
        self.0
            .is_active()
            .handle_err(|e| {
                error!("Failed to check if notification {self:?} is active: {e:?}");
            })
            .unwrap_or(false)
    }

    /// Update a toast.
    pub fn update(&self, options: &UpdateOptions) {
        self.0.update(options).handle_err(|e| {
            error!("Failed to update notification {self:?}: {e:?}");
        });
    }
}

/// Send any kind of notification.
pub fn send_any(message: &Content, r#type: Type, options: &Option<Options>) -> Option<Id> {
    warn!("Sending notification with message {message:?} and type {type:?}");
    notification::send_any(message, r#type, options).map(Id).handle_err(|e| {
        error!("Failed to send {type} notification with message {message:?}: {e:?}");
    })
}

/// Send an info notification.
pub fn info(message: &Content, options: &Option<Options>) -> Option<Id> {
    send_any(message, Type::Info, options)
}

/// Send a warning notification.
pub fn warning(message: &Content, options: &Option<Options>) -> Option<Id> {
    send_any(message, Type::Warning, options)
}

/// Send a error notification.
pub fn error(message: &Content, options: &Option<Options>) -> Option<Id> {
    send_any(message, Type::Error, options)
}

/// Send a success notification.
pub fn success(message: &Content, options: &Option<Options>) -> Option<Id> {
    send_any(message, Type::Success, options)
}
