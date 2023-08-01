//! The API is often inconvenient as it returns [`Result`]s with [`JsValue`] errors. Usually we
//! want to send notifications and forget about them, so we don't want to deal with errors.
//! This wrapper allows to do that.

use crate::prelude::*;

use crate::notification;
use crate::notification::js::HandleJsError;
use crate::notification::Content;
use crate::notification::Options;
use crate::notification::Type;
use crate::notification::UpdateOptions;

use uuid::Uuid;



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

impl From<Uuid> for Id {
    fn from(id: Uuid) -> Self {
        Self(notification::Id::from(id))
    }
}

impl Id {
    /// Close the notification.
    pub fn dismiss(&self) {
        self.0.dismiss().handle_js_err_with(|| format!("Failed to dismiss notification {self:?}."));
    }

    /// Completes the controlled progress bar.
    pub fn done(&self) {
        self.0.done().handle_js_err_with(|| format!("Failed to complete notification {self:?}."));
    }

    /// Check if a toast is displayed or not.
    pub fn is_active(&self) -> bool {
        self.0
            .is_active()
            .handle_js_err_with(|| format!("Failed to check if notification {self:?} is active."))
            .unwrap_or(false)
    }

    /// Update a toast.
    pub fn update(&self, options: &UpdateOptions) {
        self.0
            .update(options)
            .handle_js_err_with(|| format!("Failed to update notification {self:?}."));
    }
}

/// Send any kind of notification.
pub fn send_any(message: &Content, r#type: Type, options: &Option<Options>) -> Option<Id> {
    warn!("Sending notification with message {message:?} and type {type:?}");
    notification::send_any(message, r#type, options).map(Id).handle_js_err_with(|| {
        format!("Failed to send {type} notification with message {message:?}.")
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

/// Same as super::Notification, but with all errors handled by logging them using error! macro.

/// A persistent notification.
#[derive(Clone, CloneRef, Debug, Display)]
pub struct Notification(super::Notification);

impl Default for Notification {
    fn default() -> Self {
        Self::new(default())
    }
}

impl From<Id> for Notification {
    fn from(id: Id) -> Self {
        Self(super::Notification::from(id.0))
    }
}
impl Notification {
    /// Create a new notification archetype.
    ///
    /// It will not be shown until you call [`Notification::show`].
    pub fn new(options: UpdateOptions) -> Self {
        Self(super::Notification::new(options))
    }

    /// Update the notification state.
    ///
    /// If the visualization is being shown, it will be updated. If not, changes will appear the
    /// next time the notification is [
    pub fn update(&self, f: impl FnOnce(&mut UpdateOptions)) {
        self.0.update(f).handle_js_err_with(|| format!("Failed to update notification {self}."));
    }

    /// Display the notification.
    ///
    /// If it is already being shown, nothing will happen.
    pub fn show(&self) {
        self.0.show().handle_js_err_with(|| format!("Failed to show notification {self}."));
    }

    /// Dismiss the notification.
    pub fn dismiss(&self) {
        self.0.dismiss().handle_js_err_with(|| format!("Failed to dismiss notification {self}."));
    }
}
