//! This module provides a [notification API](crate::notification::api) wrapper that:
//! * Logs most of the actions.
//! * Handles errors by logging and dropping them.
//!
//! The API returns [`Result`]s with [`JsValue`] as an error type which is often inconvenient for
//! our use-cases. We often use notifications to display errors and we don't have a way to handle
//! errors from the notifications themselves.
//!
//! In general, where the original API returns [`Result`]s, this wrapper returns [`Option`]s.

use crate::prelude::*;

use crate::notification;
use crate::notification::js::HandleJsError;

use uuid::Uuid;


// ==============
// === Export ===
// ==============

pub use crate::notification::api::AutoClose;
pub use crate::notification::api::Content;
pub use crate::notification::api::Options;
pub use crate::notification::api::Type;
pub use crate::notification::api::UpdateOptions;



// ===================
// === Primary API ===
// ===================

/// Send any kind of notification.
pub fn send_any(message: &Content, r#type: Type, options: &Option<Options>) -> Option<Id> {
    let log_content = format!("Sending notification with message {message:?} and type {type:?}");
    match r#type {
        Type::Warning => warn!("{log_content}"),
        Type::Error => error!("{log_content}"),
        _ => info!("{log_content}"),
    };
    notification::api::send_any(message, r#type, options).map(Id).handle_js_err_with(|| {
        format!("Failed to send {type} notification with message {message:?}.")
    })
}

/// Send an info notification.
pub fn info(message: impl Into<Content>, options: &Option<Options>) -> Option<Id> {
    send_any(&message.into(), Type::Info, options)
}

/// Send a warning notification.
pub fn warning(message: impl Into<Content>, options: &Option<Options>) -> Option<Id> {
    send_any(&message.into(), Type::Warning, options)
}

/// Send a error notification.
pub fn error(message: impl Into<Content>, options: &Option<Options>) -> Option<Id> {
    send_any(&message.into(), Type::Error, options)
}

/// Send a success notification.
pub fn success(message: impl Into<Content>, options: &Option<Options>) -> Option<Id> {
    send_any(&message.into(), Type::Success, options)
}



// ==========
// === Id ===
// ==========

/// The unique identifier of a toast.
#[derive(Clone, Debug, Display)]
pub struct Id(notification::api::Id);

impl From<notification::api::Id> for Id {
    fn from(id: notification::api::Id) -> Self {
        Self(id)
    }
}

impl From<&str> for Id {
    fn from(id: &str) -> Self {
        Self(id.into())
    }
}

impl From<Uuid> for Id {
    fn from(id: Uuid) -> Self {
        Self(id.into())
    }
}

impl Id {
    /// Close the notification.
    pub fn dismiss(&self) {
        info!("Dismissing the notification {self}.");
        self.0.dismiss().handle_js_err_with(|| format!("Failed to dismiss notification {self:?}."));
    }

    /// Completes the controlled progress bar.
    pub fn done(&self) {
        info!("Completing the notification {self}.");
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
        info!("Updating the notification {self} with new options {options:?}.");
        self.0
            .update(options)
            .handle_js_err_with(|| format!("Failed to update notification {self:?}."));
    }
}



// ====================
// === Notification ===
// ====================

/// Same as super::Notification, but with all errors handled by logging them using error! macro.

/// A persistent notification.
#[derive(Clone, CloneRef, Debug, Display)]
pub struct Notification(notification::api::Notification);

impl Default for Notification {
    fn default() -> Self {
        Self::new(default())
    }
}

impl From<Id> for Notification {
    fn from(id: Id) -> Self {
        Self(notification::api::Notification::from(id.0))
    }
}
impl Notification {
    /// Create a new notification archetype.
    ///
    /// It will not be shown until you call [`Notification::show`].
    pub fn new(options: UpdateOptions) -> Self {
        info!("Creating a new notification with options {options:?}.");
        Self(notification::api::Notification::new(options))
    }

    /// Update the notification state.
    ///
    /// If the visualization is being shown, it will be updated. If not, changes will appear the
    /// next time the notification is [
    pub fn update(&self, f: impl FnOnce(&mut UpdateOptions)) {
        info!("Updating the notification {self}.");
        self.0.update(f).handle_js_err_with(|| format!("Failed to update notification {self}."));
    }

    /// Display the notification.
    ///
    /// If it is already being shown, nothing will happen.
    pub fn show(&self) {
        info!("Showing the notification {self}.");
        self.0.show().handle_js_err_with(|| format!("Failed to show notification {self}."));
    }

    /// Dismiss the notification.
    pub fn dismiss(&self) {
        info!("Dismissing the notification {self}.");
        self.0.dismiss().handle_js_err_with(|| format!("Failed to dismiss notification {self}."));
    }
}
