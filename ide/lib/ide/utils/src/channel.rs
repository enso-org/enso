//! General-purpose code for dealing with mpsc channels.

use futures::channel::mpsc::UnboundedSender;

/// Emit message using `UnboundedSender`. Does not care if there are listeners.
pub fn emit<T>(sender:&UnboundedSender<T>, message:T) {
    match sender.unbounded_send(message) {
        Ok(()) => {},
        Err(e) =>
            if e.is_full() {
                // Impossible, as per `futures` library docs.
                panic!("Unbounded channel should never be full.")
            } else if e.is_disconnected() {
                // It is ok for receiver to disconnect and ignore events. We
                // want to just "emit", we do not care if someone is listening.
            } else {
                // Never happens unless `futures` library changes API.
                panic!("Unrecognized error when sending event.")
            }
    }
}
