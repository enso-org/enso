//! Module defines type for event emitted by the RPC handler.



/// Event emitted by the RPC handler.
#[derive(Debug)]
pub enum Event<N> {
    /// The handler's transport has been closed.
    Closed,
    /// An error has occurred.
    Error(failure::Error),
    /// A notification has been received.
    Notification(N),
}
