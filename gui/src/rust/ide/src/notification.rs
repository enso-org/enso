//! Here are the common structures used by Controllers notifications (sent between controllers and
//! from controller to view).

use crate::prelude::*;

use flo_stream::MessagePublisher;
use flo_stream::Subscriber;



// =================
// === Publisher ===
// =================

/// A buffer size for notification publisher.
///
/// If Publisher buffer will be full, the thread sending next notification will be blocked until
/// all subscribers read message from buffer. We don't expect much traffic on file notifications,
/// therefore there is no need for setting big buffers.
pub const NOTIFICATION_BUFFER_SIZE: usize = 36;

/// A notification publisher which implements Debug, Default and CloneRef (which is same as
/// republishing for the same stream) and uses internal mutability.
pub struct Publisher<Message>(RefCell<flo_stream::Publisher<Message>>);

impl<Message: Clone> Default for Publisher<Message> {
    fn default() -> Self {
        Self(RefCell::new(flo_stream::Publisher::new(NOTIFICATION_BUFFER_SIZE)))
    }
}

impl<Message: 'static> Debug for Publisher<Message> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "notification::Publisher<{:?}>", std::any::TypeId::of::<Message>())
    }
}

impl<Message: Clone> CloneRef for Publisher<Message> {
    fn clone_ref(&self) -> Self {
        self.clone()
    }
}

impl<Message: Clone> Clone for Publisher<Message> {
    fn clone(&self) -> Self {
        Self(RefCell::new(self.0.borrow().republish()))
    }
}

impl<Message> Publisher<Message>
where
    Message: 'static + Send,
    flo_stream::Publisher<Message>: MessagePublisher<Message = Message>,
{
    /// Publish a message to the subscribers of this object.
    pub fn publish(&self, message: Message) -> StaticBoxFuture<()> {
        self.0.borrow_mut().publish(message)
    }

    /// Create a subscription to this publisher
    ///
    /// Any future messages sent here will also be sent to this subscriber.
    pub fn subscribe(&self) -> Subscriber<Message> {
        self.0.borrow_mut().subscribe()
    }

    /// Use global executor to publish a message.
    pub fn notify(&self, message: Message) {
        let notify = self.publish(message);
        executor::global::spawn(notify);
    }
}
