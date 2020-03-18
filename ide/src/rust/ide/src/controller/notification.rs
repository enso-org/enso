//! Here are the common structures used by Controllers notifications (sent between controllers and
//! from controller to view).

use crate::prelude::*;



// ===============
// === Commons ===
// ===============

/// A buffer size for notification publisher.
///
/// If Publisher buffer will be full, the thread sending next notification will be blocked until
/// all subscribers read message from buffer. We don't expect much traffic on file notifications,
/// therefore there is no need for setting big buffers.
const NOTIFICATION_BUFFER_SIZE : usize = 36;

/// A notification publisher which implements Debug and Default.
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Publisher<Message>(pub flo_stream::Publisher<Message>);

impl<Message:Clone> Default for Publisher<Message> {
    fn default() -> Self {
        Self(flo_stream::Publisher::new(NOTIFICATION_BUFFER_SIZE))
    }
}

impl<Message:'static> Debug for Publisher<Message> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "notification::Publisher<{:?}>", std::any::TypeId::of::<Message>())
    }
}



// =====================================
// === Double Representation Changes ===
// =====================================

// === Text ===

/// A notification about changes of text representation or plain text file content.
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum Text {
    /// The content should be fully reloaded.
    Invalidate,
}


// === Graphs ===

/// A notification about changes of graph representation of a module.
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum Graphs {
    /// The content should be fully reloaded.
    Invalidate,
}


// === Graph ===

/// A notification about changes of a specific graph in a module.
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum Graph {
    /// The content should be fully reloaded.
    Invalidate,
}


// === Node ===

/// A notification about changes of specific node in a graph.
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum Node {
    /// The content should be fully reloaded.
    Invalidate,
}


