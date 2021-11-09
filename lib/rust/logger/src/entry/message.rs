//! Abstraction for messages that can be logged.



// ===============
// === Message ===
// ===============

/// Message that can be logged. This trait allow a wide range of input arguments and also, allows
/// the messages to be constructed lazily, from functions.
#[allow(missing_docs)]
pub trait Message {
    fn get(self) -> String;
}
impl Message for &str {
    fn get(self) -> String {
        self.into()
    }
}
impl Message for &&str {
    fn get(self) -> String {
        (*self).into()
    }
}
impl Message for String {
    fn get(self) -> String {
        self
    }
}
impl Message for &String {
    fn get(self) -> String {
        self.clone()
    }
}
impl Message for &&String {
    fn get(self) -> String {
        (*self).clone()
    }
}
impl<F, S> Message for F
where
    F: FnOnce() -> S,
    S: Message,
{
    fn get(self) -> String {
        self().get()
    }
}
