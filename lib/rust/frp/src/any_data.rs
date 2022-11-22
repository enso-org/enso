//! Wrapper for any type of data that can be used as an FRP value.

use crate::prelude::*;



// ===============
// === AnyData ===
// ===============

/// Just like [`Rc<std::Any::Any>`], but also with a [`Default`] implementation, which allows it to
/// be used as FRP stream value.
#[allow(missing_docs)]
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct AnyData {
    pub data: Rc<dyn Any>,
}

impl AnyData {
    /// Constructor.
    pub fn new<T: 'static>(data: T) -> Self {
        let data = Rc::new(data);
        Self { data }
    }
}

impl Default for AnyData {
    fn default() -> Self {
        Self { data: Rc::new(()) }
    }
}
