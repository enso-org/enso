use crate::prelude::*;



// ============
// === Data ===
// ============

/// Trait for types that can be used as event data in the FRP system. The inherited bounds are as
/// follow:
/// - [`Debug`]: For debug purposes in order to be able to trace the data flow when needed.
/// - [`Clone + 'static`]: Needed in case the data needs to be cloned if it is passed to a sample
///   port.
pub trait Data: DataBounds {
    fn boxed_clone(&self) -> Box<dyn Data>;
}

/// Alias for bounds required on [`Data`] types.
pub trait DataBounds = where Self: Debug + 'static;

impl<T: DataBounds + Clone> Data for T {
    fn boxed_clone(&self) -> Box<dyn Data> {
        Box::new(self.clone())
    }
}
