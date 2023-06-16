//! Type of data passed in FRP events.

use crate::prelude::*;



// ============
// === Data ===
// ============

/// Trait for types that can be used as event data in the FRP system. The inherited bounds are as
/// follow:
/// - [`Debug`]: For debug purposes in order to be able to trace the data flow when needed.
/// - [`Clone + 'static`]: Needed in case the data needs to be cloned if it is passed to a sample
///   port.
#[allow(missing_docs)]
pub trait Data: DataBounds {
    fn boxed_clone(&self) -> Box<dyn Data>;
}

/// Alias for bounds required on [`Data`] types. Please note that there is no [`Clone`] bound,
/// although it is required (see the impl below). This is because we use `Data` as dyn trait, which
/// can't be `Sized` (required by `Clone`).
pub trait DataBounds = where Self: Debug + 'static;

impl<T: DataBounds + Clone> Data for T {
    fn boxed_clone(&self) -> Box<dyn Data> {
        Box::new(self.clone())
    }
}
