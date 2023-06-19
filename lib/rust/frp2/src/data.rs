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
pub trait Data = DynData + Clone;


/// Trait allowing storing any data as `Box<dyn DynData>`.
#[allow(missing_docs)]
pub trait DynData: DynDataBounds {
    fn boxed_clone(&self) -> Box<dyn DynData>;
}

/// Alias for bounds required on [`DynData`] types. Please note that there is no [`Clone`] bound,
/// although it is required (see the impl below). This is because we use `DynData` as dyn trait,
/// which can't be `Sized` (required by `Clone`).
pub trait DynDataBounds = where Self: Debug + 'static;

impl<T: DynDataBounds + Clone> DynData for T {
    fn boxed_clone(&self) -> Box<dyn DynData> {
        Box::new(self.clone())
    }
}
