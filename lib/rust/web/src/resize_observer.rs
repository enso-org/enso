//! Binding to the https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver.

use crate::prelude::*;

use crate::CleanupHandle;
use crate::Closure;
use crate::JsValue;



// =============
// === Types ===
// =============


// ======================
// === ResizeObserver ===
// ======================

/// The ResizeObserver interface reports changes to the dimensions of an DOM Element's content or
/// border box. ResizeObserver avoids infinite callback loops and cyclic dependencies that are often
/// created when resizing via a callback function. It does this by only processing elements deeper
/// in the DOM in subsequent frames.
///
/// See also https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct ResizeObserver {
    pub observer: CleanupHandle,
}

impl ResizeObserver {
    /// Constructor.
    pub fn new(target: &JsValue, listener: impl FnMut(f32, f32) + 'static) -> Self {
        Self {
            observer: CleanupHandle::new(
                Closure::<dyn FnMut(f32, f32)>::new(listener),
                |callback| crate::register_resize_observer(target, callback),
            ),
        }
    }
}
