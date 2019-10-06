use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsValue;

// =============
// === Types ===
// =============

pub type Listener = Closure<dyn FnMut(i32, i32)>;

// ===================
// === JS Bindings ===
// ===================

#[wasm_bindgen(module = "/js/resize_observer.js")]
extern "C" {
    fn resize_observe(target: &JsValue, closure: &Listener) -> usize;
    fn resize_unobserve(id: usize);
}

// ======================
// === ResizeObserver ===
// ======================

/// The ResizeObserver interface reports changes to the dimensions of an
/// DOM Element's content or border box. ResizeObserver avoids infinite callback
/// loops and cyclic dependencies that are often created when resizing via a
/// callback function. It does this by only processing elements deeper in the
/// DOM in subsequent frames.
///
/// See also https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver
#[derive(Debug)]
pub struct ResizeObserver {
    pub target:      JsValue,
    pub listener:    Listener,
    pub observer_id: usize,
}

impl ResizeObserver {
    pub fn new(target: &JsValue, listener: Listener) -> Self {
        let target = target.clone();
        let observer_id = resize_observe(&target, &listener);
        Self { target, listener, observer_id }
    }
}

impl Drop for ResizeObserver {
    fn drop(&mut self) {
        resize_unobserve(self.observer_id);
    }
}
