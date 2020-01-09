use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsValue;



// =============
// === Types ===
// =============

pub type Listener = Closure<dyn FnMut(i32, i32, i32, i32)>;



// ===================
// === JS Bindings ===
// ===================

#[wasm_bindgen(module = "/js/intersection_observer.js")]
extern "C" {
    fn intersection_observe(target: &JsValue, closure: &Listener) -> usize;
    fn intersection_unobserve(id: usize);
}



// ============================
// === IntersectionObserver ===
// ============================

/// The IntersectionObserver interface of the Intersection Observer API provides
/// a way to asynchronously observe changes in the intersection of a target
/// element with an ancestor element or with a top-level document's viewport.
/// The ancestor element or viewport is referred to as the root.
///
/// See also
/// https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver
#[derive(Debug)]
pub struct IntersectionObserver {
    pub target:      JsValue,
    pub listener:    Listener,
    pub observer_id: usize,
}

impl IntersectionObserver {
    pub fn new(target: &JsValue, listener: Listener) -> Self {
        let target = target.clone();
        let observer_id = intersection_observe(&target, &listener);
        Self { target, listener, observer_id }
    }
}

impl Drop for IntersectionObserver {
    fn drop(&mut self) {
        intersection_unobserve(self.observer_id);
    }
}
