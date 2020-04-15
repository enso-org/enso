//! Utils for managing weak display objects.

use crate::prelude::*;
use super::class::*;
use crate::control::callback::DynEvent;



// ==================
// === WeakObject ===
// ==================

/// Defines objects which could be missing (already dropped).
pub trait WeakObject {
    /// Tries to access the instance of the weak display object. Returns `None` if the object was
    /// already dropped.
    fn try_display_object(&self) -> Option<Instance>;
}

impl<T:WeakObject> WeakObject for Option<T> {
    fn try_display_object(&self) -> Option<Instance> {
        self.as_ref().and_then(|t| t.try_display_object())
    }
}



// =====================
// === WeakObjectOps ===
// =====================

impl<T:WeakObject> WeakObjectOps for T {}
#[allow(missing_docs)]
pub trait WeakObjectOps : WeakObject {
    fn add_child<T:Object>(&self, child:&T) {
        self.try_display_object().for_each(|node| ObjectOps::add_child(&node,child));
    }

    fn remove_child<T:Object>(&self, child:&T) {
        self.try_display_object().for_each(|node| ObjectOps::remove_child(&node,child));
    }

    fn id(&self) -> Option<Id> {
        self.try_display_object().map(|node| ObjectOps::id(&node))
    }

    fn unset_parent(&self) {
        self.try_display_object().for_each(|node| ObjectOps::unset_parent(&node));
    }

    fn dispatch_event(&self, event:&DynEvent) {
        self.try_display_object().for_each(|node| ObjectOps::dispatch_event(&node,event));
    }

    fn transform_matrix(&self) -> Option<Matrix4<f32>> {
        self.try_display_object().map(|node| ObjectOps::transform_matrix(&node))
    }

    fn position(&self) -> Option<Vector3<f32>> {
        self.try_display_object().map(|node| ObjectOps::position(&node))
    }

    fn scale(&self) -> Option<Vector3<f32>> {
        self.try_display_object().map(|node| ObjectOps::scale(&node))
    }

    fn rotation(&self) -> Option<Vector3<f32>> {
        self.try_display_object().map(|node| ObjectOps::rotation(&node))
    }

    fn set_position(&self, t:Vector3<f32>) {
        self.try_display_object().for_each(|node| ObjectOps::set_position(&node,t));
    }

    fn set_scale(&self, t:Vector3<f32>) {
        self.try_display_object().for_each(|node| ObjectOps::set_scale(&node,t));
    }

    fn set_rotation(&self, t:Vector3<f32>) {
        self.try_display_object().for_each(|node| ObjectOps::set_rotation(&node,t));
    }

    fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.try_display_object().for_each(|node| ObjectOps::mod_position(&node,f));
    }

    fn mod_rotation<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.try_display_object().for_each(|node| ObjectOps::mod_rotation(&node,f));
    }

    fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.try_display_object().for_each(|node| ObjectOps::mod_scale(&node,f));
    }
}
