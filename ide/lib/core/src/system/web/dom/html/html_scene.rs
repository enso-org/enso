//! This module contains `HTMLScene`, a struct to hold `HTMLObject`s.

use crate::prelude::*;

use super::HtmlObject;
use crate::display::object::DisplayObjectData;
use data::opt_vec::*;



// =================
// === HtmlScene ===
// =================

/// A collection for holding 3D `Object`s.
#[derive(Debug)]
pub struct HtmlScene {
    display_object : DisplayObjectData,
    objects        : OptVec<HtmlObject>
}

impl HtmlScene {
    /// Searches for a HtmlElement identified by id and appends to it.
    pub fn new<L:Into<Logger>>(logger:L) -> Self {
        let display_object = DisplayObjectData::new(logger.into());
        let objects        = default();
        Self{display_object,objects}
    }

    /// Moves a HTMLObject to the Scene and returns an index to it.
    pub fn add_child(&mut self, object: HtmlObject) -> Ix {
        self.display_object.add_child(&object.display_object);
        self.objects.insert(object)
    }

    /// Removes and retrieves a HTMLObject based on the index provided by
    pub fn remove_child(&mut self, index: Ix) -> Option<HtmlObject> {
        let object = self.objects.remove(index);
        if let Some(object) = &object {
            self.display_object.remove_child(&object.display_object);
        }
        object
    }

    /// Returns the number of `Object`s in the Scene,
    /// also referred to as its 'length'.
    pub fn len(&self) -> usize {
        self.objects.len()
    }

    /// Returns true if the Scene contains no `Object`s.
    pub fn is_empty(&self) -> bool {
        self.objects.is_empty()
    }

    /// Gets mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<'_, HtmlObject> { self.objects.iter_mut() }

    /// Gets iterator.
    pub fn iter(&self) -> Iter<'_, HtmlObject> { self.objects.iter() }
}

impl<'a> IntoIterator for &'a HtmlScene {
    type Item = &'a HtmlObject;
    type IntoIter = Iter<'a, HtmlObject>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut HtmlScene {
    type Item = &'a mut HtmlObject;
    type IntoIter = IterMut<'a, HtmlObject>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}
