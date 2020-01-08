#![allow(missing_docs)]

use crate::prelude::*;

use super::HTMLObject;
use super::Scene;
use crate::data::opt_vec::OptVec;
use crate::system::web::Result;
use crate::system::web::StyleSetter;
use crate::system::web::NodeAppender;
use crate::system::web::NodeRemover;


// =================
// === HTMLScene ===
// =================

/// A collection for holding 3D `HTMLObject`s.
#[derive(Shrinkwrap, Debug)]
#[shrinkwrap(mutable)]
pub struct HTMLScene {
    #[shrinkwrap(main_field)]
    pub scene   : Scene,
    pub div     : HTMLObject,
    pub camera  : HTMLObject,
    pub objects : OptVec<HTMLObject>,
}

pub type Index = usize;

impl HTMLScene {
    /// Searches for a HtmlElement identified by id and appends to it.
    pub fn new(dom_id: &str) -> Result<Self> {
        let scene    = Scene::new(dom_id)?;
        let view_dim = scene.get_dimensions();
        let width    = format!("{}px", view_dim.x);
        let height   = format!("{}px", view_dim.y);
        let div      = HTMLObject::new("div")?;
        let camera   = HTMLObject::new("div")?;
        let objects  = default();

        scene.container.set_property_or_panic("overflow", "hidden");
        scene.container.append_child_or_panic(&div.element);
        div.element.append_child_or_panic(&camera.element);
        div   .element.set_property_or_panic("width"  , &width);
        div   .element.set_property_or_panic("height" , &height);
        camera.element.set_property_or_panic("width"  , &width);
        camera.element.set_property_or_panic("height" , &height);

        Ok(Self { scene, div, camera, objects })
    }

    /// Moves a HTMLObject to the Scene and returns an index to it.
    pub fn add(&mut self, object: HTMLObject) -> Index {
        self.camera.element.append_child_or_panic(&object.element);
        self.objects.insert(object)
    }

    /// Removes and retrieves a HTMLObject based on the index provided by
    pub fn remove(&mut self, index: usize) -> Option<HTMLObject> {
        let result = self.objects.remove(index);
        result.iter().for_each(|object| {
            self.camera.element.remove_child_or_panic(&object.element);
        });
        result
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
}
