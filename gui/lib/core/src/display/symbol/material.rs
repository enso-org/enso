pub mod shader;

use crate::prelude::*;

use crate::backend::webgl::Context;
use crate::data::function::callback::*;
use crate::dirty;
use crate::dirty::traits::*;
use crate::system::web::group;
use crate::system::web::Logger;


// ================
// === Geometry ===
// ================

// === Definition ===

#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Material<OnDirty> {
    #[shrinkwrap(main_field)]
    pub dirty  : Dirty <OnDirty>,
    pub logger : Logger,
    context    : Context
}

// === Types ===

pub type Dirty <F> = dirty::SharedBool<F>;

#[macro_export]
macro_rules! promote_material_types { ($($args:tt)*) => {
    promote! {$($args)* [Material]}
};}

// === Implementation ===

impl<OnDirty: Callback0> Material<OnDirty> {
    /// Creates new material with attached callback.
    pub fn new(context:&Context, logger:Logger, on_dirty:OnDirty) -> Self {
        let dirty_logger = logger.sub("dirty");
        let dirty        = Dirty::new(dirty_logger,on_dirty);
        let context      = context.clone();
        Self {dirty,logger,context}
    }
    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            if self.dirty.check_all() {
                self.dirty.unset_all()
            }
        })
    }
}
