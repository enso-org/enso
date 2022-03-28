//! # RcWithId
//!
//! [`Rc`]/[`Weak`] types, augmented with a unique ID.

use crate::prelude::*;
use crate::unique_id::UniqueId;

/// Contains the main exports of this module.
pub mod prelude {
    pub use super::RcWithId;
    pub use super::WeakWithId;
}



// ==============================================
// === Common core of RcWithId and WeakWithId ===
// ==============================================

macro_rules! impl_ref_with_id {
    ($name:ident, $ref:ident) => {
        /// Reference type augmented with a unique ID.
        #[derive(Debug)]
        pub struct $name<T, Id: UniqueId> {
            rc: $ref<T>,
            id: Id,
        }

        impl<T, Id: UniqueId> IdentityEq for $name<T, Id> {
            /// Return whether two values are references to the same object.
            fn eq(&self, rhs: &Self) -> bool {
                self.id() == rhs.id()
            }
        }

        impl<T, Id: UniqueId> Clone for $name<T, Id> {
            fn clone(&self) -> Self {
                let rc = self.rc.clone();
                let id = self.id;
                Self { rc, id }
            }
        }

        impl<T, Id: UniqueId> CloneRef for $name<T, Id> {
            fn clone_ref(&self) -> Self {
                self.clone()
            }
        }

        // === Payload Operations ===

        impl<T, Id: UniqueId> Deref for $name<T, Id> {
            type Target = $ref<T>;
            fn deref(&self) -> &Self::Target {
                &self.rc
            }
        }


        // === ID Operations ===

        impl<T, Id: UniqueId> $name<T, Id> {
            /// Get this object's unique ID.
            pub fn id(&self) -> Id {
                self.id
            }
        }
    }
}



// ================
// === RcWithId ===
// ================

impl_ref_with_id!(RcWithId, Rc);

impl<T, Id: UniqueId> RcWithId<T, Id> {
    /// Put an object into an [`Rc`] and assign it an ID.
    ///
    /// This type is created directly from the inner object, not from an [`Rc`], to ensure it is not
    /// possible to end up with different IDs assigned to the same [`Rc`].
    pub fn new(t: T) -> Self {
        let rc = Rc::new(t);
        let id = UniqueId::new();
        Self { rc, id }
    }

    /// Downgrade to a [`WeakWithId`]. Like [`Rc::downgrade`], but preserves the ID.
    pub fn downgrade(&self) -> WeakWithId<T, Id> {
        let id = self.id;
        let rc = self.rc.downgrade();
        WeakWithId { rc, id }
    }
}



// ==================
// === WeakWithId ===
// ==================

impl_ref_with_id!(WeakWithId, Weak);

impl<T, Id: UniqueId> WeakWithId<T, Id> {
    /// Try to upgrade to a [`RcWithId`]. Like [`Weak::upgrade`], but preserves the ID.
    pub fn upgrade(&self) -> Option<RcWithId<T, Id>> {
        let id = self.id;
        self.rc.upgrade().map(|rc| RcWithId { rc, id })
    }
}
