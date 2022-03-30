//! # [`FreshCell`]
//!
//! A cell type to for use when mutating a value from concurrent processes; can be used to ensure
//! that a value created from older inputs never overwrites a value created from newer inputs, if
//! the update processes complete out of order.

use crate::prelude::*;



// =================
// === FreshCell ===
// =================

/// Cell that supports updating a value from concurrent processes.
///
/// When multiple concurrent processes may be producing a new value, a [`FreshCell`] can be used to
/// ensure that the value is never updated with the output of a process that started before the
/// process that produced the cell's current value (and was therefore working from older
/// inputs). For example, if:
/// - Some *Process A* captures some inputs, and begins producing a value for *Cell X*.
/// - Some *Process B* captures some newer inputs, and begins producing a value for *Cell X*.
/// - *Process B* finishes producing a value, and updates *Cell X*.
/// - *Process A* finishes producing a value.
/// Then a [`FreshCell`] will not allow *Process A* to overwrite the fresher value produced by
/// *Process B*.
pub struct FreshCell<T> {
    data:           Cell<T>,
    data_id:        Cell<Id>,
    last_setter_id: Cell<Id>,
}

impl<T> FreshCell<T> {
    /// Obtain a reference that can be used to set the value of the cell later. Any other existing
    /// setters become stale.
    pub fn setter(self: &Rc<Self>) -> Setter<T> {
        let cell = self.downgrade();
        let id = self.new_setter_id();
        Setter { cell, id }
    }

    /// Set the value now. Any outstanding setters will become stale.
    pub fn set(&self, data: T) {
        let id = self.new_setter_id();
        self.data.set(data);
        self.data_id.set(id);
    }

    fn new_setter_id(&self) -> Id {
        let id = self.last_setter_id.get() + 1;
        self.last_setter_id.set(id);
        id
    }
}

impl<T: Clone> FreshCell<T> {
    /// Get a copy of the current value.
    pub fn get_clone(&self) -> T {
        let clone = self.data.clone();
        clone.into_inner()
    }
}


// === Id ===

/// Counter that identifies relative age of a setter. A [`u64`] is chosen so that overflow need not
/// be a concern.
type Id = u64;




// ==============
// === Setter ===
// ==============

/// Can be used to set the value of a [`FreshCell`].
///
/// Note that it is not an error to drop this without using it; creating a [`Setter`] and dropping
/// it unused has the effect of invalidating any older setters, without setting any new value.
pub struct Setter<T> {
    cell: Weak<FreshCell<T>>,
    id:   Id,
}

impl<T> Setter<T> {
    /// Try to update the value of the cell.
    pub fn set(self, data: T) -> Result<(), Error> {
        let cell = self.cell.upgrade().ok_or(Error::Dropped)?;
        let fresh = cell.data_id.get() < self.id;
        if !fresh {
            return Err(Error::Stale);
        }
        cell.data.set(data);
        cell.data_id.set(self.id);
        Ok(())
    }
}


// === Error ===

/// Failure to update the value of a cell.
pub enum Error {
    /// There is no cell to update.
    Dropped,
    /// The setter
    Stale,
}
