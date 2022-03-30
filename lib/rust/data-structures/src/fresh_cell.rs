//! # [`Fresh`]
//!
//! When mutating a value from concurrent processes, [`Fresh`] can be used in conjunction with a
//! type of [`Cell`] to ensure that a value created from older inputs never overwrites a value
//! created from newer inputs (which otherwise may occur if the update processes do not complete in
//! the order they were started).
//!
//! Example:
//! ```
//! let cell = Rc::new(Fresh::new(Cell::new(1)));
//! let a = cell.setter();
//! let b = cell.setter(); // This operation invalidates `a`.
//! b.set(2).unwrap();
//! assert_eq!(a.set(3), Err(Error::StaleUpdate));
//! assert_eq!(cell.get(), 2)
//! ```

use crate::prelude::*;



// =============
// === Fresh ===
// =============

/// Supports updating a value from concurrent processes.
///
/// When multiple concurrent processes may be producing a new value, a [`Fresh`] can be used to
/// ensure that the value is never updated with the output of a process that started before the
/// process that produced the cell's current value (and was therefore working from older
/// inputs). For example, if:
/// - Some *Process A* captures some inputs, and begins producing a value for *Cell X*.
/// - Some *Process B* captures some newer inputs, and begins producing a value for *Cell X*.
/// - *Process B* finishes producing a value, and updates *Cell X*.
/// - *Process A* finishes producing a value.
/// Then a [`Fresh`] will not allow *Process A* to overwrite the fresher value produced by
/// *Process B*.
#[derive(Debug, Default)]
pub struct Fresh<C> {
    cell:           C,
    data_id:        Cell<Id>,
    last_setter_id: Cell<Id>,
}

impl<C: CellSetter> Fresh<C> {
    /// Create a new instance guarding a cell.
    pub fn new(cell: C) -> Self {
        let data_id = default();
        let last_setter_id = default();
        Fresh { cell, data_id, last_setter_id }
    }

    /// Obtain a reference that can be used to set the value of the cell later. Any other existing
    /// setters become stale.
    pub fn setter(self: &Rc<Self>) -> Setter<C> {
        let cell = self.downgrade();
        let id = self.new_setter_id();
        Setter { cell, id }
    }

    /// Set the value now. Any outstanding setters will become stale.
    pub fn set(&self, data: C::Item) {
        let id = self.new_setter_id();
        self.cell.set(data);
        self.data_id.set(id);
    }

    /// Return whether the current value is up-to-date, or a new setter has been created since the
    /// last time a value was set.
    pub fn is_fresh(&self) -> bool {
        self.data_id.get() == self.last_setter_id.get()
    }

    fn new_setter_id(&self) -> Id {
        let id = self.last_setter_id.get() + 1;
        self.last_setter_id.set(id);
        id
    }
}

impl<C> Deref for Fresh<C> {
    type Target = C;
    fn deref(&self) -> &Self::Target {
        &self.cell
    }
}



// === Id ===

/// Counter that identifies relative age of a setter. A [`u64`] is chosen so that overflow need not
/// be a concern.
type Id = u64;



// ==============
// === Setter ===
// ==============

/// Can be used to set the value of a [`Fresh`].
///
/// Note that it is not an error to drop this without using it; creating a [`Setter`] and dropping
/// it unused has the effect of invalidating any older setters, without setting any new value.
#[derive(Debug)]
pub struct Setter<C> {
    cell:  Weak<Fresh<C>>,
    id:    Id,
}

impl<C: CellSetter> Setter<C> {
    /// Try to update the value of the cell.
    pub fn set(self, data: C::Item) -> Result<(), Error> {
        let rc = self.cell.upgrade().ok_or(Error::Dropped)?;
        let fresh = rc.data_id.get() < self.id;
        if !fresh {
            return Err(Error::StaleUpdate);
        }
        rc.cell.set(data);
        rc.data_id.set(self.id);
        Ok(())
    }
}


// === Error ===

/// Failure to update the value of a cell.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    /// There is no cell to update.
    Dropped,
    /// The setter was invalidated by creation of a newer setter.
    StaleUpdate,
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    /// Basic test of setting a cell type.
    #[test]
    fn test_set_cell() {
        let cell = Fresh::new(Cell::new(1));
        cell.set(2);
        assert_eq!(cell.get(), 2)
    }

    /// Basic test of setting a cell type.
    #[test]
    fn test_set_clonecell() {
        let cell = Fresh::new(CloneCell::new(1));
        cell.set(2);
        assert_eq!(cell.get(), 2)
    }
}
