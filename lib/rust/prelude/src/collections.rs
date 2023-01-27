//! This module exports collections which are popular enough to be available everywhere.

use std::borrow::BorrowMut;


// ==============
// === Export ===
// ==============

pub use std::collections::BTreeMap;
pub use std::collections::HashMap;
pub use std::collections::HashSet;



// ==================
// === HashMapOps ===
// ==================

pub trait HashMapOps<K, V>: BorrowMut<HashMap<K, V>> + Sized {
    /// Returns a mutable reference to the value corresponding to the key. If the map does not have
    /// this key present, the provided function is called to insert a value. The function may fail,
    /// in which case the error is returned and value is not inserted.
    fn get_or_insert_with_result<'a, E>(
        &'a mut self,
        key: K,
        value: impl FnOnce() -> Result<V, E>,
    ) -> Result<&'a mut V, E>
    where
        K: std::hash::Hash + Eq + 'a,
    {
        use std::collections::hash_map::Entry;
        let map = self.borrow_mut();
        match map.entry(key) {
            Entry::Occupied(entry) => Ok(entry.into_mut()),
            Entry::Vacant(entry) => {
                let value = value()?;
                Ok(entry.insert(value))
            }
        }
    }
}

impl<K, V> HashMapOps<K, V> for HashMap<K, V> {}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::Infallible;

    #[test]
    fn test_get_or_insert_with_result() {
        let mut map = HashMap::new();

        let new_entry_ok = map.get_or_insert_with_result::<Infallible>(1, || Ok(1));
        assert_eq!(new_entry_ok, Ok(&mut 1));

        let new_entry_err = map.get_or_insert_with_result(2, || Err("error"));
        assert_eq!(new_entry_err, Err("error"));

        let occupied_ok = map.get_or_insert_with_result::<Infallible>(1, || Ok(2));
        assert_eq!(occupied_ok, Ok(&mut 1));

        let occupied_err = map.get_or_insert_with_result(1, || Err("error"));
        assert_eq!(occupied_err, Ok(&mut 1));

        let ok_after_err = map.get_or_insert_with_result::<Infallible>(2, || Ok(3));
        assert_eq!(ok_after_err, Ok(&mut 3));
    }
}
