//! Hashing utilities.

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;



// =====================
// === Hashing Utils ===
// =====================

/// Calculate the hash of the provided value using the `DefaultHasher`.
pub fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
