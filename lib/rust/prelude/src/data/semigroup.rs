//! In mathematics, a semigroup is an algebraic structure consisting of a set together with an
//! associative binary operation. A semigroup generalizes a monoid in that there might not exist an
//! identity element. It also (originally) generalized a group (a monoid with all inverses) to a
//! type where every element did not have to have an inverse, thus the name semigroup.

use std::collections::HashMap;
use std::hash::BuildHasher;
use std::hash::Hash;
use std::iter::Extend;



// =================
// === Semigroup ===
// =================

/// Mutable Semigroup definition. Impls should satisfy the associativity law:
/// `x.concat(y.concat(z)) = x.concat(y).concat(z)`, in symbolic form:
/// `x <> (y <> z) = (x <> y) <> z`
pub trait PartialSemigroup<T> : Clone {
    /// An associative operation.
    fn concat_mut(&mut self, other:T);

    /// An associative operation.
    fn concat_ref(&self, other:T) -> Self where Self:Clone {
        self.clone().concat(other)
    }

    /// An associative operation.
    fn concat(mut self, other:T) -> Self {
        self.concat_mut(other);
        self
    }
}

impl<T>   Semigroup for T where T : PartialSemigroup<T> + for<'t> PartialSemigroup<&'t T> {}
pub trait Semigroup : PartialSemigroup<Self> + for<'t> PartialSemigroup<&'t Self> {
    fn partial_times_mut(&mut self, n:usize) {
        let val = self.clone();
        for _ in 0..n-1 {
            self.concat_mut(&val)
        }
    }

    fn partial_times(mut self, n:usize) -> Self {
        self.partial_times_mut(n);
        self
    }
}



// ====================
// === Stdlib Impls ===
// ====================

// === Option ===

impl<T:Semigroup> PartialSemigroup<&Option<T>> for Option<T> {
    fn concat_mut(&mut self, other:&Self) {
        if let Some(r) = other {
            match self {
                None    => *self = Some(r.clone()),
                Some(l) => l.concat_mut(r)
            }
        }
    }
}

impl<T:Semigroup> PartialSemigroup<Option<T>> for Option<T> {
    fn concat_mut(&mut self, other:Self) {
        if let Some(r) = other {
            match self {
                None    => *self = Some(r),
                Some(l) => l.concat_mut(r)
            }
        }
    }
}


// === HashMap ===

impl<K,V,S> PartialSemigroup<&HashMap<K,V,S>> for HashMap<K,V,S>
where K : Eq + Hash + Clone,
      V : Semigroup,
      S : Clone + BuildHasher {
    fn concat_mut(&mut self, other:&Self) {
        for (key,new_val) in other {
            let key = key.clone();
            self.entry(key)
                .and_modify(|val| val.concat_mut(new_val))
                .or_insert_with(|| new_val.clone());
        }
    }
}

impl<K,V,S> PartialSemigroup<HashMap<K,V,S>> for HashMap<K,V,S>
    where K : Eq + Hash + Clone,
          V : Semigroup,
          S : Clone + BuildHasher {
    fn concat_mut(&mut self, other:Self) {
        for (key,new_val) in other {
            self.entry(key)
                .and_modify(|val| val.concat_mut(&new_val))
                .or_insert(new_val);
        }
    }
}


// === Vec ===

impl<T:Clone> PartialSemigroup<&Vec<T>> for Vec<T> {
    fn concat_mut(&mut self, other:&Self) {
        self.extend(other.iter().cloned())
    }
}

impl<T:Clone> PartialSemigroup<Vec<T>> for Vec<T> {
    fn concat_mut(&mut self, other:Self) {
        self.extend(other.into_iter())
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn option() {
        assert_eq!(None::<Vec<usize>>.concat(&None)     , None);
        assert_eq!(Some(vec![1]).concat(&None)          , Some(vec![1]));
        assert_eq!(None.concat(&Some(vec![1]))          , Some(vec![1]));
        assert_eq!(Some(vec![1]).concat(&Some(vec![2])) , Some(vec![1,2]));
    }
}
