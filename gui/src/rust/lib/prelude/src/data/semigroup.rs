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
pub trait Semigroup : Clone {
    /// An associative operation.
    fn concat_mut(&mut self, other:&Self);

    /// An associative operation which consumes the argument.
    fn concat_mut_take(&mut self, other:Self) {
        self.concat_mut(&other)
    }

    /// Repeat a value n times. Given that this works on a Semigroup it is allowed to fail if you
    /// request 0 or fewer repetitions, and the default definition will do so.
    fn partial_times(&mut self, n:usize) {
        let val = self.clone();
        for _ in 0..n-1 {
            self.concat_mut(&val)
        }
    }
}

/// Immutable Semigroup definition. Impls should satisfy the associativity law:
/// `x.concat(y.concat(z)) = x.concat(y).concat(z)`, in symbolic form:
/// `x <> (y <> z) = (x <> y) <> z`
pub trait SemigroupIm : Clone {
    /// An associative operation.
    fn concat(&self, other:&Self) -> Self;

    /// An associative operation which consumes the argument.
    fn concat_take(&self, other:Self) -> Self {
        self.concat(&other)
    }

    /// Repeat a value n times. Given that this works on a Semigroup it is allowed to fail if you
    /// request 0 or fewer repetitions, and the default definition will do so.
    fn partial_times(&self, n:usize) -> Self where Self:Clone {
        std::iter::repeat(self).take(n-1).fold(self.clone(),|l,r| l.concat(r))
    }
}


// === Default Impls ===

impl<T:Semigroup> SemigroupIm for T {
    default fn concat(&self, other:&Self) -> Self {
        let mut this = self.clone();
        this.concat_mut(other);
        this
    }

    default fn concat_take(&self, other:Self) -> Self {
        let mut this = self.clone();
        this.concat_mut_take(other);
        this
    }
}



// ====================
// === Stdlib Impls ===
// ====================

// === Option ===

impl<T:Semigroup> Semigroup for Option<T> {
    fn concat_mut(&mut self, other:&Self) {
        if let Some(r) = other {
            match self {
                None    => *self = Some(r.clone()),
                Some(l) => l.concat_mut(r)
            }
        }
    }

    fn concat_mut_take(&mut self, other:Self) {
        if let Some(r) = other {
            match self {
                None    => *self = Some(r),
                Some(l) => l.concat_mut_take(r)
            }
        }
    }
}


// === HashMap ===

impl<K,V,S> Semigroup for HashMap<K,V,S>
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

    fn concat_mut_take(&mut self, other:Self) {
        for (key,new_val) in other {
            self.entry(key)
                .and_modify(|val| val.concat_mut(&new_val))
                .or_insert(new_val);
        }
    }
}


// === Vec ===

impl<T:Clone> Semigroup for Vec<T> {
    fn concat_mut(&mut self, other:&Self) {
        self.extend(other.iter().cloned())
    }

    fn concat_mut_take(&mut self, other:Self) {
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
