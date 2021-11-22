//! A class for monoids (types with an associative binary operation that has an identity) with
//! various general-purpose instances.

use super::semigroup::Semigroup;



// ===============
// === Monoid ====
// ===============

/// Mutable Monoid definition.
pub trait Monoid: Default + Semigroup {
    /// Repeat a value n times. Given that this works on a Monoid it will not fail if you request 0
    /// or fewer repetitions.
    fn times_mut(&mut self, n: usize) {
        if n == 0 {
            *self = Default::default()
        } else {
            let val = self.clone();
            for _ in 0..n - 1 {
                self.concat_mut(&val)
            }
        }
    }

    fn times(&self, n: usize) -> Self {
        std::iter::repeat(self).take(n).fold(Default::default(), |l, r| l.concat_ref(r))
    }
}


// === Default Impls ===

impl<T> Monoid for T where T: Default + Semigroup {}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn option() {
        let vec_nop: Vec<usize> = vec![];
        let vec_1_2: Vec<usize> = vec![1, 2];
        let vec_1_2_times_3: Vec<usize> = vec![1, 2, 1, 2, 1, 2];
        assert_eq!(vec_1_2.times(0), vec_nop);
        assert_eq!(vec_1_2.times(1), vec_1_2);
        assert_eq!(vec_1_2.times(3), vec_1_2_times_3);
    }
}


// TODO: Think what to do with this. It would not be needed if tuples implement Iter. Alternatively
// we could immplement own tuple type.

//trait Foldable {
//    type Item : Monoid;
//    fn fold(self) -> Self::Item;
//}
//
//
//
//macro_rules! replace {
//    ($a:tt,$b:tt) => {$b};
//}
//
//
//macro_rules! define_foldable_for_tuple {
//    (0$(,$num:tt)*) => {
//        impl<T:Monoid> Foldable for (T,$(replace!{$num,T}),*) {
//            type Item = T;
//            fn fold(self) -> Self::Item {
//                self.0$(.concat(self.$num))*
//            }
//        }
//
//        impl<T:Monoid> Foldable for &(T,$(replace!{$num,T}),*) {
//            type Item = T;
//            fn fold(self) -> Self::Item {
//                self.0.clone()$(.concat(&self.$num))*
//            }
//        }
//    };
//}
//
//define_foldable_for_tuple![0];
//define_foldable_for_tuple![0,1];
//define_foldable_for_tuple![0,1,2];
//define_foldable_for_tuple![0,1,2,3];
//define_foldable_for_tuple![0,1,2,3,4];
//define_foldable_for_tuple![0,1,2,3,4,5];
//define_foldable_for_tuple![0,1,2,3,4,5,6];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7,8];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7,8,9];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7,8,9,10];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7,8,9,10,11];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7,8,9,10,11,12];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7,8,9,10,11,12,13];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7,8,9,10,11,12,13,14];
//define_foldable_for_tuple![0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
