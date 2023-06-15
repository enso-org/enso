//! Rust Generics implementation.
//!
//! Datatype-generic programming, also frequently just called generic programming or generics, is a
//! form of abstraction that allows defining functions that can operate on a large class of
//! data types. For a more in-depth introduction to generic programming in general, have a look at
//! [Datatype-Generic Programming](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/dgp.pdf), or
//! the [Libraries for Generic Programming](http://dreixel.net/research/pdf/lgph.pdf) paper.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![allow(incomplete_features)] // To be removed, see: https://github.com/enso-org/ide/issues/1559
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(generic_const_exprs)]

pub mod generic;
pub mod hlist;
pub mod tuple;

pub use generic::*;
pub use hlist::*;
pub use tuple::*;

/// Common traits.
pub mod traits {
    pub use super::generic::traits::*;
}



// =============
// === Tests ===
// =============

#[cfg(test)]
#[allow(clippy::unit_cmp)]
#[allow(clippy::let_unit_value)]
mod tests {
    use super::traits::*;
    use super::*;

    #[test]
    fn test_field_count() {
        let hlist = new![];
        assert_eq!(hlist.field_count(), 0);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.field_count(), 3);
        let tuple = ();
        assert_eq!(tuple.field_count(), 0);
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.field_count(), 3);
    }

    #[test]
    fn test_as_ref_fields() {
        let hlist = new![];
        assert_eq!(hlist.as_ref_fields(), new![]);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.as_ref_fields(), new![&1, &1.0, &vec![2, 3]]);
        let tuple = ();
        assert_eq!(tuple.as_ref_fields(), ());
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.as_ref_fields(), (&1, &1.0, &vec![2, 3]));
    }

    #[test]
    fn test_as_mut_fields() {
        let mut hlist = new![];
        assert_eq!(hlist.as_mut_fields(), new![]);
        let mut hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.as_mut_fields(), new![&mut 1, &mut 1.0, &mut vec![2, 3]]);
        let mut tuple = ();
        assert_eq!(tuple.as_mut_fields(), ());
        let mut tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.as_mut_fields(), (&mut 1, &mut 1.0, &mut vec![2, 3]));
    }

    #[test]
    fn test_into_first_field() {
        let hlist = new![1];
        assert_eq!(hlist.into_first_field(), 1);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.into_first_field(), 1);
        let tuple = (1,);
        assert_eq!(tuple.into_first_field(), 1);
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.into_first_field(), 1);
    }

    #[test]
    fn test_get_first_field() {
        let hlist = new![1];
        assert_eq!(hlist.first_field(), &1);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.first_field(), &1);
        let tuple = (1,);
        assert_eq!(tuple.first_field(), &1);
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.first_field(), &1);
    }

    #[test]
    fn test_get_first_field_mut() {
        let mut hlist = new![1];
        *hlist.first_field_mut() += 2;
        assert_eq!(hlist, new![3]);
        let mut hlist = new![1, 1.0, vec![2, 3]];
        *hlist.first_field_mut() += 2;
        assert_eq!(hlist, new![3, 1.0, vec![2, 3]]);
        let mut tuple = (1,);
        *tuple.first_field_mut() += 2;
        assert_eq!(tuple, (3,));
        let mut tuple = (1, 1.0, vec![2, 3]);
        *tuple.first_field_mut() += 2;
        assert_eq!(tuple, (3, 1.0, vec![2, 3]));
    }

    #[test]
    fn test_into_last_field() {
        let hlist = new![1];
        assert_eq!(hlist.into_last_field(), 1);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.into_last_field(), vec![2, 3]);
        let tuple = (1,);
        assert_eq!(tuple.into_last_field(), 1);
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.into_last_field(), vec![2, 3]);
    }

    #[test]
    fn test_get_last_field() {
        let hlist = new![1];
        assert_eq!(hlist.last_field(), &1);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.last_field(), &vec![2, 3]);
        let tuple = (1,);
        assert_eq!(tuple.last_field(), &1);
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.last_field(), &vec![2, 3]);
    }

    #[test]
    fn test_get_last_field_mut() {
        let mut hlist = new![1];
        *hlist.last_field_mut() += 2;
        assert_eq!(hlist, new![3]);
        let mut hlist = new![1, 1.0, vec![2, 3]];
        hlist.last_field_mut().push(4);
        assert_eq!(hlist, new![1, 1.0, vec![2, 3, 4]]);
        let mut tuple = (1,);
        *tuple.last_field_mut() += 2;
        assert_eq!(tuple, (3,));
        let mut tuple = (1, 1.0, vec![2, 3]);
        tuple.last_field_mut().push(4);
        assert_eq!(tuple, (1, 1.0, vec![2, 3, 4]));
    }

    #[test]
    fn test_into_field_at() {
        let hlist = new![1];
        assert_eq!(hlist.into_field_at::<0>(), 1);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.clone().into_field_at::<0>(), 1);
        assert_eq!(hlist.clone().into_field_at::<1>(), 1.0);
        assert_eq!(hlist.into_field_at::<2>(), vec![2, 3]);
        let tuple = (1,);
        assert_eq!(tuple.into_field_at::<0>(), 1);
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.clone().into_field_at::<0>(), 1);
        assert_eq!(tuple.clone().into_field_at::<1>(), 1.0);
        assert_eq!(tuple.into_field_at::<2>(), vec![2, 3]);
    }

    #[test]
    fn test_get_field_at() {
        let hlist = new![1];
        assert_eq!(hlist.field_at::<0>(), &1);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.field_at::<0>(), &1);
        assert_eq!(hlist.field_at::<1>(), &1.0);
        assert_eq!(hlist.field_at::<2>(), &vec![2, 3]);
        let tuple = (1,);
        assert_eq!(tuple.field_at::<0>(), &1);
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.field_at::<0>(), &1);
        assert_eq!(tuple.field_at::<1>(), &1.0);
        assert_eq!(tuple.field_at::<2>(), &vec![2, 3]);
    }

    #[test]
    fn test_get_field_at_mut() {
        let mut hlist = new![1];
        *hlist.field_at_mut::<0>() += 2;
        assert_eq!(hlist, new![3]);
        let mut hlist = new![1, 1.0, vec![2, 3]];
        *hlist.field_at_mut::<0>() += 2;
        *hlist.field_at_mut::<1>() += 3.0;
        hlist.field_at_mut::<2>().push(4);
        assert_eq!(hlist, new![3, 4.0, vec![2, 3, 4]]);
        let mut tuple = (1,);
        *tuple.field_at_mut::<0>() += 2;
        assert_eq!(tuple, (3,));
        let mut tuple = (1, 1.0, vec![2, 3]);
        *tuple.field_at_mut::<0>() += 2;
        *tuple.field_at_mut::<1>() += 3.0;
        tuple.field_at_mut::<2>().push(4);
        assert_eq!(tuple, (3, 4.0, vec![2, 3, 4]));
    }

    #[test]
    fn test_push_last_field() {
        let hlist = new![];
        assert_eq!(hlist.push_last_field(7), new![7]);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.push_last_field(7), new![1, 1.0, vec![2, 3], 7]);
        let tuple = ();
        assert_eq!(tuple.push_last_field(7), (7,));
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.push_last_field(7), (1, 1.0, vec![2, 3], 7));
    }

    #[test]
    fn test_push_first_field() {
        let hlist = new![];
        assert_eq!(hlist.push_first_field(7), new![7]);
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.push_first_field(7), new![7, 1, 1.0, vec![2, 3]]);
        let tuple = ();
        assert_eq!(tuple.push_first_field(7), (7,));
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.push_first_field(7), (7, 1, 1.0, vec![2, 3]));
    }

    #[test]
    fn test_pop_last_field() {
        let hlist = new![1];
        assert_eq!(hlist.pop_last_field(), (1, new![]));
        let hlist = new![1, 1.0, vec![2, 3]];
        assert_eq!(hlist.pop_last_field(), (vec![2, 3], new![1, 1.0]));
        let tuple = (1,);
        assert_eq!(tuple.pop_last_field(), (1, ()));
        let tuple = (1, 1.0, vec![2, 3]);
        assert_eq!(tuple.pop_last_field(), (vec![2, 3], (1, 1.0)));
    }

    #[test]
    fn test_map_fields_into() {
        let hlist = new![];
        assert_eq!(hlist.map_fields_into::<f32>(), new![]);
        let hlist = new![1_u16, 2_u16, 3_u16];
        assert_eq!(hlist.map_fields_into::<f32>(), new![1.0, 2.0, 3.0]);
        let tuple = ();
        assert_eq!(tuple.map_fields_into::<f32>(), ());
        let tuple = (1_u16, 2_u16, 3_u16);
        assert_eq!(tuple.map_fields_into::<f32>(), (1.0, 2.0, 3.0));
    }

    #[test]
    fn test_map_fields() {
        let hlist = new![];
        assert_eq!(hlist.map_fields(|t: usize| t + 1), new![]);

        let hlist = new![1, 2, 3];
        assert_eq!(hlist.map_fields(|t| t + 1), new![2, 3, 4]);

        let tuple = ();
        assert_eq!(tuple.map_fields(|t: usize| t + 1), ());

        let tuple = (1, 2, 3);
        assert_eq!(tuple.map_fields(|t| t + 1), (2, 3, 4));
    }

    #[test]
    fn test_field_iter() {
        let hlist = new![1, 2, 3];
        let mut sum = 0;
        hlist.into_field_iter(|t| sum += t);
        assert_eq!(sum, 6);

        let hlist = new![1, 2, 3];
        let mut sum = 0;
        hlist.field_iter(|t| sum += t);
        assert_eq!(sum, 6);

        let mut hlist = new![1, 2, 3];
        hlist.field_iter_mut(|t| *t += 1);
        assert_eq!(hlist, new![2, 3, 4]);

        let tuple = (1, 2, 3);
        let mut sum = 0;
        tuple.into_field_iter(|t| sum += t);
        assert_eq!(sum, 6);

        let tuple = (1, 2, 3);
        let mut sum = 0;
        tuple.field_iter(|t| sum += t);
        assert_eq!(sum, 6);

        let mut tuple = (1, 2, 3);
        tuple.field_iter_mut(|t| *t += 1);
        assert_eq!(tuple, (2, 3, 4));
    }
}
