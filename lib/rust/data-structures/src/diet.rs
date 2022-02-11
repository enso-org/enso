//! The DIET (Discrete Interval Encoding Tree) implementation.
//!
//! The discrete interval encoding tree is a structure for storing subsets of types having a total
//! order and a predecessor and a successor function. Follow the link to learn more:
//! https://web.engr.oregonstate.edu/~erwig/diet.
//!
//! # WARNING
//! This implementation is not finished. It is provided as one of alternative solutions to the
//! problem of efficient attribute memory management in EnsoGL. Read the docs of
//! [`ensogl::AttributeScopeData`] to learn more. This implementation has the following
//! shortcomings:
//!
//! 1. No implementation of merging of intervals in case they are not in the same layer. For
//!    example, assuming a tree with values `1` and `7` in one layer, and value `3` in child-layer,
//!    inserting the value `2` would not merge `1`,`2`, and `3` into a single [`Interval`].
//!
//! 2. No implementation of removing elements. This should be straightforward. The algorithm is
//!    described here: https://en.wikipedia.org/wiki/B-tree#Algorithms.
//!
//! # Benchmarks
//! This module contains a lot of benchmarks in order to compare different techniques of managing
//! free indexes for the needs of efficient attribute memory management in EnsoGL. Read the docs of
//! [`ensogl::AttributeScopeData`] to learn more.
#![allow(clippy::field_reassign_with_default)]

use crate::prelude::*;

use std::mem::MaybeUninit;



// ================
// === Interval ===
// ================

/// Closed interval. For example, [`Interval(1,2)`] means `[1,2]` in math.
#[derive(Clone, Copy, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Interval {
    pub start: usize,
    pub end:   usize,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Interval(start: usize, end: usize) -> Interval {
    Interval { start, end }
}

impl Debug for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Interval({:?},{:?})", self.start, self.end)
    }
}

impl From<usize> for Interval {
    fn from(t: usize) -> Self {
        Interval(t, t)
    }
}

impl From<(usize, usize)> for Interval {
    fn from(t: (usize, usize)) -> Self {
        Interval(t.0, t.1)
    }
}



// ============
// === Tree ===
// ============

// === Helpers ===

macro_rules! inc {
    (2) => {
        3
    };
    (4) => {
        5
    };
    (8) => {
        9
    };
    (16) => {
        17
    };
    (32) => {
        33
    };
    (64) => {
        65
    };
    (128) => {
        129
    };
    (256) => {
        257
    };
    (512) => {
        513
    };
}

/// This macro is used to define trees with different child count. After upgrading the compiler, we
/// might drop this macro and parametrize the tree with type-level `const usize` instead.
macro_rules! define_trees {
($( $modname:tt :: $name:tt($num:tt) )*) => {$(

pub use $modname::$name;

/// Module containing the specialized DIET implementation.
pub mod $modname {
    use super::*;


// === Constants ===

const DATA_SIZE : usize = $num;
type DataArray           = [Interval;$num];
// FIXME: Potential performance gain. We could initialize only the needed elements in this array
//        by changing it to `[MaybeUninit<$name>;inc!{$num}]`.
type ChildrenArray       = [$name;inc!{$num}];
type ChildrenArrayUninit = [MaybeUninit<$name>;inc!{$num}];


// === Main Impl ===

/// The DIET (Discrete Interval Encoding Tree) implementation.
///
/// The discrete interval encoding tree is a structure for storing subsets of types having a total
/// order and a predecessor and a successor function. Follow the link to learn more:
/// https://web.engr.oregonstate.edu/~erwig/diet.
#[derive(Clone)]
pub struct $name {
    pub (crate) data_count : usize,
    pub (crate) data       : DataArray,
    pub (crate) children   : Option<Box<ChildrenArray>>
}

impl $name {

    /// Create an empty data array. This function is safe because the intervals are build out of
    /// [`uint`]s, which can be initialized from raw memory. Follow the link to learn more:
    /// https://doc.rust-lang.org/std/mem/union.MaybeUninit.html.
    #[allow(unsafe_code)]
    pub (crate) fn empty_data_array() -> DataArray {
        unsafe { MaybeUninit::uninit().assume_init() }
    }

    /// Create an empty data array. This function uses unsafe Rust to initialize big arrays element
    /// by element. This is the official way of doing it. Follow the link to learn more:
    /// https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#initializing-an-array-element-by-element.
    #[allow(unsafe_code)]
    pub (crate) fn empty_children_array() -> ChildrenArray {
        let mut children: ChildrenArrayUninit = unsafe { MaybeUninit::uninit().assume_init() };
        for elem in &mut children[..] { *elem = MaybeUninit::new(default()); }
        unsafe { mem::transmute::<_,ChildrenArray>(children) }
    }

    /// Attaches uninitialized children array. Please note that this function is unsafe, as the
    /// attached children array has to be initialized to work properly. Thus, it is the
    /// responsibility of the user of this function to initialize it before it is used.
    fn unsafe_init_children(&mut self) -> &mut [$name] {
        self.children = Some(Box::new(Self::empty_children_array()));
        self.children.as_mut().unwrap().deref_mut()
    }

    /// Constructor.
    pub fn new() -> Self {
        let data_count = 0;
        let data       = Self::empty_data_array();
        let children   = None;
        Self {data_count,data,children}
    }

    /// Perform linear search of the data layer for the provided value. Returns [`Ok`] containing
    /// the index of the value or [`Err`] if the value was not found. In the later case, the result
    /// will contain the index where the value should be inserted in order to keep the right
    /// ordering.
    fn search_data(&self, t:usize) -> Result<usize,usize> {
        let mut out = Err(self.data_count);
        for i in 0..self.data_count {
            let interval = &self.data[i];
            if      t + 1 <  interval.start   { out = Err(i) ; break }
            else if t     <= interval.end + 1 { out = Ok(i)  ; break }
        }
        out
    }

    /// Split the current node in two parts assuming that it is a leaf node (without children).
    fn split_leaf(&self, left_split_index:usize, right_split_index:usize) -> ($name,$name) {
        let mut left = $name::default();
        left.data_count = left_split_index;
        // FIXME: Potential performance gain. We are splitting the current data in two arrays. The
        //        current data array will not be used anymore, so instead of creating a new array,
        //        we could reuse the current one.
        left.data[0..left_split_index].copy_from_slice(&self.data[0..left_split_index]);

        let mut right = $name::default();
        right.data_count = DATA_SIZE - right_split_index;
        // FIXME: Potential performance gain. We are splitting the current data in two arrays. The
        //        current data array will not be used anymore, so instead of creating a new array,
        //        we could reuse the current one.
        right.data[0..right.data_count].copy_from_slice(&self.data[right_split_index..]);

        (left,right)
    }

    /// Split the current node in two parts assuming that it is not a leaf node (with children).
    fn split
    ( data              : &mut DataArray
    , children          : &mut ChildrenArray
    , left_split_index  : usize
    , right_split_index : usize
    ) -> ($name,$name) {
        let mut p_left = $name::default();
        p_left.data_count = left_split_index;
        p_left.data[0..left_split_index].copy_from_slice(&data[0..left_split_index]);
        let mut left_children = Self::empty_children_array();
        // FIXME: Potential performance gain. We are splitting the current data in two arrays. The
        //        current data array will not be used anymore, so instead of creating a new array,
        //        we could reuse the current one. Moreover, the second half could take ownership
        //        of the elements instead of cloning them.
        left_children[0..=left_split_index].clone_from_slice(&children[0..=left_split_index]);
        p_left.children = Some(Box::new(left_children));

        let mut p_right = $name::default();
        p_right.data_count = DATA_SIZE - right_split_index;
        p_right.data[0..p_right.data_count].copy_from_slice(&data[right_split_index..]);
        let mut right_children = Self::empty_children_array();
        // FIXME: Potential performance gain. We are splitting the current data in two arrays. The
        //        current data array will not be used anymore, so instead of creating a new array,
        //        we could reuse the current one. Moreover, the second half could take ownership
        //        of the elements instead of cloning them.
        right_children[0..=p_right.data_count].clone_from_slice(&children[right_split_index..]);
        p_right.children = Some(Box::new(right_children));

        (p_left,p_right)
    }

    /// Insert a new value into this tree.
    pub fn insert(&mut self, t:usize) {
        if let Some((median,left,right)) = self.insert_internal(t) {
            let mut new_root = $name::default();
            new_root.data_count   = 1;
            new_root.data[0]      = median;
            let new_root_children = new_root.unsafe_init_children();
            new_root_children[0]  = left;
            new_root_children[1]  = right;
            *self = new_root;
        }
    }

    /// Internal helper for the `insert` function.
    fn insert_internal(&mut self, t:usize) -> Option<(Interval,$name,$name)> {
        match self.search_data(t) {
            Err(pos) => {
                match &mut self.children {
                    None => {
                        if self.data_count < DATA_SIZE {
                            // Insert Case (1)
                            self.data[pos..].rotate_right(1);
                            self.data[pos] = Interval(t,t);
                            self.data_count += 1;
                            None
                        } else {
                            let median_ix = DATA_SIZE / 2;
                            let (median,(left,right)) = if pos == median_ix {
                                // Insert Case (2)
                                (Interval(t,t),self.split_leaf(median_ix,median_ix))
                            } else if pos < median_ix {
                                // Insert Case (3)
                                let (mut left,right) = self.split_leaf(median_ix-1, median_ix);
                                left.insert_internal(t);
                                (self.data[median_ix-1],(left,right))
                            } else {
                                // Insert Case (4)
                                let (left, mut right) = self.split_leaf(median_ix, median_ix+1);
                                right.insert_internal(t);
                                (self.data[median_ix],(left,right))
                            };
                            Some((median,left,right))
                        }
                    }
                    Some(children) => {
                        if let Some((median,left,right)) = children[pos].insert_internal(t) {
                            if self.data_count < DATA_SIZE {
                                // Insert Case (1-4)
                                self.data[pos..].rotate_right(1);
                                children[pos..].rotate_right(1);
                                self.data[pos] = median;
                                children[pos] = left;
                                children[pos+1] = right;
                                self.data_count += 1;
                                None
                            } else {
                                // NOTE: Stack-overflow causing branch. Read docs of the module to
                                //       learn more.

                                let median_ix = DATA_SIZE / 2;
                                let data      = &mut self.data;

                                if pos == median_ix {
                                    // Insert Case (5)

                                    let mut split = |l,r| Self::split(data,children,l,r);
                                    let (mut p_left, mut p_right) = split(median_ix,median_ix);

                                    let left_children        = p_left.children.as_mut().unwrap();
                                    let right_children       = p_right.children.as_mut().unwrap();
                                    left_children[median_ix] = left;
                                    right_children[0]        = right;

                                    Some((median,p_left,p_right))

                                } else if pos < median_ix {
                                    // Insert Case (6)

                                    let left_split_ix  = median_ix - 1;
                                    let right_split_ix = median_ix;
                                    let mut split      = |l,r| Self::split(data,children,l,r);
                                    let (mut p_left,p_right) = split(left_split_ix,right_split_ix);

                                    let branch_median_ix = pos;
                                    let left_children    = p_left.children.as_mut().unwrap();
                                    left_children[branch_median_ix..].rotate_right(1);
                                    left_children[branch_median_ix]   = left;
                                    left_children[branch_median_ix+1] = right;
                                    p_left.data[branch_median_ix..].rotate_right(1);
                                    p_left.data[branch_median_ix] = median;
                                    p_left.data_count += 1;

                                    Some((self.data[left_split_ix],p_left,p_right))

                                } else {
                                    // Insert Case (7)

                                    let left_split_ix  = median_ix;
                                    let right_split_ix = median_ix + 1;
                                    let mut split      = |l,r| Self::split(data,children,l,r);
                                    let (p_left,mut p_right) = split(left_split_ix,right_split_ix);

                                    let branch_median_ix = pos-right_split_ix;
                                    let right_children   = p_right.children.as_mut().unwrap();
                                    right_children[branch_median_ix..].rotate_right(1);
                                    right_children[branch_median_ix]   = left;
                                    right_children[branch_median_ix+1] = right;
                                    p_right.data[branch_median_ix..].rotate_right(1);
                                    p_right.data[branch_median_ix] = median;
                                    p_right.data_count += 1;

                                    Some((self.data[left_split_ix],p_left,p_right))
                                }
                            }
                        } else { None }
                    },
                }
            },

            Ok(pos) => {
                let interval = &mut self.data[pos];
                if t < interval.start {
                    interval.start = t;
                }
                else if t > interval.end {
                    interval.end = t;
                    let next_pos = pos + 1;
                    if next_pos < self.data_count {
                        let next_interval = self.data[next_pos];
                        if next_interval.start == t + 1 {
                            // Merging intervals.
                            let interval = &mut self.data[pos];
                            interval.end = next_interval.end;
                            self.data[next_pos..].rotate_left(1);
                            self.data_count -= 1;
                        }
                    }
                }
                None
            }
        }
    }

    /// Convert this tree to vector of non-overlapping intervals in ascending order.
    pub fn to_vec(&self) -> Vec<Interval> {
        let mut v = vec![];
        if let Some(children) = &self.children {
            for i in 0..self.data_count {
                v.extend(children[i].to_vec());
                v.push(self.data[i])
            }
            v.extend(children[self.data_count].to_vec());
        } else {
            for i in 0..self.data_count {
                v.push(self.data[i])
            }
        }
        v
    }
}

impl Default for $name {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for $name {
    fn eq(&self, other:&Self) -> bool {
        if self.data_count != other.data_count {
            return false;
        }
        for i in 0..self.data_count {
            if self.data[i] != other.data[i] {
                return false;
            }
        }
        match (&self.children,&other.children) {
            (None,None) => {}
            (Some(children1),Some(children2)) => {
                for i in 0..=self.data_count {
                    if children1[i] != children2[i] {
                        return false;
                    }
                }
            }
            _ => return false
        }
        true
    }
}

impl Eq for $name {}

impl Debug for $name {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut repr = vec![];
        if let Some(children) = &self.children {
            for i in 0..self.data_count {
                repr.push(format!("{:?}", children[i]));
                repr.push(format!("{:?}", self.data[i]));
            }
            repr.push(format!("{:?}", children[self.data_count]));
        } else {
            for i in 0..self.data_count {
                repr.push(format!("{:?}", self.data[i]));
            }
        }
        write!(f, "$name({})", repr.join(","))
    }
}

})*};}

define_trees! {
    tree2::Tree2(2)
    tree4::Tree4(4)
    tree8::Tree8(8)
    tree16::Tree16(16)
    tree32::Tree32(32)
    tree64::Tree64(64)
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;


    // === Tree4 Testing Utilities ===

    trait FromSorted<T> {
        fn from_sorted(t: T) -> Self;
    }

    trait LeafFromSorted<T> {
        fn leaf_from_sorted(t: T) -> Self;
    }

    impl FromSorted<(Tree4, (usize, usize), Tree4)> for Tree4 {
        fn from_sorted(t: (Tree4, (usize, usize), Tree4)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 1;
            tree.data[0] = Interval((t.1).0, (t.1).1);
            let mut children = Self::empty_children_array();
            children[0] = t.0;
            children[1] = t.2;
            tree.children = Some(Box::new(children));
            tree
        }
    }

    impl FromSorted<(Tree4, usize, Tree4)> for Tree4 {
        fn from_sorted(t: (Tree4, usize, Tree4)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 1;
            tree.data[0] = Interval(t.1, t.1);
            let mut children = Self::empty_children_array();
            children[0] = t.0;
            children[1] = t.2;
            tree.children = Some(Box::new(children));
            tree
        }
    }

    impl FromSorted<(Tree4, usize, Tree4, usize, Tree4)> for Tree4 {
        fn from_sorted(t: (Tree4, usize, Tree4, usize, Tree4)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 2;
            tree.data[0] = Interval(t.1, t.1);
            tree.data[1] = Interval(t.3, t.3);
            let mut children = Self::empty_children_array();
            children[0] = t.0;
            children[1] = t.2;
            children[2] = t.4;
            tree.children = Some(Box::new(children));
            tree
        }
    }

    impl FromSorted<(Tree4, usize, Tree4, usize, Tree4, usize, Tree4)> for Tree4 {
        fn from_sorted(t: (Tree4, usize, Tree4, usize, Tree4, usize, Tree4)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 3;
            tree.data[0] = Interval(t.1, t.1);
            tree.data[1] = Interval(t.3, t.3);
            tree.data[2] = Interval(t.5, t.5);
            let mut children = Self::empty_children_array();
            children[0] = t.0;
            children[1] = t.2;
            children[2] = t.4;
            children[3] = t.6;
            tree.children = Some(Box::new(children));
            tree
        }
    }

    impl FromSorted<(Tree4, usize, Tree4, usize, Tree4, usize, Tree4, usize, Tree4)> for Tree4 {
        fn from_sorted(t: (Tree4, usize, Tree4, usize, Tree4, usize, Tree4, usize, Tree4)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 4;
            tree.data[0] = Interval(t.1, t.1);
            tree.data[1] = Interval(t.3, t.3);
            tree.data[2] = Interval(t.5, t.5);
            tree.data[3] = Interval(t.7, t.7);
            let mut children = Self::empty_children_array();
            children[0] = t.0;
            children[1] = t.2;
            children[2] = t.4;
            children[3] = t.6;
            children[4] = t.8;
            tree.children = Some(Box::new(children));
            tree
        }
    }


    impl<T1> LeafFromSorted<(T1,)> for Tree4
    where T1: Into<Interval>
    {
        fn leaf_from_sorted(t: (T1,)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 1;
            tree.data[0] = t.0.into();
            tree
        }
    }

    impl<T1, T2> LeafFromSorted<(T1, T2)> for Tree4
    where
        T1: Into<Interval>,
        T2: Into<Interval>,
    {
        fn leaf_from_sorted(t: (T1, T2)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 2;
            tree.data[0] = t.0.into();
            tree.data[1] = t.1.into();
            tree
        }
    }

    impl<T1, T2, T3> LeafFromSorted<(T1, T2, T3)> for Tree4
    where
        T1: Into<Interval>,
        T2: Into<Interval>,
        T3: Into<Interval>,
    {
        fn leaf_from_sorted(t: (T1, T2, T3)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 3;
            tree.data[0] = t.0.into();
            tree.data[1] = t.1.into();
            tree.data[2] = t.2.into();
            tree
        }
    }

    impl<T1, T2, T3, T4> LeafFromSorted<(T1, T2, T3, T4)> for Tree4
    where
        T1: Into<Interval>,
        T2: Into<Interval>,
        T3: Into<Interval>,
        T4: Into<Interval>,
    {
        fn leaf_from_sorted(t: (T1, T2, T3, T4)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 4;
            tree.data[0] = t.0.into();
            tree.data[1] = t.1.into();
            tree.data[2] = t.2.into();
            tree.data[3] = t.3.into();
            tree
        }
    }

    impl FromSorted<((usize, usize),)> for Tree4 {
        fn from_sorted(t: ((usize, usize),)) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 1;
            tree.data[0] = Interval((t.0).0, (t.0).1);
            tree
        }
    }



    impl FromSorted<((usize, usize), (usize, usize))> for Tree4 {
        fn from_sorted(t: ((usize, usize), (usize, usize))) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 2;
            tree.data[0] = Interval((t.0).0, (t.0).1);
            tree.data[1] = Interval((t.1).0, (t.1).1);
            tree
        }
    }

    impl FromSorted<((usize, usize), (usize, usize), (usize, usize))> for Tree4 {
        fn from_sorted(t: ((usize, usize), (usize, usize), (usize, usize))) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 3;
            tree.data[0] = Interval((t.0).0, (t.0).1);
            tree.data[1] = Interval((t.1).0, (t.1).1);
            tree.data[2] = Interval((t.2).0, (t.2).1);
            tree
        }
    }

    #[allow(clippy::type_complexity)]
    impl FromSorted<((usize, usize), (usize, usize), (usize, usize), (usize, usize))> for Tree4 {
        fn from_sorted(
            t: ((usize, usize), (usize, usize), (usize, usize), (usize, usize)),
        ) -> Self {
            let mut tree = Tree4::default();
            tree.data_count = 4;
            tree.data[0] = Interval((t.0).0, (t.0).1);
            tree.data[1] = Interval((t.1).0, (t.1).1);
            tree.data[2] = Interval((t.2).0, (t.2).1);
            tree.data[3] = Interval((t.3).0, (t.3).1);
            tree
        }
    }

    impl FromSorted<(usize,)> for Tree4 {
        fn from_sorted(t: (usize,)) -> Self {
            Self::from_sorted(((t.0, t.0),))
        }
    }

    impl FromSorted<(usize, usize)> for Tree4 {
        fn from_sorted(t: (usize, usize)) -> Self {
            Self::from_sorted(((t.0, t.0), (t.1, t.1)))
        }
    }

    impl FromSorted<(usize, usize, usize)> for Tree4 {
        fn from_sorted(t: (usize, usize, usize)) -> Self {
            Self::from_sorted(((t.0, t.0), (t.1, t.1), (t.2, t.2)))
        }
    }

    impl FromSorted<(usize, usize, usize, usize)> for Tree4 {
        fn from_sorted(t: (usize, usize, usize, usize)) -> Self {
            Self::from_sorted(((t.0, t.0), (t.1, t.1), (t.2, t.2), (t.3, t.3)))
        }
    }

    fn t<T>(t: T) -> Tree4
    where Tree4: FromSorted<T> {
        <Tree4 as FromSorted<T>>::from_sorted(t)
    }

    fn _l<T>(t: T) -> Tree4
    where Tree4: LeafFromSorted<T> {
        <Tree4 as LeafFromSorted<T>>::leaf_from_sorted(t)
    }

    macro_rules! t {
        ($($ts:tt)*) => {
            t(($($ts)*,))
        };
    }

    macro_rules! _l {
        ($($ts:tt)*) => {
            l(($($ts)*,))
        };
    }


    // === Tests ===

    fn intervals(bounds: &[(usize, usize)]) -> Vec<Interval> {
        bounds.iter().copied().map(|(a, b)| Interval(a, b)).collect()
    }

    fn check(tree: &Tree4, bounds: &[(usize, usize)]) {
        assert_eq!(tree.to_vec(), intervals(bounds));
    }

    #[test]
    fn leaf_insertion() {
        let mut v = Tree4::default();
        check(&v, &[]);
        v.insert(1);
        check(&v, &[(1, 1)]);
        v.insert(3);
        check(&v, &[(1, 1), (3, 3)]);
        v.insert(5);
        check(&v, &[(1, 1), (3, 3), (5, 5)]);
        v.insert(6);
        check(&v, &[(1, 1), (3, 3), (5, 6)]);
        v.insert(2);
        check(&v, &[(1, 3), (5, 6)]);
        v.insert(4);
        check(&v, &[(1, 6)]);
    }

    #[test]
    fn deep_insertion() {
        let mut v = Tree4::default();
        check(&v, &[]);
        v.insert(10);
        check(&v, &[(10, 10)]);
        v.insert(30);
        check(&v, &[(10, 10), (30, 30)]);
        v.insert(50);
        check(&v, &[(10, 10), (30, 30), (50, 50)]);
        v.insert(70);
        assert_eq!(v, t!(10, 30, 50, 70));
        v.insert(90);
        assert_eq!(v, t!(t!(10, 30), 50, t!(70, 90)));
        v.insert(110);
        assert_eq!(v, t!(t!(10, 30), 50, t!(70, 90, 110)));
        v.insert(130);
        assert_eq!(v, t!(t!(10, 30), 50, t!(70, 90, 110, 130)));
        v.insert(150);
        assert_eq!(v, t!(t!(10, 30), 50, t!(70, 90), 110, t!(130, 150)));
        v.insert(72);
        assert_eq!(v, t!(t!(10, 30), 50, t!(70, 72, 90), 110, t!(130, 150)));
        v.insert(74);
        assert_eq!(v, t!(t!(10, 30), 50, t!(70, 72, 74, 90), 110, t!(130, 150)));
        v.insert(76);
        assert_eq!(v, t!(t!(10, 30), 50, t!(70, 72), 74, t!(76, 90), 110, t!(130, 150)));
        v.insert(32);
        assert_eq!(v, t!(t!(10, 30, 32), 50, t!(70, 72), 74, t!(76, 90), 110, t!(130, 150)));
        v.insert(34);
        assert_eq!(v, t!(t!(10, 30, 32, 34), 50, t!(70, 72), 74, t!(76, 90), 110, t!(130, 150)));
        v.insert(36);
        assert_eq!(
            v,
            t!(t!(10, 30), 32, t!(34, 36), 50, t!(70, 72), 74, t!(76, 90), 110, t!(130, 150))
        );
        v.insert(52);
        assert_eq!(
            v,
            t!(t!(10, 30), 32, t!(34, 36), 50, t!(52, 70, 72), 74, t!(76, 90), 110, t!(130, 150))
        );
        v.insert(54);
        assert_eq!(
            v,
            t!(
                t!(10, 30),
                32,
                t!(34, 36),
                50,
                t!(52, 54, 70, 72),
                74,
                t!(76, 90),
                110,
                t!(130, 150)
            )
        );
    }

    #[test]
    fn insert_case_1() {
        let mut v = t!(10, 20);
        v.insert(0);
        assert_eq!(v, t!(0, 10, 20));
        let mut v = t!(10, 20);
        v.insert(15);
        assert_eq!(v, t!(10, 15, 20));
        let mut v = t!(10, 20);
        v.insert(30);
        assert_eq!(v, t!(10, 20, 30));
    }

    #[test]
    fn insert_case_2() {
        let mut v1 = t!(t!(10, 20, 30, 40), 50, t!(60, 70, 80, 90));
        let mut v2 = v1.clone();
        v1.insert(25);
        assert_eq!(v1, t!(t!(10, 20), 25, t!(30, 40), 50, t!(60, 70, 80, 90)));
        v2.insert(75);
        assert_eq!(v2, t!(t!(10, 20, 30, 40), 50, t!(60, 70), 75, t!(80, 90)));
    }

    #[test]
    fn insert_case_3() {
        let mut v1 = t!(t!(10, 20, 30, 40), 50, t!(60, 70, 80, 90));
        let mut v2 = v1.clone();
        v1.insert(15);
        assert_eq!(v1, t!(t!(10, 15), 20, t!(30, 40), 50, t!(60, 70, 80, 90)));
        v2.insert(0);
        assert_eq!(v2, t!(t!(0, 10), 20, t!(30, 40), 50, t!(60, 70, 80, 90)));
    }

    #[test]
    fn insert_case_4() {
        let mut v1 = t!(t!(10, 20, 30, 40), 50, t!(60, 70, 80, 90));
        let mut v2 = v1.clone();
        v1.insert(35);
        assert_eq!(v1, t!(t!(10, 20), 30, t!(35, 40), 50, t!(60, 70, 80, 90)));
        v2.insert(45);
        assert_eq!(v2, t!(t!(10, 20), 30, t!(40, 45), 50, t!(60, 70, 80, 90)));
    }

    #[test]
    fn insert_case_5() {
        let mut v = t!(t!(10), 20, t!(30), 40, t!(50, 52, 54, 56), 60, t!(70), 80, t!(90));
        v.insert(58);
        assert_eq!(
            v,
            t!(t!(t!(10), 20, t!(30), 40, t!(50, 52)), 54, t!(t!(56, 58), 60, t!(70), 80, t!(90)))
        );
    }

    #[test]
    fn insert_case_6() {
        let mut v = t!(t!(10, 12, 14, 16), 20, t!(30), 40, t!(50), 60, t!(70), 80, t!(90));
        v.insert(18);
        assert_eq!(
            v,
            t!(t!(t!(10, 12), 14, t!(16, 18), 20, t!(30)), 40, t!(t!(50), 60, t!(70), 80, t!(90)))
        );


        let mut v = t!(t!(10), 20, t!(30, 32, 34, 36), 40, t!(50), 60, t!(70), 80, t!(90));
        v.insert(38);
        assert_eq!(
            v,
            t!(t!(t!(10), 20, t!(30, 32), 34, t!(36, 38)), 40, t!(t!(50), 60, t!(70), 80, t!(90)))
        );
    }

    #[test]
    fn insert_case_7() {
        let mut v = t!(t!(10), 20, t!(30), 40, t!(50), 60, t!(70, 72, 74, 76), 80, t!(90));
        v.insert(78);
        assert_eq!(
            v,
            t!(t!(t!(10), 20, t!(30), 40, t!(50)), 60, t!(t!(70, 72), 74, t!(76, 78), 80, t!(90)))
        );

        let mut v = t!(t!(10), 20, t!(30), 40, t!(50), 60, t!(70), 80, t!(90, 92, 94, 96));
        v.insert(98);
        assert_eq!(
            v,
            t!(t!(t!(10), 20, t!(30), 40, t!(50)), 60, t!(t!(70), 80, t!(90, 92), 94, t!(96, 98)))
        );
    }

    #[test]
    fn insert_deep_bubble() {
        let mut v = t!(
            t!(100),
            200,
            t!(300),
            400,
            t!(t!(500), 510, t!(520), 530, t!(540, 542, 544, 546), 550, t!(560), 570, t!(580)),
            600,
            t!(700),
            800,
            t!(900)
        );
        v.insert(548);
        assert_eq!(
            v,
            t!(
                t!(t!(100), 200, t!(300), 400, t!(t!(500), 510, t!(520), 530, t!(540, 542))),
                544,
                t!(t!(t!(546, 548), 550, t!(560), 570, t!(580)), 600, t!(700), 800, t!(900))
            )
        )
    }
}



// ==================
// === Benchmarks ===
// ==================

extern crate test;


// This is a simplified implementation created for the needs of benchmarking of the
// `std::collections::BTreeSet`. It works correctly only for inserting unit intervals (where
// `start` = `end`).
#[cfg(test)]
impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// This is a simplified implementation created for the needs of benchmarking of the
// `std::collections::BTreeSet`. It works correctly only for inserting unit intervals (where
// `start` = `end`).
#[cfg(test)]
impl Ord for Interval {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if other.start + 1 < self.start {
            std::cmp::Ordering::Greater
        } else if other.start > self.end + 1 {
            std::cmp::Ordering::Less
        } else {
            std::cmp::Ordering::Equal
        }
    }
}

/// # How the results were measured
///
/// The results provided below are shown only for intuition building and would vary depending on the
/// used hardware. They were measured by using the MacBook Pro 2019 with Intel Core i9 2.4GHz.
///
///
/// # Summary
///
/// There are several interesting facts about the current implementation:
///
/// 1. It seems that (at least for now) the best performing implementation is the `Tree16`.
///
/// 2. When performing insertions of ascending, non-overlapping intervals, the `Tree16` performs
///    60% SLOWER than `std::collections::BTreeSet`.
///
/// 3. When performing insertions of descending, non-overlapping intervals, the `Tree16` performs
///    40% SLOWER than `std::collections::BTreeSet`.
///
/// 4. In case of `Tree16`, the insertion in ascending order is 50% slower than in descending order.
///    In case of `std::collections::BTreeSet`, insertion in ascending order is 40% slower than in
///    descending order.
///
/// 5. This implementation is 4x FASTER than the `lz_diet` crate.
#[cfg(test)]
mod benches {
    use super::*;
    use test::Bencher;

    /// # Results (ms)
    ///                                -> BEST <-
    ///        | Tree2 | Tree4 | Tree8 | Tree16 | Tree32 |
    ///   10^4 | 12.5  | 4     | 2.3   | 1.4    | 1.8    |
    ///   10^5 |       | 63.9  | 31.6  | 18.3   | 21.2   |
    ///   10^6 |       |       |       | 285.5  |        |
    #[bench]
    fn bench_insert_ascending(b: &mut Bencher) {
        b.iter(|| {
            let mut v = Tree16::default();
            for i in 0..test::black_box(1000) {
                v.insert(i * 2);
            }
        });
    }

    /// # Results (ms)
    ///                                -> BEST <-
    ///        | Tree2 | Tree4 | Tree8 | Tree16 | Tree32 | Tree64 |
    ///   10^4 | 12.3  | 3.6   | 1.8   | 0.92   | 1      | 1.7    |
    ///   10^5 | 200   | 62    | 27.5  | 12     | 12     | 18.9   |
    ///   10^6 |       |       |       | 212    |        |        |
    #[bench]
    fn bench_insert_descending(b: &mut Bencher) {
        b.iter(|| {
            let max = test::black_box(100_000);
            let mut v = Tree16::default();
            for i in 0..max {
                v.insert((max - i) * 2);
            }
        });
    }

    /// # Results (ms)
    ///                                -> BEST <-
    ///        | Tree2 | Tree4 | Tree8 | Tree16 | Tree32 | Tree64 |
    ///   10^4 | 14.1  | 5.2   | 3.4   | 2.7    | 3.32   | 4.8    |
    ///   10^5 |       |       | 43.9  | 32     | 39.5   |        |
    #[bench]
    fn bench_insert_ascending_growing(b: &mut Bencher) {
        b.iter(|| {
            let max = test::black_box(1000);
            let mut v = Tree16::default();
            for i in 0..max {
                v.insert(i * 4);
            }
            for i in 0..max {
                v.insert(i * 4 + 1);
            }
            for i in 0..max {
                v.insert(i * 4 + 2);
            }
        });
    }

    /// # Results (ms)
    ///
    ///   10^4 | 0.92 |
    ///   10^5 | 11.8 |
    ///   10^6 | 149  |
    #[bench]
    fn bench_insert_ascending_std(b: &mut Bencher) {
        b.iter(|| {
            let mut v = std::collections::BTreeSet::<Interval>::default();
            for i in 1..test::black_box(1000) {
                let j = i * 2;
                v.insert(Interval(j, j));
            }
        });
    }

    /// # Results (ms)
    ///
    ///   10^4 | 0.6 |
    ///   10^5 | 8.8 |
    ///   10^6 | 101 |
    #[bench]
    fn bench_insert_descending_std(b: &mut Bencher) {
        b.iter(|| {
            let mut v = std::collections::BTreeSet::<Interval>::default();
            let max = test::black_box(1000);
            for i in 0..max {
                let j = (max - i) * 2;
                v.insert(Interval(j, j));
            }
        });
    }

    /// # Results (ms)
    ///
    ///   10^4 | 0.08  |
    ///   10^5 | 9.7   |
    ///   10^6 | 115.5 |
    #[bench]
    fn bench_insert_ascending_std_usize(b: &mut Bencher) {
        b.iter(|| {
            let mut v = std::collections::BTreeSet::<usize>::default();
            for i in 1..test::black_box(1_000_000) {
                v.insert(i * 2);
            }
        });
    }

    /// # Results (ms)
    ///
    ///   10^5 | 0.1 |
    ///   10^6 | 2.5 |
    #[bench]
    fn bench_insert_vec_non_sorted(b: &mut Bencher) {
        b.iter(|| {
            let mut v = Vec::<usize>::default();
            for i in 1..test::black_box(1000) {
                v.push(i * 2);
            }
        });
    }

    /// # Results (ms)
    ///
    ///        | 100  | 1000 | 10_000 | (sort_every)
    ///   10^4 | 0.3  | 0.09 | 0.06   |
    ///   10^5 | 26   | 3.5  | 0.9    |
    ///   10^6 |      |      | 63.8   |
    #[bench]
    fn bench_insert_vec_sort_every_x(b: &mut Bencher) {
        b.iter(|| {
            let sort_every = test::black_box(10000);
            let mut v = Vec::<usize>::default();
            for i in 1..test::black_box(1000) {
                v.push(i * 2);
                if i % sort_every == 0 {
                    v.sort_unstable()
                }
            }
        });
    }

    /// # Results (ms)
    ///
    ///   10^5 | 0.04 |
    ///   10^6 | 0.5  |
    ///   10^7 | 7.5  |
    ///   10^8 | 89.4 |
    #[bench]
    fn vec_sort_already_almost_sorted(b: &mut Bencher) {
        let mut v = Vec::<usize>::default();
        let num = test::black_box(1000);
        for i in 0..num {
            v.push(num - i);
        }
        b.iter(|| v.sort_unstable());
    }

    /// # Results (ms)
    ///
    ///   10^8 | 8 |
    ///
    /// # Comparison to not using `Rc<Cell<...>>`
    /// Note that there is NO DIFFERENCE between this and the version without `Rc<Cell<...>>`.
    /// However, this may behave differently in real-world use case, so we need to make benchmarks
    /// before using it in EnsoGL attribute manageent sytem. Read the docs of
    /// [`ensogl::AttributeScopeData`] to learn more.
    #[bench]
    fn mode_rc_cell_num(b: &mut Bencher) {
        let v = Rc::new(Cell::new(0));
        let num = test::black_box(100_000_000);
        b.iter(|| {
            for i in 0..num {
                if i % 2 == 0 {
                    v.set(v.get() + 1)
                } else {
                    v.set(v.get() - 1)
                }
            }
        });
    }

    /// # Results (ms)
    ///
    ///   10^8 | 8 |
    #[bench]
    fn mode_num(b: &mut Bencher) {
        let mut v = 0;
        let num = test::black_box(100_000_000);
        b.iter(|| {
            for i in 0..num {
                if i % 2 == 0 {
                    v += 1
                } else {
                    v -= 1
                }
            }
        });
    }

    // /// Benchmarks of the `lz_diet-0.1.6` crate. Disabled in order not to include it in the final
    // /// binary.
    // /// # Results (ms)
    // ///   10^4 | 7.4  |
    // ///   10^5 | 85.1 |
    // #[bench]
    // fn bench_insert_ascending_lz_diet(b:&mut Bencher) {
    //     b.iter(|| {
    //         let mut v = lz_diet::Diet::new();
    //         for i in 0 .. test::black_box(1000_00) {
    //             v.insert(i*2);
    //         }
    //     });
    // }
}
