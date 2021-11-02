//! General purpose code for dealing with iterators.

#[allow(unused_imports)]
use crate::prelude::*;

use itertools::Itertools;

/// Applies predicate to all items and returns two vectors:
/// first with items not matching, second with items matching the predicate.
///
/// ```
/// use utils::iter::split_by_predicate;
///
/// let (odd, even) = split_by_predicate(1..=5, |i| i % 2 == 0);
/// assert_eq!(even, vec![2, 4]);
/// assert_eq!(odd, vec![1, 3, 5]);
/// ```
pub fn split_by_predicate<Iter, Item, Predicate>(
    input: Iter,
    predicate: Predicate,
) -> (Vec<Item>, Vec<Item>)
where
    Iter: IntoIterator<Item = Item> + Sized,
    Predicate: Fn(&Item) -> bool,
{
    let mut grouped = input.into_iter().into_group_map_by(predicate);
    (grouped.remove(&false).unwrap_or_default(), grouped.remove(&true).unwrap_or_default())
}
