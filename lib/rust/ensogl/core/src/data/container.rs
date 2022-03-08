// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]



// ==============
// === AddMut ===
// ==============

/// An abstraction for container which can be provided with new elements. The element type is
/// polymorphic, allowing the container to reuse the function for different item types.
/// Please note that this trait is very similar to the `std::ops::Add` one, however, it allows for
/// a much nicer API. For example, you can write `foo.bar.add(x)`, because `foo.bar` will be
/// automatically used as mutable reference while using this trait.
pub trait AddMut<T> {
    type Output = ();
    fn add(&mut self, component: T) -> Self::Output;
}



// =======================
// === CachingIterator ===
// =======================

/// Iterator wrapper caching the last retrieved value.
///
/// The item type is `(Option<T>, T)` where the second tuple element is
/// a current value and first element is a previous one `None` on the first
/// iteration.
#[derive(Debug)]
pub struct CachingIterator<T: Clone, It: Iterator<Item = T>> {
    last: Option<T>,
    iter: It,
}

impl<T: Clone, It: Iterator<Item = T>> Iterator for CachingIterator<T, It> {
    type Item = (Option<T>, T);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|value| {
            let new_last = Some(value.clone());
            let old_last = std::mem::replace(&mut self.last, new_last);
            (old_last, value)
        })
    }
}

/// A trait for wrapping in caching iterator.
///
/// It is implemented for each iterator over cloneable items.
pub trait IntoCachingIterator {
    type Item: Clone;
    type Iter: Iterator<Item = Self::Item>;

    fn cache_last_value(self) -> CachingIterator<Self::Item, Self::Iter>;
}

impl<T: Clone, It: Iterator<Item = T>> IntoCachingIterator for It {
    type Item = T;
    type Iter = Self;

    fn cache_last_value(self) -> CachingIterator<Self::Item, Self::Iter> {
        CachingIterator { last: None, iter: self }
    }
}

#[cfg(test)]
mod tests {
    use crate::data::container::IntoCachingIterator;

    #[test]
    fn caching_iterator_on_empty() {
        let data = Vec::<i32>::new();
        let result = data.iter().cache_last_value().next();
        assert_eq!(None, result);
    }

    #[test]
    fn caching_iterator() {
        let data = vec![2, 3, 5];
        let mut caching_iterator = data.iter().cloned().cache_last_value();
        assert_eq!(Some((None, 2)), caching_iterator.next());
        assert_eq!(Some((Some(2), 3)), caching_iterator.next());
        assert_eq!(Some((Some(3), 5)), caching_iterator.next());
        assert_eq!(None, caching_iterator.next());
    }
}
