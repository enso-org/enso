//! Path implementation used to navigate in the cascading style sheet hierarchy.

use crate::prelude::*;



// ==================
// === StaticPath ===
// ==================

/// Static version of `Path`. Defined as a strongly typed wrapper over `&'static str`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct StaticPath {
    pub str: &'static str,
}

impl StaticPath {
    /// Constructor.
    pub const fn new(str: &'static str) -> Self {
        Self { str }
    }

    /// Conversion to `Path` provided as method for convenience.
    pub fn path(self) -> Path {
        self.into()
    }
}



// ============
// === Path ===
// ============

/// Path is a set of strings which describes how the variable or style sheet is nested in the
/// cascading style sheet map.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Path {
    pub rev_segments: Vec<String>,
}

impl Path {
    /// Builds the path from the provided segment iterator. Please note that the internal path
    /// representation is reversed, as the style sheet dependencies in the style sheet map tree are
    /// also kept in a reversed order. Please use the `visualize` utility to inspect the internal
    /// structure and learn more.
    pub fn from_segments<T, I, Item>(t: T) -> Self
    where
        T: IntoIterator<IntoIter = I, Item = Item>,
        I: DoubleEndedIterator + Iterator<Item = Item>,
        Item: ToString, {
        Self::from_rev_segments(t.into_iter().rev())
    }

    /// Builds the path from reversed segment iterator. See `from_segments` to learn more.
    pub fn from_rev_segments<T, Item>(t: T) -> Self
    where
        T: IntoIterator<Item = Item>,
        Item: ToString, {
        Self { rev_segments: t.into_iter().map(|s| s.to_string().trim().to_string()).collect() }
    }

    /// Empty path constructor.
    pub fn empty() -> Self {
        Self { rev_segments: default() }
    }

    /// Return a new one with the segment appended to the end.
    pub fn sub(&self, segment: impl Into<String>) -> Self {
        self.clone().into_sub(segment)
    }

    /// Consume the path and return a new one with the segment appended to the end.
    pub fn into_sub(mut self, segment: impl Into<String>) -> Self {
        self.rev_segments.reverse();
        self.rev_segments.push(segment.into());
        self.rev_segments.reverse();
        self
    }

    /// Consume the path and return a new one with the segments appended to the end.
    pub fn into_subs<I>(mut self, segment: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<String>, {
        self.rev_segments.reverse();
        self.rev_segments.extend(segment.into_iter().map(|t| t.into()));
        self.rev_segments.reverse();
        self
    }
}

impl AsRef<Path> for Path {
    fn as_ref(&self) -> &Path {
        self
    }
}

impls! {              From<&str>        for Path { |t| Self::from_rev_segments(t.rsplit('.')) } }
impls! {              From<&&str>       for Path { |t| (*t).into() }}
impls! {              From<String>      for Path { |t| t.as_str().into() }}
impls! {              From<&String>     for Path { |t| t.as_str().into() }}
impls! {              From<&Path>       for Path { |t| t.clone() }}
impls! {              From<StaticPath>  for Path { |t| t.str.into() }}
impls! {              From<&StaticPath> for Path { |t| t.str.into() }}
impls! { [T:ToString] From<Vec<T>>      for Path { |t| Self::from_segments(t.into_iter()) }}

impl<T> From<&Vec<T>> for Path
where for<'t> &'t T: ToString
{
    fn from(t: &Vec<T>) -> Self {
        Self::from_segments(t.iter())
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.rev_segments.iter().rev().join("."))
    }
}

macro_rules! gen_var_path_conversions {
    ($($($num:tt)?),* $(,)?) => {$(
        impl<T> From<&[T$(;$num)?]> for Path
        where for<'t> &'t T : ToString {
            fn from(t:&[T$(;$num)?]) -> Self {
                Self::from_segments(t.into_iter())
            }
        }
    )*};
}

// Generate instances of the following form (where N is the provided number):
//     impl<T> From<&[T;N]> for Path where for<'t> &'t T: ToString { ... }
gen_var_path_conversions!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
