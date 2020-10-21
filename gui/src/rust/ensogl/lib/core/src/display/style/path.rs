//! Path implementation used to navigate in the cascading style sheet hierarchy.

use crate::prelude::*;



// ============
// === Path ===
// ============

/// Path is a set of strings which describes how the variable or style sheet is nested in the
/// cascading style sheet map.
#[derive(Clone,Debug,PartialEq,Eq)]
#[allow(missing_docs)]
pub struct Path {
    pub rev_segments : Vec<String>
}

impl Path {
    /// Builds the path from the provided segment iterator. Please note that the internal path
    /// representation is reversed, as the style sheet dependencies in the style sheet map tree are
    /// also kept in a reversed order. Please use the `visualize` utility to inspect the internal
    /// structure and learn more.
    pub fn from_segments<T,I,Item>(t:T) -> Self
    where T : IntoIterator<IntoIter=I,Item=Item>,
          I : DoubleEndedIterator + Iterator<Item=Item>,
          Item : ToString {
        Self::from_rev_segments(t.into_iter().rev())
    }

    /// Builds the path from reversed segment iterator. See `from_segments` to learn more.
    pub fn from_rev_segments<T,Item>(t:T) -> Self
    where T:IntoIterator<Item=Item>, Item:ToString {
        Self { rev_segments : t.into_iter().map(|s|s.to_string().trim().to_string()).collect() }
    }

    /// Empty path constructor.
    pub fn empty() -> Self {
        Self { rev_segments : default() }
    }
}

impl AsRef<Path> for Path { fn as_ref(&self) -> &Path { self } }

impls! {              From<&str>   for Path { |t| Self::from_rev_segments(t.rsplit('.')) } }
impls! {              From<&&str>  for Path { |t| (*t).into() }}
impls! {              From<&Path>  for Path { |t| t.clone() }}
impls! { [T:ToString] From<Vec<T>> for Path { |t| Self::from_segments(t.into_iter()) }}

impl<T> From<&Vec<T>> for Path
where for<'t> &'t T : ToString {
    fn from(t:&Vec<T>) -> Self {
        Self::from_segments(t.iter())
    }
}

macro_rules! gen_var_path_conversions {
    ($($($num:tt)?),*) => {$(
        impl<T> From<&[T$(;$num)?]> for Path
        where for<'t> &'t T : ToString {
            fn from(t:&[T$(;$num)?]) -> Self {
                Self::from_segments(t.into_iter())
            }
        }
    )*};
}

gen_var_path_conversions!(1,2,3,4,5,6,7,8,9,10,);
