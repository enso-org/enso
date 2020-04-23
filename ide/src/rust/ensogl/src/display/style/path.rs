//! Path implementation used to navigate in the cascading style sheet hierarchy.

use crate::prelude::*;



// ============
// === Path ===
// ============

/// Path is a set of strings which describes how the variable or style sheet is nested in the
/// cascading style sheet map.
#[derive(Clone,Debug)]
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
          Item : Into<String> {
        Self::from_rev_segments(t.into_iter().rev())
    }

    /// Builds the path from reversed segment iterator. See `from_segments` to learn more.
    pub fn from_rev_segments<T,Item>(t:T) -> Self
    where T : IntoIterator<Item=Item>,
          Item : Into<String> {
        let rev_segments = t.into_iter().map(|s|s.into()).collect();
        Self {rev_segments}
    }
}

impl From<&str> for Path {
    fn from(t:&str) -> Self {
        Self::from_rev_segments(t.rsplit('.'))
    }
}

impl From<&Path> for Path {
    fn from(t:&Path) -> Self {
        t.clone()
    }
}

macro_rules! gen_var_path_conversions {
    ($($($num:tt)?),*) => {$(
        impl<T:?Sized> From<&[&T$(;$num)?]> for Path
        where for <'t> &'t T : Into<String> {
            fn from(t:&[&T$(;$num)?]) -> Self {
                Self::from_segments(t.into_iter().map(|s|*s))
            }
        }
    )*};
}

gen_var_path_conversions!(1,2,3,4,5,6,7,8,9,10,);
