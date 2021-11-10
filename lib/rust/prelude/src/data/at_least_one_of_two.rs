//! Definition of `AtLeastOneOfTwo`.



// =======================
// === AtLeastOneOfTwo ===
// =======================

/// A struct similar to `Option` and `Either`. It can contain the first value, or the second value,
/// or both of them at the same time.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum AtLeastOneOfTwo<T1, T2> {
    First(T1),
    Second(T2),
    Both(T1, T2),
}

impl<T: PartialEq> AtLeastOneOfTwo<T, T> {
    /// Checks whether the values are equal.
    pub fn same(&self) -> bool {
        match self {
            Self::Both(t1, t2) => t1 == t2,
            _ => false,
        }
    }
}

impl<T1, T2> AtLeastOneOfTwo<T1, T2> {
    /// Extracts the first value if exists.
    pub fn first(&self) -> Option<&T1> {
        match self {
            Self::Both(t1, _) => Some(t1),
            Self::First(t1) => Some(t1),
            _ => None,
        }
    }

    /// Extracts the second value if exists.
    pub fn second(&self) -> Option<&T2> {
        match self {
            Self::Both(_, t2) => Some(t2),
            Self::Second(t2) => Some(t2),
            _ => None,
        }
    }

    /// Extracts both of the value if exist.
    pub fn both(&self) -> Option<(&T1, &T2)> {
        match self {
            Self::Both(t1, t2) => Some((t1, t2)),
            _ => None,
        }
    }
}
