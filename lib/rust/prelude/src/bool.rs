/// Extensions to the [`bool`] type.

pub trait BoolOps {
    /// If true, returns the provided value. Otherwise, returns a default value.
    fn then_val_or_default<T: Default>(self, val: T) -> T;

    /// If true, runs the provided function. Otherwise, returns a default value.
    fn then_or_default<T: Default>(self, f: impl FnOnce() -> T) -> T;
}

impl BoolOps for bool {
    fn then_val_or_default<T: Default>(self, val: T) -> T {
        if self {
            val
        } else {
            Default::default()
        }
    }

    fn then_or_default<T: Default>(self, f: impl FnOnce() -> T) -> T {
        if self {
            f()
        } else {
            Default::default()
        }
    }
}
