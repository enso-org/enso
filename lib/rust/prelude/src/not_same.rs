//! [`NotSame`] trait definition.



/// Defines the [`NotSame`] trait. It can be used to disambiguate conflicting trait implementations.
/// For example, it is not allowed to implement `impl<U, T> From<MyType<U>> for MyType<T>`, because
/// Rust standard library defines `impl<T> From<T> for T`. This trait allows to disambiguate such
/// cases by writing `impl<U, T> From<MyType<U>> for MyType<T> where (U, T) : NotSame`. However,
/// because of some strange reasons, it does not work if it is defined in another crate and has to
/// be defined locally, on-demand. As soon as it will be possible to define it in prelude, it should
/// be refactored. See its usages to learn more.
#[macro_export]
macro_rules! define_not_same_trait {
    () => {
        auto trait NotSame {}
        impl<T> !NotSame for (T, T) {}
    };
}
