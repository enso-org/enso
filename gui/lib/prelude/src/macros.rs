//! This macro defines set of common macros which are useful across different projects.


/// Allows for nicer definition of impls, similar to what Haskell or Scala does. Reduces the needed
/// boilerplate. For example, the following usage:
///
/// ```compile_fail
/// struct A { name:String };
/// impls! { From<A> for String { |t| t.name.clone() } }
/// ```
///
/// compiles to:
/// ```
/// struct A { name:String };
/// impl From<A> for String {
///     fn from(t:A) -> Self {
///         t.name.clone()
///     }
/// }
/// ```
///
/// This macro is meant to support many standard traits (like From) and should grow in the future.
#[macro_export]
macro_rules! impls {
    ($([$($impl_params:tt)*])? From<$ty:ty> for $target:ty $(where [$($bounds:tt)*])? {
        |$arg:tt| $($result:tt)*
    } ) => {
        #[allow(clippy::redundant_closure_call)]
        impl <$($($impl_params)*)?> From <$ty> for $target $(where $($bounds)*)? {
            fn from (arg:$ty) -> Self {
                (|$arg:$ty| $($result)*)(arg)
            }
        }
    };

    ($([$($impl_params:tt)*])? PhantomFrom<$ty:ty> for $target:ty {
        $($result:tt)*
    } ) => {
        impl <$($($impl_params)*)?> From <PhantomData<$ty>> for $target {
            fn from (_:PhantomData<$ty>) -> Self {
                $($result)*
            }
        }
    };
}

#[macro_export]
macro_rules! alias {
    ($( $(#$meta:tt)* $name:ident = {$($tok:tt)*} )*) => {$(
        $(#$meta)*
        pub trait $name: $($tok)* {}
        impl<T:$($tok)*> $name for T {}
    )*};

    (no_docs $( $(#$meta:tt)* $name:ident = {$($tok:tt)*} )*) => {$(
        $(#$meta)*
        #[allow(missing_docs)]
        pub trait $name: $($tok)* {}
        impl<T:$($tok)*> $name for T {}
    )*};
}
