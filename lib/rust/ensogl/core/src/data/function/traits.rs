//! Definitions of function-like traits with a wide range of implementations. Seethe docs in this
//! file to learn more.



// ===================
// === Definitions ===
// ===================

/// Defines a function-like trait and a set of implementations for a wide range of types, including
/// the empty type. This makes these traits a very powerful mechanism for implementing callback
/// like functionality. Consider the following structure:
///
/// ```text
/// pub struct Animator<OnStep = ()> {
///     on_step: OnStep,
/// }
///
/// impl<OnStep: Function1<f32>> Animator<OnStep> {
///     fn step(&self) {
///         self.on_step.call(0.0);
///     }
/// }
/// ```
///
/// This type can be parametrized with an user-defined callback, but the callback may also not be
/// defined (it will default to empty tuple), and the whole code will still compile and optimize
/// during compilation to remove the parametrization and the inner type all together.
///
/// For the input of `[mut] FnMut FnMut1<T1>`, the following code will be generated:
///
/// ```
/// pub trait FnMut1<T1> {
///     type Output;
///     fn call(&mut self, T1: T1) -> Self::Output;
/// }
///
/// impl<T1> FnMut1<T1> for () {
///     type Output = ();
///     fn call(&mut self, _: T1) {}
/// }
///
/// impl<T, T1> FnMut1<T1> for Option<T>
/// where T: FnMut1<T1>
/// {
///     type Output = Option<T::Output>;
///     fn call(&mut self, t1: T1) -> Self::Output {
///         match self {
///             Some(f) => Some(f.call(t1)),
///             None => None,
///         }
///     }
/// }
///
/// impl<F, T, T1> FnMut1<T1> for F
/// where F: FnMut(T1) -> T
/// {
///     type Output = T;
///     fn call(&mut self, t1: T1) -> Self::Output {
///         self(t1)
///     }
/// }
/// ```

macro_rules! define_fn {
    ($( [$($mut:ident)?] $fn_name:ident $name:ident $(<$($arg:ident),*>)?; )*) => {$(
        /// Function like element that can be called.
        pub trait $name $(<$($arg),*>)? {
            /// The output of the call method.
            type Output;
            /// The call method.
            fn call(& $($mut)? self,$($(_:$arg),*)?) -> Self::Output;
        }

        impl $(<$($arg),*>)? $name $(<$($arg),*>)? for () {
            type Output = ();
            fn call(& $($mut)? self,$($(_:$arg),*)?) {}
        }

        #[allow(non_snake_case)]
        #[allow(clippy::manual_map)]
        // We cannot really use map, as this code is generic for mut an non-mut types.
        // As such, some instantiations generate clippy warning, while others won't compile
        // with its suggested resolution.
        impl<T,$($($arg),*)?> $name $(<$($arg),*>)? for Option<T>
        where T : $name $(<$($arg),*>)? {
            type Output = Option<T::Output>;
            fn call(& $($mut)? self,$($($arg:$arg),*)?) -> Self::Output {
                match self {
                    Some(f) => Some(f.call($($($arg),*)?)),
                    None    => None,
                }
            }
        }

        #[allow(non_snake_case)]
        impl<F,T,$($($arg),*)?> $name $(<$($arg),*>)? for F
        where F : $fn_name($($($arg),*)?) -> T {
            type Output = T;
            fn call(& $($mut)? self,$($($arg:$arg),*)?) -> Self::Output {
                self($($($arg),*)?)
            }
        }
    )*};
}

define_fn! {
    []    Fn    Fn0;
    []    Fn    Fn1<T1>;
    []    Fn    Fn2<T1,T2>;
    []    Fn    Fn3<T1,T2,T3>;
    []    Fn    Fn4<T1,T2,T3,T4>;
    []    Fn    Fn5<T1,T2,T3,T4,T5>;
    [mut] FnMut FnMut0;
    [mut] FnMut FnMut1<T1>;
    [mut] FnMut FnMut2<T1,T2>;
    [mut] FnMut FnMut3<T1,T2,T3>;
    [mut] FnMut FnMut4<T1,T2,T3,T4>;
    [mut] FnMut FnMut5<T1,T2,T3,T4,T5>;
}
