//! This module defines set of common macros which are useful across different projects.


// ==============
// === Export ===
// ==============

pub use enso_shapely::ForEachVariant;



/// Allows for nicer definition of impls, similar to what Haskell or Scala does. Reduces the needed
/// boilerplate. For example, the following usage:
///
/// ```text
/// struct A { name:String };
/// impls! { From<A> for String { |t| t.name.clone() } }
/// ```
///
/// compiles to:
/// ```
/// struct A {
///     name: String,
/// };
/// impl From<A> for String {
///     fn from(t: A) -> Self {
///         t.name.clone()
///     }
/// }
/// ```
///
/// This macro is meant to support many standard traits (like From) and should grow in the future.
/// Currently supported ones are:
/// * From<…>
/// * From + &From<…>
/// * Into + &Into<…>
/// * PhantomFrom<…>
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

    ($([$($impl_params:tt)*])? From + &From <$ty:ty> for $target:ty $(where [$($bounds:tt)*])? {
        |$arg:tt| $($result:tt)*
    } ) => {
        #[allow(clippy::redundant_closure_call)]
        #[allow(clippy::identity_conversion)]
        impl <$($($impl_params)*)?> From <$ty> for $target $(where $($bounds)*)? {
            fn from (arg:$ty) -> Self {
                (|$arg:$ty| $($result)*)(arg)
            }
        }

        #[allow(clippy::redundant_closure_call)]
        #[allow(clippy::identity_conversion)]
        impl <$($($impl_params)*)?> From <&$ty> for $target $(where $($bounds)*)? {
            fn from (arg:&$ty) -> Self {
                (|$arg:&$ty| $($result)*)(arg)
            }
        }
    };

    ($([$($impl_params:tt)*])? Into + &Into <$ty:ty> for $target:ty $(where [$($bounds:tt)*])? {
        |$arg:tt| $($result:tt)*
    } ) => {
        #[allow(clippy::redundant_closure_call)]
        #[allow(clippy::identity_conversion)]
        impl <$($($impl_params)*)?> Into <$ty> for $target $(where $($bounds)*)? {
            fn into(self) -> $ty {
                (|$arg:Self| $($result)*)(self)
            }
        }

        #[allow(clippy::redundant_closure_call)]
        #[allow(clippy::identity_conversion)]
        impl <$($($impl_params)*)?> Into <$ty> for &$target $(where $($bounds)*)? {
            fn into(self) -> $ty {
                (|$arg:Self| $($result)*)(self)
            }
        }
    };

    ($([$($impl_params:tt)*])? PhantomFrom<$ty:ty> for $target:ty {
        $($result:tt)*
    } ) => {
        impl <$($($impl_params)*)?> From <ZST<$ty>> for $target {
            fn from (_:ZST<$ty>) -> Self {
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



// ==============
// === Lambda ===
// ==============

/// Clones all arguments from the first argument list by using `CloneRef` and defines lambda with
/// arguments from the second argument list (if present). For example, the following usage
///
/// ```text
/// f! { (a,b)(c) a + b + c }
/// ```
///
/// is equivalent to:
///
/// ```text
/// {
///     let a = a.clone_ref();
///     let b = b.clone_ref();
///     move |c| { a + b + c }
/// }
/// ```
#[macro_export]
macro_rules! f {
    ([$($name:ident),*] ($($args:tt)*) $($expr:tt)*) => {
        {
            $(let $name = $name.clone_ref();)*
            move |$($args)*| { $($expr)* }
        }
    };

    ([$($name:ident),*] $($expr:tt)*) => {
        {
            $(let $name = $name.clone_ref();)*
            move || { $($expr)* }
        }
    };

    (($($args:tt)*) $name:ident . $($toks:tt)*) => {
        f! { [$name] ($($args)*) $name . $($toks)* }
    };

    (($($args:tt)*) { $name:ident . $($toks:tt)* }) => {
        f! { [$name] ($($args)*) { $name . $($toks)* } }
    };

    ($name:ident . $($toks:tt)*) => {
        f! { [$name] $name . $($toks)* }
    };
}

/// Variant of the `f` macro producing a lambda which drops its first argument.
#[macro_export]
macro_rules! f_ {
    ([$($name:ident),*] $($expr:tt)*) => {
        f! { [$($name),*] (_) $($expr)*  }
    };

    ($name:ident . $($toks:tt)*) => {
        f_! { [$name] $name . $($toks)* }
    };

    ( { $name:ident . $($toks:tt)* } ) => {
        f_! { [$name] { $name . $($toks)* } }
    };
}

/// Variant of the `f` macro producing a lambda which drops its first and second arguments.
#[macro_export]
macro_rules! f__ {
    ([$($name:ident),*] $($expr:tt)*) => {
        f! { [$($name),*] (_,_) $($expr)*  }
    };

    ($name:ident . $($toks:tt)*) => {
        f__! { [$name] $name . $($toks)* }
    };

    ( { $name:ident . $($toks:tt)* } ) => {
        f__! { [$name] { $name . $($toks)* } }
    };
}



// ===================
// === Unreachable ===
// ===================

/// A macro for use in situations where the code is unreachable.
///
/// This macro will panic in debug builds, but in release builds it expands to
/// the unsafe [`std::hint::unreachable_unchecked()`] function, which allows the
/// compiler to optimise more.
#[macro_export]
macro_rules! unreachable_panic {
    () => {
        unreachable_panic!("This code was marked as unreachable.")
    };
    ($msg:tt) => {
        if cfg!(debug_assertions) {
            panic!($msg)
        } else {
            use std::hint::unreachable_unchecked;
            #[allow(unsafe_code)]
            unsafe {
                unreachable_unchecked()
            }
        }
    };
}



// ====================
// === ReflectMatch ===
// ====================

/// Used to match a value against a set of candidates, while keeping track of the candidates.
///
/// This achieves the same function as using a `HashMap` to dispatch between a set of handlers, but
/// does not require reifying the handlers, which can be inconvenient (e.g. if they contain
/// `.await`, of if they need conflicting captures from the environment).
///
/// # Example
///
/// ```
/// use enso_prelude::*;
///
/// let selected = "foo";
/// let out = reflect_match!(match selected as options {
///     "bar" => Ok(1),
///     "baz" => Ok(2),
///     _ => Err(format!("Unexpected choice: {selected}. Must be one of: {options:?}.")),
/// });
/// ```
///
/// This is functionally equivalent to:
///
/// ```
/// # use std::collections::HashMap;
///
/// let selected = "foo";
/// let mut dispatch = HashMap::new();
/// dispatch.insert("bar", 1);
/// dispatch.insert("baz", 2);
/// let options = dispatch.keys();
/// let error = format!("Unexpected choice: {selected}. Must be one of: {options:?}.");
/// let out = dispatch.get(selected).ok_or(error);
/// ```
#[macro_export]
macro_rules! reflect_match {
    (@acc ($dispatch:ident, $value:expr, $candidates:ident, {
        _ => $fallback:expr $(,)?
    }) -> {$( $branches:tt )*}) => {{
        let mut $dispatch = $crate::ReflectMatch::new($value);
        match () {
            $( $branches )*
            _ => {
                let $candidates = $dispatch.into_candidates();
                $fallback
            }
        }
    }};
    (@acc ($dispatch:ident, $value:expr, $candidates:ident, {
        $candidate:literal => $branch:expr,
        $( $rest:tt )*
    }) -> {$( $branches:tt )*}) => {
        reflect_match!(@acc ($dispatch, $value, $candidates, { $( $rest )* }) -> {
            $( $branches )*
            _ if $dispatch.matches($candidate) => $branch,
        })
    };
    (match $value:tt as $candidates:tt { $( $branches:tt )* }) => {
        reflect_match!(@acc (dispatch, $value, $candidates, { $( $branches )* }) -> {})
    };
}


// === ReflectMatch Runtime Support ===

/// Match a value against a set of candidates; if no match is found, the list of candidates is
/// available. See [`reflect_match!`] for motivation and usage examples.
#[derive(Debug)]
pub struct ReflectMatch<T, U> {
    value:      T,
    candidates: Vec<U>,
}

impl<T, U> ReflectMatch<T, U> {
    /// Create a new dispatcher, for a given value.
    pub fn new(value: T) -> Self {
        let candidates = Default::default();
        Self { value, candidates }
    }

    /// Test the value against a candidate. Return whether it's a match.
    pub fn matches(&mut self, key: U) -> bool
    where T: PartialEq<U> {
        let matches = self.value == key;
        self.candidates.push(key);
        matches
    }

    /// Return the candidates the match was tested against.
    pub fn into_candidates(self) -> Vec<U> {
        self.candidates
    }
}
