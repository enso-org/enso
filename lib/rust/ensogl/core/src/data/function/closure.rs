// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]



// ===============
// === Closure ===
// ===============

/// Two implementations are provided, you can switch between them by setting the
/// `no_unboxed_callbacks` flag. We should use unboxed closures whenever
/// possible, however there is a bug in Rust which prevents this version from
/// compilation: https://github.com/rust-lang/rust/issues/65918
#[macro_export]
macro_rules! closure {
    (fn $name:ident <$($param:ident : $param_type:ty),*>
        ($($arg:ident : $arg_type:ty),*) -> $type:ident {
            |$($larg:ident : $larg_type:ty),*| $($body:tt)*
        }
    ) => {
        closure! {
            fn $name <$($param:$param_type),*>
            ($( $arg  : $arg_type  ),*)
            ($( $larg : $larg_type ),*)
            -> $type
            { $($body)* }
        }
    };

    (fn $name:ident <$($param:ident : $param_type:ty),*>
        ($($arg:ident : $arg_type:ty),*) -> $type:ident {
            || $($body:tt)*
        }
    ) => {
        closure! {
            fn $name <$($param:$param_type),*>
            ($($arg:$arg_type),*) ()
            -> $type
            { $($body)* }
        }
    };

    (fn $name:ident
        ($($arg:ident : $arg_type:ty),*) -> $type:ident {
            |$($larg:ident : $larg_type:ty),*| $($body:tt)*
        }
    ) => {
        closure! {
            fn $name <>
            ($( $arg  : $arg_type  ),*)
            ($( $larg : $larg_type ),*)
            -> $type
            { $($body)* }
        }
    };

    (fn $name:ident
        ($($arg:ident : $arg_type:ty),*) -> $type:ident {
            || $($body:tt)*
        }
    ) => {
        closure! {
            fn $name <>
            ($($arg:$arg_type),*) ()
            -> $type
            { $($body)* }
        }
    };

    (fn $name:ident
        <$($param:ident : $param_type:ty),*>
        ($($arg:ident   : $arg_type:ty),*)
        ($($larg:ident  : $larg_type:ty),*)
        -> $type:ident
        $body:tt
    ) => { paste::item! {
        #[cfg(not(feature = "no_unboxed_callbacks"))]
        /// Closure type.
        pub type $type<$($param),*> =
            impl Fn($($larg_type),*) + Clone;

        #[cfg(not(feature = "no_unboxed_callbacks"))]
        /// Closure constructor.
        pub fn $name<$($param:$param_type),*>
        ($($arg:$arg_type),*) -> $type<$($param),*> {
            move |$($larg),*| $body
        }

        #[cfg(feature = "no_unboxed_callbacks")]
        /// Closure type.
        pub type $type<$($param),*> =
            Box<dyn Fn($($larg_type),*)>;

        #[cfg(feature = "no_unboxed_callbacks")]
        /// Closure constructor.
        pub fn $name<$($param:$param_type),*>
        ($($arg:$arg_type),*)
        -> Box<dyn Fn($($larg_type),*)> {
            Box::new(move |$($larg),*| $body)
        }
    }};
}
