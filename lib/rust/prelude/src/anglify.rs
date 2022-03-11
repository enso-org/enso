/// This module contains a utility macro that transforms tokens of form `foo [<[bar]>] baz` to
/// `foo <bar> baz`. There are two versions of the macro: `anglify_shallow`, which only does a
/// single pass over the input and `anglify`, which recursively transforms tokens.

/// `anglify` transforms tokens of form `foo [<[bar]>] baz` to `foo <bar> baz`. Works recursively,
/// so `[<[foo [<[bar]>]]>] baz` will produce `<foo <bar>> baz`.
///
/// The macro works by traversing the input based on `tt` tokens, which means it consumes
/// either single token or a token enclosed in matching `[]`/`()` (see
/// https://doc.rust-lang.org/reference/macros-by-example.html#metavariables).
///
/// The syntax was chosen because it shows the intention and it is very unusual that normal Rust
/// code would use it anywhere.
#[macro_export]
macro_rules! anglify {
    ($($ts:tt)*) => {
        $crate::anglify_internal!{[] $($ts)*}
    };
}

#[macro_export(local_inner_macros)]
macro_rules! anglify_internal {
    ([$($out:tt)*])                             => { $($out)* };
    ([$($out:tt)*] [<[$($xs:tt)*]>] $($ts:tt)*) => { anglify_internal!{ [$($out)* <] $($xs)* > $($ts)*} };
    ([$($out:tt)*] $t:tt            $($ts:tt)*) => { anglify_internal!{ [$($out)* $t]          $($ts)*} };
}

/// Shallow version of the `anglify` macro that does not recursively expand `[<] [>]`. For example,
/// `[<[foo [<[bar]>]]>] baz` will be translated to `<foo [<[bar]>]> baz`, while `anglify` would
/// produce `<foo <bar>> baz`.
#[macro_export]
macro_rules! anglify_shallow {
    ($($ts:tt)*) => {
        $crate::anglify_shallow_internal!{[] $($ts)*}
    };
}

#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! anglify_shallow_internal {
    ([$($out:tt)*])                             => { $($out)* };
    ([$($out:tt)*] [<[$($xs:tt)*]>] $($ts:tt)*) => { anglify_shallow_internal!{ [$($out)* <$($xs)*>] $($ts)*} };
    ([$($out:tt)*] $t:tt            $($ts:tt)*) => { anglify_shallow_internal!{ [$($out)* $t]        $($ts)*} };
}
