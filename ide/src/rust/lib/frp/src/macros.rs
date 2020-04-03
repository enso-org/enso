//! This module defines common macros for FRP netwrok definition.



/// Utility for easy definition of the FRP network. In order to keep the network easy to debug and
/// reason about, each node constructor consumes a label. Providing labels manually is time
/// consuming and error prone. This utility infers the name from the assignment shape and provides
/// it automatically to the FRP node constructor.
#[macro_export]
macro_rules! frp {
    ($($ts:tt)*) => { $crate::split_on_terminator! { [[$crate::frp_lines]] [] [] $($ts)* } };
}

/// Utility for easy definition of the FRP network definition. Read docs of `frp` macro to learn
/// more.
#[macro_export]
macro_rules! frp_def {
    ($var:ident = $fn:ident $(::<$ty:ty>)? ($($args:tt)*)) => {
        let $var = $crate::Dynamic $(::<$ty>)? :: $fn
        ( stringify!{$var}, $($args)* );
    };

    ($scope:ident . $var:ident = $fn:ident $(::<$ty:ty>)? ($($args:tt)*)) => {
        let $var = $crate::Dynamic $(::<$ty>)? :: $fn
        ( concat! {stringify!{$scope},".",stringify!{$var}}, $($args)* );
    };

    ($var:ident = $fn:ident $(.$fn2:ident)* $(::<$ty:ty>)? ($($args:tt)*)) => {
        let $var = $fn $(.$fn2)* $(::<$ty>)?
        ( concat! {stringify!{$var}}, $($args)* );
    };

    ($scope:ident . $var:ident = $fn1:ident . $fn2:ident $(.$fn3:ident)* $(::<$ty:ty>)? ($($args:tt)*)) => {
        let $var = $fn1 . $fn2 $(.$fn3)* $(::<$ty>)?
        ( concat! {stringify!{$scope},".",stringify!{$var}}, $($args)* );
    };
}

/// Internal helper for the `frp` macro.
#[macro_export]
macro_rules! frp_lines {
    ([ $([$($line:tt)*])* ]) => {
        $( $crate::frp_def! { $($line)* } )*
    };
}

/// Splits the token stream on terminators.
#[macro_export]
macro_rules! split_on_terminator {
    ([[$($f:tt)*] $args:tt] $out:tt []) => {
        $($f)*! { $args $out }
    };

    ([[$($f:tt)*]] $out:tt []) => {
        $($f)*! { $out }
    };

    ($f:tt [$($out:tt)*] $current:tt ; $($ts:tt)*) => {
        $crate::split_on_terminator! { $f [$($out)* $current] [] $($ts)* }
    };

    ($f:tt $out:tt [$($current:tt)*] $t:tt $($ts:tt)*) => {
        $crate::split_on_terminator! { $f $out [$($current)* $t] $($ts)* }
    };

    ([[$($f:tt)*] $($args:tt)?] [$($out:tt)*] $current:tt) => {
        $crate::split_on_terminator! { [[$($f)*] $($args)?] [$($out)* $current] [] }
    };
}
