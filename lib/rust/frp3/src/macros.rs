//! This module defines common macros for FRP network definition.

/// # Overview
///
/// Utility for an easy definition of a new FRP network. In order to keep the network easy to debug
/// and reason about, each node constructor consumes a label. Providing labels manually is time
/// consuming and error prone. This utility infers the name from the assignment shape and provides
/// it automatically to the FRP node constructor.
///
///
/// # FRP Embedded Domain Specific Language (EDSL)
///
/// The macro exposes an EDSL for FRP network description. The syntax is very similar to standard
/// Rust syntax. There are a few new keywords - `def`, `trace`, and `eval` and a bunch of operators.
/// Every line which does not start with a keyword and does not use the new operators is interpreted
/// just as a regular Rust code. Moreover, there is a special flag `TRACE_ALL` which you can use as
/// the first text inside of macro, which will automatically enable each definition to be traced.
///
/// The following elements are available:
///
/// - Variable definition. ```text def src = source::<()>(); ``` Desugars to: ```text let src =
///   network.source::<()>("src")(); ```
///
///
/// - Value trace. ```text trace my_var; ``` Desugars to: ```text network.trace("my_var")(&my_var);
///   ```
///
/// - Eval. You should use it to indicate that the line is meant to be the end of the FRP network.
///   Currently, it is a simple sugar, but might become something more complex in FRP 3.0. ```text
///   eval node_selected ((id) model.select(id)); ``` Desugars to: ```text def _eval =
///   node_selected.map(f!((id) model.select(id))); ```
///
///
/// - Chained operations. ```text amount <- source.count().inc(); ``` Desugars to: ```text def
///   amount = source.count(); def amount = amount.inc(); ```
///
///
/// - Variable definition. ```text amount <- source.count(); ``` Desugars to: ```text def amount =
///   source.count(); ```
///
///
/// - Stream merge. ```text all_nodes <- any (selected_nodes,non_selected_nodes); ``` Desugars to:
///   ```text def all_nodes = any2 (&selected_nodes,&non_selected_nodes); ```
///
///
/// - Stream merge dropping input values. ```text all_nodes <- any_
///   (selected_nodes,non_selected_nodes); ``` Desugars to: ```text def all_nodes = any2_
///   (&selected_nodes,&non_selected_nodes); ```
///
///
/// - Stream dynamic merge. ```text each_node <+ some_nodes; ``` Desugars to: ```text
///   each_node.attach(&some_nodes); ```
///
///
/// - Stream iteration. ```text each_node <= all_nodes; ``` Desugars to: ```text def each_node =
///   all_nodes.iter(); ```
#[macro_export]
macro_rules! new_network {
    ([TRACE_ALL] $($ts:tt)*) => { $crate::_new_network! { TRACE    $($ts)* } };
    ([]          $($ts:tt)*) => { $crate::_new_network! { NO_TRACE $($ts)* } };
    (TRACE_ALL   $($ts:tt)*) => { $crate::_new_network! { TRACE    $($ts)* } };
    ($($ts:tt)*)             => { $crate::_new_network! { NO_TRACE $($ts)* } };
}

/// Just like `new_network` but for the dynamic FRP mode.
#[macro_export]
macro_rules! new_dynamic_network {
    ([TRACE_ALL] $($ts:tt)*) => { $crate::_new_dynamic_network! { TRACE    $($ts)* } };
    ([]          $($ts:tt)*) => { $crate::_new_dynamic_network! { NO_TRACE $($ts)* } };
    (TRACE_ALL   $($ts:tt)*) => { $crate::_new_dynamic_network! { TRACE    $($ts)* } };
    ($($ts:tt)*)             => { $crate::_new_dynamic_network! { NO_TRACE $($ts)* } };
}

/// Extends the provided network with new rules. See documentation of `new_network` to learn more.
#[macro_export]
macro_rules! extend {
    ([TRACE_ALL] $($ts:tt)*) => { $crate::_extend! { TRACE    $($ts)* } };
    ([]          $($ts:tt)*) => { $crate::_extend! { NO_TRACE $($ts)* } };
    (TRACE_ALL   $($ts:tt)*) => { $crate::_extend! { TRACE    $($ts)* } };
    ($($ts:tt)*)             => { $crate::_extend! { NO_TRACE $($ts)* } };
}

// ===================
// === Private API ===
// ===================

// === New ===

/// Internal helper for `new_network` macro.
#[macro_export]
macro_rules! _new_network {
    ($trace:ident $network:ident $($ts:tt)*) => {
        let $network = $crate::Network::new();
        $crate::_extend! { $trace $network $($ts)* }
    };
}

/// Creates a new `BridgeNetwork` for the provided networks.
#[macro_export]
macro_rules! new_bridge_network {
    ([$($($path:ident).*),*] $label:ident $($ts:tt)*) => {
        let _bridge_network_ = $crate::Network::new();
        $crate::extend! { _bridge_network_ $($ts)* }
        let _bridge_network_ = $crate::BridgeNetwork::from(_bridge_network_);
        $($($path).*.register_bridge_network(&_bridge_network_);)*
    };
}

// === Extend ===

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! _extend {
    ($trace:ident $network:ident $($ts:tt)*) => {
        $crate::divide_on_terminator! { [[$crate::extend_line] [$trace $network]] $($ts)* }
    };
}

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! extend_line {
    ($l:tt TRACE $network:ident def $name:ident $($toks:tt)*) => {
        $crate::extend_line2! { ($l) [] $network def $name $($toks)* }
        $crate::extend_line2! { ($l) [] $network trace $name }
    };
    ($l:tt TRACE $network:ident $name:ident <- $($toks:tt)*) => {
        $crate::extend_line2! { ($l) [] $network $name <- $($toks)* }
        $crate::extend_line2! { ($l) [] $network trace $name }
    };
    ($l:tt TRACE $network:ident $name:ident <= $($toks:tt)*) => {
        $crate::extend_line2! { ($l) [] $network $name <= $($toks)* }
        $crate::extend_line2! { ($l) [] $network trace $name }
    };
    ($l:tt TRACE $network:ident $name:ident <-_ $($toks:tt)*) => {
        $crate::extend_line2! { ($l) [] $network $name <-_ $($toks)* }
        $crate::extend_line2! { ($l) [] $network trace $name }
    };
    ($l:tt $trace:ident $($toks:tt)*) => {
        $crate::extend_line2! { ($l) [] $($toks)* }
    };
}

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! extend_line2 {
    (($l:tt) [$($lines:tt)*] $net:ident def $name:ident = $name2:ident) => { $($lines)* };
    (($l:tt) [$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? =                                                                       $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { ($l) [$($lines)* let $name $(:$ty)? = $net.span($l, stringify!($name)).$base$(::<$param>)?($($arg)*)                               ;] $net def $name = $name $($ts)* } };
    (($l:tt) [$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident                                                         . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { ($l) [$($lines)* let $name $(:$ty)? = $net.span($l, stringify!($name)).$base$(::<$param>)?($tgt1,$($arg)*)                         ;] $net def $name = $name $($ts)* } };
    (($l:tt) [$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident . $tgt2:ident                                           . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { ($l) [$($lines)* let $name $(:$ty)? = $net.span($l, stringify!($name)).$base$(::<$param>)?($tgt1.$tgt2,$($arg)*)                   ;] $net def $name = $name $($ts)* } };
    (($l:tt) [$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident . $tgt2:ident . $tgt3:ident                             . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { ($l) [$($lines)* let $name $(:$ty)? = $net.span($l, stringify!($name)).$base$(::<$param>)?($tgt1.$tgt2.$tgt3,$($arg)*)             ;] $net def $name = $name $($ts)* } };
    (($l:tt) [$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident . $tgt2:ident . $tgt3:ident . $tgt4:ident               . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { ($l) [$($lines)* let $name $(:$ty)? = $net.span($l, stringify!($name)).$base$(::<$param>)?($tgt1.$tgt2.$tgt3.$tgt4,$($arg)*)       ;] $net def $name = $name $($ts)* } };
    (($l:tt) [$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident . $tgt2:ident . $tgt3:ident . $tgt4:ident . $tgt5:ident . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { ($l) [$($lines)* let $name $(:$ty)? = $net.span($l, stringify!($name)).$base$(::<$param>)?($tgt1.$tgt2.$tgt3.$tgt4.$tgt5,$($arg)*) ;] $net def $name = $name $($ts)* } };

    (($l:tt) [] $net:ident $name:ident <- $($arg1:ident).+ || $($arg2:ident).+       ) => {$crate::extend_line2! { ($l) [] $net $name <- or($($arg1).+,$($arg2).+)               } };
    (($l:tt) [] $net:ident $name:ident <- $($arg1:ident).+ && $($arg2:ident).+       ) => {$crate::extend_line2! { ($l) [] $net $name <- and($($arg1).+,$($arg2).+)              } };
    (($l:tt) [] $net:ident $name:ident <- $($arg1:ident).+ ?? $($arg2:ident).+       ) => {$crate::extend_line2! { ($l) [] $net $name <- bool($($arg1).+,$($arg2).+)             } };

    (($l:tt) [] $net:ident $name:ident <- source ($val:expr)              $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net $name <- source_with($val)               $($ts)* } };
    (($l:tt) [] $net:ident $name:ident <- any (...)                       $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net $name <- any_mut()                       $($ts)* } };
    (($l:tt) [] $net:ident $name:ident <- any ( $($($arg:ident).+),+ )    $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net def $name = any(($($($arg).+,)+))        $($ts)* } };
    (($l:tt) [] $net:ident $name:ident <- any_ (...)                      $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net $name <- any_mut_()                      $($ts)* } };
    (($l:tt) [] $net:ident $name:ident <- any_ ( $($($arg:ident).+),+ )   $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net def $name = any_(($($($arg).+,)+))       $($ts)* } };

    (($l:tt) [] $net:ident $name:ident <- all (...)                       $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net $name <- all(())                       $($ts)* } };
    (($l:tt) [] $net:ident $name:ident <- all ( $($($arg:ident).+),+ )    $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net def $name = all(($($($arg).+,)+))        $($ts)* } };
    (($l:tt) [] $net:ident $name:ident <- all [...]                       $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net $name <- all_mut()                       $($ts)* } };
    (($l:tt) [] $net:ident $name:ident <- all [ $($($arg:ident).+),+ ]    $($ts:tt)* ) => {$crate::extend_line2! { ($l) [] $net def $name = all_vec(($($($arg).+,)+))    $($ts)* } };

    (($l:tt) [] $net:ident $name:ident <= $($toks:tt)*) => {$crate::extend_line2! { ($l) [] $net def $name = $($toks)* . iter()} };
    (($l:tt) [] $net:ident $name:ident <- $($toks:tt)*) => {$crate::extend_line2! { ($l) [] $net def $name = $($toks)* } };
    (($l:tt) [] $net:ident $($tgt:ident).+ <+_ $($toks:tt)*) => { $crate::extend_line2! { ($l) [] $net $($tgt).+ <+ $($toks)*.constant(()) }  };
    (($l:tt) [] $net:ident $($tgt:ident).+ <+ $($src:ident).+) => { $($tgt).+.attach_in_rt($net.rt(), &$($src).+); };
    (($l:tt) [] $net:ident $($tgt:ident).+ <+ $($toks:tt)*) => {
        $crate::extend_line2! { ($l) [] $net __unnamed__ <- $($toks)* }
        $($tgt).+.attach(&__unnamed__);
    };

    (($l:tt) [] $net:ident eval  $($tgt:ident).+ ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { ($l) [] $net def _eval = $($tgt).+ .eval(f! ($($args)*)) $($ts)* } };
    (($l:tt) [] $net:ident eval_ $($tgt:ident).+ ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { ($l) [] $net def _eval = $($tgt).+ .eval(f_!($($args)*)) $($ts)* } };

    (($l:tt) [] $net:ident trace $($ts:tt)*) => { $net.span($l, stringify!($($ts)*)).trace(&$($ts)*); };
    (($l:tt) [] $net:ident $($ts:tt)*) => { $($ts)*; }
}

// === Utils ===

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
///     let a = a.clone();
///     let b = b.clone();
///     move |c| { a + b + c }
/// }
/// ```
#[macro_export]
macro_rules! f {
    ([$($name:ident),*] ($($args:tt)*) $($expr:tt)*) => {
        {
            $(let $name = $name.clone();)*
            move |$($args)*| { $($expr)* }
        }
    };

    ([$($name:ident),*] $($expr:tt)*) => {
        {
            $(let $name = $name.clone();)*
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

pub use frp_macros::divide_on_terminator;
