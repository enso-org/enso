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
/// - Variable definition. ```compile_fail def src = source::<()>(); ``` Desugars to:
///   ```compile_fail let src = network.source::<()>("src")(); ```
///
///
/// - Value trace. ```compile_fail trace my_var; ``` Desugars to: ```compile_fail
///   network.trace("my_var")(&my_var); ```
///
/// - Eval. You should use it to indicate that the line is meant to be the end of the FRP network.
///   Currently, it is a simple sugar, but might become something more complex in FRP 3.0.
///   ```compile_fail eval node_selected ((id) model.select(id)); ``` Desugars to: ```compile_fail
///   def _eval = node_selected.map(f!((id) model.select(id))); ```
///
///
/// - Chained operations. ```compile_fail amount <- source.count().inc(); ``` Desugars to:
///   ```compile_fail def amount = source.count(); def amount = amount.inc(); ```
///
///
/// - Variable definition. ```compile_fail amount <- source.count(); ``` Desugars to:
///   ```compile_fail def amount = source.count(); ```
///
///
/// - Stream merge. ```compile_fail all_nodes <- any (selected_nodes,non_selected_nodes); ```
///   Desugars to: ```compile_fail def all_nodes = any2 (&selected_nodes,&non_selected_nodes); ```
///
///
/// - Stream merge dropping input values. ```compile_fail all_nodes <- any_
///   (selected_nodes,non_selected_nodes); ``` Desugars to: ```compile_fail def all_nodes = any2_
///   (&selected_nodes,&non_selected_nodes); ```
///
///
/// - Stream dynamic merge. ```compile_fail each_node <+ some_nodes; ``` Desugars to:
///   ```compile_fail each_node.attach(&some_nodes); ```
///
///
/// - Stream iteration. ```compile_fail each_node <= all_nodes; ``` Desugars to: ```compile_fail def
///   each_node = all_nodes.iter(); ```
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
        let $network = $crate::Network::new(stringify!($network));
        $crate::_extend! { $trace $network $($ts)* }
    };
}

/// Internal helper for `new_dynamic_network` macro.
#[macro_export]
macro_rules! _new_dynamic_network {
    ($trace:ident $($ts:tt)*) => {
        let __dynamic__ = $crate::DynamicNetwork::new();
        $crate::_extend! { $trace __dynamic__ $($ts)* }
    };
}

/// Creates a new `BridgeNetwork` for the provided networks.
#[macro_export]
macro_rules! new_bridge_network {
    ([$($($path:ident).*),*] $label:ident $($ts:tt)*) => {
        let _birdge_network_ = $crate::Network::new(stringify!($label));
        $crate::extend! { _birdge_network_ $($ts)* }
        let _birdge_network_ = $crate::BridgeNetwork::from(_birdge_network_);
        $($($path).*.register_bridge_network(&_birdge_network_);)*
    };
}


// === Extend ===

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! _extend {
    ($trace:ident $network:ident $($ts:tt)*) => {
        $crate::divide_on_terminator! { [[$crate::extend_lines] [$trace $network]] $($ts)* }
    };
}

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! extend_lines {
    ([$trace:ident $network:ident] [ $([$($line:tt)*])* ]) => {$(
        $crate::extend_line1! { $trace $network $($line)* }
    )*}
}

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! extend_line1 {
    (TRACE $network:ident def $name:ident $($toks:tt)*) => {
        $crate::extend_line2! { [] $network def $name $($toks)* }
        $crate::extend_line2! { [] $network trace $name }
    };
    (TRACE $network:ident $name:ident <- $($toks:tt)*) => {
        $crate::extend_line2! { [] $network $name <- $($toks)* }
        $crate::extend_line2! { [] $network trace $name }
    };
    (TRACE $network:ident $name:ident <= $($toks:tt)*) => {
        $crate::extend_line2! { [] $network $name <= $($toks)* }
        $crate::extend_line2! { [] $network trace $name }
    };
    (TRACE $network:ident $name:ident <-_ $($toks:tt)*) => {
        $crate::extend_line2! { [] $network $name <-_ $($toks)* }
        $crate::extend_line2! { [] $network trace $name }
    };
    ($trace:ident $($toks:tt)*) => {
        $crate::extend_line2! { [] $($toks)* }
    };
}

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! extend_line2 {
    ([$($lines:tt)*] $net:ident def $name:ident = $name2:ident) => { $($lines)* };
    ([$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? =                                                                       $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [$($lines)* let $name $(:$ty)? = $net.$base$(::<$param>)?(concat!(module_path!(),"::",stringify!($name),":",line!()),$($arg)*)                                ;] $net def $name = $name $($ts)* } };
    ([$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident                                                         . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [$($lines)* let $name $(:$ty)? = $net.$base$(::<$param>)?(concat!(module_path!(),"::",stringify!($name),":",line!()),&$tgt1,$($arg)*)                         ;] $net def $name = $name $($ts)* } };
    ([$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident . $tgt2:ident                                           . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [$($lines)* let $name $(:$ty)? = $net.$base$(::<$param>)?(concat!(module_path!(),"::",stringify!($name),":",line!()),&$tgt1.$tgt2,$($arg)*)                   ;] $net def $name = $name $($ts)* } };
    ([$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident . $tgt2:ident . $tgt3:ident                             . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [$($lines)* let $name $(:$ty)? = $net.$base$(::<$param>)?(concat!(module_path!(),"::",stringify!($name),":",line!()),&$tgt1.$tgt2.$tgt3,$($arg)*)             ;] $net def $name = $name $($ts)* } };
    ([$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident . $tgt2:ident . $tgt3:ident . $tgt4:ident               . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [$($lines)* let $name $(:$ty)? = $net.$base$(::<$param>)?(concat!(module_path!(),"::",stringify!($name),":",line!()),&$tgt1.$tgt2.$tgt3.$tgt4,$($arg)*)       ;] $net def $name = $name $($ts)* } };
    ([$($lines:tt)*] $net:ident def $name:ident $(:$ty:ty)? = $tgt1:ident . $tgt2:ident . $tgt3:ident . $tgt4:ident . $tgt5:ident . $base:ident$(::<$param:ty>)?($($arg:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [$($lines)* let $name $(:$ty)? = $net.$base$(::<$param>)?(concat!(module_path!(),"::",stringify!($name),":",line!()),&$tgt1.$tgt2.$tgt3.$tgt4.$tgt5,$($arg)*) ;] $net def $name = $name $($ts)* } };

    ([] $net:ident $name:ident <- $($arg1:ident).+ || $($arg2:ident).+                                                         ) => {$crate::extend_line2! { [] $net $name <- or(&$($arg1).+,&$($arg2).+)                                        } };
    ([] $net:ident $name:ident <- $($arg1:ident).+ && $($arg2:ident).+                                                         ) => {$crate::extend_line2! { [] $net $name <- and(&$($arg1).+,&$($arg2).+)                                       } };
    ([] $net:ident $name:ident <- $($arg1:ident).+ ?? $($arg2:ident).+                                                         ) => {$crate::extend_line2! { [] $net $name <- bool(&$($arg1).+,&$($arg2).+)                                      } };

    ([] $net:ident $name:ident <- any (...)                                                                         $($ts:tt)* ) => {$crate::extend_line2! { [] $net $name <- any_mut()                                                  $($ts)* } };
    ([] $net:ident $name:ident <- any ( $($arg1:ident).+ )                                                                     ) => { let $name = $($arg1).+.clone_ref(); };
    ([] $net:ident $name:ident <- any ( $($arg1:ident).+ , $($arg2:ident).+ )                                       $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = any2(&$($arg1).+,&$($arg2).+)                           $($ts)* } };
    ([] $net:ident $name:ident <- any ( $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ )                    $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = any3(&$($arg1).+,&$($arg2).+,&$($arg3).+)               $($ts)* } };
    ([] $net:ident $name:ident <- any ( $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ ) $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = any4(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+)   $($ts)* } };

    ([] $net:ident $name:ident <- any_ (...)                                                                         $($ts:tt)* ) => {$crate::extend_line2! { [] $net $name <- any_mut_()                                                $($ts)* } };
    ([] $net:ident $name:ident <- any_ ( $($arg1:ident).+ )                                                                     ) => { let $name = $($arg1).+.constant(()); };
    ([] $net:ident $name:ident <- any_ ( $($arg1:ident).+ , $($arg2:ident).+ )                                       $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = any2_(&$($arg1).+,&$($arg2).+)                         $($ts)* } };
    ([] $net:ident $name:ident <- any_ ( $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ )                    $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = any3_(&$($arg1).+,&$($arg2).+,&$($arg3).+)             $($ts)* } };
    ([] $net:ident $name:ident <- any_ ( $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ ) $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = any4_(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+) $($ts)* } };

    ([] $net:ident $name:ident <- all (...)                                                                         $($ts:tt)* ) => {$crate::extend_line2! { [] $net $name <- all_mut()                                                  $($ts)* } };
    ([] $net:ident $name:ident <- all ( $($arg1:ident).+ )                                                                     ) => { let $name = $($arg1).+.clone_ref(); };
    ([] $net:ident $name:ident <- all ( $($arg1:ident).+ , $($arg2:ident).+ )                                       $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all2(&$($arg1).+,&$($arg2).+)                           $($ts)* } };
    ([] $net:ident $name:ident <- all ( $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ )                    $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all3(&$($arg1).+,&$($arg2).+,&$($arg3).+)               $($ts)* } };
    ([] $net:ident $name:ident <- all ( $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ ) $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all4(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+)   $($ts)* } };

    ([] $net:ident $name:ident <- all [...]                                                                         $($ts:tt)* ) => {$crate::extend_line2! { [] $net $name <- all_mut()                                                  $($ts)* } };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ ]                                                                     ) => { let $name = $($arg1).+.clone_ref(); };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ , $($arg2:ident).+ ]                                       $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all_vec2(&$($arg1).+,&$($arg2).+)                         $($ts)* } };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ ]                    $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all_vec3(&$($arg1).+,&$($arg2).+,&$($arg3).+)             $($ts)* } };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ ] $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all_vec4(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+) $($ts)* } };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ , $($arg5:ident).+ ] $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all_vec5(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+,&$($arg5).+) $($ts)* } };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ , $($arg5:ident).+ , $($arg6:ident).+ ] $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all_vec6(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+,&$($arg5).+,&$($arg6).+) $($ts)* } };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ , $($arg5:ident).+ , $($arg6:ident).+ , $($arg7:ident).+ ] $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all_vec7(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+,&$($arg5).+,&$($arg6).+,&$($arg7).+) $($ts)* } };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ , $($arg5:ident).+ , $($arg6:ident).+ , $($arg7:ident).+, $($arg8:ident).+ ] $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all_vec8(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+,&$($arg5).+,&$($arg6).+,&$($arg7).+,&$($arg8).+) $($ts)* } };
    ([] $net:ident $name:ident <- all [ $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ , $($arg5:ident).+ , $($arg6:ident).+ , $($arg7:ident).+, $($arg8:ident).+, ($arg9:ident).+ ] $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all_vec9(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+,&$($arg5).+,&$($arg6).+,&$($arg7).+,&$($arg8).+,&$($arg9).+) $($ts)* } };

    ([] $net:ident $name:ident <- all_ (...)                                                                         $($ts:tt)* ) => {$crate::extend_line2! { [] $net $name <- all_mut_()                                                $($ts)* } };
    ([] $net:ident $name:ident <- all_ ( $($arg1:ident).+ )                                                                     ) => { let $name = $($arg1).+.constant(()); };
    ([] $net:ident $name:ident <- all_ ( $($arg1:ident).+ , $($arg2:ident).+ )                                       $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all2_(&$($arg1).+,&$($arg2).+)                         $($ts)* } };
    ([] $net:ident $name:ident <- all_ ( $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ )                    $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all3_(&$($arg1).+,&$($arg2).+,&$($arg3).+)             $($ts)* } };
    ([] $net:ident $name:ident <- all_ ( $($arg1:ident).+ , $($arg2:ident).+ , $($arg3:ident).+ , $($arg4:ident).+ ) $($ts:tt)* ) => {$crate::extend_line2! { [] $net def $name = all4_(&$($arg1).+,&$($arg2).+,&$($arg3).+,&$($arg4).+) $($ts)* } };


    ([] $net:ident $name:ident <= $($toks:tt)*) => {$crate::extend_line2! { [] $net def $name = $($toks)* . iter()} };
    ([] $net:ident $name:ident <- $($toks:tt)*) => {$crate::extend_line2! { [] $net def $name = $($toks)* } };
    ([] $net:ident $($tgt:ident).+ <+ $($src:ident).+) => { $($tgt).+.attach(&$($src).+); };
    ([] $net:ident $($tgt:ident).+ <+ $($toks:tt)*) => {
        $crate::extend_line2! { [] $net __unnamed__ <- $($toks)* }
        $($tgt).+.attach(&__unnamed__);
    };

    ([] $net:ident eval $tgt1:ident                                                         ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1                                 . map (f!($($args)*)) $($ts)* } };
    ([] $net:ident eval $tgt1:ident . $tgt2:ident                                           ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1 . $tgt2                         . map (f!($($args)*)) $($ts)* } };
    ([] $net:ident eval $tgt1:ident . $tgt2:ident . $tgt3:ident                             ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1 . $tgt2 . $tgt3                 . map (f!($($args)*)) $($ts)* } };
    ([] $net:ident eval $tgt1:ident . $tgt2:ident . $tgt3:ident . $tgt4:ident               ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1 . $tgt2 . $tgt3 . $tgt4         . map (f!($($args)*)) $($ts)* } };
    ([] $net:ident eval $tgt1:ident . $tgt2:ident . $tgt3:ident . $tgt4:ident . $tgt5:ident ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1 . $tgt2 . $tgt3 . $tgt4 . $tgt5 . map (f!($($args)*)) $($ts)* } };

    ([] $net:ident eval_ $tgt1:ident                                                         ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1                                 . map (f_!($($args)*)) $($ts)* } };
    ([] $net:ident eval_ $tgt1:ident . $tgt2:ident                                           ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1 . $tgt2                         . map (f_!($($args)*)) $($ts)* } };
    ([] $net:ident eval_ $tgt1:ident . $tgt2:ident . $tgt3:ident                             ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1 . $tgt2 . $tgt3                 . map (f_!($($args)*)) $($ts)* } };
    ([] $net:ident eval_ $tgt1:ident . $tgt2:ident . $tgt3:ident . $tgt4:ident               ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1 . $tgt2 . $tgt3 . $tgt4         . map (f_!($($args)*)) $($ts)* } };
    ([] $net:ident eval_ $tgt1:ident . $tgt2:ident . $tgt3:ident . $tgt4:ident . $tgt5:ident ($($args:tt)*) $($ts:tt)*) => { $crate::extend_line2! { [] $net def _eval = $tgt1 . $tgt2 . $tgt3 . $tgt4 . $tgt5 . map (f_!($($args)*)) $($ts)* } };

    ([] $net:ident trace $($path:ident).*) => { $net.trace(stringify!($($path).*),&$($path).*); };
    ([] $net:ident $($ts:tt)*) => { $($ts)*; }
}


// === Utils ===

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! divide_on_terminator {
    ($f:tt $($ts:tt)*) => { $crate::_divide_on_terminator! { $f [] [] $($ts)* } };
}

/// Internal helper for `extend` macro.
#[macro_export]
macro_rules! _divide_on_terminator {
    ([[$($f:tt)*] $args:tt] $lines:tt       [])                              => { $($f)*! {$args $lines} };
    ([[$($f:tt)*] $args:tt] [$($lines:tt)*] $line:tt)                        => { MISSING_SEMICOLON };
    ($f:tt                  [$($lines:tt)*] [$($line:tt)*] ;     $($ts:tt)*) => { $crate::_divide_on_terminator! {$f               [$($lines)* [$($line)*]] []             $($ts)*} };
    ($f:tt                  $lines:tt       [$($line:tt)*] $t:tt $($ts:tt)*) => { $crate::_divide_on_terminator! {$f               $lines                   [$($line)* $t] $($ts)*} };
}
