//! # Introduction
//!
//! This library defines `eval` macro and several helpers. The macro allows
//! writing macros like they were macro-level functions. Example is always worth
//! more than 1000 words, so let's consider the following code:
//!
//! ```compile_fail
//! eval!{ drop(1,split_comma([a,b,c]),3) }
//! ```
//!
//! It will first evaluate macro `split_comma!{ [a,b,c] }` to `[[a][b][c]]`,
//! and then evaluate `drop` with the resukts as `drop!{ [1] [[a][b][c]] [3] }`.
//!
//! Because this library uses tt-munchers, each argument to a macro is provided
//! inside braces, like in the example above.
//!
//! # Debugging
//! The best way to debug a macro is to enable `#![feature(trace_macros)]` and
//! use the `drop` macro to discard the results, as shown above.
//!
//! # Limitations
//!
//! This library is in a very early shape, so changes may apply soon. There is
//! also a limitation right now. Functions nested deeper than in the example
//! above are not evaluated, so if you would use
//! `eval!{ drop(1,split_comma(foo(5)),3) }`, the `foo` function would not be
//! evaluated. This is just because the implementation is not finished and
//! there are comments in code where it should be added.

#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

// ============
// === Eval ===
// ============

#[macro_export]
macro_rules! eval_step {
    ( $name:ident
      $evaled:tt
      [ [$f:ident($($f_args:tt)*)] $($not_evaled_args:tt)* ]) =>
    {
        // TODO Here we just call the argument function. Instead we should
        // TODO eval it and all its arguments first.
        $f!{[ $name $evaled [$($not_evaled_args)*] ] $($f_args)*}
    };
    ($name:ident [$($evaled:tt)*] [$arg:tt $($remaining_args:tt)*]) => {
        $crate::eval_step!{$name [$($evaled)* $arg] [$($remaining_args)*]}
    };
    ($name:ident [$($evaled:tt)*] []) => {
        $name! {$($evaled)*}
    };
}

#[macro_export]
macro_rules! eval_tt {
    ($name:ident [$($args:tt)*]) => {
        $crate::eval_step!{$name [] [$($args)*]}
    };
}

#[macro_export]
macro_rules! eval {
    ($name:ident($($args:tt)*)) => {
        $crate::eval_tt! { eval_tt [$name [split_comma([$($args)*])]] }
    };
}

#[macro_export]
macro_rules! apply_result {
    ([$name:ident [$($evaled:tt)*] $not_evaled_args:tt], $result:tt) => {
        $crate::eval_step!{$name [$($evaled)* $result] $not_evaled_args}
    };
}

// ============
// === Drop ===
// ============

#[macro_export]
macro_rules! drop {
    ($($toks:tt)*) => {};
}

// ==================
// === SplitComma ===
// ==================

#[macro_export]
macro_rules! split_comma {
    ($f:tt [$($rest:tt)*]) => {
        $crate::split_comma_helper!{$f [] [] [] $($rest)*}
    };
}

#[macro_export]
macro_rules! split_comma_helper {
    ($f:tt [] [$($items:tt)*] $this:tt , $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [] [$($items)* $this] [] $($rest)*}
    };
    ($f:tt [$($depth:tt)*] $items:tt [$($this:tt)*] < $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [. $($depth)*] $items [$($this)* <] $($rest)*}
    };
    ($f:tt [$($depth:tt)*] $items:tt [$($this:tt)*] << $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [. . $($depth)*] $items [$($this)* <<] $($rest)*}
    };
    ($f:tt [$($depth:tt)*] $items:tt [$($this:tt)*] <<< $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [. . . $($depth)*] $items [$($this)* <<<] $($rest)*}
    };
    ($f:tt [$($depth:tt)*] $items:tt [$($this:tt)*] <<<< $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [. . . . $($depth)*] $items [$($this)* <<<<] $($rest)*}
    };
    ($f:tt [$($depth:tt)*] $items:tt [$($this:tt)*] <<<<< $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [. . . . . $($depth)*] $items [$($this)* <<<<<] $($rest)*}
    };
    ($f:tt [. $($depth:tt)*] $items:tt [$($this:tt)*] > $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [$($depth)*] $items [$($this)* >] $($rest)*}
    };
    ($f:tt [. . $($depth:tt)*] $items:tt [$($this:tt)*] >> $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [$($depth)*] $items [$($this)* >>] $($rest)*}
    };
    ($f:tt [. . . $($depth:tt)*] $items:tt [$($this:tt)*] >>> $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [$($depth)*] $items [$($this)* >>>] $($rest)*}
    };
    ($f:tt [. . . . $($depth:tt)*] $items:tt [$($this:tt)*] >>>> $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [$($depth)*] $items [$($this)* >>>>] $($rest)*}
    };
    ($f:tt [. . . . . $($depth:tt)*] $items:tt [$($this:tt)*] >>>>> $($rest:tt)*) => {
        $crate::split_comma_helper!{$f [$($depth)*] $items [$($this)* >>>>>] $($rest)*}
    };
    ($f:tt $depth:tt $items:tt [$($this:tt)*] , $($rest:tt)*) => {
        $crate::split_comma_helper!{$f $depth $items [$($this)* ,] $($rest)*}
    };
    ($f:tt $depth:tt $items:tt [$($this:tt)*] $new:tt $($rest:tt)*) => {
        $crate::split_comma_helper!{$f $depth $items [$($this)* $new] $($rest)*}
    };
    ($f:tt [] $result:tt []) => {
        $crate::apply_result!{$f,$result}
    };
    ($f:tt $depth:tt $items:tt $this:tt) => {
        $crate::split_comma_helper!{$f $depth $items $this,}
    };
}

// ================
// === Examples ===
// ================

eval! { drop(1,split_comma([a,b,c]),3) }
