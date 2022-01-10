//! Utilities for easy profiling usage and definition.

/// Check whether the given tokens end with a semicolon. If so, call the first argument with
/// true/false and the second argument.
#[doc(hidden)]
#[macro_export]
macro_rules! ends_with_colon {
    ($f:ident $args:tt [;]) => {
        $crate::$f! {true, $args}
    };
    ($f:ident $args:tt []) => {
        $crate::$f! {false, $args}
    };
    ($f:ident $args:tt [$t:tt $($ts:tt)*]) => {
        $crate::ends_with_colon! {$f $args [$($ts)*]}
    };
}

/// Turn the given identifier in a function call in this crate namespace.
#[doc(hidden)]
#[macro_export]
macro_rules! make_call {
    ($name:ident) => {
        $name();
    };
}

/// Put the given code block between the two passed functions. The result will either be of the
/// form
/// ```no_compile
/// f1();
/// foo;
/// bar;
/// f2();
/// ```
/// or
/// ```no_compile
/// f1();
/// foo;
/// out = bar;
/// f2()
/// out
/// ```
/// to allow for return values.
#[doc(hidden)]
#[macro_export]
macro_rules! wrap_block {
    ($fn_start:ident $fn_end:ident $($t:tt)*) => {
        $crate::ends_with_colon!{apply_block_wrapper [$fn_start, $fn_end $($t)*] [$($t)*]}
    };
}

#[doc(hidden)]
#[allow(missing_docs)]
#[macro_export]
macro_rules! apply_block_wrapper {
    ($ending:tt, [$fn_start:ident, $fn_end:ident $($expr:stmt)*]) => {
        $crate::block_wrapper_start!{$ending, $fn_start,  $fn_end, [$($expr;)*]}
    };
}

#[doc(hidden)]
#[allow(missing_docs)]
#[macro_export]
macro_rules! block_wrapper_start {
    (false, $fn_start:ident, $fn_end:ident, [$($expr:stmt;)*]) => {
        $crate::make_call!{$fn_start}
        $crate::block_wrapper_continue!{$fn_end, $($expr;)*}
    };
    (true, $fn_start:ident, $fn_end:ident, [$($expr:stmt;)*]) => {
        $crate::make_call!{$fn_start}
        $($expr;)*
        $crate::make_call!{$fn_end}
    };
}
#[allow(missing_docs)]
#[macro_export]
macro_rules! block_wrapper_continue {
    ($fn_end:ident, $expr:stmt;) => {
        let out = { $expr };
        $crate::make_call!{$fn_end}
        out
    };
    ($fn_end:ident, $expr:stmt; $($exprs:stmt;)*) => {
        $expr;
        $crate::block_wrapper_continue!{$fn_end:ident, $($exprs;)*}
    };
}

/// Create a Metadata struct that has the source location prepopulated via the `file!` and
/// `line!` macros.
#[doc(hidden)]
#[macro_export]
macro_rules! make_metadata {
    ($profiling_level:expr, $interval_name:expr) => {
        $crate::Metadata {
            source:          $crate::SourceLocation { file: file!().to_string(), line: line!() },
            profiling_level: $profiling_level.to_string(),
            label:           $interval_name.to_string(),
        }
    };
}

/// Start measuring the interval of the given profiling level and name.
#[doc(hidden)]
#[macro_export]
macro_rules! start_interval {
    ($profiling_level:expr, $interval_name:expr) => {
        $crate::mark_start_interval($crate::make_metadata!($profiling_level, $interval_name))
    };
}

/// End measuring the interval of the given profiling level and name.
#[doc(hidden)]
#[macro_export]
macro_rules! end_interval {
    ($profiling_level:expr, $interval_name:expr) => {
        $crate::warn_on_error($crate::mark_end_interval($crate::make_metadata!(
            $profiling_level,
            $interval_name
        )))
    };
}

/// Measure an interval with the given profiling level and name that starts before the given
/// expressions and ends after the given expressions.
#[doc(hidden)]
#[macro_export]
macro_rules! measure_interval {
        ($d:tt, $profiling_level:expr, $interval_name:expr, $($body:tt)*) => {
             {
                 fn start() {
                     $crate::start_interval!($profiling_level,$interval_name).release();
                 }

                 fn end() {
                     $crate::end_interval!($profiling_level,$interval_name);
                 }

                 $crate::wrap_block! { start end $($body)* }
             }
        }
    }



/// Define a new boolean variable whose value is determined by a feature flag in the crate.
/// The name of the variable is `ENABLED` and it will be true if a feature flag
/// `enable-<profiling_level_name>-profiling` is set, otherwise it will be false.
#[doc(hidden)]
#[macro_export]
macro_rules! define_profiling_toggle {
    ($profiling_level_name:ident) => {
        paste::item! {
            #[doc = "Defines whether the log level `" $profiling_level_name "` should be used."]
            #[cfg(feature = "enable-" $profiling_level_name "-profiling")]
            pub const ENABLED: bool = true;
            #[doc = "Defines whether the log level `" $profiling_level_name "` should be used."]
            #[cfg(not(feature = "enable-" $profiling_level_name  "-profiling"))]
            pub const ENABLED: bool = false;
        }
    };
}


/// Define a new profiling module that exposes `start`, `end` and `measure` methods. The profiling
/// can be activated and de-activated via a crate feature flag named
/// `enable-<profiling_module_name>-profiling`, which will turn the profiling methods into no-ops.
#[doc(hidden)]
#[macro_export]
macro_rules! define_profiler {
    ($($d:tt, $profiling_level:expr, $profiling_level_name_upper:ident, $profiling_level_name:ident,
    $start:ident, $end:ident, $measure:ident ;)*) => {$(
        /// Profiler module that exposes methods to measure named intervals.
        pub mod $profiling_level_name {

            $crate::define_profiling_toggle!($profiling_level_name);

            /// Start measuring a named time interval. Return an `IntervalHandle` that can be used
            /// to end the profiling.
            #[macro_export]
            macro_rules! $start {
                ($interval_name:expr) => {
                    $crate::start_interval!($profiling_level, $interval_name)
                };
            }

            /// Manually end measuring a named time interval.
            #[macro_export]
            macro_rules! $end {
                ($interval_name:expr) => {
                    $crate::end_interval!($profiling_level, $interval_name)
                };
            }

            /// Profile the execution of the given closure.
            #[macro_export]
            macro_rules! $measure {
                ($interval_name:expr, $d($d body:tt)*) => {
                    $crate::measure_interval!(
                        A, $profiling_level, $interval_name, $d($d body)*)
                };
            }
        })*
    }
}
