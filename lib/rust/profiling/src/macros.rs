//! Utilities for easy profiling usage and definition.

/// Create a Metadata struct that has the source location prepopulated via the `file!` and
/// `line!` macros.
#[macro_export]
macro_rules! make_metadata {
    ($profiling_level:expr, $interval_name:expr) => {
        $crate::Metadata {
            source:          $crate::SourceLocation { file: file!().to_string(), line: line!() },
            profiling_level: $profiling_level.to_string(),
            label:           $interval_name.to_string(),

            rendering: None,
        }
    };
}

/// Start measuring the interval of the given profiling level and name.
#[macro_export]
macro_rules! start_interval {
    ($profiling_level:expr, $interval_name:expr) => {
        $crate::mark_start_interval($crate::make_metadata!($profiling_level, $interval_name))
    };
}

/// End measuring the interval of the given profiling level and name.
#[macro_export]
macro_rules! end_interval {
    ($profiling_level:expr, $interval_name:expr, $stat_index:expr) => {
        let _ = $crate::mark_end_interval($crate::make_metadata!($profiling_level, $interval_name), $stat_index);
    };
}

/// Measure an interval with the given profiling level and name that starts before the given
/// expressions and ends after the given expressions.
#[macro_export]
macro_rules! measure_interval {
        ($profiling_level:expr, $interval_name:expr, $($body:tt)*) => {
             {
                 let stat_index = $crate::start_interval!($profiling_level, $interval_name).release();
                 let out = { $($body)* };
                 $crate::end_interval!($profiling_level, $interval_name, stat_index);
                out
             }
        };
    }



/// Define a new boolean variable whose value is determined by a feature flag in the crate.
/// The name of the variable is `ENABLED` and it will be true if a feature flag
/// `enable-<profiling_level_name>-profiling` is set, otherwise it will be false.
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
#[macro_export]
macro_rules! define_profiler {
    ($($d:tt, $profiling_level:expr, $profiling_level_name_upper:ident, $profiling_level_name:ident, $start:ident, $end:ident, $measure:ident ;)*) => {$(
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
                        ($interval_name:expr, || $d($d body:tt)*) => {
                            $crate::measure_interval!($profiling_level, $interval_name, $d($d body)*)
                        };
                    }
        })*
    }
}
