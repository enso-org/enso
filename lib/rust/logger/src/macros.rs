//! Utilities for easy logger usage. Defines such macros as `debug!` or `warning!`.



// ==============
// === Macros ===
// ==============

/// Internal utility for logging macros.
#[macro_export]
macro_rules! log_template {
    ($expand:ident, $level:path, $logger:expr, $msg:ident) => {
        $crate::LoggerOps::<$level>::log(&$logger,$level,$msg)
    };

    ($expand:ident, $level:path, $logger:expr, $msg:tt) => {
        $crate::LoggerOps::<$level>::log(&$logger,$level,||iformat!($msg))
    };

    ($expand:ident, $level:path, $logger:expr, || $msg:expr) => {
        $crate::LoggerOps::<$level>::log(&$logger,$level,|| $msg)
    };

    ($expand:ident, $level:path, $logger:expr, $msg:tt, $new_expand:ident || $($body:tt)*) => {
        $crate::log_template!{$new_expand,$level,$logger,$msg,||$($body)*}
    };

    ($expand:ident, $level:path, $logger:expr, || $msg:expr, $new_expand:ident || $($body:tt)*) => {
        $crate::log_template!{$new_expand,$level,$logger,|| $msg,||$($body)*}
    };

    ($expand:ident, $level:path, $logger:expr, $msg:ident, || $($body:tt)*) => {
        $crate::log_template_group!($expand,$level,$logger,[$msg],||$($body)*)
    };

    ($expand:ident, $level:path, $logger:expr, $msg:tt, || $($body:tt)*) => {
        $crate::log_template_group!($expand,$level,$logger,[||iformat!($msg)],||$($body)*)
    };

    ($expand:ident, $level:path, $logger:expr, || $msg:expr, || $($body:tt)*) => {
        $crate::log_template_group!($expand,$level,$logger,[||$msg],||$($body)*)
    };
}

/// Internal utility for logging macros.
#[macro_export]
macro_rules! log_template_group {
    ($expand:ident, $level:path, $logger:expr, [$($msg:tt)*], || $($body:tt)*) => {
        {
            $crate::LoggerOps::<$level>::group_begin
                (&$logger,$level,$crate::collapsed_to_bool!($expand),$($msg)*);
            let out = $($body)*;
            $crate::LoggerOps::<$level>::group_end(&$logger,$level);
            out
        }
    };
}

/// Internal utility for logging macros.
#[macro_export]
macro_rules! collapsed_to_bool {
    (collapsed) => {
        true
    };
    (expanded) => {
        false
    };
}


// ========================
// === Macro Generation ===
// ========================

/// Internal utility for logging macros. Generates all the logging macros.
///
/// The generated macros can be used in a variety of forms:
/// - `warning!(logger,"literal")`, where literal will be formatted with `iformat`.
/// - `warning!(logger,identifier)`, where identifier is a string-like variable.
/// - `warning!(logger,|| expr)`, where expr returns a string-like variable.
///
/// Moreover, for each form, you can pass a third parameter. If passed, the macro will become a
/// group, like `warning!(logger,"test",|| { ... }`. You can also use macro-keywords `collapsed`
/// or `expanded` just before `||` to print the group collapsed or expanded by default,
/// respectively. If not provided, the warning and error group macros are collapsed by default.
///
/// # Implementation Details
/// Please note that the special pattern `$d` expands to just `$` in the generated macro from this
/// macro.
macro_rules! define_log_macros {
    ($($d:tt $name:ident $tp_name:ident $expand:ident;)*) => {$(
        /// $tp_name logging macro.
        #[macro_export]
        macro_rules! $name {
            ($d($d ts:tt)*) => {
                $crate::log_template!{$expand,$crate::entry::level::$tp_name,$d($d ts)*}
            };
        }
    )*};
}

define_log_macros! {
    $ trace   Trace   expanded;
    $ debug   Debug   expanded;
    $ info    Info    expanded;
    $ warning Warning collapsed;
    $ error   Error   collapsed;
}
