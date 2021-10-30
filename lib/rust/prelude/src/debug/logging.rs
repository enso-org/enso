//! This module provides several logging macros.
//!
//! Because in recent Rust toolchain versions it is impossible to redirect standard ouput stream to
//! Web Console. This made impossible to quickly add ad-hoc logs with `println!` during development.
//! Macros provided by this module aim to fill this gap.
//!
//! Macros intentionally defy our naming rules by being in UPPERCASE. They are not to be used in
//! production-ready code, so they should be easy to visually catch during code reviews.
//! Also, this gets us good names that otherwise would be already used.

/// Defines the methods from given names.
///
/// Must be invoked with arguments `$ [...] [...]...` where [...] is triple `[lower UPPER color]`.
/// `lower` refers to the name of the Web Console function.
/// `UPPER` is the name of the generated logging macro.
/// `color` is the log color that will be used when writing to native (non-web) console output.
///
/// For each given triple `[lower UPPER color]` two symbols are defined:
/// * a function `$lower` that writes given text to standard output on native targets and to Web
///   Console on wasm targets.
/// * a macro `$UPPER` that wraps the above function with `println`-like syntax.
///
/// Note: The first argument `$d` must be `$` (dollar sign). It is used to insert dollar sign in the
/// nested macro.
macro_rules! define_debug_macros {
    ($d:tt $([$lower:ident $upper:ident $color:ident])*) => {$(
        /// Writes given text either to the stdout (non-wasm) or Web Console (wasm).
        pub fn $lower(text:impl AsRef<str>) {
            cfg_if::cfg_if! {
                if #[cfg(target_arch="wasm32")] {
                    use web_sys::console::*;
                    concat_idents!($lower,_1)(&wasm_bindgen::JsValue::from_str(text.as_ref()));
                } else {
                    use colored::*;
                    println!("[{}] {}", stringify!($upper).$color(), text.as_ref());
                }
            }
        }

        // FIXME [mwu] Should be restored. See [Clippy ICE workaround]
        // /// Special logging macro that prints to the Web Console on wasm targets and stdout
        // /// otherwise. It is supposed to be used only for development purposes and shouldn't be
        // /// present in a production-ready code.
        // /// Macro follows `iformat` formatting convention.
        // #[macro_export] macro_rules! $upper  {
        //     ($d($d arg:tt)*) => {
        //         $crate::debug::logging:: $lower($crate::iformat!($d ($d arg)*))
        //     }
        // }
    )*}
}

// FIXME [mwu] Should be removed. See [Clippy ICE workaround]
mod manually_expanded;

// Note [Clippy ICE workaround]
// ~~~~~~~~~~~~~~~~~~~~~
// The recent Clippy introduced ICE that happens when other crate uses debug macros in a lambda.
// To workaround this we need to define them manually, rather than with `define_debug_macros`.
// When https://github.com/rust-lang/rust-clippy/issues/7272 is resolved, we should bump and:
// 1) uncomment the second part of `define_debug_macros`;
// 2) remove the `manually_expanded` module altogether.

define_debug_macros!{$
    [trace TRACE   purple]
    [debug DEBUG   blue]
    [info  INFO    white]
    [warn  WARNING yellow]
    [error ERROR   red]
}

#[cfg(test)]
mod tests {
    use crate::*;
    use wasm_bindgen_test::*;
    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    // We don't have any convenient means to check what gets written. So at least we check that the
    // all macros are present and can be invoked.

    #[test]
    fn native_calls() {
        let var = 39;
        TRACE!("test");
        DEBUG!("Using new iformat syntax: var = " var ". Is that much?");
        INFO!("Using old iformat syntax: var = {var}. Is that much?");
        WARNING!("test");
        ERROR!("test");
    }
    #[wasm_bindgen_test]
    fn wasm_calls() {
        let var = 39;
        TRACE!("test");
        DEBUG!("Using new iformat syntax: var = " var ". Is that much?");
        INFO!("Using old iformat syntax: var = {var}. Is that much?");
        WARNING!("test");
        ERROR!("test");
    }
}
