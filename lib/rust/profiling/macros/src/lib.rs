//! Support crate that allows to define loggers based on a configuration file.
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

use proc_macro2::TokenStream;
use quote::quote;

/// Calls the given macro with each defined profiler and creates a method that checks whether
/// a given profiler is activated via the crates feature flags.
/// The macro that is expected to be passed is `define_logger` from the profiling crate. The
/// call is built to satisfy the input requirements of this macro. The `define_logger` macro
/// needs to be defined in the profiling crate as it exposes API related structs that also need
/// to be part of the public API there and to avoid circular dependencies.
#[proc_macro]
pub fn define_hierarchy(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let profilers: Vec<String> = parse_profilers(tokens.into());
    let mut out_tokens = proc_macro::TokenStream::default();
    out_tokens.extend(make_profiling_level_is_active_function(&profilers).into_iter());
    out_tokens
}

fn parse_profilers(tokens: proc_macro2::TokenStream) -> Vec<String> {
    let mut profiler_names = Vec::default();
    for token in tokens {
        if let proc_macro2::TokenTree::Ident(ident) = token {
            profiler_names.push(ident.to_string());
        }
    }
    profiler_names
}

/// Creates a function that checks whether a given profiling level name corresponds to an active
/// profiling level. The profilers are interpreted to be hierarchical, so if a profiler further
/// down the list is active, all profilers above are expected to be active, too.
fn make_profiling_level_is_active_function(profilers: &[String]) -> proc_macro::TokenStream {
    let mut match_arms = TokenStream::default();
    for ix in 0..profilers.len() {
        let match_arm: TokenStream = if ix == (profilers.len() - 1) {
            make_match_line(&profilers[ix], None)
        } else {
            make_match_line(&profilers[ix], Some(&profilers[ix + 1]))
        };
        match_arms.extend(match_arm.into_iter())
    }

    let is_active_fn_tokens: proc_macro::TokenStream = quote! {
         fn profiling_level_is_active(log_level: ProfilingLevel) -> bool {
            match log_level.as_str() {
                #match_arms
                invalid_level => {
                    WARNING!(
                        format!("Tried to check for invalid profiling level: {:?}", invalid_level)
                    );
                    false
                }
            }
        }
    }
    .into();

    is_active_fn_tokens
}

/// Helper function that creates a match arm for the `make_profiling_level_is_active_function`.
/// The match arm will check whether the given profiler is enabled, and if there is a next one,
/// insert a recursive call to check whether the next profiler in the hierarchy is enabled.
fn make_match_line(current: &str, next: Option<&str>) -> TokenStream {
    let current = current.to_lowercase();
    let current_profiler_mod = quote::format_ident!("{}", current);
    if let Some(next_profiling_level) = next {
        let next_profiling_level = next_profiling_level.to_lowercase();
        quote! {
             #current => {
                #current_profiler_mod::ENABLED
                    || profiling_level_is_active(#next_profiling_level.to_string())
             }
        }
    } else {
        quote! {
             #current => #current_profiler_mod::ENABLED,
        }
    }
}
