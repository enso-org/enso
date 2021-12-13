//! Support crate that allows to define loggers based on a configuration file.
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;

/// Config file that contains a list of the loggers that should exist.
const PROFILERS: &str =
    include_str!("../../../../../app/ide-desktop/lib/profiling/src/profilers.json");

/// Calls the given macro with each defined profiler and creates a method that checks whether
/// a given profiler is activated via the crates feature flags.
/// The macro that is expected to be passed is `define_logger` from the profiling crate. The
/// call is built to satisfy the input requirements of this macro. The `define_logger` macro
/// needs to be defined in the profiling crate as it exposes API related structs that also need
/// to be part of the public API there and to avoid circular dependencies.
#[proc_macro]
pub fn with_all_profiling_levels(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = item.into();
    let input_macro: Ident = {
        match input.into_iter().next().unwrap() {
            TokenTree::Ident(i) => i,
            _ => panic!("`with_all_profiling_levels` requires an identifier as input."),
        }
    };
    let profilers: Vec<String> = serde_json::from_str(PROFILERS).unwrap();

    let mut out_tokens = proc_macro::TokenStream::default();
    out_tokens.extend(make_profilers(input_macro, &profilers).into_iter());
    out_tokens.extend(make_profiling_level_is_active_function(&profilers).into_iter());

    out_tokens
}

/// Create the call to the `define_logger` macro (which needs to be passed as identifier to this
/// method)
fn make_profilers(input_macro: Ident, profilers: &[String]) -> proc_macro::TokenStream {
    let mut out_tokens = proc_macro::TokenStream::default();
    for profiler in profilers {
        let profiler_name = profiler.clone();
        let profiler_name_lowercase = profiler.to_lowercase();
        let name = quote::format_ident!("{}", profiler_name);
        let name_lowercase = quote::format_ident!("{}", profiler_name_lowercase);
        let start = quote::format_ident!("start_{}", name_lowercase);
        let end = quote::format_ident!("end_{}", name_lowercase);
        let measure = quote::format_ident!("measure_{}", name_lowercase);
        let tokens: proc_macro::TokenStream = quote! {
        #input_macro!(#profiler_name_lowercase, #name, #name_lowercase, #start, #end, #measure);
        }
        .into();
        out_tokens.extend(tokens.into_iter())
    }
    out_tokens
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
                    WARNING!(format!("Tried to check for invalid profiling level: {:?}", invalid_level));
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
             #current => #current_profiler_mod::ENABLED || profiling_level_is_active(#next_profiling_level.to_string()),
        }
    } else {
        quote! {
             #current => #current_profiler_mod::ENABLED,
        }
    }
}
