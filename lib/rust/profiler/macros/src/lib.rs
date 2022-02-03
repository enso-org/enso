//! Proc macros supporting the implementation of the `enso_profiler` library.
//!
//! The profiler API uses procedural macros for two reasons:
//! - To define the hierarchy of profiler types ([`define_hierarchy`]). Each profiler type (e.g.
//!   [`enso_profiler::Objective`]) needs an implementation of [`enso_profiler::Parent`] for each
//!   lower-level profiler type (e.g. [`enso_profiler::Task`]); implementing this without proc
//!   macros would be complex and repetitious.
//! - To implement the `#[profile]` attribute macro ([`profile`]).

#![feature(proc_macro_span)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

use inflector::Inflector;
use quote::ToTokens;
use std::env;
use syn::parse::Parser;
use syn::punctuated;


// =========================
// === define_hierarchy! ===
// =========================

#[allow(non_snake_case)]
/// Define a profiler type for an enabled profiling level.
fn enabled_profiler(level: &syn::Ident, ObjIdent: &syn::Ident) -> proc_macro::TokenStream {
    let start = quote::format_ident!("start_{}", level);
    let with_same_start = quote::format_ident!("{}_with_same_start", level);
    let ts = quote::quote! {
        // =================================
        // === Profiler (e.g. Objective) ===
        // =================================

        /// A lightweight object identifying a #snoke_ident-level profiler.
        #[derive(Copy, Clone, Debug)]
        pub struct #ObjIdent(ProfilerId);


        // === Trait Implementations ===

        impl Profiler for #ObjIdent {
            type Started = ProfilerData;
            fn finish(self, data: &Self::Started) {
                data.finish(self.0);
            }
        }


        // === Constructor macros ===

        /// Create a new #level-level profiler.
        #[macro_export]
        macro_rules! #start {
            ($parent: expr, $label: expr) => {{
                use profiler::Parent;
                let label = concat!($label, " (", file!(), ":", line!(), ")");
                let profiler: profiler::Started<profiler::#ObjIdent> = $parent.new_child(label);
                profiler
            }}
        }

        /// Create a #level-level profiler, with the same start as an existing profiler.
        #[macro_export]
        macro_rules! #with_same_start {
            ($parent: expr, $label: expr) => {{
                use profiler::Parent;
                let label = concat!($label, " (", file!(), ":", line!(), ")");
                let profiler: profiler::Started<profiler::#ObjIdent> =
                    $parent.new_child_same_start(label);
                profiler
            }}
        }
    };
    ts.into()
}

/// Define a profiler type for a disabled profiling level.
#[allow(non_snake_case)]
fn disabled_profiler(snake_ident: &syn::Ident, ObjIdent: &syn::Ident) -> proc_macro::TokenStream {
    let start = quote::format_ident!("start_{}", snake_ident);
    let with_same_start = quote::format_ident!("{}_with_same_start", snake_ident);
    let ts = quote::quote! {
        // =================================
        // === Profiler (e.g. Objective) ===
        // =================================

        /// A lightweight object identifying a #snoke_ident-level profiler.
        #[derive(Copy, Clone, Debug)]
        pub struct #ObjIdent(());


        // === Trait Implementations ===

        impl Profiler for #ObjIdent {
            type Started = ();
            fn finish(self, _: &Self::Started) {}
        }


        // === Constructor macros ===

        /// Create a new #level-level profiler.
        #[macro_export]
        macro_rules! #start {
            ($parent: expr, $label: expr) => {
                profiler::Started { profiler: profiler::#ObjIdent(()), data: () }
            }
        }
        /// Create a #level-level profiler, with the same start as an existing profiler.
        #[macro_export]
        macro_rules! #with_same_start {
            ($parent: expr, $label: expr) => {
                profiler::Started { profiler: profiler::#ObjIdent(()), data: () }
            }
        }
    };
    ts.into()
}

/// Generates an implementation of the [`Parent`] trait relating the given parent and child, when
/// both are enabled.
#[allow(non_snake_case)]
fn enabled_impl_parent(
    ParentIdent: &syn::Ident,
    ChildIdent: &syn::Ident,
) -> proc_macro::TokenStream {
    let ts = quote::quote! {
        impl Parent<#ChildIdent> for #ParentIdent {
            fn new_child(&self, label:Label) -> Started<#ChildIdent> {
                let profiler = #ChildIdent(ProfilerId::new());
                let parent = self.0;
                let start = Some(Timestamp::now());
                let data = ProfilerData { parent, start, label };
                Started { profiler, data }
            }
            fn new_child_same_start(&self, label:Label) -> Started<#ChildIdent> {
                let profiler = #ChildIdent(ProfilerId::new());
                let parent = self.0;
                let start = None;
                let data = ProfilerData { parent, start, label };
                Started { profiler, data }
            }
        }
    };
    ts.into()
}

/// Generates an implementation of the [`Parent`] trait relating the given parent and child, when
/// the child is disabled.
#[allow(non_snake_case)]
fn disabled_impl_parent(
    ParentIdent: &syn::Ident,
    ChildIdent: &syn::Ident,
) -> proc_macro::TokenStream {
    let ts = quote::quote! {
        impl Parent<#ChildIdent> for #ParentIdent {
            fn new_child(&self, label: Label) -> Started<#ChildIdent> {
                self.new_child_same_start(label)
            }
            fn new_child_same_start(&self, _label: Label) -> Started<#ChildIdent> {
                let profiler = #ChildIdent(());
                let data = ();
                Started { profiler, data }
            }
        }
    };
    ts.into()
}

/// Return the numeric Profiling Level (counting from 0 = top-level only).
fn get_enabled_level(levels: &[impl AsRef<str>]) -> usize {
    const PROFILING_LEVEL_ENV_VAR: &str = "ENSO_MAX_PROFILING_LEVEL";
    let enabled = match env::var(PROFILING_LEVEL_ENV_VAR) {
        Ok(level) => level,
        // If the variable isn't set, we default to the minimum.
        Err(_) => return 0,
    };
    for (i, name) in levels.iter().enumerate() {
        if &enabled[..] == name.as_ref() {
            return i;
        }
    }
    panic!("{} set to unknown profiling level: {}", PROFILING_LEVEL_ENV_VAR, enabled);
}

/// Defines a hierarchy of profiler levels.
#[proc_macro]
#[allow(non_snake_case)]
pub fn define_hierarchy(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parser = punctuated::Punctuated::<syn::Ident, syn::Token![,]>::parse_terminated;
    let idents: Vec<_> = parser.parse(ts).unwrap().into_iter().collect();
    let level_names: Vec<_> = idents.iter().map(|id| id.to_string().to_snake_case()).collect();
    let enabled_level = get_enabled_level(&level_names);
    let mut out = proc_macro::TokenStream::new();
    for ((i, ObjIdent), level_name) in idents.iter().enumerate().zip(level_names.iter()) {
        let snake_ident = syn::Ident::new(level_name, proc_macro2::Span::call_site());
        let profiler_def = match enabled_level >= i {
            true => enabled_profiler(&snake_ident, ObjIdent),
            false => disabled_profiler(&snake_ident, ObjIdent),
        };
        out.extend(profiler_def);
        for (j, ChildIdent) in idents[i..].iter().enumerate() {
            let parent_impl = match enabled_level >= i + j {
                true => enabled_impl_parent(ObjIdent, ChildIdent),
                false => disabled_impl_parent(ObjIdent, ChildIdent),
            };
            out.extend(parent_impl);
        }
    }
    out
}



// ================
// === profile! ===
// ================

/// Instruments a function.
#[proc_macro_attribute]
pub fn profile(
    args: proc_macro::TokenStream,
    ts: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    assert!(args.is_empty(), "#[profile] does not expect any arguments");
    let mut func = syn::parse_macro_input!(ts as syn::ItemFn);
    let impl_name = &format!("__profiler_wrapped__{}", func.sig.ident);
    let impl_ident = syn::Ident::new(impl_name, proc_macro2::Span::call_site());
    let wrapper_ident = std::mem::replace(&mut func.sig.ident, impl_ident);
    let wrapper = make_wrapper(wrapper_ident, &func);
    func.vis = syn::Visibility::Inherited;

    // Emit wrapped function and wrapper..
    let mut out = proc_macro2::TokenStream::new();
    wrapper.to_tokens(&mut out);
    func.to_tokens(&mut out);
    out.into()
}

fn make_wrapper(wrapper_ident: syn::Ident, func: &syn::ItemFn) -> proc_macro2::TokenStream {
    let profiler_label = make_label(&wrapper_ident);

    // Create the wrapper signature, based on the impl signature.
    let vis = func.vis.clone();
    let mut wrapper_sig = func.sig.clone();
    wrapper_sig.ident = wrapper_ident;
    let impl_ident = &func.sig.ident;

    // Gather argument idents for call in wrapper
    let args = forward_args(&wrapper_sig.inputs);

    // Parse profiler argument in signature.
    let last_arg = match wrapper_sig.inputs.last_mut().unwrap() {
        syn::FnArg::Typed(pat_type) => pat_type,
        _ => panic!("Last argument to function annotated with #[profile] must be a profiler type"),
    };
    let profiler_ident = last_arg.pat.clone();

    // Replace profiler type `T `in sig with `impl Parent<T>`.
    let profiler_type_in = last_arg.ty.clone();
    let profiler_generic_parent = quote::quote! { impl profiler::Parent<#profiler_type_in> };
    last_arg.ty = Box::new(syn::Type::Verbatim(profiler_generic_parent));

    let call = match func.sig.asyncness {
        None => quote::quote! { #impl_ident(#args) },
        Some(_) => quote::quote! { #impl_ident(#args).await },
    };
    quote::quote! {
        #vis #wrapper_sig {
            let _profiler = #profiler_ident.new_child(#profiler_label);
            let #profiler_ident = _profiler.profiler;
            #call
        }
    }
}

/// Create a [`Measurement`] label, with its file:line info determined by the proc_macro's call
/// site.
fn make_label(ident: &syn::Ident) -> String {
    let span = proc_macro::Span::call_site();
    let file = span.source_file().path();
    let path = file.as_path().to_string_lossy();
    let line = span.start().line;
    format!("{} ({}:{})", ident, path, line)
}

type SigInputs = punctuated::Punctuated<syn::FnArg, syn::Token![,]>;
type CallArgs = punctuated::Punctuated<syn::Expr, syn::Token![,]>;

// Given a function's inputs, produce the argument list the function would use to recurse with all
// its original arguments--or call another function with the same signature.
fn forward_args(inputs: &SigInputs) -> CallArgs {
    let mut args = CallArgs::new();
    for input in inputs {
        args.push(match input {
            syn::FnArg::Receiver(r) => ident_to_expr(r.self_token.into()),
            syn::FnArg::Typed(pat_type) => pat_to_expr(&pat_type.pat),
        });
    }
    args
}

fn pat_to_expr(pat: &syn::Pat) -> syn::Expr {
    match pat {
        syn::Pat::Ident(syn::PatIdent { ident, .. }) => ident_to_expr(ident.to_owned()),
        syn::Pat::Reference(syn::PatReference { pat, mutability, .. }) => {
            let expr = Box::new(pat_to_expr(pat));
            let mutability = mutability.to_owned();
            let attrs = Default::default();
            let and_token = Default::default();
            let raw = Default::default();
            syn::ExprReference { expr, mutability, attrs, and_token, raw }.into()
        }
        _ => unimplemented!("destructuring-bind in signature of fn wrapped by #[profile]"),
    }
}

fn ident_to_expr(ident: syn::Ident) -> syn::Expr {
    let path = ident.into();
    let attrs = Default::default();
    let qself = Default::default();
    syn::ExprPath { path, qself, attrs }.into()
}
