//! Proc macros supporting the implementation of the `enso_profiler` library.
//!
//! The profiler API uses procedural macros for two reasons:
//! - To define the hierarchy of profiler types ([`define_hierarchy!`]). Each profiler type (e.g.
//!   [`Objective`](../enso_profiler/struct.Objective.html)) needs an implementation of
//!   [`Parent`](../enso_profiler/trait.Parent.html) for each finer-grained profiler type;
//!   implementing this without proc macros would be complex and repetitious.
//! - To implement the [`#[profile]`](macro@profile) attribute macro.

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
/// Define a profiler type.
fn define_profiler(
    level: &syn::Ident,
    ObjIdent: &syn::Ident,
    enabled: bool,
) -> proc_macro::TokenStream {
    let start = quote::format_ident!("start_{}", level);
    let with_same_start = quote::format_ident!("{}_with_same_start", level);
    let level_link = format!("[{}-level](index.html#{})", level, level);
    let doc_obj = format!("Identifies a {} profiler.", level_link);
    let doc_start = format!("Start a new {} profiler.", level_link);
    let doc_with_same_start = format!(
        "Create a new {} profiler, with the same start as an existing profiler.",
        level_link,
    );
    let ts = if enabled {
        quote::quote! {
            // =================================
            // === Profiler (e.g. Objective) ===
            // =================================

            #[doc = #doc_obj]
            #[derive(Copy, Clone, Debug)]
            pub struct #ObjIdent(pub ProfilerId);


            // === Trait Implementations ===

            impl Profiler for #ObjIdent {
                type Started = ProfilerData;
                fn finish(self, data: &Self::Started) {
                    data.finish(self.0);
                }
            }


            // === Constructor macros ===

            #[doc = #doc_start]
            #[macro_export]
            macro_rules! #start {
                ($parent: expr, $label: expr) => {{
                    use profiler::Parent;
                    let label = concat!($label, " (", file!(), ":", line!(), ")");
                    let profiler: profiler::Started<profiler::#ObjIdent> = $parent.new_child(label);
                    profiler
                }}
            }

            #[doc = #doc_with_same_start]
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
        }
    } else {
        quote::quote! {
            // =================================
            // === Profiler (e.g. Objective) ===
            // =================================

            #[doc = #doc_obj]
            #[derive(Copy, Clone, Debug)]
            pub struct #ObjIdent(pub ());


            // === Trait Implementations ===

            impl Profiler for #ObjIdent {
                type Started = ();
                fn finish(self, _: &Self::Started) {}
            }


            // === Constructor macros ===

            #[doc = #doc_start]
            #[macro_export]
            macro_rules! #start {
                ($parent: expr, $label: expr) => {
                    profiler::Started { profiler: profiler::#ObjIdent(()), data: () }
                }
            }
            #[macro_export]
            #[doc = #doc_with_same_start]
            macro_rules! #with_same_start {
                ($parent: expr, $label: expr) => {
                    profiler::Started { profiler: profiler::#ObjIdent(()), data: () }
                }
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
///
/// # Usage
///
/// ```ignore
/// enso_profiler_macros::define_hierarchy![Objective, Task, Detail, Debug];
/// ```
///
/// Profiler-levels must be specified from coarsest to finest.
///
/// Profiler-level names should be given in CamelCase.
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
        out.extend(define_profiler(&snake_ident, ObjIdent, enabled_level >= i));
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

// [Documented in `profiler`](../enso_profiler/attr.profile.html).
#[allow(missing_docs)]
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
    let args = forward_args(&mut wrapper_sig.inputs);

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

    let is_method = match func.sig.inputs.first().unwrap() {
        syn::FnArg::Receiver(_) => true,
        syn::FnArg::Typed(_) => false,
    };
    let mut segments = punctuated::Punctuated::<syn::PathSegment, syn::Token![::]>::new();
    if is_method {
        let ident = syn::Ident::new("Self", proc_macro2::Span::call_site());
        segments.push(ident.into());
    };
    segments.push(impl_ident.clone().into());
    let impl_path = syn::Path { leading_colon: None, segments };

    let call = match func.sig.asyncness {
        None => quote::quote! { #impl_path(#args) },
        Some(_) => quote::quote! { #impl_path(#args).await },
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

// === Argument forwarding ===

type SigInputs = punctuated::Punctuated<syn::FnArg, syn::Token![,]>;
type CallArgs = punctuated::Punctuated<syn::Expr, syn::Token![,]>;

/// Given a function's inputs, produce a list of exprs the function can use to forward its arguments
/// to another function with the same signature.
///
/// May modify input bindings to simplify forwarding.
fn forward_args(inputs: &mut SigInputs) -> CallArgs {
    let mut args = CallArgs::new();
    for (i, input) in inputs.iter_mut().enumerate() {
        let ident = match input {
            syn::FnArg::Receiver(r) => r.self_token.into(),
            syn::FnArg::Typed(pat_type) => {
                let arg = syn::Ident::new(&format!("arg{}", i), proc_macro2::Span::call_site());
                pat_type.pat = Box::new(ident_to_pat(arg.clone()));
                arg
            },
        };
        args.push(ident_to_expr(ident));
    }
    args
}

fn ident_to_expr(ident: syn::Ident) -> syn::Expr {
    let path = ident.into();
    let attrs = Default::default();
    let qself = Default::default();
    syn::ExprPath { path, qself, attrs }.into()
}

fn ident_to_pat(ident: syn::Ident) -> syn::Pat {
    let attrs = Default::default();
    let by_ref = Default::default();
    let mutability = Default::default();
    let subpat = Default::default();
    syn::PatIdent { attrs, by_ref, mutability, ident, subpat }.into()
}
