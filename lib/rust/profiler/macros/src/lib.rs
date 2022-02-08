//! Proc macros supporting the implementation of the `enso_profiler` library.
//!
//! The profiler API uses procedural macros for two reasons:
//! - To define the hierarchy of profiler types ([`define_hierarchy!`]). Each profiler type (e.g.
//!   [`Objective`](../enso_profiler/struct.Objective.html)) needs an implementation of
//!   [`Parent`](../enso_profiler/trait.Parent.html) for each finer-grained profiler type;
//!   implementing this without proc macros would be complex and repetitious.
//! - To implement the [`#[profile]`](macro@profile) attribute macro.

#![cfg_attr(feature = "lineno", feature(proc_macro_span))]
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
use std::mem;
use syn::parse::Parser;
use syn::punctuated;
use syn::visit_mut;
use syn::visit_mut::VisitMut;



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
    let body_default =
        Box::new(syn::Block { brace_token: Default::default(), stmts: vec![] });
    let mut new_body = body_default.clone();
    let orig_body = mem::replace(&mut func.block, body_default);
    let orig_sig = func.sig.clone();

    // Emit instrumentation at the beginning of the new body; genericize the wrapper's signature.
    instrument(&mut func.sig, &mut new_body);
    // Instead of inserting the original body directly, wrap it in a closure that is not generic
    // over `impl Parent`; this avoids the code-size cost of monomorphizing it.
    wrap_body(&orig_sig, *orig_body, &mut new_body);

    func.block = new_body;
    func.into_token_stream().into()
}

// === instrument ===

/// Insert instrumentation into the body, and genericize the profiler input to `impl Parent`.
fn instrument(sig: &mut syn::Signature, body: &mut syn::Block) {
    let profiler_label = make_label(&sig.ident);

    // Parse profiler argument in signature.
    let last_arg = match sig.inputs.last_mut() {
        Some(syn::FnArg::Typed(pat_type)) => pat_type,
        _ => panic!("Function annotated with #[profile] must have a profiler argument."),
    };
    let profiler = last_arg.pat.clone();

    // Replace profiler type `T `in sig with `impl Parent<T>`.
    let profiler_type_in = last_arg.ty.clone();
    let profiler_generic_parent = quote::quote! { impl profiler::Parent<#profiler_type_in> };
    last_arg.ty = Box::new(syn::Type::Verbatim(profiler_generic_parent));

    // Emit instrumentation statements.
    let start_profiler = quote::quote! {
        let _profiler = #profiler.new_child(#profiler_label);
    };
    let get_profiler = quote::quote! {
        let #profiler = _profiler.profiler;
    };
    body.stmts.push(syn::parse(start_profiler.into()).unwrap());
    body.stmts.push(syn::parse(get_profiler.into()).unwrap());
}

#[cfg(not(feature = "lineno"))]
/// Decorate the input with file:line info determined by the proc_macro's call site.
fn make_label(ident: &syn::Ident) -> String {
    format!("{} (?:?)", ident)
}

#[cfg(feature = "lineno")]
/// Decorate the input with file:line info determined by the proc_macro's call site.
fn make_label(ident: &syn::Ident) -> String {
    let span = proc_macro::Span::call_site();
    let file = span.source_file().path();
    let path = file.as_path().to_string_lossy();
    let line = span.start().line;
    format!("{} ({}:{})", ident, path, line)
}

// === wrap_body ===

type ClosureInputs = punctuated::Punctuated<syn::Pat, syn::Token![,]>;
type CallArgs = punctuated::Punctuated<syn::Expr, syn::Token![,]>;

/// Transforms a function's body into a new body that:
/// - Defines a nested function whose body is the original body.
/// - Calls the inner function, forwarding all arguments, returning its result.
///
/// The output of this transformation is operationally equivalent to the input; it is only useful in
/// conjunction with additional transformations. In particular, if after further transformation the
/// outer function is more generic than the inner function for some parameters, this pattern avoids
/// the code-size cost of monomorphization for those parameters.
///
/// # Implementation
///
/// For the nested function, we generate a closure rather than the more common `fn` seen with this
/// pattern because we need to be able to forward a `self` argument, but have no way to determine
/// the type of `Self`, so we can't write a function signature. In a closure definition, we can
/// simply leave the type of that argument implicit.
///
/// # Limitations
///
/// Not currently supported:
/// - `unsafe` functions
/// - functions with destructuring bindings
/// - functions containing an `impl` for a `struct` or `trait`
fn wrap_body(sig: &syn::Signature, mut body: syn::Block, body_out: &mut syn::Block) {
    // If there's a `self` parameter, we need to replace it with an ordinary binding in the wrapped
    // block (here) and signature (below, building `closure_inputs`).
    let placeholder_self = syn::Ident::new("__wrap_body__self", proc_macro2::Span::call_site());
    let find = syn::Ident::new("self", proc_macro2::Span::call_site());
    let replace = placeholder_self.clone();
    ReplaceAll { find, replace }.visit_block_mut(&mut body);

    let mut call_args = CallArgs::new();
    let mut closure_inputs = ClosureInputs::new();
    for input in &sig.inputs {
        call_args.push(match input {
            syn::FnArg::Receiver(r) => ident_to_expr(r.self_token.into()),
            syn::FnArg::Typed(pat_type) => pat_to_expr(&pat_type.pat),
        });
        closure_inputs.push(match input {
            syn::FnArg::Receiver(_) => ident_to_pat(placeholder_self.clone()),
            syn::FnArg::Typed(pat_ty) => pat_ty.clone().into(),
        });
    }

    let wrapped = match &sig.asyncness {
        None => quote::quote! {
            return (|#closure_inputs| #body)(#call_args);
        },
        Some(_) => quote::quote! {
            return (|#closure_inputs| async { #body })(#call_args).await;
        },
    };
    body_out.stmts.push(syn::parse(wrapped.into()).unwrap());
}

struct ReplaceAll {
    find:    proc_macro2::Ident,
    replace: proc_macro2::Ident,
}
impl visit_mut::VisitMut for ReplaceAll {
    fn visit_ident_mut(&mut self, i: &mut syn::Ident) {
        if i == &self.find {
            *i = self.replace.clone();
        }
    }
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

fn ident_to_pat(ident: syn::Ident) -> syn::Pat {
    let attrs = Default::default();
    let by_ref = None;
    let mutability = None;
    let subpat = None;
    syn::PatIdent { attrs, by_ref, mutability, ident, subpat }.into()
}
