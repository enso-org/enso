//! Proc macros supporting the implementation of the `enso_profiler` library.
//!
//! The profiler API uses procedural macros for two reasons:
//! - To define the hierarchy of profiler types ([`define_hierarchy!`]). Each profiler type (e.g.
//!   [`Objective`](../enso_profiler/struct.Objective.html)) needs an implementation of
//!   [`Parent`](../enso_profiler/trait.Parent.html) for each finer-grained profiler type;
//!   implementing this without proc macros would be complex and repetitious.
//! - To implement the [`#[profile]`](macro@profile) attribute macro.

#![cfg_attr(feature = "lineno", feature(proc_macro_span))]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

use inflector::Inflector;
use quote::ToTokens;
use std::env;
use std::fmt;
use syn::parse::Parser;
use syn::punctuated;
use syn::visit_mut;
use syn::visit_mut::VisitMut;



// =========================
// === define_hierarchy! ===
// =========================

/// Define a profiler type.
fn define_profiler(
    level: &syn::Ident,
    obj_ident: &syn::Ident,
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
            pub struct #obj_ident(pub EventId);


            // === Trait Implementations ===

            impl Profiler for #obj_ident {
                fn start(
                    parent: EventId,
                    label: StaticLabel,
                    time: Option<Timestamp>,
                    start: StartState,
                ) -> Self {
                    #obj_ident(EventLog.start(parent, label, time, start))
                }
                fn finish(self) {
                    EventLog.end(self.0, Timestamp::now())
                }
                fn pause(self) {
                    EventLog.pause(self.0, Timestamp::now());
                }
                fn resume(self) {
                    EventLog.resume(self.0, Timestamp::now());
                }
            }


            // === Constructor macros ===

            #[doc = #doc_start]
            #[macro_export]
            macro_rules! #start {
                ($parent: expr, $label: expr) => {{
                    use profiler::Parent;
                    let label = profiler::internal::Label(
                        concat!($label, " (", file!(), ":", line!(), ")"));
                    let profiler: profiler::internal::Started<profiler::#obj_ident> =
                        $parent.new_child(label);
                    profiler
                }}
            }

            #[doc = #doc_with_same_start]
            #[macro_export]
            macro_rules! #with_same_start {
                ($parent: expr, $label: expr) => {{
                    use profiler::Parent;
                    let label = profiler::internal::Label(
                        concat!($label, " (", file!(), ":", line!(), ")"));
                    let profiler: profiler::internal::Started<profiler::#obj_ident> =
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
            pub struct #obj_ident(pub ());


            // === Trait Implementations ===

            impl Profiler for #obj_ident {
                fn start(
                    _: EventId,
                    _: StaticLabel,
                    _: Option<Timestamp>,
                    _: StartState,
                ) -> Self {
                    Self(())
                }
                fn finish(self) {}
                fn pause(self) {}
                fn resume(self) { }
            }


            // === Constructor macros ===

            #[doc = #doc_start]
            #[macro_export]
            macro_rules! #start {
                ($parent: expr, $label: expr) => {
                    profiler::internal::Started(profiler::#obj_ident(()))
                }
            }
            #[macro_export]
            #[doc = #doc_with_same_start]
            macro_rules! #with_same_start {
                ($parent: expr, $label: expr) => {
                    profiler::internal::Started(profiler::#obj_ident(()))
                }
            }
        }
    };
    ts.into()
}

/// Generates an implementation of the [`Parent`] trait relating the given parent and child, when
/// both are enabled.
fn enabled_impl_parent(
    parent_ident: &syn::Ident,
    child_ident: &syn::Ident,
) -> proc_macro::TokenStream {
    let ts = quote::quote! {
        impl Parent<#child_ident> for #parent_ident {
            fn new_child(&self, label: StaticLabel) -> Started<#child_ident> {
                let start = Some(Timestamp::now());
                Started(#child_ident::start(self.0, label, start, StartState::Active))
            }
            fn new_child_same_start(&self, label: StaticLabel) -> Started<#child_ident> {
                Started(#child_ident::start(self.0, label, None, StartState::Active))
            }
        }
    };
    ts.into()
}

/// Generates an implementation of the [`Parent`] trait relating the given parent and child, when
/// the child is disabled.
fn disabled_impl_parent(
    parent_ident: &syn::Ident,
    child_ident: &syn::Ident,
) -> proc_macro::TokenStream {
    let ts = quote::quote! {
        impl Parent<#child_ident> for #parent_ident {
            fn new_child(&self, label: StaticLabel) -> Started<#child_ident> {
                self.new_child_same_start(label)
            }
            fn new_child_same_start(&self, _label: StaticLabel) -> Started<#child_ident> {
                Started(#child_ident(()))
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
pub fn define_hierarchy(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parser = punctuated::Punctuated::<syn::Ident, syn::Token![,]>::parse_terminated;
    let idents: Vec<_> = parser.parse(ts).unwrap().into_iter().collect();
    let level_names: Vec<_> = idents.iter().map(|id| id.to_string().to_snake_case()).collect();
    let enabled_level = get_enabled_level(&level_names);
    let mut out = proc_macro::TokenStream::new();
    for ((i, obj_ident), level_name) in idents.iter().enumerate().zip(level_names.iter()) {
        let snake_ident = syn::Ident::new(level_name, proc_macro2::Span::call_site());
        out.extend(define_profiler(&snake_ident, obj_ident, enabled_level >= i));
        for (j, child_ident) in idents[i..].iter().enumerate() {
            let parent_impl = match enabled_level >= i + j {
                true => enabled_impl_parent(obj_ident, child_ident),
                false => disabled_impl_parent(obj_ident, child_ident),
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
    let mut func = syn::parse_macro_input!(ts as syn::ItemFn);
    let level: syn::Ident = syn::parse(args)
        .expect("The `profile` macro requires a profiling-level argument, e.g. #[profile(Task)]");
    let label = make_label(&func.sig.ident);
    // Instrument awaits, whether at the top level (if this is an async fn) or in inner async
    // blocks.
    WrapAwait.visit_block_mut(&mut func.block);
    // Different transformations for async or non-async.
    let async_block_origin = match func.sig.asyncness {
        // Async: transform it to an non-async fn containing an async block. The outer fn does
        // not need any top-level profiling; all it does is set up the async block. We'll
        // instrument the block below.
        Some(_) => {
            wrap_async_fn(&mut func);
            AsyncBlockOrigin::FnBody
        }
        // Non-async: instrument the top level of the function.
        None => {
            profile_sync(&level, &label, &mut func);
            AsyncBlockOrigin::Block
        }
    };
    // Instrument any async blocks in the body.
    let name = func.sig.ident.to_string();
    let mut instrumentor = InstrumentAsync { level, func: name, origin: async_block_origin };
    instrumentor.visit_block_mut(&mut func.block);
    func.into_token_stream().into()
}

#[cfg(not(feature = "lineno"))]
/// Decorate the input with file:line info determined by the proc_macro's call site.
fn make_label<L: fmt::Display>(name: L) -> String {
    format!("{} (?:?)", name)
}

#[cfg(feature = "lineno")]
/// Decorate the input with file:line info determined by the proc_macro's call site.
fn make_label<L: fmt::Display>(name: L) -> String {
    let span = proc_macro::Span::call_site();
    let file = span.source_file().path();
    let path = file.as_path().to_string_lossy();
    let line = span.start().line;
    format!("{} ({}:{})", name, path, line)
}


// === VisitMut helpers ===

/// Used in a `impl VisitMut` to block descent into inner function items.
macro_rules! ignore_inner_fn_items {
    () => {
        fn visit_item_fn_mut(&mut self, _: &mut syn::ItemFn) {}
    };
}


// === profile_sync ===

fn profile_sync(obj_ident: &syn::Ident, label: &str, func: &mut syn::ItemFn) {
    let start_profiler = start_profiler(obj_ident, label, func.sig.asyncness.is_some());
    let block = &func.block;
    let body = quote::quote! {{
        #start_profiler
        let __profiler_scope = profiler::internal::Started(__profiler);
        #block
    }};
    func.block = syn::parse2(body).unwrap();
}

fn start_profiler(
    obj_ident: &syn::Ident,
    label: &str,
    asyncness: bool,
) -> proc_macro2::TokenStream {
    let state = match asyncness {
        true => quote::quote! { profiler::internal::StartState::Paused },
        false => quote::quote! { profiler::internal::StartState::Active },
    };
    quote::quote! {
        let __profiler = {
            use profiler::internal::Profiler;
            let parent = profiler::internal::EventId::implicit();
            let now = Some(profiler::internal::Timestamp::now());
            let label = profiler::internal::Label(#label);
            profiler::#obj_ident::start(parent, label, now, #state)
        };
    }
}


// === wrap_async_fn ===

/// Convert an `async fn` into a `fn` returning a `Future`, implemented with an `async` block.
///
/// The output is functionally equivalent to the input (except the output won't `impl Send` even if
/// the original `async fn` did); this is useful as a basis for further transformation.
fn wrap_async_fn(func: &mut syn::ItemFn) {
    // Wrap the body.
    let block = &func.block;
    let ret_ty = match &func.sig.output {
        syn::ReturnType::Default => quote::quote! { () },
        syn::ReturnType::Type(_, ty) => ty.into_token_stream(),
    };
    let body = quote::quote! {{
        (async move {
            let result: #ret_ty = #block;
            result
        })
    }};
    func.block = syn::parse2(body).unwrap();

    // Transform the signature.
    let output_ty = match &func.sig.output {
        syn::ReturnType::Default => quote::quote! { () },
        syn::ReturnType::Type(_, ty) => ty.to_token_stream(),
    };
    let output_lifetime = syn::Lifetime::new("'__profiler_out", proc_macro2::Span::call_site());
    let lifetimes = explicitize_lifetimes(&mut func.sig);
    let output = if lifetimes.is_empty() {
        quote::quote! {
            -> impl std::future::Future<Output=#output_ty>
        }
    } else {
        // Bound all inputs on the output lifetime.
        let type_bound = syn::TypeParamBound::Lifetime(output_lifetime.clone());
        for param in &mut func.sig.generics.params {
            match param {
                syn::GenericParam::Lifetime(def) => def.bounds.push(output_lifetime.clone()),
                syn::GenericParam::Type(def) => def.bounds.push(type_bound.clone()),
                syn::GenericParam::Const(_) => (),
            }
        }
        for arg in &mut func.sig.inputs {
            if let syn::FnArg::Typed(syn::PatType { ty, .. }) = arg {
                if let syn::Type::ImplTrait(def) = ty.as_mut() {
                    def.bounds.push(type_bound.clone());
                }
            }
        }
        // Add a definition for the output lifetime.
        let lifetime_def = syn::LifetimeDef::new(output_lifetime.clone());
        func.sig.generics.params.insert(0, syn::GenericParam::Lifetime(lifetime_def));
        // Apply the output lifetime to the output.
        quote::quote! {
            -> impl std::future::Future<Output=#output_ty> + #output_lifetime
        }
    };
    func.sig.asyncness = None;
    func.sig.output = syn::parse2(output).unwrap();
}

/// Make all lifetimes in function signature explicit.
///
/// Returns the lifetimes used in the function signature.
fn explicitize_lifetimes(sig: &mut syn::Signature) -> Vec<syn::Lifetime> {
    // Go through the args; find:
    // - anonymous lifetime: '_
    // - implicit lifetimes: &foo
    // - explicit lifetimes: &'a
    // Make all input lifetimes explicit:
    // - Use new lifetime explicitly in arg list.
    // - Define new lifetime in generic params.
    let mut input_transformer = ExplicitizeInputLifetimes::default();
    for input in &mut sig.inputs {
        input_transformer.visit_fn_arg_mut(input);
    }
    let ExplicitizeInputLifetimes { new_lifetimes, existing_lifetimes } = input_transformer;
    let mut all_lifetimes = existing_lifetimes;
    all_lifetimes.extend_from_slice(&new_lifetimes);
    let new_lifetimes =
        new_lifetimes.into_iter().map(|lt| syn::GenericParam::Lifetime(syn::LifetimeDef::new(lt)));
    sig.generics.params.extend(new_lifetimes);
    // There are two cases where output lifetimes may be elided:
    // - There's exactly one lifetime in the inputs.
    // - There's a receiver with a lifetime.
    // If either case occurs, make any implicit output lifetimes explicit.
    let default_lifetime = if all_lifetimes.len() == 1 {
        Some(all_lifetimes[0].clone())
    } else {
        get_receiver_lifetime(sig).cloned()
    };
    if let Some(lifetime) = default_lifetime {
        ExplicitizeOutputLifetimes { lifetime }.visit_return_type_mut(&mut sig.output);
    }
    all_lifetimes
}

#[derive(Default)]
struct ExplicitizeInputLifetimes {
    new_lifetimes:      Vec<syn::Lifetime>,
    existing_lifetimes: Vec<syn::Lifetime>,
}

impl ExplicitizeInputLifetimes {
    fn gen_lifetime(&mut self) -> syn::Lifetime {
        let name = format!("'__profiler{}", self.new_lifetimes.len());
        let new = syn::Lifetime::new(&name, proc_macro2::Span::call_site());
        self.new_lifetimes.push(new.clone());
        new
    }

    fn visit_elidable_lifetime(&mut self, lifetime: &mut Option<syn::Lifetime>) {
        match lifetime {
            Some(lifetime) => self.visit_lifetime_mut(lifetime),
            None => *lifetime = Some(self.gen_lifetime()),
        }
    }
}

impl visit_mut::VisitMut for ExplicitizeInputLifetimes {
    ignore_inner_fn_items!();

    // Handles 'x in generic parameters in types of non-self arguments.
    fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
        let name = lifetime.ident.to_string();
        if &name == "_" {
            *lifetime = self.gen_lifetime();
        } else {
            self.existing_lifetimes.push(lifetime.clone());
        }
    }

    // Handles &self.
    fn visit_receiver_mut(&mut self, receiver: &mut syn::Receiver) {
        if let Some((_, lifetime)) = &mut receiver.reference {
            self.visit_elidable_lifetime(lifetime);
        }
    }

    // Handles & in types of non-self arguments.
    fn visit_type_reference_mut(&mut self, type_reference: &mut syn::TypeReference) {
        self.visit_elidable_lifetime(&mut type_reference.lifetime);
    }
}

struct ExplicitizeOutputLifetimes {
    lifetime: syn::Lifetime,
}

impl visit_mut::VisitMut for ExplicitizeOutputLifetimes {
    ignore_inner_fn_items!();

    // Handles 'x in generic parameters in types.
    fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
        if &lifetime.ident.to_string() == "_" {
            *lifetime = self.lifetime.clone();
        }
    }

    // Handles & in types.
    fn visit_type_reference_mut(&mut self, type_reference: &mut syn::TypeReference) {
        if type_reference.lifetime.is_none() {
            type_reference.lifetime = Some(self.lifetime.clone());
        }
    }
}

fn get_receiver_lifetime(sig: &syn::Signature) -> Option<&syn::Lifetime> {
    match sig.inputs.first() {
        Some(syn::FnArg::Receiver(syn::Receiver {
            reference: Some((_, Some(lifetime))), ..
        })) => Some(lifetime),
        _ => None,
    }
}


// === WrapAwait ===

struct WrapAwait;

impl visit_mut::VisitMut for WrapAwait {
    ignore_inner_fn_items!();

    fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
        match expr {
            syn::Expr::Await(await_) => *expr = wrap_await(await_),
            _ => syn::visit_mut::visit_expr_mut(self, expr),
        }
    }
}

fn wrap_await(await_: &mut syn::ExprAwait) -> syn::Expr {
    let expr = &mut await_.base;
    WrapAwait.visit_expr_mut(expr);
    assert!(
        await_.attrs.is_empty(),
        "#[profile] cannot wrap a function that applies attributes to an .await expression"
    );
    let wrapped = quote::quote! {
        ({
            let future = #expr;
            profiler::internal::Profiler::pause(__profiler);
            let result = future.await;
            profiler::internal::Profiler::resume(__profiler);
            result
        })
    };
    syn::parse2(wrapped).unwrap()
}


// === InstrumentAsync ===

/// Inserts instrumentation into all async block in an item (ignoring inner items).
struct InstrumentAsync {
    level:  syn::Ident,
    func:   String,
    origin: AsyncBlockOrigin,
}

impl visit_mut::VisitMut for InstrumentAsync {
    ignore_inner_fn_items!();

    fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
        match expr {
            syn::Expr::Async(async_) => *expr = self.instrument_async(async_),
            _ => syn::visit_mut::visit_expr_mut(self, expr),
        }
    }
}

impl InstrumentAsync {
    /// Insert instrumentation into an async block.
    fn instrument_async(&self, expr: &mut syn::ExprAsync) -> syn::Expr {
        self.inner_instrumentor().visit_block_mut(&mut expr.block);
        assert!(
            expr.attrs.is_empty(),
            "#[profile] cannot wrap a function that applies attributes to an async block"
        );
        let label = match self.origin {
            AsyncBlockOrigin::FnBody => make_label(&self.func),
            AsyncBlockOrigin::Block => {
                let name = format!("<async block in {}>", &self.func);
                make_label(name)
            }
        };
        let start_profiler = start_profiler(&self.level, &label, true);
        let move_ = &expr.capture;
        let block = &expr.block;
        let wrapped = if move_.is_some() {
            quote::quote! {{
                #start_profiler
                let __profiler_scope = profiler::internal::Started(__profiler);
                async move {
                    profiler::internal::Profiler::resume(__profiler);
                    let result = #block;
                    std::mem::drop(__profiler_scope);
                    result
                }
            }}
        } else {
            // We have to move the profiler into the async block, because borrowing it would
            // restrict the lifetime of the block. So we use an outer `move` block to
            // capture `__profiler`, and an inner non-move block to match the behavior
            // of the original non-move block.
            quote::quote! {{
                #start_profiler
                let __profiler_scope = profiler::internal::Started(__profiler);
                let inner = async #block;
                async move {
                    profiler::internal::Profiler::resume(__profiler);
                    let result = inner.await;
                    std::mem::drop(__profiler_scope);
                    result
                }
            }}
        };
        syn::parse2(wrapped).unwrap()
    }

    /// Produce an instrumentor suitable for instrumenting blocks nested inside this block.
    fn inner_instrumentor(&self) -> Self {
        let level = self.level.clone();
        let func = self.func.clone();
        let origin = AsyncBlockOrigin::Block;
        Self { level, func, origin }
    }
}

/// Distinguishes between an async block that was originally the body of an `async fn`, versus an
/// async block that was originated as an async block in the source.
enum AsyncBlockOrigin {
    FnBody,
    Block,
}
