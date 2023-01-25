//! Implementation of the [`#[profile]`] proc-macro.

use crate::wrap_async;

use quote::ToTokens;
use std::fmt;
use syn::visit_mut;
use syn::visit_mut::VisitMut;



/// The `#[profile]` proc-macro.
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
            wrap_async::wrap_async_fn(&mut func);
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

/// Decorate the input with file:line info determined by the proc_macro's call site.
fn make_label<L: fmt::Display>(name: L) -> String {
    let span = proc_macro::Span::call_site();
    let file = span.source_file().path();
    let path = file.as_path().to_string_lossy();
    let line = span.start().line;
    format!("{} ({}:{})", name, path, line)
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
