//! Generation of the Profiling Low-level API and its implementation.

use crate::level;



// =====================================
// === Low-level profiling interface ===
// =====================================

/// Produces source code defining the Low-Level Profiling API for the given hierarchy of profiling
/// levels.
pub fn define_profiling_levels(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut out = proc_macro::TokenStream::new();
    let levels = level::parse_levels(crate::PROFILING_LEVEL_ENV_VAR, ts);
    for level in &levels {
        let profiler = Profiler::new(&level.name, level.obj_ident.clone());
        out.extend(if level.enabled {
            define_enabled_profiler(&profiler)
        } else {
            define_disabled_profiler(&profiler)
        });
    }
    for (i, parent) in levels.iter().enumerate() {
        for child in &levels[i..] {
            out.extend(impl_parent(&parent.obj_ident, &child.obj_ident, child.enabled));
        }
    }
    let ident = syn::Ident::new("ProfilingLevel", proc_macro2::Span::call_site());
    out.extend(level::make_enum(ident, levels.iter().map(|level| &level.obj_ident)));
    out
}

struct Profiler {
    ident:      syn::Ident,
    start:      syn::Ident,
    create:     syn::Ident,
    doc_obj:    String,
    doc_start:  String,
    doc_create: String,
}

impl Profiler {
    fn new(level: impl AsRef<str>, ident: syn::Ident) -> Self {
        let level = level.as_ref();
        let level_link = format!("[{level}-level](index.html#{level})");
        Self {
            ident,
            start: quote::format_ident!("start_{level}"),
            create: quote::format_ident!("create_{level}"),
            doc_obj: format!("Identifies a {level_link} profiler."),
            doc_start: format!("Start a new {level_link} profiler."),
            doc_create: format!("Create a new {level_link} profiler, in unstarted state."),
        }
    }
}

fn define_enabled_profiler(profiler: &Profiler) -> proc_macro::TokenStream {
    let Profiler { ident, start, create, doc_obj, doc_start, doc_create } = profiler;
    let profiling_level_variant = ident;
    (quote::quote! {
        // =================================
        // === Profiler (e.g. Objective) ===
        // =================================

        #[doc = #doc_obj]
        #[derive(Copy, Clone, Debug)]
        pub struct #ident(pub EventId);


        // === Trait Implementations ===

        impl Profiler for #ident {
            fn start(
                parent: EventId,
                label: Label,
                time: Option<Timestamp>,
                start: StartState,
            ) -> Self {
                let level = crate::ProfilingLevel::#profiling_level_variant;
                #ident(EventLog.start(parent, label, time, start, level))
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
                let profiler: profiler::internal::Started<profiler::#ident> =
                    $parent.start_child(label);
                profiler
            }}
        }

        #[doc = #doc_create]
        #[macro_export]
        macro_rules! #create {
            ($label: expr) => {{
                let label = profiler::internal::Label(
                    concat!($label, " (", file!(), ":", line!(), ")"));
                let parent = profiler::internal::EventId::implicit();
                let now = Some(profiler::internal::Timestamp::now());
                let paused = profiler::internal::StartState::Paused;
                profiler::#ident::start(parent, label, now, paused)
            }}
        }
    })
    .into()
}

fn define_disabled_profiler(profiler: &Profiler) -> proc_macro::TokenStream {
    let Profiler { ident, start, create, doc_obj, doc_start, doc_create, .. } = profiler;
    (quote::quote! {
        // =================================
        // === Profiler (e.g. Objective) ===
        // =================================

        #[doc = #doc_obj]
        #[derive(Copy, Clone, Debug)]
        pub struct #ident(pub ());


        // === Trait Implementations ===

        impl Profiler for #ident {
            fn start(
                _: EventId,
                _: Label,
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
            ($parent: expr, $label: expr) => {{
                let _unused_at_this_profiling_level = $parent;
                profiler::internal::Started(profiler::#ident(()))
            }}
        }

        #[doc = #doc_create]
        #[macro_export]
        macro_rules! #create {
            ($label: expr) => {{
                profiler::#ident(())
            }}
        }
    })
    .into()
}

/// Generates an implementation of the [`Parent`] trait relating the given parent and child.
fn impl_parent(
    parent_ident: &syn::Ident,
    child_ident: &syn::Ident,
    enabled: bool,
) -> proc_macro::TokenStream {
    let body = if enabled {
        quote::quote! {
            let start = Some(Timestamp::now());
            Started(#child_ident::start(self.0, label, start, StartState::Active))
        }
    } else {
        quote::quote! {
            Started(#child_ident(()))
        }
    };
    (quote::quote! {
        impl Parent<#child_ident> for #parent_ident {
            fn start_child(&self, label: Label) -> Started<#child_ident> {
                #body
            }
        }
    })
    .into()
}
