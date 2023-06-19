use crate::prelude::*;
use tracing_subscriber::prelude::*;

use crate::global;

use std::io;
use std::sync::Once;
use tracing::span::Attributes;
use tracing::subscriber::Interest;
use tracing::Event;
use tracing::Id;
use tracing::Metadata;
use tracing::Subscriber;
use tracing_subscriber::filter::LevelFilter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::Registry;



pub fn is_our_module_path(path: impl AsRef<str>) -> bool {
    // true
    ["ide_ci::", "enso"].into_iter().any(|prefix| path.as_ref().starts_with(prefix))
}

#[derive(Clone, Copy, Debug, Display)]
pub struct MyLayer;

impl<S: Subscriber + Debug + for<'a> LookupSpan<'a>> tracing_subscriber::Layer<S> for MyLayer {
    fn register_callsite(&self, metadata: &'static Metadata<'static>) -> Interest {
        if metadata.module_path().is_some_and(is_our_module_path) {
            Interest::always()
        } else {
            // dbg!(metadata);
            Interest::never()
        }
    }

    fn on_new_span(
        &self,
        _attrs: &Attributes<'_>,
        _id: &Id,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        // let span = ctx.span(id).unwrap();
        // let bar = crate::global::new_spinner(format!("In span {id:?}: {:?}", span.name()));
        // span.extensions_mut().insert(bar);
        // crate::global::println(format!("Create {id:?}"));
    }
    fn on_event(&self, _event: &Event<'_>, _ctx: tracing_subscriber::layer::Context<'_, S>) {
        // tracing_log::dbg!(event);
    }
    fn on_enter(&self, _id: &Id, _ctx: tracing_subscriber::layer::Context<'_, S>) {
        // ide_ci::global::println(format!("Enter {id:?}"));
    }
    fn on_exit(&self, _id: &Id, _ctx: tracing_subscriber::layer::Context<'_, S>) {
        // ide_ci::global::println(format!("Leave {id:?}"));
    }

    fn on_close(&self, _id: Id, _ctx: tracing_subscriber::layer::Context<'_, S>) {
        // crate::global::println(format!("Close {id:?}"));
    }
}


pub fn setup_logging() -> Result {
    static GUARD: Once = Once::new();
    GUARD.call_once(|| {
        let filter = tracing_subscriber::EnvFilter::builder()
            .with_env_var("ENSO_BUILD_LOG")
            .with_default_directive(LevelFilter::TRACE.into())
            .from_env_lossy();

        let progress_bar = global::multi_progress_bar();
        let progress_bar_writer = IndicatifWriter::new(progress_bar);

        tracing::subscriber::set_global_default(
            Registry::default().with(MyLayer).with(
                tracing_subscriber::fmt::layer()
                    .without_time()
                    .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
                    .with_writer(progress_bar_writer)
                    .with_filter(filter),
            ),
        )
        .unwrap()
    });
    Ok(())
}



// =======================
// === IndicatifWriter ===
// =======================

/// A writer that writes to stderr, but suspends any active progress bars while doing so.
///
/// Progress bars use `stderr` to draw themselves. If we log to `stderr` while there is a progress
/// bar visible, the progress bar will be overwritten by the log message. This results in the
/// previous states of the progress bar remaining visible on the screen, which is not desirable.
///
/// To avoid this, the writer suspends the progress bars when writing logs.
#[derive(Clone, Debug)]
struct IndicatifWriter {
    progress_bar: indicatif::MultiProgress,
}


// === Main `impl` ===

impl IndicatifWriter {
    pub fn new(progress_bar: indicatif::MultiProgress) -> Self {
        Self { progress_bar }
    }
}


// === Trait `impl`s ===

impl std::io::Write for IndicatifWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.progress_bar.suspend(|| io::stderr().write(buf))
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.progress_bar.suspend(|| io::stderr().flush())
    }
}

impl tracing_subscriber::fmt::MakeWriter<'_> for IndicatifWriter {
    type Writer = Self;

    fn make_writer(&self) -> Self::Writer {
        self.clone()
    }
}
