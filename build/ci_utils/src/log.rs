use crate::prelude::*;
use tracing_subscriber::prelude::*;

use crate::global;

use std::io;
use tracing::subscriber::Interest;
use tracing::Metadata;
use tracing::Subscriber;
use tracing_subscriber::filter::LevelFilter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::Layer;
use tracing_subscriber::Registry;



pub fn is_our_module_path(path: impl AsRef<str>) -> bool {
    ["ide_ci::", "enso"].into_iter().any(|prefix| path.as_ref().starts_with(prefix))
}

/// A layer that filters out all spans/events that are not in our module path.
#[derive(Clone, Copy, Debug, Display)]
pub struct GlobalFilteringLayer;

impl<S: Subscriber + Debug + for<'a> LookupSpan<'a>> Layer<S> for GlobalFilteringLayer {
    fn register_callsite(&self, metadata: &'static Metadata<'static>) -> Interest {
        if metadata.module_path().is_some_and(is_our_module_path) {
            Interest::always()
        } else {
            Interest::never()
        }
    }
}

/// Layer that prints logs to stderr.
///
/// It uses the `ENSO_BUILD_LOG` environment variable to determine the [log filtering](https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html#directives).
pub fn stderr_log_layer<S>() -> impl Layer<S> + Debug
where S: Subscriber + for<'a> LookupSpan<'a> + Debug + Send + Sync + 'static {
    let filter = tracing_subscriber::EnvFilter::builder()
        .with_env_var("ENSO_BUILD_LOG")
        .with_default_directive(LevelFilter::TRACE.into())
        .from_env_lossy();

    let progress_bar_writer = IndicatifWriter::new();
    tracing_subscriber::fmt::layer()
        .without_time()
        .with_ansi(false)
        .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
        .with_writer(progress_bar_writer)
        .with_filter(filter)
}

pub fn file_log_layer<S>(file: std::fs::File) -> impl Layer<S> + Debug
where S: Subscriber + for<'a> LookupSpan<'a> + Debug + Send + Sync + 'static {
    tracing_subscriber::fmt::layer()
        .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
        .with_thread_names(true)
        .with_writer(file)
}

/// Install global `tracing` subscriber that logs to stderr.
///
/// Should be called only once, otherwise it will fail.
///
/// When using this function in unit tests, the result should be ignored, to allow multiple tests
/// to be run in a single batch.
pub fn setup_logging() -> Result {
    let registry = Registry::default().with(GlobalFilteringLayer).with(stderr_log_layer());
    tracing::subscriber::set_global_default(registry)
        .context("Failed to set global default subscriber.")
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
struct IndicatifWriter;


// === Main `impl` ===

impl IndicatifWriter {
    pub fn new() -> Self {
        Self
    }
}


// === Trait `impl`s ===

impl io::Write for IndicatifWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        global::with_suspend_multi_progress_bar(|| io::stderr().write(buf))
    }

    fn flush(&mut self) -> io::Result<()> {
        global::with_suspend_multi_progress_bar(|| io::stderr().flush())
    }
}

impl tracing_subscriber::fmt::MakeWriter<'_> for IndicatifWriter {
    type Writer = Self;

    fn make_writer(&self) -> Self::Writer {
        self.clone()
    }
}
