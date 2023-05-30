//! GPU profiler allowing measurements of various metrics, like draw call time or data upload
//! time.

use crate::prelude::*;

use crate::system::gpu::context::extension;
use crate::system::gpu::context::native::ContextWithExtensions;

use std::collections::VecDeque;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlQuery;



// ==============
// === Metric ===
// ==============

/// A metric, like the gpu draw call time or data upload time.
///
/// The metric contains a queue of queries and a queue of results. A query is created on every frame
/// and the results may be received with a few-frame delay. However, it is impossible to receive the
/// results in a different order than the order of the queries. For example, the following scenario
/// is impossible, where "Q2" means a query created on frame 2 and "R1" means a result of a query
/// created on frame 1:
///
/// ```text
///          |  draw calls  |  data upload  |  Results emitted from `start_frame`
/// frame 1  |  Q1          |  Q1           |  
/// frame 2  |  Q2, R1      |  Q2           |
/// frame 3  |  Q3          |  Q3, R1       |  R1
/// frame 4  |  Q4, R2, R3  |  Q4, R2, R3   |  R2, R3
/// frame 5  |  Q5          |  Q5           |
/// frame 6  |  Q6          |  Q6           |
/// frame 7  |  Q7, R4      |  Q7           |
/// frame 8  |  Q7, R5      |  Q7, R4       |  R4
/// ...
/// ```
#[derive(Debug, Default)]
struct Metric {
    queue:   VecDeque<WebGlQuery>,
    results: VecDeque<f64>,
}



// ================
// === Profiler ===
// ================

/// The profiler is used to measure the performance of the GPU. It is used to measure the time of
/// draw calls, data uploads, etc. To see what metrics are available, see the [`define_metrics!`]
/// macro.
///
/// # WARNING: WORKS IN CHROME DESKTOP ONLY
/// The profiler uses the [`WebGL2RenderingContext::EXT_disjoint_timer_query_webgl2`] extension,
/// which currently (2023) is only available in Chrome Desktop. It was removed from all browsers
/// (including Chrome) due to ["GLitch" exploit](https://www.vusec.net/projects/glitch). However,
/// as Site Isolation shipped in Chrome on Desktop, the extension was re-enabled there because the
/// only data that could be read was data that came from the same origin. To learn more,
/// see: https://bugs.chromium.org/p/chromium/issues/detail?id=1230926
#[derive(Debug)]
pub struct Profiler {
    data: Option<InitializedProfiler>,
}

#[derive(Debug)]
struct InitializedProfiler {
    ext:            extension::ExtDisjointTimerQueryWebgl2,
    context:        ContextWithExtensions,
    metrics:        RefCell<Metrics>,
    assertions:     Assertions,
    frame:          Cell<usize>,
    frame_reported: Cell<usize>,
}

#[derive(Debug, Default)]
struct Assertions {
    // The WebGL API does not allow nested measurements, so we are checking that it is not
    // happening by accident.
    during_measurement:     Cell<bool>,
    // We need to be sure that all measurements were fired per frame. Otherwise, we would not get
    // the correct results, as we are gathering results from all measurements and adding them
    // together.
    measurements_per_frame: Cell<usize>,
}

impl Profiler {
    /// Constructor.
    pub fn new(context: &ContextWithExtensions) -> Self {
        let data = context.extensions.ext_disjoint_timer_query_webgl2.as_ref().map(|ext| {
            let ext = ext.clone();
            let context = context.clone();
            let metrics = default();
            let assertions = default();
            let frame = default();
            let frame_reported = default();
            InitializedProfiler { ext, context, metrics, assertions, frame, frame_reported }
        });
        Self { data }
    }
}

impl InitializedProfiler {
    fn update_results_of(&self, target: &mut Metric) {
        while let Some(query) = target.queue.front() {
            let available_enum = WebGl2RenderingContext::QUERY_RESULT_AVAILABLE;
            let disjoint_enum = self.ext.gpu_disjoint_ext;
            let available = self.context.get_query_parameter(query, available_enum);
            let disjoint = self.context.get_parameter(*disjoint_enum).unwrap();
            let available = available.as_bool().unwrap();
            let disjoint = disjoint.as_bool().unwrap();
            let ready = available && !disjoint;
            if ready {
                let query_result_enum = WebGl2RenderingContext::QUERY_RESULT;
                let result = self.context.get_query_parameter(query, query_result_enum);
                let result = result.as_f64().unwrap() / 1_000_000.0;
                target.results.push_back(result);
                self.context.delete_query(Some(query));
                target.queue.pop_front();
            } else {
                break;
            }
        }
    }

    fn measure<T>(&self, target: &mut Metric, f: impl FnOnce() -> T) -> T {
        assert!(!self.assertions.during_measurement.get());
        self.assertions.during_measurement.set(true);
        self.assertions.measurements_per_frame.modify(|t| *t += 1);
        let query = self.context.create_query().unwrap();
        self.context.begin_query(*self.ext.time_elapsed_ext, &query);
        let result = f();
        self.context.end_query(*self.ext.time_elapsed_ext);
        target.queue.push_back(query);
        self.assertions.during_measurement.set(false);
        result
    }
}

impl Profiler {
    /// Function that should be called on every frame. Gathers results of queries from previous
    /// frames. Please note, that not all results may be available yet, or results from multiple
    /// previous frames may be returned at once.
    pub fn start_frame(&self) -> Option<Vec<Results>> {
        self.data.as_ref().map(|t| t.start_frame())
    }
}



// ======================
// === Define Metrics ===
// ======================

macro_rules! define_metrics {
    ($($name:ident),*) => {
        paste! {
            #[derive(Debug, Default)]
            struct Metrics {
                $(pub $name: Metric),*
            }

            /// Results of measurements for each defined metric.
            #[allow(missing_docs)]
            #[derive(Clone, Copy, Debug, Default)]
            pub struct Results {
                pub frame_offset: usize,
                pub total: f64,
                $(pub $name: f64),*
            }

            impl Profiler {
                $(
                    /// Measure the time of the given metric.
                    pub fn [<measure_ $name>]<T>(&self, f: impl FnOnce() -> T) -> T {
                        if let Some(data) = &self.data {
                            data.[<measure_ $name>](f)
                        } else {
                            f()
                        }
                    }
                )*
            }

            impl InitializedProfiler {
                $(
                    fn [<measure_ $name>]<T>(&self, f: impl FnOnce() -> T) -> T {
                        self.measure(&mut self.metrics.borrow_mut().$name, f)
                    }
                )*

                fn start_frame(&self) -> Vec<Results> {
                    #[allow(clippy::redundant_closure_call)]
                    let target_measurements_per_frame = 0 $(+ (|_| 1)(stringify!($name)) )*;
                    let measurements_per_frame = self.assertions.measurements_per_frame.take();
                    let results = if measurements_per_frame == 0 {
                        default()
                    } else if measurements_per_frame == target_measurements_per_frame {
                        let mut results = vec![];
                        let metrics = &mut *self.metrics.borrow_mut();
                        $(self.update_results_of(&mut metrics.$name);)*
                        $(let $name = &mut metrics.$name;)*
                        while true $(&& $name.results.len() > 0)* {
                            let frame_offset = self.frame.get() - self.frame_reported.get();
                            $(let $name = $name.results.pop_front().unwrap();)*
                            let total = 0.0 $(+ $name)*;
                            results.push(Results { frame_offset, total, $($name),* });
                            self.frame_reported.modify(|t| *t += 1);
                        }
                        results
                    } else {
                        error!("Expected {target_measurements_per_frame} metrics per frame, \
                            but got {measurements_per_frame}.");
                        default()
                    };
                    self.frame.modify(|t| *t += 1);
                    results
                }
            }
        }
    };
}

define_metrics! { data_upload, drawing }
