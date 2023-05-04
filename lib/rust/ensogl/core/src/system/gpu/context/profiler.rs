use crate::prelude::*;

use crate::system::gpu::context::extension;
use crate::system::gpu::context::native::ContextWithExtensions;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlQuery;


#[derive(Debug)]
pub struct Profiler {
    data: Option<InitializedProfiler>,
}

#[derive(Debug)]
pub struct InitializedProfiler {
    ext:     extension::ExtDisjointTimerQueryWebgl2,
    context: ContextWithExtensions,
    frame:   RefCell<Vec<WebGlQuery>>,
}

impl Profiler {
    pub fn new(context: &ContextWithExtensions) -> Self {
        let data = context.extensions.ext_disjoint_timer_query_webgl2.as_ref().map(|ext| {
            let ext = ext.clone();
            let context = context.clone();
            let frame = default();
            InitializedProfiler { ext, context, frame }
        });
        Self { data }
    }
}

impl InitializedProfiler {
    pub fn check_results(&self) {
        console_log!(">>> {}", self.frame.borrow().len());
        self.frame.borrow_mut().retain(|query| {
            let available_enum = WebGl2RenderingContext::QUERY_RESULT_AVAILABLE;
            let disjoint_enum = self.ext.gpu_disjoint_ext;
            let available = self.context.get_query_parameter(query, available_enum);
            let disjoint = self.context.get_parameter(*disjoint_enum).unwrap();
            let available = available.as_bool().unwrap();
            let disjoint = disjoint.as_bool().unwrap();
            let ready = available && !disjoint;
            if ready {
                let result =
                    self.context.get_query_parameter(query, WebGl2RenderingContext::QUERY_RESULT);
                let result = result.as_f64().unwrap() as u64;
                // console_log!("result: {:?}", result);
                self.context.delete_query(Some(query));
                false
            } else {
                true
            }
        });
    }

    pub fn start_frame(&self) {
        self.check_results();
        self.frame.borrow_mut().push(self.context.create_query().unwrap());
        self.context
            .begin_query(*self.ext.time_elapsed_ext, &self.frame.borrow().last().as_ref().unwrap());
    }

    pub fn end_frame(&self) {
        self.context.end_query(*self.ext.time_elapsed_ext);
    }

    // fn measure_draw_calls<T>(&self, f: impl FnOnce() -> T) -> T {
    //     self.measure(&self.draw_calls, f)
    // }
}

impl Profiler {
    pub fn start_frame(&self) {
        if let Some(data) = &self.data {
            data.start_frame();
        }
    }

    pub fn end_frame(&self) {
        if let Some(data) = &self.data {
            data.end_frame();
        }
    }
}
