//! This module contains implementation of microtask scheduler that is used in implementation of
//! some FRP nodes.

use crate::prelude::*;
use enso_callback::traits::*;

use enso_callback as callback;
use enso_web::traits::WindowOps;
use enso_web::Closure;
use enso_web::JsEventHandler;
use enso_web::JsValue;
use enso_web::Promise;



/// =================
/// === Constants ===
/// =================

/// Maximum number of times a new microtask can be scheduled from within previously scheduled
/// microtask handler. A safety mechanism to prevent infinite loops. See [`next_tick`] for more
/// information.
const MAX_RESURSIVE_MICROTASKS: usize = 1000;



/// =================
/// === next_tick ===
/// =================

/// Schedules a callback for evaluation during next microtask. This is useful for scheduling tasks
/// that should be performed right after current JS event loop iteration, but before animation
/// frame. All tasks scheduled with this function will be performed in the same order as they were
/// scheduled. Further microtasks are scheduled in the same render loop as long as `next_tick`
/// is recursively called within the scheduled task, up to a safety limit defined by
/// [`MAX_RESURSIVE_MICROTASKS`]. If the limit is reached, an error will be raised and further tasks
/// will be scheduled after next animation frame.
pub fn next_tick(f: impl FnOnce() + 'static) -> callback::Handle {
    SCHEDULER.with(|schedule| schedule.add(f))
}

/// Schedules a callback for evaluation once all microtasks in `next_tick` have been performed. If
/// that callback calls `next_tick` or `next_tick_late` again, those tasks will be scheduled after
/// the tasks scheduled with this function, but before control is returned to the environment.
///
/// TODO[PG]: Use this to delay rendering logic until the microtask queue is empty.
/// TODO - add task number after review is done
#[allow(dead_code)]
pub fn next_tick_late(f: impl FnOnce() + 'static) -> callback::Handle {
    SCHEDULER.with(|schedule| schedule.add_late(f))
}



// =================
// === Scheduler ===
// =================

thread_local! {
    static SCHEDULER: Scheduler = Scheduler::new();
}

/// A microtask scheduler. It is used to schedule tasks that should be performed after current JS
/// event handler is done, but before handling the next one. All tasks scheduled with
/// [`Scheduler::add`], will be performed in the same order as they were scheduled. Tasks scheduled
/// with [`Scheduler::add_late`] will be performed in their schedule order once the standard task
/// queue is empty.
struct Scheduler {
    data: Rc<SchedulerData>,
}

impl Scheduler {
    fn new() -> Self {
        let data = Rc::new_cyclic(|weak: &Weak<SchedulerData>| {
            let resolved_promise = Promise::resolve(&JsValue::NULL);
            let callbacks = default();
            let late_callbacks = default();
            let schedule_depth = default();
            let is_scheduled = default();
            let closure = Closure::new(f!([weak] (_: JsValue) {
                if let Some(data) = weak.upgrade() {
                    data.run_all();
                }
            }));
            let schedule_closure = Closure::new(f!([weak] (_: f64) {
                if let Some(data) = weak.upgrade() {
                    data.schedule_task();
                }
            }));
            SchedulerData {
                is_scheduled,
                callbacks,
                late_callbacks,
                schedule_depth,
                resolved_promise,
                closure,
                schedule_closure,
            }
        });
        Self { data }
    }

    fn add(&self, f: impl FnOnce() + 'static) -> callback::Handle {
        let handle = self.data.callbacks.add(f);
        self.data.schedule_task();
        handle
    }

    fn add_late(&self, f: impl FnOnce() + 'static) -> callback::Handle {
        let handle = self.data.late_callbacks.add(f);
        self.data.schedule_task();
        handle
    }
}

struct SchedulerData {
    is_scheduled:     Cell<bool>,
    callbacks:        callback::registry::NoArgsOnce,
    late_callbacks:   callback::registry::NoArgsOnce,
    schedule_depth:   Cell<usize>,
    resolved_promise: Promise,
    closure:          JsEventHandler,
    schedule_closure: JsEventHandler<f64>,
}

impl SchedulerData {
    fn schedule_task(&self) {
        if !self.is_scheduled.replace(true) {
            // Result left unused on purpose. We only care about `closure` being run in the next
            // microtask, which is a guaranteed side effect of providing it to [`Promise::then`]
            // method on already resolved promise.
            let _ = self.resolved_promise.then(&self.closure);
        }
    }
    fn schedule_task_past_limit(&self) {
        if !self.is_scheduled.replace(true) {
            enso_web::window.request_animation_frame_with_closure_or_panic(&self.schedule_closure);
        }
    }

    fn run_all(&self) {
        let current_count = self.schedule_depth.get();
        let task_limit_reached = current_count >= MAX_RESURSIVE_MICROTASKS;
        if task_limit_reached {
            error!(
                "Too many microtasks scheduled. This is a safety limit to prevent infinite loops. \
                 Further tasks will be scheduled after next animation frame."
            );
            self.schedule_depth.set(0);
            self.is_scheduled.set(false);
            self.schedule_task_past_limit();
            return;
        }

        self.callbacks.run_all();

        if self.callbacks.is_empty() {
            self.late_callbacks.run_all();
        }

        self.is_scheduled.set(false);
        if self.callbacks.is_empty() && self.late_callbacks.is_empty() {
            self.schedule_depth.set(0);
        } else {
            self.schedule_depth.set(current_count + 1);
            self.schedule_task();
        }
    }
}
