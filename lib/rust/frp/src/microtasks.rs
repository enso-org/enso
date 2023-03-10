//! This module contains implementation of microtask scheduler that is used in implementation of
//! some FRP nodes.

use crate::prelude::*;
use enso_callback::traits::*;

use enso_callback as callback;
use enso_generics::Cons;
use enso_generics::Nil;
use enso_generics::PushBack;
use enso_web::traits::WindowOps;
use enso_web::Closure;
use enso_web::JsEventHandler;
use enso_web::JsValue;
use enso_web::Promise;



/// =================
/// === Constants ===
/// =================

/// Maximum number of times a new microtask can be scheduled from within previously scheduled
/// microtask handler. A safety mechanism to prevent infinite loops. See [`next_microtask`] for more
/// information.
const MAX_RESURSIVE_MICROTASKS: usize = 1000;



/// =================
/// === next_microtask ===
/// =================

/// Schedules a callback for evaluation during next microtask. This is useful for scheduling tasks
/// that should be performed right after current JS event loop iteration, but before animation
/// frame. All tasks scheduled with this function will be performed in the same order as they were
/// scheduled. Further microtasks are scheduled in the same render loop as long as `next_microtask`
/// is recursively called within the scheduled task, up to a safety limit defined by
/// [`MAX_RESURSIVE_MICROTASKS`]. If the limit is reached, an error will be raised and further tasks
/// will be scheduled after next animation frame.
pub fn next_microtask(f: impl FnOnce() + 'static) -> callback::Handle {
    SCHEDULER.with(|scheduler| scheduler.add(f))
}

/// Schedules a callback for evaluation once all microtasks scheduled with `next_microtask` have
/// been performed. If that callback calls `next_microtask` or `next_microtask_late` again, those
/// tasks will be scheduled after all tasks scheduled so far, but before control is returned to the
/// environment (e.g. before next paint).
#[allow(dead_code)]
pub fn next_microtask_late(f: impl FnOnce() + 'static) -> callback::Handle {
    SCHEDULER.with(|scheduler| scheduler.add_late(f))
}

/// Flushes the microtask queue. It is guaranteed that both standard and late microtask queues
/// of this scheduler will be empty after this function returns. This is mainly useful for
/// testing.
///
/// NOTE: This function performs all microtasks scheduled by the main scheduler synchronously. It
/// preserves their relative order of execution. If there are other microtasks scheduled by the
/// environment outside of this scheduler (e.g. `promise.then` calls), they will not be executed
/// within this function.
pub fn flush_microtasks() {
    SCHEDULER.with(|scheduler| scheduler.flush())
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
            let run_all_closure = Closure::new(f!([weak] (_: JsValue) {
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
                run_all_closure,
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

    fn flush(&self) {
        self.data.flush();
    }
}

struct SchedulerData {
    is_scheduled:     Cell<bool>,
    callbacks:        callback::registry::NoArgsOnce,
    late_callbacks:   callback::registry::NoArgsOnce,
    schedule_depth:   Cell<usize>,
    resolved_promise: Promise,
    run_all_closure:  JsEventHandler,
    schedule_closure: JsEventHandler<f64>,
}

impl SchedulerData {
    fn schedule_task(&self) {
        if !self.is_scheduled.replace(true) {
            // Result left unused on purpose. We only care about `closure` being run in the next
            // microtask, which is a guaranteed side effect of providing it to [`Promise::then`]
            // method on already resolved promise.
            let _ = self.resolved_promise.then(&self.run_all_closure);
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

    fn flush(&self) {
        while !self.callbacks.is_empty() || !self.late_callbacks.is_empty() {
            self.callbacks.run_all();
            if self.callbacks.is_empty() {
                self.late_callbacks.run_all();
            }
        }
    }
}

// ==================
// === TickPhases ===
// ==================

/// A type that represents a list of phases that should be performed within the same browser task,
/// one after another. The phases are always performed in the same order as they were added,
/// allowing the microtask queue to be emptied between each phase. Each phase is queued using
/// `next_microtask_late` at the end of the previous phase.
#[must_use]
#[allow(missing_debug_implementations)]
pub struct TickPhases<T> {
    handle_cell: Rc<Cell<callback::Handle>>,
    phase_list:  T,
}


impl TickPhases<Nil> {
    /// Create a new empty phases list.
    pub fn new(handle_cell: Rc<Cell<callback::Handle>>) -> Self {
        TickPhases { handle_cell, phase_list: Nil }
    }
}

impl<T> TickPhases<T> {
    /// Add a phase to the list. The phase will be performed after all the phases that were added
    /// before.
    pub fn then<F>(self, f: F) -> TickPhases<T::Output>
    where T: PushBack<F> {
        let handle_cell = self.handle_cell;
        let phase_list = self.phase_list.push_back(f);
        TickPhases { handle_cell, phase_list }
    }

    /// Schedule the phases for execution. The phases will be performed asynchronously in order,
    /// once the current microtask queue is emptied.
    pub fn run(self)
    where Self: TaskPhasesRunImpl {
        TaskPhasesRunImpl::run(self);
    }
}

/// Implementation of [`RunnablePhases`] run method for a list of phases. This needs to be a trait,
/// so there is a way to specialize the implementation for different variants.
pub trait TaskPhasesRunImpl {
    /// Schedule the phases for execution. The phases will be performed asynchronously in order,
    /// once the current microtask queue is emptied.
    fn run(self);
}

impl TaskPhasesRunImpl for TickPhases<Nil> {
    fn run(self) {}
}

impl<H, T> TaskPhasesRunImpl for TickPhases<Cons<H, T>>
where
    TickPhases<T>: TaskPhasesRunImpl + 'static,
    H: FnOnce() + 'static,
{
    fn run(self) {
        let Cons(head, tail) = self.phase_list;
        let tail_phases = TickPhases { handle_cell: self.handle_cell.clone(), phase_list: tail };
        let handle = next_microtask_late(move || {
            head();
            tail_phases.run();
        });
        self.handle_cell.set(handle);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
