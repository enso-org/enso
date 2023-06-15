//! This module contains implementation of task scheduler that is used in some FRP nodes. The
//! scheduler uses details of JavaScript event loop to schedule deferred tasks in a way that is
//! more useful than using `setTimeout`. That allow us to implement FRP nodes that require their
//! execution to be deferred after current function execution, but before the current animation
//! frame is painted.
//!
//! # Event loop tasks
//!
//! JavaScript event loop tasks (also known as macrotasks) are a way of organizing the order in
//! which certain events are handled within JavaScript code. When a JavaScript program runs, it
//! executes a series of tasks, such as receiving network data, handling `setTimeout` timers,
//! handling mouse and keyboard events or rendering. Those macrotasks are scheduled to the
//! Javascript event loop, and are executed sequentially on a single thread.
//!
//!
//! # Microtasks
//!
//! In some scenarios, it is useful to allow the currently executing program to complete, and then
//! execute additional logic before next macrotask begins. This is where microtasks come in.
//! Microtasks are a way of scheduling tasks that should be performed after the current task has
//! finished, but before the next macrotask in the event loop is executed. Microtasks are executed
//! in the order in which they were added to the microtask queue. Notably, the microtask queue
//! is guaranteed to be processed before the browser is allowed to perform a repaint, which means
//! that microtasks are a good way to schedule tasks that need to be performed before the next
//! animation frame, even if the currently executed task is a `requestAnimationFrame` handler.
//!
//! Examples of JavaScript microtasks include promise callbacks, process.nextTick(), and mutation
//! observers. These tasks are executed in the order in which they were added to the microtask
//! queue.
//!
//! event loop tasks: onClick event            compute styles  requestAnimationFrame   paint
//!                   ╠══════════╦═════╦═══╦═  ╚═════════════  ╠═══════════╦════════   ╚══════
//! microtasks:       ╙───┬──┬─ A╙─┬─ B╙─ C╙─                  ╙───┬───── A╙────────
//! scheduled microtasks: A  B     C                               A
//!
//! In this diagram, first microtask within the macrotask is representing the initial handler of
//! given task, as passed to `addEventListener`, `setTimeout` or `requestAnimationFrame`. The event
//! loop also contains native tasks that don't execute any JS code, e.g. paint.
//!
//!
//! Read about microtasks in more details here:
//! https://developer.mozilla.org/en-US/docs/Web/API/HTML_DOM_API/Microtask_guide
//!
//!
//! # Scheduler
//!
//! This module contains implementation of microtask scheduler, which allows us to schedule
//! microtasks from within our own code. The scheduler manages two tasks queues with different
//! priorities:
//! - [`next_microtask`] queue, which executes its tasks immediately after the current task has
//!  finished, but before returning control to event loop.
//! - [`next_microtask_late`] queue, which executes its tasks after all tasks scheduled with
//! `next_microtask` have been executed.
//!
//! When a new task is scheduled within a microtask handler, it will be handled within the same
//! event loop task. The late task queue will only start processing tasks after the normal
//! queue has been fully processed, including all tasks scheduled within other tasks. If late
//! tasks spawn another set of tasks, the whole process will be repeated, all within the same
//! event loop task.
//!
//! Since microtasks can themselves enqueue more microtasks, and the event loop continues processing
//! microtasks until the queue is empty, there's a real risk of getting the event loop endlessly
//! processing microtasks. In order to prevent completely blocking rendering, the scheduler has a
//! safety mechanism that prevents it from processing schedule queue more than
//! [`MAX_RECURSIVE_MICROTASKS`] times from within a single event loop task. If the limit is
//! reached, an error will be raised and further scheduled tasks will continue execution after the
//! next animation frame.
//!
//! # Rendering loop
//!
//! All of our render loop tasks are scheduled with [`next_microtask_late`]. That way rendering
//! logic is guaranteed to be executed after all other normal priority tasks has been performed,
//! including all tasks enqueued within those tasks. That means all FRP nodes that use the scheduler
//! (e.g. `debounce` or `batch`) will be guaranteed to be fully processed before the rendering logic
//! is executed.

use crate::prelude::*;
use enso_callback::traits::*;

use enso_callback as callback;
use enso_generics::Cons;
use enso_generics::Nil;
use enso_generics::PushLastField;
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
const MAX_RECURSIVE_MICROTASKS: usize = 1000;



/// ======================
/// === next_microtask ===
/// ======================

/// Schedules a callback for evaluation during next microtask. This is useful for scheduling tasks
/// that should be performed right after current JS event loop iteration, but before animation
/// frame. All tasks scheduled with this function will be performed in the same order as they were
/// scheduled. Further microtasks are scheduled in the same render loop as long as `next_microtask`
/// is recursively called within the scheduled task, up to a safety limit defined by
/// [`MAX_RECURSIVE_MICROTASKS`]. If the limit is reached, an error will be raised and further tasks
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
        let handle = self.data.callbacks.add(profile(f));
        self.data.schedule_task();
        handle
    }

    fn add_late(&self, f: impl FnOnce() + 'static) -> callback::Handle {
        let handle = self.data.late_callbacks.add(profile(f));
        self.data.schedule_task();
        handle
    }

    fn flush(&self) {
        self.data.flush();
    }
}

/// Add profiling to a function. The current profiler (if any) will be the parent, regardless of
/// when the scheduled task is actually executed.
fn profile(f: impl FnOnce() + 'static) -> impl FnOnce() + 'static {
    let profiler = profiler::create_debug!("<microtask>");
    move || {
        profiler.resume();
        f();
        profiler.pause();
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
    #[profile(Task)]
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

    #[profile(Task)]
    fn run_all(&self) {
        let current_count = self.schedule_depth.get();
        let task_limit_reached = current_count >= MAX_RECURSIVE_MICROTASKS;
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

/// A type that represents a list of late microtask handlers that should scheduled within the same
/// browser task, one after another. The phases are always performed in the same order as they were
/// added, allowing the microtask queue to be emptied between each phase. Each phase is queued using
/// `next_microtask_late` at the end of the previous phase, guaranteeing that any microtasks
/// scheduled within given phase will be executed before the next phase is started.
#[must_use]
#[allow(missing_debug_implementations)]
pub struct TickPhases<T> {
    handle_cell: Weak<Cell<callback::Handle>>,
    phase_list:  T,
}


impl TickPhases<Nil> {
    /// Create a new empty phases list.
    pub fn new(handle_cell: &Rc<Cell<callback::Handle>>) -> Self {
        TickPhases { handle_cell: handle_cell.downgrade(), phase_list: Nil }
    }
}

impl<T> TickPhases<T> {
    /// Add a phase to the list. The phase will be performed after all the phases that were added
    /// before.
    pub fn then<F>(self, f: F) -> TickPhases<T::Output>
    where T: PushLastField<F> {
        let handle_cell = self.handle_cell;
        let phase_list = self.phase_list.push_last_field(f);
        TickPhases { handle_cell, phase_list }
    }

    /// Schedule the phases for execution. The phases will be performed asynchronously in order,
    /// once the current microtask queue is emptied.
    pub fn schedule(self)
    where Self: TaskPhasesScheduleImpl {
        TaskPhasesScheduleImpl::schedule(self);
    }
}

/// Implementation of [`RunnablePhases`] run method for a list of phases. This needs to be a trait,
/// so there is a way to specialize the implementation for different variants.
pub trait TaskPhasesScheduleImpl {
    /// Schedule the phases for execution. The phases will be performed asynchronously in order,
    /// once the current microtask queue is emptied.
    fn schedule(self);
}

impl TaskPhasesScheduleImpl for TickPhases<Nil> {
    fn schedule(self) {}
}

impl<H, T> TaskPhasesScheduleImpl for TickPhases<Cons<H, T>>
where
    TickPhases<T>: TaskPhasesScheduleImpl + 'static,
    H: FnOnce() + 'static,
{
    fn schedule(self) {
        if let Some(handle_cell) = self.handle_cell.upgrade() {
            let Cons(head, tail) = self.phase_list;
            let tail_phases = TickPhases { handle_cell: self.handle_cell, phase_list: tail };
            let handle = next_microtask_late(move || {
                head();
                tail_phases.schedule();
            });
            handle_cell.set(handle);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Derivative, CloneRef, Debug, Default)]
    #[derivative(Clone(bound = ""))]
    struct Collector<T> {
        vec: Rc<RefCell<Vec<T>>>,
    }

    impl<T> Collector<T> {
        fn push(&self, value: T) {
            self.vec.borrow_mut().push(value);
        }

        fn flush(&self) -> Vec<T> {
            flush_microtasks();
            self.vec.take()
        }

        #[track_caller]
        fn flush_and_assert_ordered(&self, length: usize)
        where T: Ord + Debug {
            let data = self.flush();
            let is_sorted = data.windows(2).all(|w| w[0] <= w[1]);
            assert!(is_sorted, "Expected ordered, got {data:?}");
            assert_eq!(data.len(), length, "Expected length {length}, got {data:?}");
        }
    }

    #[test]
    fn scheduled_microtask_runs() {
        let collector = Collector::default();
        next_microtask(f!(collector.push(1))).forget();
        assert_eq!(collector.flush(), [1]);
    }
    #[test]
    fn scheduled_late_microtask_runs() {
        let collector = Collector::default();
        next_microtask_late(f!(collector.push(1))).forget();
        collector.flush_and_assert_ordered(1);
    }

    #[test]
    fn dropped_microtasks_do_not_run() {
        let collector = Collector::default();

        next_microtask(f!(collector.push(1))).forget();

        let _ = next_microtask(|| unreachable!());
        let _ = next_microtask_late(|| unreachable!());

        collector.flush_and_assert_ordered(1);
    }

    #[test]
    fn scheduled_microtasks_run_in_order() {
        let collector = Collector::default();
        next_microtask(f!(collector.push(1))).forget();
        next_microtask(f!(collector.push(2))).forget();
        next_microtask(f!(collector.push(3))).forget();
        collector.flush_and_assert_ordered(3);
    }

    #[test]
    fn nested_microtasks_order() {
        let collector = Collector::default();
        next_microtask(f!([collector] () {
            collector.push(1);
            next_microtask(f!(collector.push(4))).forget();
        }))
        .forget();
        next_microtask(f!([collector] () {
            collector.push(2);
            next_microtask(f!(collector.push(5))).forget();
        }))
        .forget();
        next_microtask(f!(collector.push(3))).forget();

        collector.flush_and_assert_ordered(5);
    }

    #[test]
    fn mixed_early_and_late_order() {
        let collector = Collector::default();
        next_microtask_late(f!(collector.push(5))).forget();
        next_microtask(f!(collector.push(1))).forget();
        next_microtask(f!(collector.push(2))).forget();
        next_microtask_late(f!(collector.push(6))).forget();
        next_microtask(f!(collector.push(3))).forget();
        next_microtask_late(f!(collector.push(7))).forget();
        next_microtask(f!(collector.push(4))).forget();
        collector.flush_and_assert_ordered(7);
    }

    #[test]
    fn mixed_early_and_late_nested_order() {
        let collector = Collector::default();

        next_microtask_late(f!([collector] () {
            collector.push(7);
            next_microtask(f!(collector.push(11))).forget();
        }))
        .forget();

        next_microtask(f!([collector] () {
            collector.push(1);
            next_microtask_late(f!(collector.push(10))).forget();
        }))
        .forget();

        next_microtask(f!([collector] () {
            collector.push(2);
            next_microtask(f!(collector.push(6))).forget();
        }))
        .forget();

        next_microtask(f!(collector.push(3))).forget();

        next_microtask_late(f!([collector] () {
            collector.push(8);
            next_microtask_late(f!(collector.push(12))).forget();
        }))
        .forget();

        next_microtask(f!(collector.push(4))).forget();
        next_microtask_late(f!(collector.push(9))).forget();
        next_microtask(f!(collector.push(5))).forget();

        collector.flush_and_assert_ordered(12);
    }

    #[test]
    fn simple_tick_phases_order() {
        let collector = Collector::default();
        let cell = default();
        TickPhases::new(&cell)
            .then(f!(collector.push(1)))
            .then(f!(collector.push(2)))
            .then(f!(collector.push(3)))
            .schedule();
        collector.flush_and_assert_ordered(3);
    }

    #[test]
    fn dropped_tick_phases() {
        let collector = Collector::default();
        let cell = default();
        TickPhases::new(&cell)
            .then(f!(collector.push(1)))
            .then(f!(collector.push(2)))
            .then(f!(collector.push(3)))
            .schedule();
        drop(cell);
        collector.flush_and_assert_ordered(0);
    }

    #[test]
    fn dropped_tick_phases_mid_execution() {
        let collector = Collector::default();
        let cell = default();
        TickPhases::new(&cell)
            .then(f!(collector.push(1)))
            .then(f!([collector] () {
                collector.push(2);
                drop(cell);
            }))
            .then(f!(collector.push(3)))
            .schedule();
        collector.flush_and_assert_ordered(2);
    }

    #[test]
    fn tick_phases_with_nested_tasks() {
        let collector = Collector::default();
        let cell = default();
        TickPhases::new(&cell)
            .then(f!([collector] () {
                collector.push(1);
                next_microtask(f!(collector.push(2))).forget();
                next_microtask_late(f!(collector.push(3))).forget();
            }))
            .then(f!(collector.push(4)))
            .then(f!([collector] () {
                collector.push(5);
                next_microtask(f!(collector.push(6))).forget();
            }))
            .schedule();
        collector.flush_and_assert_ordered(6);
    }
}
