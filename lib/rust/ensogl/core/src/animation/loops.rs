//! This module contains implementation of loops mainly used for per-frame callbacks firing.

use crate::prelude::*;
use crate::system::web::traits::*;
use enso_callback::traits::*;

use crate::system::web;
use crate::types::unit2::Duration;

use enso_callback as callback;
use frp::microtasks::TickPhases;
use web::Closure;



// ================
// === TimeInfo ===
// ================

/// Time data of a given animation frame. Contains information about the animation loop start,
/// previous frame time, and the time elapsed since the animation loop start till now.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[allow(missing_docs)]
pub struct TimeInfo {
    pub animation_loop_start:         Duration,
    pub previous_frame:               Duration,
    pub since_animation_loop_started: Duration,
}

impl TimeInfo {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Creates a new [`TimeInfo`] for the next frame with the provided time. The frame time will
    /// be computed based on the current and the new frame time.
    pub fn new_frame(mut self, since_animation_loop_started: Duration) -> Self {
        self.previous_frame = since_animation_loop_started - self.since_animation_loop_started;
        self.since_animation_loop_started = since_animation_loop_started;
        self
    }

    /// Return the time this frame started, relative to the time origin.
    pub fn frame_start(&self) -> Duration {
        self.animation_loop_start + self.since_animation_loop_started
    }
}



// ==============
// === JsLoop ===
// ==============

// === Types ===

/// Callback for `JsLoop`.
pub trait RawOnFrameCallback = FnMut(Duration) + 'static;


// === Definition ===

/// Binding to JS `requestAnimationFrame` function. This is not exported publicly because it should
/// not be used directly as it does not give us control over the animation loop evaluation order.
/// Use [`Loop`] instead.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
struct JsLoop<OnFrame> {
    data: Rc<RefCell<JsLoopData<OnFrame>>>,
}

impl<OnFrame> JsLoop<OnFrame>
where OnFrame: RawOnFrameCallback
{
    /// Create and start a new animation loop.
    fn new(on_frame: OnFrame) -> Self {
        let data = Rc::new(RefCell::new(JsLoopData::new(on_frame)));
        let weak_data = Rc::downgrade(&data);
        let js_on_frame =
            move |time: f64| weak_data.upgrade().for_each(|t| t.borrow_mut().run(time));
        data.borrow_mut().js_on_frame = Some(Closure::new(js_on_frame));
        let js_on_frame_handle_id = web::window.request_animation_frame_with_closure_or_panic(
            data.borrow_mut().js_on_frame.as_ref().unwrap(),
        );
        data.borrow_mut().js_on_frame_handle_id = js_on_frame_handle_id;
        Self { data }
    }
}

/// The internal state of the `JsLoop`.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct JsLoopData<OnFrame> {
    #[derivative(Debug = "ignore")]
    on_frame:              OnFrame,
    js_on_frame:           Option<Closure<dyn FnMut(f64)>>,
    js_on_frame_handle_id: i32,
}

impl<OnFrame> JsLoopData<OnFrame> {
    /// Constructor.
    fn new(on_frame: OnFrame) -> Self {
        let js_on_frame = default();
        let js_on_frame_handle_id = default();
        Self { on_frame, js_on_frame, js_on_frame_handle_id }
    }

    // FIXME: We are converting `f64` to `f32` here which is a mistake. We should revert to `f64`
    //        for a better time precision.
    fn run(&mut self, current_time_ms: f64)
    where OnFrame: FnMut(Duration) {
        let on_frame = &mut self.on_frame;
        self.js_on_frame_handle_id = self.js_on_frame.as_ref().map_or(default(), |js_on_frame| {
            on_frame((current_time_ms as f32).ms());
            web::window.request_animation_frame_with_closure_or_panic(js_on_frame)
        })
    }
}

impl<OnFrame> Drop for JsLoopData<OnFrame> {
    fn drop(&mut self) {
        web::window.cancel_animation_frame_or_warn(self.js_on_frame_handle_id);
    }
}



// ============
// === Loop ===
// ============

/// Animation loop handler. This is the basic animation loop that you should use in most cases.
#[derive(Clone, Debug)]
pub struct Loop {
    handle: callback::Handle,
}

impl Loop {
    /// Constructor.
    pub fn new_before_animations(f: impl BeforeAnimationCallback) -> Self {
        let handle = LOOP_REGISTRY.with(|registry| registry.add_before_animation_callback(f));
        Self { handle }
    }

    /// Constructor.
    pub fn new_animation(f: impl AnimationCallback) -> Self {
        let handle = LOOP_REGISTRY.with(|registry| registry.add_animation_callback(f));
        Self { handle }
    }
}



// ====================
// === LoopRegistry ===
// ====================

// === Types ===

/// Type of the function that will be called on every animation frame.
pub trait AnimationCallback = FnMut(FixedFrameRateStep<TimeInfo>) + 'static;

pub trait BeforeAnimationCallback = FnMut(TimeInfo) + 'static;


crate::define_endpoints_2! {
    Output {
        on_frame_start(Duration),
        on_before_animations(TimeInfo),
        on_after_animations(TimeInfo),
        on_before_rendering(TimeInfo),
        frame_end(TimeInfo),
    }
}

// === Definition ===

thread_local! {
    static LOOP_REGISTRY: LoopRegistry = LoopRegistry::new();
}

/// Fires at the beginning of every animation frame.
pub fn on_frame_start() -> enso_frp::Sampler<Duration> {
    LOOP_REGISTRY.with(|registry| registry.on_frame_start.clone_ref())
}

/// Fires at the end of every animation frame.
pub fn frame_end() -> enso_frp::Sampler<TimeInfo> {
    LOOP_REGISTRY.with(|registry| registry.frame_end.clone_ref())
}

/// Fires before the animations are evaluated.
pub fn on_before_animations() -> enso_frp::Sampler<TimeInfo> {
    LOOP_REGISTRY.with(|registry| registry.on_before_animations.clone_ref())
}

/// Fires after the animations are evaluated.
pub fn on_after_animations() -> enso_frp::Sampler<TimeInfo> {
    LOOP_REGISTRY.with(|registry| registry.on_after_animations.clone_ref())
}

/// Fires before the rendering is performed.
pub fn on_before_rendering() -> enso_frp::Sampler<TimeInfo> {
    LOOP_REGISTRY.with(|registry| registry.on_before_rendering.clone_ref())
}

/// An animation loop. Runs the provided [`OnFrame`] callback on every animation frame.
#[derive(CloneRef, Derivative, Deref)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct LoopRegistry {
    #[deref]
    frp: Frp,
    before_animations_callbacks: callback::registry::Copy1<TimeInfo>,
    animations_callbacks: callback::registry::Copy1<FixedFrameRateStep<TimeInfo>>,
    animation_loop: JsLoop<OnFrameClosure>,
}

impl LoopRegistry {
    /// Constructor.
    fn new() -> Self {
        let frp = default();
        let before_animations_callbacks = default();
        let animations_callbacks = default();
        let animation_loop = JsLoop::new(on_frame_closure(
            &frp,
            &before_animations_callbacks,
            &animations_callbacks,
        ));
        Self { frp, before_animations_callbacks, animations_callbacks, animation_loop }
    }

    fn add_before_animation_callback(
        &self,
        callback: impl BeforeAnimationCallback,
    ) -> callback::Handle {
        self.before_animations_callbacks.add(callback)
    }

    fn add_animation_callback(&self, callback: impl AnimationCallback) -> callback::Handle {
        self.animations_callbacks.add(callback)
    }
}

#[derive(Default)]
struct InitializedTimeInfo {
    is_initialized: bool,
    time_info:      TimeInfo,
}

impl InitializedTimeInfo {
    fn next_frame(&mut self, current_time: Duration) -> TimeInfo {
        let prev_time = self.time_info;
        let prev_start = prev_time.animation_loop_start;
        let animation_loop_start = if self.is_initialized { prev_start } else { current_time };
        let since_animation_loop_started = current_time - animation_loop_start;
        let previous_frame = since_animation_loop_started - prev_time.since_animation_loop_started;
        let time = TimeInfo { animation_loop_start, previous_frame, since_animation_loop_started };
        self.time_info = time;
        self.is_initialized = true;
        time
    }
}


/// Callback for an animation frame.
pub type OnFrameClosure = impl FnMut(Duration);
fn on_frame_closure(
    frp: &Frp,
    before_animations: &callback::registry::Copy1<TimeInfo>,
    animations: &callback::registry::Copy1<FixedFrameRateStep<TimeInfo>>,
) -> OnFrameClosure {
    let before_animations = before_animations.clone_ref();
    let animations = animations.clone_ref();
    let output = frp.private.output.clone_ref();
    let mut time_info = InitializedTimeInfo::default();
    let h_cell = Rc::new(Cell::new(callback::Handle::default()));
    let fixed_fps_sampler = Rc::new(RefCell::new(FixedFrameRateSampler::default()));

    move |frame_time: Duration| {
        let time_info = time_info.next_frame(frame_time);
        let on_frame_start = output.on_frame_start.clone_ref();
        let on_before_animations = output.on_before_animations.clone_ref();
        let on_after_animations = output.on_after_animations.clone_ref();
        let frame_end = output.frame_end.clone_ref();
        let output = output.clone_ref();
        let before_animations = before_animations.clone_ref();
        let animations = animations.clone_ref();
        let _profiler = profiler::start_debug!(profiler::APP_LIFETIME, "@on_frame");
        let fixed_fps_sampler = fixed_fps_sampler.clone_ref();

        TickPhases::new(&h_cell)
            .then(move || on_frame_start.emit(frame_time))
            .then(move || on_before_animations.emit(time_info))
            .then(move || before_animations.run_all(time_info))
            .then(move || fixed_fps_sampler.borrow_mut().run(time_info, |t| animations.run_all(t)))
            .then(move || on_after_animations.emit(time_info))
            .then(move || frame_end.emit(time_info))
            .then(move || {
                output.on_before_rendering.emit(time_info);
                drop(_profiler);
            })
            .schedule();
    }
}



// =============================
// === FixedFrameRateSampler ===
// =============================

/// A single [`FixedFrameRateSampler`] step. Either a normal frame or an indicator that too many
/// frames were skipped.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub enum FixedFrameRateStep<T> {
    Normal(T),
    TooManyFramesSkipped,
}

impl<T> FixedFrameRateStep<T> {
    /// Map the value inside the step.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> FixedFrameRateStep<U> {
        match self {
            Self::Normal(t) => FixedFrameRateStep::Normal(f(t)),
            Self::TooManyFramesSkipped => FixedFrameRateStep::TooManyFramesSkipped,
        }
    }
}

/// A frame rate sampler that ensures the animations look the same no matter the frame rate. In case
/// the frame rate is slower than expected, the sampler will call the provided function multiple
/// times. In case the skipped frame count is too big, the provided function will be called with the
/// [`FixedFrameRateStep::TooManyFramesSkipped`] argument indicating the animation engine to skip
/// the animation completely.
#[derive(Clone, Copy, Debug)]
pub struct FixedFrameRateSampler {
    desired_fps:        f32,
    max_skipped_frames: usize,
    desired_frame_time: Duration,
    time_buffer:        Duration,
}

impl FixedFrameRateSampler {
    /// Constructor.
    pub fn new(desired_fps: f32) -> Self {
        let max_skipped_frames = 5;
        let desired_frame_time = (1000.0 / desired_fps).ms();
        let time_buffer = default();
        Self { desired_fps, desired_frame_time, max_skipped_frames, time_buffer }
    }

    fn run(&mut self, time_info: TimeInfo, f: impl Fn(FixedFrameRateStep<TimeInfo>)) {
        self.time_buffer += time_info.previous_frame;
        let desired_frame_time_2 = self.desired_frame_time * 0.5;
        let time_diff = (self.time_buffer.as_ms() - desired_frame_time_2.as_ms()).max(0.0);
        let skipped_frames = (time_diff / self.desired_frame_time.as_ms()) as usize;
        let too_many_frames_skipped = skipped_frames > self.max_skipped_frames;
        if !too_many_frames_skipped {
            let mut local_time = time_info.since_animation_loop_started - self.time_buffer;
            for _ in 0..skipped_frames {
                local_time += self.desired_frame_time;
                self.time_buffer -= self.desired_frame_time;
                let animation_loop_start = time_info.animation_loop_start;
                let previous_frame = self.desired_frame_time;
                let since_animation_loop_started = local_time;
                let time2 =
                    TimeInfo { animation_loop_start, previous_frame, since_animation_loop_started };
                f(FixedFrameRateStep::Normal(time2));
            }
            let not_too_fast_refresh_rate = self.time_buffer >= -desired_frame_time_2;
            if not_too_fast_refresh_rate {
                self.time_buffer -= self.desired_frame_time;
                f(FixedFrameRateStep::Normal(time_info));
            }
        } else {
            debug!("Skipping animations because of {skipped_frames} frame delay.");
            self.time_buffer = 0.ms();
            f(FixedFrameRateStep::TooManyFramesSkipped);
        }
    }
}

impl Default for FixedFrameRateSampler {
    fn default() -> Self {
        Self::new(60.0)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::VecDeque;

    #[test]
    fn fixed_frame_rate_sampler_test() {
        let mut count_check = 0;
        let count = Rc::new(Cell::new(0));
        let too_many_frames_skipped_count = Rc::new(Cell::new(0));
        let frame_times = Rc::new(RefCell::new(VecDeque::new()));
        let mut lp = FixedFrameRateSampler::new(10.0);
        let mut time = TimeInfo {
            animation_loop_start:         0.ms(),
            previous_frame:               0.ms(),
            since_animation_loop_started: 0.ms(),
        };

        let callback = |t: FixedFrameRateStep<TimeInfo>| match t {
            FixedFrameRateStep::Normal(t) => {
                frame_times.borrow_mut().push_back(t);
                count.set(count.get() + 1);
            }
            FixedFrameRateStep::TooManyFramesSkipped => {
                too_many_frames_skipped_count.set(too_many_frames_skipped_count.get() + 1);
            }
        };

        let mut step = |frame_time: Duration,
                        sub_frames: &[Duration],
                        offset: Duration,
                        skipped_count: Option<usize>| {
            let time2 = time.new_frame(frame_time);
            lp.run(time2, callback);
            for sub_frame in sub_frames {
                count_check += 1;
                time = time.new_frame(*sub_frame);
                assert_eq!(frame_times.borrow_mut().pop_front(), Some(time));
            }
            time = time.new_frame(time2.since_animation_loop_started);
            if skipped_count.is_none() {
                count_check += 1;
                assert_eq!(frame_times.borrow_mut().pop_front(), Some(time));
            }
            assert_eq!(frame_times.borrow_mut().pop_front(), None);
            assert_eq!(count.get(), count_check);
            assert_eq!(lp.time_buffer, offset);
            if let Some(skipped_count) = skipped_count {
                assert_eq!(too_many_frames_skipped_count.get(), skipped_count);
            }
        };

        // Start frame.
        step(0.ms(), &[], 0.ms(), None);

        // // Perfectly timed next frame.
        // step(100.ms(), &[], 0.ms(), None);
        //
        // // Skipping 2 frames.
        // step(400.ms(), &[200.ms(), 300.ms()], 0.ms(), None);
        //
        // // Perfectly timed next frame.
        // step(500.ms(), &[], 0.ms(), None);
        //
        // // Next frame too slow.
        // step(640.ms(), &[], 40.ms(), None);
        //
        // // Next frame too slow.
        // step(800.ms(), &[740.ms()], 0.ms(), None);
        //
        // // Not-perfectly timed next frames.
        // step(870.ms(), &[], -30.ms(), None);
        // step(1010.ms(), &[], 10.ms(), None);
        // step(1090.ms(), &[], -10.ms(), None);
        // step(1200.ms(), &[], 0.ms(), None);
        //
        // // Next frames way too fast.
        // step(1210.ms(), &[], -90.ms(), None);
        // // Time compression – we don't want to accumulate too much of negative time buffer for
        // // monitors with bigger refresh-rate than assumed. The total accumulated time buffer
        // would // be -180 here, so we add a frame time to it (100).
        // step(1220.ms(), &[], -80.ms(), None);
        //
        // // Too many frames skipped.
        // step(2000.ms(), &[], 0.ms(), Some(1));
    }
}
