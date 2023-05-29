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

/// Type of the animation function that will be called on every frame.
pub trait AnimationCallback = FnMut(FixedFrameRateStep<TimeInfo>) + 'static;

/// Type of a function that will be called on every frame before animation callbacks are run.
pub trait BeforeAnimationCallback = FnMut(TimeInfo) + 'static;


crate::define_endpoints_2! {
    Output {
        on_frame_start(Duration),
        on_before_animations(TimeInfo),
        on_after_animations(TimeInfo),
        on_before_layout(TimeInfo),
        on_before_rendering(TimeInfo),
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

/// Fires before the animations are evaluated.
pub fn on_before_animations() -> enso_frp::Sampler<TimeInfo> {
    LOOP_REGISTRY.with(|registry| registry.on_before_animations.clone_ref())
}

/// Fires after the animations are evaluated.
pub fn on_after_animations() -> enso_frp::Sampler<TimeInfo> {
    LOOP_REGISTRY.with(|registry| registry.on_after_animations.clone_ref())
}

/// Fires before the layout is performed.
pub fn on_before_layout() -> enso_frp::Sampler<TimeInfo> {
    LOOP_REGISTRY.with(|registry| registry.on_before_layout.clone_ref())
}

/// Fires before the rendering is performed.
pub fn on_before_rendering() -> enso_frp::Sampler<TimeInfo> {
    LOOP_REGISTRY.with(|registry| registry.on_before_rendering.clone_ref())
}

/// A wrapper for JavaScript `requestAnimationFrame` (RAF) loop. It allows registering callbacks and
/// also exposes FRP endpoints that will emit signals on every loop iteration.
///
///
/// # Warning, the following information is valid in Chrome only.
/// Handling of events, animation frames, and scheduling tasks is an implementation detail of every
/// browser. We did an in-depth analysis of how Chrome handles these things. Other browsers'
/// behavior might differ. This should not affect the correctness of either application behavior nor
/// performance measurements, but it is important to understand if you are working on
/// performance-related issues.
///
///
/// # Events scheduling in Chrome
/// The following graph shows an example order of tasks per frame with the assumptions that all the
/// frame tasks can be executed within that frame and the application runs with native FPS.
///
/// ```text
///             ║    Frame 1     ║    Frame 2     ║    Frame 3     ║ ...
/// Idle time   ║█            ███║██         █████║████           █║ ...
/// Other tasks ║ █          █   ║  █       █     ║    █        █  ║ ...
/// I/O Events  ║  ██   █████    ║      ████      ║     █   ████   ║ ...
/// RAF         ║    ███         ║   ███          ║      ███       ║ ...
/// GPU         ║      ████      ║     ███        ║        ██████  ║ ...
/// ```
///
/// A few things should be noted here:
///
/// - **Frame start idle time.** At the beginning of almost every frame, there will be an idle time
///   of up to a few milliseconds (we've observed idle times between 0 ms and 4 ms). This time is
///   likely caused by frames being synchronized with the screen refresh rate, while the tasks are
///   performed by the Chrome task scheduler, which is not fired at the synchronization points. To
///   learn more, see [this link](https://bugs.chromium.org/p/chromium/issues/detail?id=607650). It
///   is not clear whether Chrome considers this a bug or if they will ever fix it.
///
/// - **Other tasks.** These are Chrome-internal tasks. For example, before every animation frame,
///   Chrome performs some preparation work, which in most cases is very short (we've observed times
///   between 20 μs and 40 μs). At the end of the frame, Chrome performs pre-paint, layout,
///   layerize, and commit jobs, which, in the case of EnsoGL applications (when no HTML elements
///   are on the stage), are also short, ranging between 0.1 ms and 0.5 ms.
///
/// - **I/O Events.** Events such as mouse down/up/move can be scheduled before or after the RAF
///   callback. Some of these events might originate in the previous frame, and their handling might
///   be scheduled for the next frame by Chrome.
///
/// - **RAF.** The RAF (Request Animation Frame) callback triggers WebGL per-frame functions, such
///   as animations, Display Object layout recomputation, WebGL buffer updates, and invoking WebGL
///   draw calls.
///
/// - **GPU.** The GPU-related work starts at the end of the RAF callback, as soon as the first
///   WebGL calls are issued. It is performed in parallel with the rest of the RAF callback, I/O
///   events, and other tasks performed by Chrome.
///
/// In case the total time of all the per-frame tasks is longer than the frame duration with the
/// native refresh rate, some frames might be skipped, and a very interesting time report behavior
/// can be observed. Let's consider the following events graph:
///
/// ```text
///             ║ Real frame 1 work  ┆          Real frame 2 work         ┆            ...
///             ║    Frame 1     ║   ┆Frame 2     ║ Frame 3 (skip) ║    Frame 4     ║  ...
/// Idle time   ║███             ║   ┆            ║                ║      ┆         ║  ...
/// Other tasks ║   █            ║  █┆            ║                ║     █┆         ║  ...
/// RAF         ║    ██████████  ║   ┆  ██████████████████████████████    ┆ ██████████ ...
/// I/O Events  ║              █████ ┆██          ║                ║  ██  ┆█        ║  ...
/// GPU         ║              ███   ┆            ║                ║     █┆         ║  ...
/// ```
///
/// A few things should be noted here:
///
/// - **Frame start idle time.** As previously mentioned, we can observe an idle time at the
///   beginning of the first frame. The Chrome task scheduler rendered the previous frame in time,
///   so there was no pressure to start the next tasks immediately. However, there is no idle time
///   present in the next frames as their computations took longer than the native frame duration.
///
/// - **Frame times.** Even if the real computation time can exceed the time of a few frames issued
///   with the native refresh rate, the frames are always scheduled with the native refresh rate
///   time. Thus, some of these frames might be skipped, like frame 3 in the example above.
///
/// - **RAF start time.** When the RAF callback is called, it is provided with the frame start time.
///   However, this time might be smaller than the time reported by performance.now(). For example,
///   in the graph above, the third call to the RAF callback will report the beginning time of frame
///   4, even though it was performed in the middle of frame 4. If we measured the time at the end
///   of the second RAF callback with performance.now(), this time would be bigger than the time
///   reported by the third RAF callback.
///
///
/// # It is impossible to measure correct CPU-time from within JavaScript.
/// In Chrome, it is possible to get precise results of the GPU time (see
/// [ExtDisjointTimerQueryWebgl2]). However, even if we measure the time of all I/O events and the
/// time of the RAF callback, we cannot measure the time of the other tasks performed by Chrome
/// because we cannot register an event at the end of the per-frame job. Thus, when measuring
/// performance within the application, the only reliable way is to base it on the time reported by
/// the RAF callback and the GPU time measurements.
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
        self.animations_callbacks.add(create_callback_wrapper(callback))
    }
}

fn create_callback_wrapper(mut callback: impl AnimationCallback) -> impl AnimationCallback {
    let mut loop_start_time: Option<Duration> = None;
    move |time: FixedFrameRateStep<TimeInfo>| {
        let local_time = time.map(|mut time| {
            let start_time = loop_start_time.unwrap_or_else(|| {
                let start_time = time.since_animation_loop_started - time.previous_frame;
                loop_start_time = Some(start_time);
                start_time
            });
            time.since_animation_loop_started -= start_time;
            time
        });
        callback(local_time)
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
    let output = frp.private.output.clone_ref();
    let before_animations = before_animations.clone_ref();
    let animations = animations.clone_ref();
    let mut time_info = InitializedTimeInfo::default();
    let h_cell = Rc::new(Cell::new(callback::Handle::default()));
    let fixed_fps_sampler = Rc::new(RefCell::new(FixedFrameRateSampler::default()));

    move |frame_time: Duration| {
        let _profiler = profiler::start_debug!(profiler::APP_LIFETIME, "@on_frame");
        let time_info = time_info.next_frame(frame_time);
        let on_frame_start = output.on_frame_start.clone_ref();
        let on_before_animations = output.on_before_animations.clone_ref();
        let on_after_animations = output.on_after_animations.clone_ref();
        let on_before_layout = output.on_before_layout.clone_ref();
        let on_before_rendering = output.on_before_rendering.clone_ref();
        let before_animations = before_animations.clone_ref();
        let animations = animations.clone_ref();
        let fixed_fps_sampler = fixed_fps_sampler.clone_ref();

        TickPhases::new(&h_cell)
            .then(move || on_frame_start.emit(frame_time))
            .then(move || on_before_animations.emit(time_info))
            .then(move || before_animations.run_all(time_info))
            .then(move || fixed_fps_sampler.borrow_mut().run(time_info, |t| animations.run_all(t)))
            .then(move || on_after_animations.emit(time_info))
            .then(move || on_before_layout.emit(time_info))
            .then(move || on_before_rendering.emit(time_info))
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
    time:               Duration,
}

impl FixedFrameRateSampler {
    /// Constructor.
    pub fn new(desired_fps: f32) -> Self {
        let max_skipped_frames = 5;
        let desired_frame_time = (1000.0 / desired_fps).ms();
        let time = default();
        Self { desired_fps, desired_frame_time, max_skipped_frames, time }
    }

    fn run(&mut self, time: TimeInfo, f: impl Fn(FixedFrameRateStep<TimeInfo>)) {
        let time_diff = time.since_animation_loop_started - self.time;
        let frames_to_run = (time_diff / self.desired_frame_time).ceil() as usize;
        let time_step = time_diff / frames_to_run as f32;
        // warn!("frames_to_run: {frames_to_run:?}");
        let too_many_frames_skipped = frames_to_run > self.max_skipped_frames;
        if !too_many_frames_skipped {
            for _ in 0..frames_to_run {
                self.time += time_step;
                let local_time = TimeInfo {
                    animation_loop_start:         time.animation_loop_start,
                    previous_frame:               time_step,
                    since_animation_loop_started: self.time,
                };
                f(FixedFrameRateStep::Normal(local_time));
            }
        } else {
            debug!("Skipping animations because of {frames_to_run} frame delay.");
            f(FixedFrameRateStep::TooManyFramesSkipped);
        }
        self.time = time.since_animation_loop_started;
    }
}

impl Default for FixedFrameRateSampler {
    fn default() -> Self {
        Self::new(60.0)
    }
}
