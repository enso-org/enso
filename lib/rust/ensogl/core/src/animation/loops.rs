//! This module contains implementation of loops mainly used for per-frame callbacks firing.

use crate::prelude::*;
use crate::system::web::traits::*;

use crate::system::web;
use crate::types::unit2::Duration;

use web::Closure;



// ================
// === TimeInfo ===
// ================


/// Time data of a given animation frame. Contains information about the animation loop start,
/// previous frame time, and the time elapsed since the animation loop start till now.
///
/// Note: the [`animation_loop_start`] field will be computed on first loop iteration. We cannot
/// compute it upfront, as other time functions, like `performance.now()`, can output results that
/// can provide different values (both earlier or later than the time stamp provided to the
/// `requestAnimationFrame` callback. The exact results differ across browsers and browser versions.
/// We have even observed that `performance.now()` can sometimes provide a bigger value than time
/// provided to `requestAnimationFrame` callback later, which resulted in a negative frame time.
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

    /// Check whether the time info was initialized. See the documentation of the struct to learn
    /// more.
    pub fn is_initialized(&self) -> bool {
        self.animation_loop_start != 0.ms()
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



// ===============
// === RawLoop ===
// ===============

// === Types ===

/// Callback for `RawLoop`.
pub trait RawOnFrameCallback = FnMut(Duration) + 'static;


// === Definition ===

/// The most performant animation loop possible. However, if you are looking for a way to define
/// an animation loop, you are probably looking for the [`Loop`] which adds slight complexity
/// in order to provide better time information. The complexity is so small that it would not be
/// noticeable in almost any use case.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct RawLoop<OnFrame> {
    data: Rc<RefCell<RawLoopData<OnFrame>>>,
}

impl<OnFrame> RawLoop<OnFrame>
where OnFrame: RawOnFrameCallback
{
    /// Create and start a new animation loop.
    pub fn new(on_frame: OnFrame) -> Self {
        let data = Rc::new(RefCell::new(RawLoopData::new(on_frame)));
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

/// The internal state of the `RawLoop`.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct RawLoopData<OnFrame> {
    #[derivative(Debug = "ignore")]
    on_frame:              OnFrame,
    js_on_frame:           Option<Closure<dyn FnMut(f64)>>,
    js_on_frame_handle_id: i32,
}

impl<OnFrame> RawLoopData<OnFrame> {
    /// Constructor.
    fn new(on_frame: OnFrame) -> Self {
        let js_on_frame = default();
        let js_on_frame_handle_id = default();
        Self { on_frame, js_on_frame, js_on_frame_handle_id }
    }

    /// Run the animation frame. The time will be converted to [`f32`] because of performance
    /// reasons. See https://hugotunius.se/2017/12/04/rust-f64-vs-f32.html to learn more.
    fn run(&mut self, current_time_ms: f64)
    where OnFrame: FnMut(Duration) {
        let on_frame = &mut self.on_frame;
        self.js_on_frame_handle_id = self.js_on_frame.as_ref().map_or(default(), |js_on_frame| {
            on_frame((current_time_ms as f32).ms());
            web::window.request_animation_frame_with_closure_or_panic(js_on_frame)
        })
    }
}

impl<OnFrame> Drop for RawLoopData<OnFrame> {
    fn drop(&mut self) {
        web::window.cancel_animation_frame_or_panic(self.js_on_frame_handle_id);
    }
}



// ============
// === Loop ===
// ============

// === Types ===

/// Type of the function that will be called on every animation frame.
pub trait OnFrameCallback = FnMut(TimeInfo) + 'static;


// === Definition ===

/// An animation loop. Runs the provided [`OnFrame`] callback on every animation frame.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct Loop<OnFrame> {
    animation_loop: RawLoop<OnFrameClosure<OnFrame>>,
    time_info:      Rc<Cell<TimeInfo>>,
}

impl<OnFrame> Loop<OnFrame>
where OnFrame: OnFrameCallback
{
    /// Constructor.
    pub fn new(on_frame: OnFrame) -> Self {
        let time_info = Rc::new(Cell::new(TimeInfo::new()));
        let animation_loop = RawLoop::new(on_frame_closure(on_frame, time_info.clone_ref()));
        Self { animation_loop, time_info }
    }
}

/// Callback for an animation frame.
pub type OnFrameClosure<OnFrame> = impl FnMut(Duration);
fn on_frame_closure<OnFrame>(
    mut on_frame: OnFrame,
    time_info: Rc<Cell<TimeInfo>>,
) -> OnFrameClosure<OnFrame>
where
    OnFrame: OnFrameCallback,
{
    move |current_time: Duration| {
        let _profiler = profiler::start_debug!(profiler::APP_LIFETIME, "@on_frame");
        let prev_time = time_info.get();
        let is_initialized = prev_time.is_initialized();
        let prev_start = prev_time.animation_loop_start;
        let animation_loop_start = if is_initialized { prev_start } else { current_time };
        let since_animation_loop_started = current_time - animation_loop_start;
        let previous_frame = since_animation_loop_started - prev_time.since_animation_loop_started;
        let time = TimeInfo { animation_loop_start, previous_frame, since_animation_loop_started };
        time_info.set(time);
        on_frame(time);
    }
}



// =============================
// === FixedFrameRateSampler ===
// =============================

/// An animation loop transformer. Calls the provided [`OnFrame`] callback on every animation frame.
/// If the real animation frames are too long, it will emit virtual frames in between. In case the
/// delay is too long (more than [`max_skipped_frames`]), the [`OnTooManyFramesSkipped`] callback
/// will be used instead.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[allow(missing_docs)]
pub struct FixedFrameRateSampler<OnFrame, OnTooManyFramesSkipped> {
    pub max_skipped_frames:     usize,
    frame_time:                 Duration,
    local_time:                 Duration,
    time_buffer:                Duration,
    #[derivative(Debug = "ignore")]
    callback:                   OnFrame,
    #[derivative(Debug = "ignore")]
    on_too_many_frames_skipped: OnTooManyFramesSkipped,
}

impl<OnFrame, OnTooManyFramesSkipped> FixedFrameRateSampler<OnFrame, OnTooManyFramesSkipped> {
    /// Constructor.
    pub fn new(
        fps: f32,
        callback: OnFrame,
        on_too_many_frames_skipped: OnTooManyFramesSkipped,
    ) -> Self {
        let max_skipped_frames = 2;
        let frame_time = (1000.0 / fps).ms();
        let local_time = default();
        // The first call to this sampler will be with frame time 0, which would drop this
        // `time_buffer` to 0.
        let time_buffer = frame_time;
        Self {
            max_skipped_frames,
            frame_time,
            local_time,
            time_buffer,
            callback,
            on_too_many_frames_skipped,
        }
    }
}

impl<OnFrame: FnOnce<(TimeInfo,)>, OnTooManyFramesSkipped> FnOnce<(TimeInfo,)>
    for FixedFrameRateSampler<OnFrame, OnTooManyFramesSkipped>
{
    type Output = ();
    extern "rust-call" fn call_once(self, args: (TimeInfo,)) -> Self::Output {
        self.callback.call_once(args);
    }
}

impl<OnFrame, OnTooManyFramesSkipped> FnMut<(TimeInfo,)>
    for FixedFrameRateSampler<OnFrame, OnTooManyFramesSkipped>
where
    OnFrame: FnMut(TimeInfo),
    OnTooManyFramesSkipped: FnMut(),
{
    extern "rust-call" fn call_mut(&mut self, args: (TimeInfo,)) -> Self::Output {
        let mut time = args.0;
        self.time_buffer += time.since_animation_loop_started - self.local_time;

        let frame_time_2 = self.frame_time * 0.5;
        let skipped_frames = ((self.time_buffer - frame_time_2) / self.frame_time) as usize;
        let too_many_frames_skipped = skipped_frames > self.max_skipped_frames;
        if !too_many_frames_skipped {
            for _ in 0..skipped_frames {
                self.local_time += self.frame_time;
                self.time_buffer -= self.frame_time;
                let animation_loop_start = time.animation_loop_start;
                let previous_frame = self.frame_time;
                let since_animation_loop_started = self.local_time;
                let time2 =
                    TimeInfo { animation_loop_start, previous_frame, since_animation_loop_started };
                self.callback.call_mut((time2,));
            }
            let not_too_fast_refresh_rate = self.time_buffer >= -frame_time_2;
            if not_too_fast_refresh_rate {
                self.time_buffer -= self.frame_time;
            }
            time.previous_frame = time.since_animation_loop_started - self.local_time;
            self.local_time = time.since_animation_loop_started;
            (self.callback)(time);
        } else {
            self.local_time = time.since_animation_loop_started;
            self.time_buffer = 0.ms();
            (self.on_too_many_frames_skipped)();
        }
    }
}



// ==========================
// === FixedFrameRateLoop ===
// ==========================

/// Callback used if too many frames were skipped.
pub trait OnTooManyFramesSkippedCallback = FnMut() + 'static;

/// Loop with a `FixedFrameRateSampler` attached.
pub type FixedFrameRateLoop<OnFrame, OnTooManyFramesSkipped> =
    Loop<FixedFrameRateSampler<OnFrame, OnTooManyFramesSkipped>>;

impl<OnFrame: OnFrameCallback, OnTooManyFramesSkipped: OnTooManyFramesSkippedCallback>
    FixedFrameRateLoop<OnFrame, OnTooManyFramesSkipped>
{
    /// Constructor.
    pub fn new_with_fixed_frame_rate(
        fps: f32,
        on_frame: OnFrame,
        on_too_many_frames_skipped: OnTooManyFramesSkipped,
    ) -> Self {
        Self::new(FixedFrameRateSampler::new(fps, on_frame, on_too_many_frames_skipped))
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
        let mut lp = FixedFrameRateSampler::new(
            10.0,
            |t| {
                frame_times.borrow_mut().push_back(t);
                count.set(count.get() + 1);
            },
            || {
                too_many_frames_skipped_count.set(too_many_frames_skipped_count.get() + 1);
            },
        );
        let mut time = TimeInfo {
            animation_loop_start:         0.ms(),
            previous_frame:               0.ms(),
            since_animation_loop_started: 0.ms(),
        };

        let mut step = |frame_time: Duration,
                        sub_frames: &[Duration],
                        offset: Duration,
                        skipped_count: Option<usize>| {
            let time2 = time.new_frame(frame_time);
            lp(time2);
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

        // Perfectly timed next frame.
        step(100.ms(), &[], 0.ms(), None);

        // Skipping 2 frames.
        step(400.ms(), &[200.ms(), 300.ms()], 0.ms(), None);

        // Perfectly timed next frame.
        step(500.ms(), &[], 0.ms(), None);

        // Next frame too slow.
        step(640.ms(), &[], 40.ms(), None);

        // Next frame too slow.
        step(800.ms(), &[740.ms()], 0.ms(), None);

        // Not-perfectly timed next frames.
        step(870.ms(), &[], -30.ms(), None);
        step(1010.ms(), &[], 10.ms(), None);
        step(1090.ms(), &[], -10.ms(), None);
        step(1200.ms(), &[], 0.ms(), None);

        // Next frames way too fast.
        step(1210.ms(), &[], -90.ms(), None);
        // Time compression – we don't want to accumulate too much of negative time buffer for
        // monitors with bigger refresh-rate than assumed. The total accumulated time buffer would
        // be -180 here, so we add a frame time to it (100).
        step(1220.ms(), &[], -80.ms(), None);

        // Too many frames skipped.
        step(2000.ms(), &[], 0.ms(), Some(1));
    }
}
