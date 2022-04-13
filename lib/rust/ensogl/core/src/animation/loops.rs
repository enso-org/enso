//! This module contains implementation of loops mainly used for per-frame callbacks firing.

use crate::prelude::*;
use crate::system::web::traits::*;

use crate::system::web;
use crate::types::unit2::Duration;

use web::Closure;



// ================
// === TimeInfo ===
// ================

/// Note: the `start` field will be computed on first run. We cannot compute it upfront, as other
/// time functions, like `performance.now()` can output not precise results. The exact results
/// differ across browsers and browser versions. We have even observed that `performance.now()` can
/// sometimes provide a bigger value than time provided to `requestAnimationFrame` callback later,
/// which resulted in a negative frame time.
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
}



// ===============
// === RawLoop ===
// ===============

// === Types ===

/// Callback for `RawLoop`.
pub trait RawLoopCallback = FnMut(Duration) + 'static;


// === Definition ===

/// The most performant animation loop possible. However, if you are looking for a way to define
/// an animation loop, you are probably looking for the `Loop` which adds slight complexity
/// in order to provide better time information. The complexity is so small that it would not be
/// noticeable in almost any use case.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct RawLoop<Callback> {
    data: Rc<RefCell<RawLoopData<Callback>>>,
}

impl<Callback> RawLoop<Callback>
where Callback: RawLoopCallback
{
    /// Create and start a new animation loop.
    pub fn new(callback: Callback) -> Self {
        let data = Rc::new(RefCell::new(RawLoopData::new(callback)));
        let weak_data = Rc::downgrade(&data);
        let on_frame = move |time: f64| weak_data.upgrade().for_each(|t| t.borrow_mut().run(time));
        data.borrow_mut().on_frame = Some(Closure::new(on_frame));
        let handle_id = web::window.request_animation_frame_with_closure_or_panic(
            data.borrow_mut().on_frame.as_ref().unwrap(),
        );
        data.borrow_mut().handle_id = handle_id;
        Self { data }
    }
}

/// The internal state of the `RawLoop`.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct RawLoopData<Callback> {
    #[derivative(Debug = "ignore")]
    callback:  Callback,
    on_frame:  Option<Closure<dyn FnMut(f64)>>,
    handle_id: i32,
}

impl<Callback> RawLoopData<Callback> {
    /// Constructor.
    fn new(callback: Callback) -> Self {
        let on_frame = default();
        let handle_id = default();
        Self { callback, on_frame, handle_id }
    }

    /// Run the animation frame. The time will be converted to [`f32`] because of performance
    /// reasons. See https://hugotunius.se/2017/12/04/rust-f64-vs-f32.html to learn more.
    fn run(&mut self, current_time_ms: f64)
    where Callback: FnMut(Duration) {
        let callback = &mut self.callback;
        self.handle_id = self.on_frame.as_ref().map_or(default(), |on_frame| {
            callback((current_time_ms as f32).ms());
            web::window.request_animation_frame_with_closure_or_panic(on_frame)
        })
    }
}

impl<Callback> Drop for RawLoopData<Callback> {
    fn drop(&mut self) {
        web::window.cancel_animation_frame_or_panic(self.handle_id);
    }
}



// ============
// === Loop ===
// ============

// === Types ===

pub trait LoopCallback = FnMut(TimeInfo) + 'static;
pub trait OnTooManyFramesSkippedCallback = FnMut() + 'static;


// === Definition ===

/// An animation loop. Runs the provided `Callback` every animation frame. It uses the
/// `RawLoop` under the hood. If you are looking for a more complex version where you can
/// register new callbacks for every frame, take a look at the ``.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct Loop<Callback> {
    animation_loop: RawLoop<OnFrame<Callback>>,
    time_info:      Rc<Cell<TimeInfo>>,
}

impl<Callback> Loop<Callback>
where Callback: LoopCallback
{
    /// Constructor.
    pub fn new(callback: Callback) -> Self {
        let time_info = Rc::new(Cell::new(TimeInfo::new()));
        let animation_loop = RawLoop::new(on_frame(callback, time_info.clone_ref()));
        Self { animation_loop, time_info }
    }
}

/// Callback for an animation frame.
pub type OnFrame<Callback> = impl FnMut(Duration);
fn on_frame<Callback>(
    mut callback: Callback,
    time_info_ref: Rc<Cell<TimeInfo>>,
) -> OnFrame<Callback>
where
    Callback: LoopCallback,
{
    move |current_time: Duration| {
        let _profiler = profiler::start_debug!(profiler::APP_LIFETIME, "@on_frame");
        let prev_time = time_info_ref.get();
        let animation_loop_start =
            if prev_time.is_initialized() { prev_time.animation_loop_start } else { current_time };
        let since_animation_loop_started = current_time - animation_loop_start;
        let previous_frame = since_animation_loop_started - prev_time.since_animation_loop_started;
        let time_info =
            TimeInfo { animation_loop_start, previous_frame, since_animation_loop_started };
        time_info_ref.set(time_info);
        callback(time_info);
    }
}



// =============================
// === FixedFrameRateSampler ===
// =============================

/// A callback `FnMut(TimeInfo) -> FnMut(TimeInfo)` transformer. Calls the inner callback with a
/// constant frame rate. If too many frames were skipped, the [`on_too_many_frames_skipped`]
/// callback will be used instead.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[allow(missing_docs)]
pub struct FixedFrameRateSampler<Callback, OnTooManyFramesSkipped> {
    pub max_skipped_frames:     usize,
    frame_time:                 Duration,
    local_time:                 Duration,
    time_buffer:                Duration,
    #[derivative(Debug = "ignore")]
    callback:                   Callback,
    #[derivative(Debug = "ignore")]
    on_too_many_frames_skipped: OnTooManyFramesSkipped,
}

impl<Callback, OnTooManyFramesSkipped> FixedFrameRateSampler<Callback, OnTooManyFramesSkipped> {
    /// Constructor.
    pub fn new(
        frame_rate: f32,
        callback: Callback,
        on_too_many_frames_skipped: OnTooManyFramesSkipped,
    ) -> Self {
        let max_skipped_frames = 2;
        let frame_time = (1000.0 / frame_rate).ms();
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

impl<Callback: FnOnce<(TimeInfo,)>, OnTooManyFramesSkipped> FnOnce<(TimeInfo,)>
    for FixedFrameRateSampler<Callback, OnTooManyFramesSkipped>
{
    type Output = ();
    extern "rust-call" fn call_once(self, args: (TimeInfo,)) -> Self::Output {
        self.callback.call_once(args);
    }
}

impl<Callback, OnTooManyFramesSkipped> FnMut<(TimeInfo,)>
    for FixedFrameRateSampler<Callback, OnTooManyFramesSkipped>
where
    Callback: FnMut(TimeInfo),
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

/// Loop with a `FixedFrameRateSampler` attached.
pub type FixedFrameRateLoop<Callback, OnTooManyFramesSkipped> =
    Loop<FixedFrameRateSampler<Callback, OnTooManyFramesSkipped>>;

impl<Callback, OnTooManyFramesSkipped> FixedFrameRateLoop<Callback, OnTooManyFramesSkipped>
where
    Callback: LoopCallback,
    OnTooManyFramesSkipped: OnTooManyFramesSkippedCallback,
{
    /// Constructor.
    pub fn new_with_fixed_frame_rate(
        frame_rate: f32,
        callback: Callback,
        on_too_many_frames_skipped: OnTooManyFramesSkipped,
    ) -> Self {
        Self::new(FixedFrameRateSampler::new(frame_rate, callback, on_too_many_frames_skipped))
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
