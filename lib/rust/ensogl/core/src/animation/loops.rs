//! This module contains implementation of `DynamicLoop`, a loop manager which runs a
//! `DynamicLoopCallback` once per frame.

use crate::prelude::*;

use crate::control::callback;
use crate::system::web;

use wasm_bindgen::prelude::Closure;



// ================
// === TimeInfo ===
// ================

/// Note: the `start` field will be computed on first run. We cannot compute it upfront, as other
/// time functions, like `performance.now()` can output nor precise results. The exact results
/// differ across browsers and browser versions. We have even observed that `performance.now()` can
/// sometimes provide a bigger value than time provided to `requestAnimationFrame` callback later,
/// which resulted in a negative frame time.
#[derive(Clone, Copy, Debug, Default)]
pub struct TimeInfo {
    /// Start time of the animation loop.
    pub start: f32,
    /// The last frame time.
    pub frame: f32,
    /// The time which passed since the animation loop was started.
    pub local: f32,
}

impl TimeInfo {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}



// ===============
// === RawLoop ===
// ===============

// === Types ===

/// Callback for `RawLoop`.
pub trait RawLoopCallback = FnMut(f32) + 'static;


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
        let on_frame = move |time| weak_data.upgrade().for_each(|t| t.borrow_mut().run(time));
        data.borrow_mut().on_frame = Some(Closure::new(on_frame));
        let handle_id = web::request_animation_frame(data.borrow_mut().on_frame.as_ref().unwrap());
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

    /// Run the animation frame.
    fn run(&mut self, current_time_ms: f64)
    where Callback: FnMut(f32) {
        let callback = &mut self.callback;
        self.handle_id = self.on_frame.as_ref().map_or(default(), |on_frame| {
            callback(current_time_ms as f32);
            web::request_animation_frame(on_frame)
        })
    }
}

impl<Callback> Drop for RawLoopData<Callback> {
    fn drop(&mut self) {
        web::cancel_animation_frame(self.handle_id);
    }
}



// ============
// === Loop ===
// ============

// === Types ===

pub trait LoopCallback = FnMut(TimeInfo) + 'static;


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
pub type OnFrame<Callback> = impl FnMut(f32);
fn on_frame<Callback>(
    mut callback: Callback,
    time_info_ref: Rc<Cell<TimeInfo>>,
) -> OnFrame<Callback>
where
    Callback: LoopCallback,
{
    move |current_time: f32| {
        let time_info = time_info_ref.get();
        let start = if time_info.start == 0.0 { current_time } else { time_info.start };
        let frame = current_time - start - time_info.local;
        let local = current_time - start;
        let time_info = TimeInfo { start, frame, local };
        time_info_ref.set(time_info);
        callback(time_info);
    }
}



// =============================
// === FixedFrameRateSampler ===
// =============================

/// A callback `FnMut(TimeInfo) -> FnMut(TimeInfo)` transformer. Calls the inner callback with a
/// constant frame rate.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct FixedFrameRateSampler<Callback> {
    frame_time:  f32,
    local_time:  f32,
    time_buffer: f32,
    #[derivative(Debug = "ignore")]
    callback:    Callback,
}

impl<Callback> FixedFrameRateSampler<Callback> {
    /// Constructor.
    pub fn new(frame_rate: f32, callback: Callback) -> Self {
        let frame_time = 1000.0 / frame_rate;
        let local_time = default();
        let time_buffer = default();
        Self { frame_time, local_time, time_buffer, callback }
    }
}

impl<Callback: FnOnce<(TimeInfo,)>> FnOnce<(TimeInfo,)> for FixedFrameRateSampler<Callback> {
    type Output = ();
    extern "rust-call" fn call_once(self, args: (TimeInfo,)) -> Self::Output {
        self.callback.call_once(args);
    }
}

impl<Callback: FnMut<(TimeInfo,)>> FnMut<(TimeInfo,)> for FixedFrameRateSampler<Callback> {
    extern "rust-call" fn call_mut(&mut self, args: (TimeInfo,)) -> Self::Output {
        let time = args.0;
        self.time_buffer += time.frame;
        loop {
            if self.time_buffer < 0.0 {
                break;
            } else {
                self.time_buffer -= self.frame_time;
                let start = time.start;
                let frame = self.frame_time;
                let local = self.local_time;
                let time2 = TimeInfo { start, frame, local };
                self.local_time += self.frame_time;
                self.callback.call_mut((time2,));
            }
        }
    }
}



// ==========================
// === FixedFrameRateLoop ===
// ==========================

/// Loop with a `FixedFrameRateSampler` attached.
pub type FixedFrameRateLoop<Callback> = Loop<FixedFrameRateSampler<Callback>>;

impl<Callback> FixedFrameRateLoop<Callback>
where Callback: LoopCallback
{
    /// Constructor.
    pub fn new_with_fixed_frame_rate(frame_rate: f32, callback: Callback) -> Self {
        Self::new(FixedFrameRateSampler::new(frame_rate, callback))
    }
}



// ===================
// === DynamicLoop ===
// ===================

/// A callback to register in DynamicLoop, taking time_ms:f32 as its input.
pub trait DynamicLoopCallback = callback::CopyCallbackMut1Fn<TimeInfo>;

/// Animation loop which allows registering and unregistering callbacks dynamically. After a
/// callback is registered, a `callback::Handle` is returned. The callback is automatically removed
/// as soon as its handle is dropped. You can also use the `forget` method on the handle to make the
/// callback registered forever, but beware that it can easily lead to memory leaks.
///
/// Please refer to `Loop` if you don't need the ability to add / remove callbacks dynamically and
/// you want better performance.
#[derive(Clone, CloneRef, Debug)]
pub struct DynamicLoop {
    raw_loop: Loop<Box<dyn FnMut(TimeInfo)>>,
    data:     Rc<RefCell<DynamicLoopData>>,
}

/// Internal representation for `DynamicLoop`.
#[derive(Debug, Default)]
pub struct DynamicLoopData {
    on_frame:        callback::CopyRegistry1<TimeInfo>,
    on_before_frame: callback::CopyRegistry1<TimeInfo>,
    on_after_frame:  callback::CopyRegistry1<TimeInfo>,
}

impl Default for DynamicLoop {
    fn default() -> Self {
        let data = Rc::new(RefCell::new(DynamicLoopData::default()));
        let weak = Rc::downgrade(&data);
        let raw_loop: Loop<Box<dyn FnMut(TimeInfo)>> = Loop::new(Box::new(move |time| {
            weak.upgrade().for_each(|data| {
                let mut data_mut = data.borrow_mut();
                data_mut.on_before_frame.run_all(time);
                data_mut.on_frame.run_all(time);
                data_mut.on_after_frame.run_all(time);
            })
        }));
        Self { raw_loop, data }
    }
}

impl DynamicLoop {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Add new callback which will be run on every animation frame.
    pub fn on_frame<F: DynamicLoopCallback>(&self, callback: F) -> callback::Handle {
        self.data.borrow_mut().on_frame.add(Box::new(callback))
    }

    /// Add new callback which will be run on before all callbacks registered with `on_frame`.
    pub fn on_before_frame<F: DynamicLoopCallback>(&self, callback: F) -> callback::Handle {
        self.data.borrow_mut().on_before_frame.add(Box::new(callback))
    }

    /// Add new callback which will be run on after all callbacks registered with `on_frame`.
    pub fn on_after_frame<F: DynamicLoopCallback>(&self, callback: F) -> callback::Handle {
        self.data.borrow_mut().on_after_frame.add(Box::new(callback))
    }
}
