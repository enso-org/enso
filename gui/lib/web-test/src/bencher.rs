use crate::prelude::*;

use super::BenchContainer;
pub use crate::system::web::get_performance;
pub use crate::system::web::animation_frame_loop::AnimationFrameLoop;

use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use std::rc::Rc;
use std::cell::RefCell;



// ===================
// === BencherCell ===
// ===================

/// Cell, used to hold Bencher's data
pub struct BencherCell {
    func: Box<dyn FnMut()>,
    container  : BenchContainer,
    iterations : usize,
    total_time : f64,
    anim_loop  : Option<AnimationFrameLoop>
}

impl BencherCell {
    pub fn new(f:Box<dyn FnMut()>, container:BenchContainer) -> Self {
        let func       = f;
        let iterations = 0;
        let total_time = 0.0;
        let anim_loop  = None;
        Self { func,container,iterations,total_time,anim_loop }
    }

    /// Adds the duration of the next iteration and updates the UI.
    pub fn add_iteration_time(&mut self, time : f64) {
        self.iterations += 1;
        self.total_time += time;
        let iterations   = format!("{} iterations", self.iterations);
        let average      = self.total_time / self.iterations as f64;
        let display      = format!("{:.2}ms", average);

        self.container.iter.set_inner_html(&iterations);
        self.container.time.set_inner_html(&display);
    }
}



// ===================
// === BencherData ===
// ===================

#[derive(Shrinkwrap)]
pub struct BencherData {
    cell : RefCell<BencherCell>
}

impl BencherData {
    pub fn new(f:Box<dyn FnMut()>, container:BenchContainer) -> Rc<Self> {
        let cell = RefCell::new(BencherCell::new(f, container));
        Rc::new(Self { cell })
    }

    /// Starts the benchmarking loop.
    fn start(self:&Rc<Self>) {
        let data_clone = self.clone();
        let performance = get_performance().expect("Performance object");
        let mut t0 = performance.now();
        let anim_loop = AnimationFrameLoop::new(Box::new(move |_| {
            let mut data = data_clone.borrow_mut();

            (&mut data.func)();

            let t1 = performance.now();
            let dt = t1 - t0;
            t0     = t1;

            data.add_iteration_time(dt);
        }));
        self.borrow_mut().anim_loop = Some(anim_loop);
    }

    /// Stops the benchmarking loop.
    fn stop(self:&Rc<Self>) {
        self.borrow_mut().anim_loop = None;
    }

    /// Check if the loop is running.
    fn is_running(self:&Rc<Self>) -> bool {
        self.borrow().anim_loop.is_some()
    }
}



// ===============
// === Bencher ===
// ===============

/// The Bencher struct with an API compatible to Rust's test Bencher.
#[derive(Clone)]
pub struct Bencher {
    data : Rc<BencherData>
}

impl Bencher {
    /// Creates a Bencher with a html test container.
    pub fn new(container:BenchContainer) -> Self {
        let func = Box::new(|| ());
        let data = BencherData::new(func, container);

        let data_clone = data.clone();
        let closure = Box::new(move || {
            if data_clone.is_running() {
                data_clone.stop();
            } else {
                data_clone.start();
            }
        }) as Box<dyn FnMut()>;

        {
            let closure = Closure::wrap(closure);
            let cell = data.cell.borrow();
            let measurement = &cell.container.measurement;
            measurement.set_onclick(Some(closure.as_ref().unchecked_ref()));
            closure.forget();
        }

        Self { data }
    }

    pub fn is_running(&self) -> bool {
        self.data.is_running()
    }

    /// Callback for benchmark functions to run in their body.
    pub fn iter<T, F:FnMut() -> T + 'static>(&mut self, mut func:F) {
        self.data.borrow_mut().func = Box::new(move || { func(); });
    }
}
