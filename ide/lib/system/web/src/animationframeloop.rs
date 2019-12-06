use super::request_animation_frame;

use std::rc::Rc;
use std::cell::RefCell;
use wasm_bindgen::prelude::Closure;


// ==========================
// === AnimationFrameData ===
// ==========================

struct AnimationFrameData {
    run : bool
}

pub struct AnimationFrameLoop {
    forget : bool,
    data   : Rc<RefCell<AnimationFrameData>>
}


// ==========================
// === AnimationFrameLoop ===
// ==========================

impl AnimationFrameLoop {
    pub fn new(mut func:Box<dyn FnMut()>) -> Self {
        let nop_func       = Box::new(|| ()) as Box<dyn FnMut()>;
        let nop_closure    = Closure::once(nop_func);
        let callback       = Rc::new(RefCell::new(nop_closure));
        let run            = true;
        let data           = Rc::new(RefCell::new(AnimationFrameData { run }));
        let callback_clone = callback.clone();
        let data_clone     = data.clone();

        *callback.borrow_mut() = Closure::wrap(Box::new(move || {
            if data_clone.borrow().run {
                func();
                let clb = &callback_clone.borrow();
                request_animation_frame(&clb).expect("Request Animation Frame");
            }
        }) as Box<dyn FnMut()>);
        request_animation_frame(&callback.borrow()).unwrap();

        let forget = false;
        AnimationFrameLoop{forget,data}
    }

    pub fn forget(mut self) {
        self.forget = true;
    }
}

impl Drop for AnimationFrameLoop {
    fn drop(&mut self) {
        if !self.forget {
            self.data.borrow_mut().run = false;
        }
    }
}
