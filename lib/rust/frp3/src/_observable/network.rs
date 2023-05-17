use crate::effect::EffectData;
use crate::runtime::with_runtime;
use crate::Effect;
use crate::Signal;
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

slotmap::new_key_type! { pub(crate) struct NetworkId; }

#[derive(Clone, Copy)]
pub struct Network {
    id:         NetworkId,
    next_label: &'static str,
}

pub fn create_network<T>(f: impl FnOnce(Network) -> T) -> T {
    with_runtime(|rt| {
        let id = rt.new_network();
        let cx = Network { id };
        f(cx)
    })
}

impl Network {
    pub fn signal<T: Any>(self, value: T) -> Signal<T> {
        let value = Rc::new(RefCell::new(value));
        with_runtime(|rt| {
            let id = rt.new_signal(self.id, value);
            Signal::new_with_id(id)
        })
    }

    pub fn effect<T>(self, f: impl Fn(Option<T>) -> T + 'static) -> Effect
    where
        T: 'static,
    {
        let effect = Rc::new(EffectData::new(f));
        with_runtime(|rt| {
            let id = rt.new_effect(self.id, effect);
            rt.run_effect(id);
            Effect::new_with_id(id)
        })
    }
}
