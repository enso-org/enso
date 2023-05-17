use std::cell::RefCell;

slotmap::new_key_type! { pub(crate) struct EffectId; }

pub struct Effect {
    id: EffectId,
}

impl Effect {
    pub(crate) fn new_with_id(id: EffectId) -> Self {
        Effect { id }
    }
}

pub(crate) struct EffectData<T, F> {
    f: F,
    value: RefCell<Option<T>>,
}

impl<T, F> EffectData<T, F>
where
    T: 'static,
    F: Fn(Option<T>) -> T,
{
    pub(crate) fn new(f: F) -> Self {
        EffectData {
            f,
            value: RefCell::new(None),
        }
    }
}

pub(crate) trait AnyEffect {
    fn run(&self);
}

impl<T, F> AnyEffect for EffectData<T, F>
where
    T: 'static,
    F: Fn(Option<T>) -> T,
{
    fn run(&self) {
        let value = self.value.take();
        let new_value = (self.f)(value);
        *self.value.borrow_mut() = Some(new_value);
    }
}
