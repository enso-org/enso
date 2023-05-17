use crate::runtime::Attachable;
use crate::runtime::Consumable;
use std::any::Any;

pub trait Data: Any {
    fn as_dyn(&self) -> &dyn Data;
    fn as_any(&self) -> &dyn Any;
}

pub trait AnyEvent = ?Sized + Data + Consumable + Attachable<dyn Data> + Attachable<Self>;
pub trait AnyBehavior = AnyEvent + Clone;

impl<T: Any> Data for T {
    #[inline(always)]
    fn as_dyn(&self) -> &dyn Data {
        self
    }

    #[inline(always)]
    fn as_any(&self) -> &dyn Any {
        self
    }
}
