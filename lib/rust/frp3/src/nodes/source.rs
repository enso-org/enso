use crate::prelude::*;

use std::borrow::Borrow;

use crate::callstack::DefInfo;
use crate::runtime::with_runtime;

use super::prelude::*;

impl Network {
    #[track_caller]
    pub fn source<T: Data + Default>(&self) -> Source<T> {
        self.source_with(default())
    }

    #[track_caller]
    pub fn source_with<T: Data>(&self, init: T) -> Source<T> {
        let node = self.new_node().with_value(init);
        Source { emitter: node.emitter() }
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Copy(bound = ""), Debug(bound = ""))]
pub struct Source<T = ()> {
    emitter: Emitter<T, KindBehavior>,
}

impl<T: Data + Clone> Source<T> {
    #[track_caller]
    pub fn emit_in_rt<V: Borrow<T>>(self, rt: Rt, value: V) {
        rt.in_stack_frame(DefInfo::emit(), || {
            self.emitter.emit_event(rt, value.borrow());
        })
    }

    #[track_caller]
    pub fn emit<V: Borrow<T>>(self, value: V) {
        let def = DefInfo::emit();
        with_runtime(|rt| {
            rt.in_stack_frame(def, || {
                self.emitter.emit_event(rt, value.borrow());
            })
        })
    }
}

impl<T: Data> AsStream for Source<T> {
    type Event = T;
    fn as_stream(&self) -> Stream<T> {
        self.emitter.stream()
    }
}

impl<T: Data + Clone> AsBehavior for Source<T> {
    type Value = T;
    fn as_behavior(&self, _: Rt) -> Behavior<Self::Value> {
        self.emitter.behavior()
    }
}
