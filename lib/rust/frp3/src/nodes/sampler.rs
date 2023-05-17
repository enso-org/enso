use crate::runtime::KindBehavior;

use super::prelude::*;
use crate::prelude::*;

impl Network {
    #[track_caller]
    pub fn sampler<T>(&self, src: T) -> Behavior<T::Event>
    where
        T: AsStream,
        T::Event: Clone + Default, {
        self.sampler_with_init(src, default())
    }

    #[track_caller]
    pub fn sampler_with_init<T>(&self, src: T, init: T::Event) -> Behavior<T::Event>
    where
        T: AsStream,
        T::Event: Clone, {
        let node = self.new_node().with_value(init);
        let emitter = node.emitter();
        node.with_input(SamplerNode { emitter }).with_attached(src.as_stream()).behavior()
    }
}

pub struct SamplerNode<T> {
    emitter: Emitter<T, KindBehavior>,
}

impl<T: Data + Clone> EventConsumer for SamplerNode<T> {
    type Event = T;
    fn on_event(&self, rt: Rt, _: NodeId, value: &T) {
        self.emitter.emit_event(rt, value);
    }
}
