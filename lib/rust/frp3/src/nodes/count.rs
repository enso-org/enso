use std::cell::Cell;

use crate::runtime::KindBehavior;

use super::prelude::*;

impl Network {
    #[track_caller]
    pub fn count(&self, src: impl AsStream) -> Behavior<usize> {
        self.count_from(src, 0)
    }

    #[track_caller]
    #[inline]
    pub fn count_from(&self, src: impl AsStream, init: usize) -> Behavior<usize> {
        let node = self.new_node().with_value(init);
        let emitter = node.emitter();
        node.with_input(CountNode { emitter, count: Cell::new(init) })
            .with_attached(src.as_stream())
            .behavior()
    }
}

struct CountNode {
    emitter: Emitter<usize, KindBehavior>,
    count:   Cell<usize>,
}

impl EventConsumer for CountNode {
    type Event = dyn Data;
    fn on_event(&self, rt: Rt, _: NodeId, _: &dyn Data) {
        let next = self.count.get() + 1;
        self.count.set(next);
        self.emitter.emit_event(rt, &next);
    }
}
