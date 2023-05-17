use crate::prelude::*;

use crate::runtime::KindBehavior;

use super::prelude::*;

impl Network {
    #[track_caller]
    pub fn toggle(&self, src: impl AsStream) -> Behavior<bool> {
        let node = self.new_node().with_value(false);
        let emitter = node.emitter();
        node.with_input(ToggleNode { emitter, state: default() })
            .with_attached(src.as_stream())
            .behavior()
    }

    #[track_caller]
    pub fn toggle_true(&self, src: impl AsStream) -> Behavior<bool> {
        let node = self.new_node().with_value(true);
        let emitter = node.emitter();
        node.with_input(ToggleNode { emitter, state: Cell::new(true) })
            .with_attached(src.as_stream())
            .behavior()
    }
}

struct ToggleNode {
    emitter: Emitter<bool, KindBehavior>,
    state:   Cell<bool>,
}

impl EventConsumer for ToggleNode {
    type Event = dyn Data;
    fn on_event(&self, rt: Rt, _: NodeId, _: &dyn Data) {
        let next = !self.state.get();
        self.state.set(next);
        self.emitter.emit_event(rt, &next);
    }
}
