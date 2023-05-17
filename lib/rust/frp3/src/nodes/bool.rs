use super::prelude::*;

impl Network {
    #[track_caller]
    pub fn bool<A, B>(&self, switch_false: A, switch_true: B) -> Behavior<bool>
    where
        A: AsStream,
        B: AsStream,
    {
        let switch_false = switch_false.as_stream();
        let switch_true = switch_true.as_stream();
        let node = self.new_node().with_value(false);
        let emitter = node.emitter();
        node.with_input(BoolNode { emitter, true_source: switch_true.node_id() })
            .with_attached(switch_false)
            .with_attached(switch_true)
            .behavior()
    }
}

struct BoolNode {
    emitter:     Emitter<bool, KindBehavior>,
    true_source: NodeId,
}

impl EventConsumer for BoolNode {
    type Event = dyn Data;
    fn on_event(&self, rt: Rt, source: NodeId, _: &dyn Data) {
        let received_true = source == self.true_source;
        self.emitter.emit_event(rt, &received_true);
    }
}
