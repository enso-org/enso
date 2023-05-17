use super::prelude::*;

impl Network {
    #[track_caller]
    pub fn constant<T: Data>(&self, value: T) -> Behavior<T> {
        self.new_node().with_value(value).behavior()
    }

    #[track_caller]
    pub fn map_constant<T: Data>(&self, src: impl AsStream, value: T) -> Stream<T> {
        let node = self.new_node().with_output();
        let emitter = node.emitter();
        node.with_input(ConstantNode { emitter, value })
            .with_attached(src.as_stream().into_dyn())
            .stream()
    }
}

struct ConstantNode<T> {
    value:   T,
    emitter: Emitter<T>,
}

impl<T: Data> EventConsumer for ConstantNode<T> {
    type Event = dyn Data;
    fn on_event(&self, rt: Rt, _: NodeId, _: &dyn Data) {
        self.emitter.emit_event(rt, &self.value);
    }
}
