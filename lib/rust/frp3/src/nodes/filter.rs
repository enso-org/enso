use super::prelude::*;

impl Network {
    #[track_caller]
    pub fn filter<T, F>(&self, src: T, f: F) -> Stream<T::Event>
    where
        T: AsStream,
        F: Fn(&T::Event) -> bool + 'static,
    {
        let node = self.new_node().with_output();
        let emitter = node.emitter();
        node.with_input_fn(move |rt, v| {
            if f(v) {
                emitter.emit_event(rt, v);
            }
        })
        .with_attached(src.as_stream())
        .stream()
    }
}
