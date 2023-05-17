use super::prelude::*;

impl Network {
    // Special kind of mapper that calls a method, but returns nothing.
    #[track_caller]
    pub fn trace<T>(&self, src: T)
    where
        T: AsStream,
        T::Event: std::fmt::Debug,
    {
        let node = self.new_node();
        let def = node.def_info(self.rt());
        node.with_input_fn(move |rt: Rt, event: &T::Event| {
            println!("[FRP] {}: {event:?} at {} ", def.label, def.location);
            let stack = rt.stack_trace();
            println!("{stack}");
        })
        .with_attached(src.as_stream());
    }
}
