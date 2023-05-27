use super::prelude::*;

impl Network {
    #[track_caller]
    pub fn gate<Src, Cond>(&self, src: Src, condition: Cond) -> Stream<Src::Event>
    where
        Src: AsStream,
        Cond: AsBehavior<Value = bool>, {
        self.gate_eq(src, condition, true)
    }

    pub fn gate_not<Src, Cond>(&self, src: Src, condition: Cond) -> Stream<Src::Event>
    where
        Src: AsStream,
        Cond: AsBehavior<Value = bool>, {
        self.gate_eq(src, condition, false)
    }

    pub fn gate_eq<Src, Cond>(
        &self,
        src: Src,
        compare: Cond,
        eq: Cond::Value,
    ) -> Stream<Src::Event>
    where
        Src: AsStream,
        Cond: AsBehavior,
        Cond::Value: PartialEq,
    {
        fn make_gate_not<T: AnyBehavior + PartialEq>(compare: Behavior<T>, eq: T) -> impl GateCond {
            move |_, _| compare.value() == eq
        }
        let cmp = compare.as_behavior(self.rt());
        self.gate_common(src, make_gate_not(cmp, eq))
    }

    #[track_caller]
    fn gate_common<Src, C>(&self, src: Src, condition: C) -> Stream<Src::Event>
    where
        C: GateCond,
        Src: AsStream, {
        let tok = make_erased!(Src::Event);

        let node = self.new_node().with_output();
        let emitter = node.emitter();
        let stream = node
            .with_input(GateNode { emitter: emitter.erase(tok), condition })
            .with_attached(src.as_stream().erase(tok))
            .stream();
        stream
    }

    /// Multiplex single source into multiple output streams, based on a selector behavior.
    pub fn mux<const N: usize, Sel, Src>(
        &self,
        select: Sel,
        source: Src,
    ) -> [Stream<Src::Event>; N]
    where
        Sel: AsBehavior<Value = usize>,
        Src: AsStream,
    {
        // let tok = make_erased!(Src::Event);
        //
        // let nodes: [_; N] = std::array::from_fn(|_| self.new_node().with_output::<Src::Event>());
        // let emitters: [_; N] = std::array::from_fn(|i| nodes[i].emitter().erase(tok));
        // let streams: [_; N] = std::array::from_fn(|i| nodes[i].stream());
        // for first_node in nodes {
        //     let select = select.as_behavior(self.rt());
        //     first_node
        //         .with_input(MuxNode { select, emitters })
        //         .with_attached(source.as_stream().erase(tok));
        //     // Only attach first node.
        //     break;
        // }
        // return streams;
        panic!()
    }
}

struct MuxNode<const N: usize, T: ?Sized + 'static> {
    select:   Behavior<usize>,
    emitters: [Emitter<T>; N],
}

impl<const N: usize, T: Consumable + ?Sized> EventConsumer for MuxNode<N, T> {
    type Event = T;
    fn on_event(&self, rt: Rt, _: NodeId, value: &T) {
        let sel = self.select.try_value_in_rt(rt).filter(|sel| *sel < N);
        if let Some(sel) = sel {
            self.emitters[sel].emit_event(rt, value);
        }
    }
}

trait GateCond = Fn(Rt, NodeId) -> bool + 'static;

struct GateNode<T: ?Sized + 'static, F> {
    emitter:   Emitter<T>,
    condition: F,
}

impl<T: Consumable + ?Sized, F: GateCond> EventConsumer for GateNode<T, F> {
    type Event = T;
    fn on_event(&self, rt: Rt, source: NodeId, value: &T) {
        if (self.condition)(rt, source) {
            self.emitter.emit_event(rt, value);
        }
    }
}
