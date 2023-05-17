use std::borrow::Borrow;
use std::marker::PhantomData;

use super::prelude::*;

use crate::callstack::DefInfo;
use crate::runtime::with_runtime;
use crate::runtime::AsSameStreams;
use crate::runtime::Consumable;
use crate::runtime::KindStream;

impl Network {
    #[track_caller]
    pub fn any_mut<T: Data + ?Sized>(&self) -> Any<T> {
        self.any_mut_relaxed().as_same()
    }

    #[track_caller]
    pub fn any_mut_relaxed<T>(&self) -> AnyRelaxed<T>
    where
        T: Data + ?Sized,
    {
        let node = self.new_node().with_output();
        let any = AnyNode::new(node.emitter());
        let node = node.with_input(any).seal();
        Any { node, phantom: PhantomData }
    }

    #[track_caller]
    pub fn any_mut_(&self) -> AnyUnit {
        let node = self.new_node().with_output();
        let any = AnyUnitNode { emitter: node.emitter() };
        let node = node.with_input(any).seal();
        AnyUnit { node }
    }

    #[track_caller]
    pub fn any<T: Data>(&self, streams: impl AsSameStreams<T>) -> Stream<T> {
        let streams = streams.as_streams();
        let any = self.any_mut::<T>();
        for stream in streams.as_array() {
            any.attach(stream);
        }
        any.as_stream()
    }

    #[track_caller]
    pub fn any_(&self, streams: impl AsStreams) -> Stream<()> {
        let phantom = streams.phantom();
        let streams = streams.as_streams();
        let any = self.any_mut_();
        AsStreams::impl_attach_dyn(phantom, &streams, any.node.consumer());
        any.as_stream()
    }
}

pub enum AttachSame {}
pub enum AttachRelaxed {}

pub type AnyRelaxed<T> = Any<T, AttachRelaxed>;

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Copy(bound = ""), Debug(bound = ""))]
pub struct Any<T: ?Sized = (), AttachMode = AttachSame> {
    node:    Node<dyn Data, T, KindStream>,
    phantom: PhantomData<AttachMode>,
}

impl<T: Data> Any<T, AttachSame> {
    pub fn attach_in_rt<S: AsStream<Event = T>>(&self, rt: Rt, src: S) {
        self.node.attach_in_rt(rt, src.as_stream());
    }

    pub fn attach<S: AsStream<Event = T>>(&self, src: S) {
        self.node.attach(src.as_stream());
    }
}
impl<T: Data + ?Sized> Any<T, AttachRelaxed> {
    pub fn attach_in_rt<S: AsStream>(&self, rt: Rt, src: S)
    where
        S::Event: Attachable<T>,
    {
        self.node.attach_in_rt(rt, src.as_stream());
    }

    pub fn attach<S: AsStream>(&self, src: S)
    where
        S::Event: Attachable<T>,
    {
        self.node.attach(src.as_stream());
    }

    pub fn as_same(self) -> Any<T, AttachSame> {
        Any { node: self.node, phantom: PhantomData }
    }
}

impl<T: Consumable + ?Sized, AttachMode> Any<T, AttachMode> {
    #[track_caller]
    pub fn emit_in_rt<V: Borrow<T>>(self, rt: Rt, value: V) {
        rt.in_stack_frame(DefInfo::emit(), || {
            self.node.emit_event(rt, value.borrow());
        })
    }

    #[track_caller]
    pub fn emit<V: Borrow<T>>(self, value: V) {
        let def = DefInfo::emit();
        with_runtime(|rt| {
            rt.in_stack_frame(def, || {
                self.node.emit_event(rt, value.borrow());
            })
        })
    }

    /// Allocate arbitrary data on the node's bump allocator. The data will be stored for the whole
    /// lifetime of this node's network. Avoid using this allocator for unbounded number of objects,
    /// because in long-lived network they may never be freed for the whole application lifetime.
    #[inline(always)]
    pub(super) fn alloc<V>(&self, rt: Rt, value: V) -> BumpRc<V> {
        self.node.alloc(rt, value)
    }
}

impl<T: ?Sized + AnyEvent, Mode> AsStream for Any<T, Mode> {
    type Event = T;

    #[inline(always)]
    fn as_stream(&self) -> Stream<T> {
        self.node.stream()
    }
}
pub struct AnyNode {
    emitter: Emitter<dyn Data>,
}

impl AnyNode {
    #[inline(always)]
    fn new<T: ?Sized>(emitter: Emitter<T>) -> Self {
        // Erase emitter type, as we don't actually care about the value type in `Any` node event
        // consumer. It allows us to cut down on monomorphic codegen and unnecessary downcasting.
        //
        // Safety:
        // - We only ever pass through the `any` input value unmodified. Therefore we know that the
        //   erased emitter type will only ever receive values of its original type.
        Self { emitter: unsafe { emitter.into_dyn() } }
    }
}

impl EventConsumer for AnyNode {
    type Event = dyn Data;
    fn on_event(&self, rt: Rt, _: NodeId, value: &dyn Data) {
        // Safety:
        // - Keep the invariants of [`AnyNode::new`]. The value must be passed through unmodified.
        self.emitter.emit_event(rt, value)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct AnyUnit {
    node: Node<dyn Data, (), KindStream>,
}

impl AnyUnit {
    pub fn attach_in_rt<S: AsStream>(&self, rt: Rt, src: S) {
        self.node.attach_in_rt(rt, src.as_stream().into_dyn())
    }

    pub fn attach<S: AsStream>(&self, src: S) {
        self.node.attach(src.as_stream().into_dyn())
    }

    #[track_caller]
    pub fn emit_in_rt(self, rt: Rt) {
        rt.in_stack_frame(DefInfo::emit(), || {
            self.node.emit_event(rt, &());
        })
    }

    #[track_caller]
    pub fn emit(self) {
        let def = DefInfo::emit();
        with_runtime(|rt| {
            rt.in_stack_frame(def, || {
                self.node.emit_event(rt, &());
            })
        })
    }
}

pub struct AnyUnitNode {
    emitter: Emitter<()>,
}

impl EventConsumer for AnyUnitNode {
    type Event = dyn Data;

    fn on_event(&self, rt: Rt, _: NodeId, _: &dyn Data) {
        self.emitter.emit_event(rt, &())
    }
}

impl AsStream for AnyUnit {
    type Event = ();

    #[inline(always)]
    fn as_stream(&self) -> Stream<()> {
        self.node.stream()
    }
}
