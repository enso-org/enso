#![allow(dead_code)]
#![allow(unsafe_code)]

use crate::prelude::*;

use crate::callstack::CallStack;
use crate::callstack::DefInfo;
use crate::callstack::Label;
use crate::data::Data;
use crate::rc_arena::BumpRc;
use crate::rc_arena::RcArena;
use crate::tuple::TupleAsArray;
use crate::AnyBehavior;
use crate::AnyEvent;
use derivative::Derivative;
use slotmap::Key;
use slotmap::SecondaryMap;
use slotmap::SlotMap;
use smallvec::SmallVec;
use std::cell::Cell;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;



// ===============
// === Metrics ===
// ===============

#[derive(Default, Clone)]
pub struct Metrics {
    pub total_networks:      Cell<u64>,
    pub total_network_refs:  Cell<u64>,
    pub total_networks_ever: Cell<u64>,
    pub total_nodes:         Cell<u64>,
    pub total_nodes_ever:    Cell<u64>,
}

impl std::fmt::Debug for Metrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Metrics")
            .field("total_networks", &self.total_networks.get())
            .field("total_network_refs", &self.total_network_refs.get())
            .field("total_networks_ever", &self.total_networks_ever.get())
            .field("total_nodes", &self.total_nodes.get())
            .field("total_nodes_ever", &self.total_nodes_ever.get())
            .finish()
    }
}

impl Metrics {
    fn inc_networks(&self) {
        self.total_networks.set(self.total_networks.get().wrapping_add(1));
        self.total_networks_ever.set(self.total_networks_ever.get().wrapping_add(1));
    }

    fn dec_networks(&self) {
        self.total_networks.set(self.total_networks.get().wrapping_sub(1));
    }

    fn inc_network_refs(&self) {
        self.total_network_refs.set(self.total_network_refs.get().wrapping_add(1));
    }

    fn dec_network_refs(&self) {
        self.total_network_refs.set(self.total_network_refs.get().wrapping_sub(1));
    }

    fn inc_nodes(&self) {
        self.total_nodes.set(self.total_nodes.get().wrapping_add(1));
        self.total_nodes_ever.set(self.total_nodes_ever.get().wrapping_add(1));
    }

    fn sub_nodes(&self, dec: u64) {
        self.total_nodes.set(self.total_nodes.get().wrapping_sub(dec));
    }
}



// ===============
// === Runtime ===
// ===============

thread_local! {
    static RUNTIME: Rc<Runtime>  = default();
}

#[inline(always)]
pub fn with_runtime<T>(f: impl FnOnce(Rt) -> T) -> T {
    RUNTIME.with(|runtime| f(Rt(runtime)))
}

fn rc_runtime() -> Rc<Runtime> {
    RUNTIME.with(|runtime| runtime.clone())
}



// ==========
// === Rt ===
// ==========

/// Public-facing opaque handle to a runtime. Provides no access to internals. Allows to cut down
/// on `with_runtime` calls across the codebase, allowing for more performant code which is easier
/// for the compiler to optimize.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Rt<'a>(&'a Runtime);

impl<'a> From<&'a Runtime> for Rt<'a> {
    fn from(runtime: &'a Runtime) -> Self {
        Rt(runtime)
    }
}

impl<'a> Rt<'a> {
    pub fn stack_trace(&self) -> &CallStack {
        &self.0.stack
    }

    pub fn in_stack_frame<T>(&self, _def: DefInfo, f: impl FnOnce() -> T) -> T {
        #[cfg(feature = "stack-trace")]
        {
            self.0.push_stack(_def);
            let out = f();
            self.0.pop_stack();
            out
        }

        #[cfg(not(feature = "stack-trace"))]
        {
            f()
        }
    }

    pub fn metrics(&self) -> Metrics {
        self.0.metrics.clone()
    }
}



slotmap::new_key_type! { pub struct NetworkId; }
slotmap::new_key_type! { pub struct NodeId; }

type TargetsVec = SmallVec<[NodeId; 1]>;



#[derive(Default)]
pub(crate) struct Runtime {
    networks:          RefCell<SlotMap<NetworkId, NetworkData>>,
    nodes:             RefCell<SlotMap<NodeId, NodeData>>,
    consumers:         RefCell<SecondaryMap<NodeId, BumpRc<dyn RawEventConsumer>>>,
    /// Map for each node to its behavior, if any. For nodes that are natively behaviors, points to
    /// itself. For non-behavior streams, may point to a sampler node that listens to it.
    behaviors:         RefCell<SecondaryMap<NodeId, NodeId>>,
    /// For each node that is a behavior, stores the current value of the behavior. A node can be
    /// checked for being a behavior by checking if it is present in this map.
    behavior_values:   RefCell<SecondaryMap<NodeId, BumpRc<dyn Data>>>,
    targets:           RefCell<SecondaryMap<NodeId, TargetsVec>>,
    target_store_pool: RefCell<Vec<TargetsVec>>,
    metrics:           Metrics,
    stack:             CallStack,
    current_node:      Cell<NodeId>,
}

#[derive(Clone, Copy, Debug)]
struct NodeData {
    network_id: NetworkId,
    def:        DefInfo,
}

impl Runtime {
    // Emit an event to all targets of the given node.
    fn emit_event(&self, node_id: NodeId, event: &dyn Data) {
        let Some(node) = self.nodes.borrow().get(node_id).copied() else { return };

        // Emit events to all targets. In case there are no targets, we are done.
        let local_targets = self.targets.with_borrowed(|targets| {
            targets.get(node_id).map(|node_targets| {
                // store the targets to emit to in a temporary buffer, so we can release the borrow.
                let vec_from_pool = self.target_store_pool.borrow_mut().pop();
                let mut local_targets = vec_from_pool.unwrap_or_default();
                local_targets.extend_from_slice(node_targets);
                local_targets
            })
        });

        let Some(mut local_targets) = local_targets else { return };

        let cleanup_targets = self.stack.with(node.def, || {
            let mut cleanup_targets = false;
            for target in &local_targets {
                let consumer = self.consumers.borrow().get(*target).cloned();
                if let Some(consumer) = consumer {
                    consumer.on_event(Rt(self), node_id, event);
                } else {
                    cleanup_targets = true;
                }
            }
            cleanup_targets
        });

        // Return temporary buffer to pool.
        local_targets.clear();
        self.target_store_pool.borrow_mut().push(local_targets);

        // Remove targets that have been dropped.
        if cleanup_targets {
            let mut targets = self.targets.borrow_mut();
            let node_targets = targets.get_mut(node_id);
            if let Some(targets) = node_targets {
                let consumers = self.consumers.borrow();
                targets.retain(|target| consumers.contains_key(*target));
            }
        }
    }

    fn push_stack(&self, def: DefInfo) {
        self.stack.push(def);
    }

    fn pop_stack(&self) {
        self.stack.pop();
    }

    fn write_behavior(&self, id: NodeId, value: BumpRc<dyn Data>) {
        self.behavior_values.borrow_mut().insert(id, value);
    }

    fn mut_access_behavior(&self, id: NodeId) -> Option<RefMut<BumpRc<dyn Data>>> {
        RefMut::filter_map(self.behavior_values.borrow_mut(), |values| values.get_mut(id)).ok()
    }

    fn access_behavior(&self, id: NodeId) -> Option<Ref<BumpRc<dyn Data>>> {
        Ref::filter_map(self.behavior_values.borrow(), |values| values.get(id)).ok()
    }

    fn get_node_def_info(&self, id: NodeId) -> DefInfo {
        self.nodes
            .borrow()
            .get(id)
            .map(|node| node.def)
            .unwrap_or(DefInfo::labelled(0, Label::dropped()))
    }

    /// Allocate a new value in the given node's network's arena. If either the node or arena is
    /// already dropped, the allocated `BumpRc` is standalone, i.e. not tied to any arena.
    fn alloc_in_node<T>(&self, node: NodeId, data: T) -> BumpRc<T> {
        let nodes = self.nodes.borrow();
        let networks = self.networks.borrow();
        let network = nodes.get(node).and_then(|node| networks.get(node.network_id));

        match network {
            Some(network) => network.arena.alloc(data),
            None => BumpRc::alloc_standalone(data),
        }
    }

    fn new_network(&self) -> NetworkId {
        self.metrics.inc_networks();
        self.metrics.inc_network_refs();
        let mut networks = self.networks.borrow_mut();
        let arena = RcArena::new();
        networks.insert(NetworkData { nodes: Vec::new(), refs: 1, arena })
    }

    fn add_network_ref(&self, network_id: NetworkId) -> bool {
        self.metrics.inc_network_refs();
        let mut networks = self.networks.borrow_mut();
        if let Some(network) = networks.get_mut(network_id) {
            network.refs += 1;
            true
        } else {
            false
        }
    }

    fn remove_network_ref(&self, network_id: NetworkId) {
        self.metrics.dec_network_refs();
        let mut networks = self.networks.borrow_mut();
        if let Some(network) = networks.get_mut(network_id) {
            network.refs -= 1;
            if network.refs >= 1 {
                return;
            }
        } else {
            return;
        };

        // No more references to this network. We can remove it.
        if let Some(network) = networks.remove(network_id) {
            self.metrics.dec_networks();
            self.remove_nodes(&network.nodes);
        }
    }

    fn new_node(&self, network_id: NetworkId, def: DefInfo) -> NodeId {
        self.metrics.inc_nodes();
        let mut nodes = self.nodes.borrow_mut();

        let mut networks = self.networks.borrow_mut();
        if let Some(network) = networks.get_mut(network_id) {
            let id = nodes.insert(NodeData { network_id, def });
            network.nodes.push(id);
            id
        } else {
            NodeId::null()
        }
    }

    /// Remove a list of nodes and their consumers from the network.
    ///
    /// Borrows:
    /// - mutable `nodes`
    /// - mutable `consumers`
    fn remove_nodes(&self, to_remove: &[NodeId]) {
        // Split node and consumer borrows, as the consumer `Drop` implementation may attempt to
        // borrow nodes. [`NodeData`] is a [`Copy`] type, so there is no drop to worry about.
        {
            self.metrics.sub_nodes(to_remove.len() as u64);
            let mut nodes = self.nodes.borrow_mut();
            for node_id in to_remove {
                nodes.remove(*node_id);
            }
        }

        {
            let mut consumers = self.consumers.borrow_mut();
            for node_id in to_remove {
                consumers.remove(*node_id);
            }
        }

        // Eagerly drop all node values, as they may hold references to the network arena.
        {
            let mut values = self.behavior_values.borrow_mut();
            for node_id in to_remove {
                values.remove(*node_id);
            }
        }
    }

    /// Declare an event consumer for node. The receiver will be called whenever the node receives
    /// an event from other nodes in the network. This effectively defines input types that the node
    /// can handle.
    fn set_node_consumer(&self, node: NodeId, consumer: BumpRc<dyn RawEventConsumer>) {
        let prev_consumer = self.consumers.borrow_mut().insert(node, consumer);
        assert!(prev_consumer.is_none(), "Receiver already registered for node");
    }

    /// Register a target for a node. Whenever the node emits an event, it will be sent to all its
    /// targets. The target must be able to handle the sent event type. Typically used at in the
    /// implementation on target side.
    ///
    /// In case the target node's consumer unsafely downcasts the event type, its definition API
    /// must maintain all required safety invariants to disallow connecting sources that emit
    /// incompatible event types. When in doubt, use guarded downcast.
    fn register_target(&self, source: NodeId, target: NodeId) {
        let mut targets = self.targets.borrow_mut();
        if let Some(entry) = targets.entry(source) {
            entry.or_default().push(target);
        }
    }
}

// ===============
// === Network ===
// ===============

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Network {
    pub(crate) id: NetworkId,
    #[derivative(Debug = "ignore")]
    rt:            Rc<Runtime>,
    #[cfg(feature = "stack-trace")]
    span:          Cell<Option<DefInfo>>,
    #[cfg(not(feature = "stack-trace"))]
    span:          (),
}

pub struct NetworkSpan<'a> {
    pub(crate) network:   &'a Network,
    #[cfg(feature = "stack-trace")]
    pub(crate) prev_span: Option<DefInfo>,
}

impl<'a> NetworkSpan<'a> {
    #[inline(always)]
    pub fn new(network: &'a Network, _span: DefInfo) -> Self {
        Self {
            network,
            #[cfg(feature = "stack-trace")]
            prev_span: network.span.replace(Some(_span)),
        }
    }
}

impl Deref for NetworkSpan<'_> {
    type Target = Network;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.network
    }
}

impl Drop for NetworkSpan<'_> {
    #[inline(always)]
    fn drop(&mut self) {
        #[cfg(feature = "stack-trace")]
        self.network.span.set(self.prev_span);
    }
}

struct NetworkData {
    nodes: Vec<NodeId>,
    refs:  usize,
    arena: RcArena,
}

#[derive(Debug, Clone, Copy)]
pub struct WeakNetwork {
    id: NetworkId,
}

impl WeakNetwork {
    pub fn upgrade(&self, rt: Rt) -> Option<Network> {
        rt.0.add_network_ref(self.id).then_some(Network {
            rt:   rc_runtime(),
            id:   self.id,
            span: default(),
        })
    }
}

impl Clone for Network {
    fn clone(&self) -> Self {
        let added = self.rt.add_network_ref(self.id);
        debug_assert!(added);
        Self { rt: self.rt.clone(), id: self.id, span: default() }
    }
}

impl Drop for Network {
    fn drop(&mut self) {
        self.rt.remove_network_ref(self.id);
    }
}

impl Network {
    #[track_caller]
    pub(crate) fn new_node(&self) -> NodeBuilder<Unassigned, Unassigned, Unassigned> {
        NodeBuilder::new(self.rt(), self.id, self.get_span())
    }

    #[track_caller]
    #[inline(always)]
    fn get_span(&self) -> DefInfo {
        #[cfg(feature = "stack-trace")]
        {
            self.span.get().unwrap_or(DefInfo::unlabelled())
        }
        #[cfg(not(feature = "stack-trace"))]
        DefInfo::hidden()
    }

    pub fn new() -> Self {
        let rt = rc_runtime();
        let id = rt.new_network();
        Network { rt, id, span: default() }
    }

    #[inline(always)]
    #[track_caller]
    pub fn span(&self, line_number: u32, label: &'static str) -> NetworkSpan {
        NetworkSpan::new(self, DefInfo::labelled(line_number, label.into()))
    }

    #[inline(always)]
    pub fn hide_span(&self) -> NetworkSpan {
        NetworkSpan::new(self, DefInfo::hidden())
    }

    #[inline(always)]
    pub fn downgrade(&self) -> WeakNetwork {
        WeakNetwork { id: self.id }
    }

    #[inline(always)]
    pub fn rt(&self) -> Rt {
        Rt(&self.rt)
    }
}

/// An invariant phantom type for FRP events.
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Copy(bound = ""), Debug(bound = ""))]
pub struct EventTy<T: ?Sized>(PhantomData<*mut T>);

// ==============
// === Stream ===
// ==============

/// An output of a node. Can be connected to other nodes.
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Copy(bound = ""), Debug(bound = ""))]
pub struct Stream<T: ?Sized = ()> {
    id:        NodeId,
    output_ty: EventTy<T>,
}

impl<T: ?Sized + Data> Stream<T> {
    // Create a stream that behaves as if its network has been already dropped.
    pub fn null() -> Self {
        Stream { id: NodeId::null(), output_ty: EventTy(PhantomData) }
    }

    pub(crate) fn output_ty(self) -> EventTy<T> {
        self.output_ty
    }

    pub fn into_dyn(self) -> Stream<dyn Data> {
        Stream { id: self.id, output_ty: EventTy(PhantomData) }
    }

    pub fn node_id(self) -> NodeId {
        self.id
    }

    pub fn into_dyn_checked(self) -> DynStream {
        DynStream { id: self.id, output_ty: std::any::TypeId::of::<T>() }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DynStream {
    id:        NodeId,
    output_ty: std::any::TypeId,
}

impl DynStream {
    pub fn downcast<T: ?Sized + Data>(self) -> Option<Stream<T>> {
        let type_matches = self.output_ty == std::any::TypeId::of::<T>();
        type_matches.then_some(Stream { id: self.id, output_ty: EventTy(PhantomData) })
    }
}

// ================
// === Behavior ===
// ================

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Copy(bound = ""))]
pub struct Behavior<T> {
    id:        NodeId,
    output_ty: EventTy<T>,
}

impl<T: Data> Behavior<T> {
    // Create a behavior that behaves as if its network has been already dropped.
    pub fn null() -> Self {
        Behavior { id: NodeId::null(), output_ty: EventTy(PhantomData) }
    }

    #[inline(always)]
    pub fn try_value(&self) -> Option<T>
    where T: Clone {
        with_runtime(|rt| self.try_value_in_rt(rt))
    }

    #[inline(always)]
    pub fn value(&self) -> T
    where T: Clone {
        with_runtime(|rt| self.value_in_rt(rt))
    }

    #[inline(always)]
    pub fn try_value_in_rt(&self, rt: Rt) -> Option<T>
    where T: Clone {
        let borrow = rt.0.behavior_values.borrow();
        self.get_from_map(&*borrow).map(|rc| (&**rc).clone())
    }

    #[inline(always)]
    pub fn value_in_rt(&self, rt: Rt) -> T
    where T: Clone {
        #[inline(never)]
        fn do_panic() -> ! {
            loop {}
            // panic!("Trying to get a value from a dropped behavior.");
        }
        match self.try_value_in_rt(rt) {
            Some(value) => value,
            None => do_panic(),
        }
    }

    pub fn get(&self) -> Option<BumpRc<T>> {
        with_runtime(|rt| self.get_in_rt(rt))
    }

    pub fn get_in_rt(&self, rt: Rt) -> Option<BumpRc<T>> {
        let borrow = rt.0.behavior_values.borrow();
        self.get_from_map(&*borrow).cloned()
    }

    fn get_from_map<'a>(
        &self,
        map: &'a SecondaryMap<NodeId, BumpRc<dyn Data>>,
    ) -> Option<&'a BumpRc<T>> {
        let erased = map.get(self.id)?;
        // Safety:
        // We know that the data comes from a behavior of type `T`, as all behaviors are
        // strongly typed and `T` is always `Sized`.
        Some(unsafe { erased.downcast_ref_data_unchecked() })
    }
}

impl<T: Data + Clone> AsBehavior for Behavior<T> {
    type Value = T;
    fn as_behavior(&self, _: Rt) -> Behavior<T> {
        *self
    }
}

impl<T: AnyEvent> AsStream for Behavior<T> {
    type Event = T;
    fn as_stream(&self) -> Stream<T> {
        Stream { id: self.id, output_ty: self.output_ty }
    }
}

// =====================
// === EventConsumer ===
// =====================

pub trait EventConsumer: 'static {
    type Event: Consumable + ?Sized;
    fn on_event(&self, rt: Rt, source: NodeId, value: &Self::Event);
}

pub trait RawEventConsumer {
    /// Receive an event from another node in the network.
    fn on_event(&self, rt: Rt, source: NodeId, event: &dyn Data);
}

struct FnConsumer<T: ?Sized, F> {
    input_ty: EventTy<T>,
    f:        F,
}

impl<T, F> EventConsumer for FnConsumer<T, F>
where
    T: Consumable + ?Sized,
    F: Fn(Rt, &T),
    Self: 'static,
{
    type Event = T;
    fn on_event(&self, rt: Rt, _: NodeId, value: &T) {
        (self.f)(rt, value)
    }
}
// forbid direct construction of [`DowncastConsumer`] to ensure that the safety invariants are
// upheld.
pub use consumer::Consumable;
mod consumer {
    use super::*;

    /// A version of `Data` that can be consumed by a generic [`EventConsumer`]. This trait is
    /// implemented for all sized `Data` types, as well as specifically for `dyn Data`. That allows
    /// automatically choosing the right raw event consumer implementation that either performs a
    /// downcast, or skips it when it's not necessary.
    pub trait Consumable {
        /// # Safety
        /// The consumer must only be registered to a node that receives events of this type.
        unsafe fn raw_consumer<C: EventConsumer<Event = Self>>(
            arena: &RcArena,
            consumer_impl: C,
            input_ty: EventTy<Self>,
        ) -> BumpRc<dyn RawEventConsumer>;
        fn into_dyn(&self) -> &dyn Data;
    }

    // Note that T must be [`Sized`] for this implementation to be correct. Only `Sized` types can
    // be downcasted from `dyn Data`.
    impl<T: Data> Consumable for T {
        /// # Safety
        /// The consumer must only be registered to a node that receives events of this type.
        unsafe fn raw_consumer<C: EventConsumer<Event = T>>(
            arena: &RcArena,
            consumer_impl: C,
            input_ty: EventTy<T>,
        ) -> BumpRc<dyn RawEventConsumer> {
            arena.alloc(DowncastConsumer { consumer_impl, input_ty })
        }
        fn into_dyn(&self) -> &dyn Data {
            self
        }
    }

    /// A consumer that is able to consume events of a single static type.
    struct DowncastConsumer<T: EventConsumer> {
        consumer_impl: T,
        input_ty:      EventTy<T::Event>,
    }

    impl<T: EventConsumer + 'static> RawEventConsumer for DowncastConsumer<T>
    where T::Event: Sized
    {
        #[inline(always)]
        fn on_event(&self, rt: Rt, source: NodeId, event: &dyn Data) {
            // # Safety
            // Due to safety invariants on `raw_consumer`, we know that the consumer is registered
            // only to a node that receives events of type `T::Event`. Therefore, we can
            // downcast received event to that type.
            let event = unsafe { event.as_any().downcast_ref_unchecked::<T::Event>() };
            self.consumer_impl.on_event(rt, source, event);
        }
    }

    impl Consumable for dyn Data {
        // Safety:
        // It is always allowed to register a dyn consumer for nodes of any input type, including
        // polymorphic nodes.
        unsafe fn raw_consumer<C: EventConsumer<Event = dyn Data>>(
            arena: &RcArena,
            consumer_impl: C,
            input_ty: EventTy<dyn Data>,
        ) -> BumpRc<dyn RawEventConsumer> {
            arena.alloc(DynConsumer { consumer_impl, input_ty })
        }
        fn into_dyn(&self) -> &dyn Data {
            self
        }
    }

    impl<Brand: OpaqueBrand> Consumable for OpaqueBranded<Brand> {
        // Safety:
        // It is always allowed to register an opaque consumer for nodes of any input type, as it is
        // effectively a `dyn Data`.
        unsafe fn raw_consumer<C: EventConsumer<Event = OpaqueBranded<Brand>>>(
            arena: &RcArena,
            consumer_impl: C,
            input_ty: EventTy<OpaqueBranded<Brand>>,
        ) -> BumpRc<dyn RawEventConsumer> {
            arena.alloc(DynConsumer { consumer_impl, input_ty })
        }
        fn into_dyn(&self) -> &dyn Data {
            &self.1
        }
    }

    /// A consumer that is able to consume events of `dyn Data` type.
    struct DynConsumer<T: EventConsumer> {
        consumer_impl: T,
        input_ty:      EventTy<T::Event>,
    }

    trait DynData {
        fn from_dyn(data: &dyn Data) -> &Self;
    }

    impl DynData for dyn Data {
        fn from_dyn(data: &dyn Data) -> &Self {
            data
        }
    }

    impl<Brand: OpaqueBrand> DynData for OpaqueBranded<Brand> {
        #[inline(always)]
        fn from_dyn(data: &dyn Data) -> &Self {
            unsafe { &*(data as *const dyn Data as *const Self) }
        }
    }

    impl<T> RawEventConsumer for DynConsumer<T>
    where
        T: EventConsumer,
        T::Event: DynData,
    {
        #[inline(always)]
        fn on_event(&self, rt: Rt, source: NodeId, event: &dyn Data) {
            self.consumer_impl.on_event(rt, source, T::Event::from_dyn(event));
        }
    }
}

/// A trait implemented for stream data types that can be attached to [`Node<T>`] as input.
pub trait Attachable<T: ?Sized> {}
impl<T: Data> Attachable<T> for T {}
impl<T: Data> Attachable<dyn Data> for T {}
impl Attachable<dyn Data> for dyn Data {}
impl<B: OpaqueBrand> Attachable<OpaqueBranded<B>> for OpaqueBranded<B> {}

impl<In: ?Sized + Consumable, Out: ?Sized, Kind> Node<In, Out, Kind> {
    #[inline(always)]
    pub(crate) fn consumer(&self) -> Consumer<In> {
        Consumer { id: self.id, input_ty: self.input_ty }
    }

    #[inline(always)]
    pub(crate) fn attach_in_rt<A: ?Sized + Attachable<In>>(&self, rt: Rt, src: Stream<A>) {
        self.consumer().attach_in_rt(rt, src)
    }

    #[inline(always)]
    pub(crate) fn attach<A: ?Sized + Attachable<In>>(&self, src: Stream<A>) {
        self.consumer().attach(src)
    }
}

/// ============
/// === Node ===
/// ============

pub enum Unassigned {}
pub enum KindStream {}
pub enum KindBehavior {}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) struct NodeBuilder<'a, In: ?Sized, Out: ?Sized, Kind> {
    network_id: NetworkId,
    node:       Node<In, Out, Kind>,
    #[derivative(Debug = "ignore")]
    rt:         Rt<'a>,
}

impl<In: ?Sized, Out: ?Sized, Kind> std::ops::Deref for NodeBuilder<'_, In, Out, Kind> {
    type Target = Node<In, Out, Kind>;
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}
impl<'a, In: ?Sized, Out: ?Sized, Kind> NodeBuilder<'a, In, Out, Kind> {
    #[inline(always)]
    fn map_node<I, O, K, F>(self, map: F) -> NodeBuilder<'a, I, O, K>
    where
        I: ?Sized,
        O: ?Sized,
        F: FnOnce(Node<In, Out, Kind>) -> Node<I, O, K>, {
        NodeBuilder { network_id: self.network_id, node: map(self.node), rt: self.rt }
    }

    /// Seals the node, preventing any builder-specific methods to be called on it again. This
    /// method does not have to be called if the [`Node`] type is not going to be used. its only
    /// effect is to strip away the data that is only useful during the building process, and to
    /// enable `Clone` and `Copy` on the node. The resulting node can be stored within the
    /// consumer implementation and will not have any unnecessary overhead.
    pub fn seal(self) -> Node<In, Out, Kind> {
        self.node
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Copy(bound = ""))]
pub(crate) struct Node<In: ?Sized, Out: ?Sized, Kind> {
    id:        NodeId,
    input_ty:  EventTy<In>,
    output_ty: EventTy<Out>,
    kind:      PhantomData<Kind>,
}

impl<In: ?Sized, Out: ?Sized, Kind> Node<In, Out, Kind> {
    #[inline(always)]
    pub(crate) fn emitter(&self) -> Emitter<Out, Kind> {
        Emitter { id: self.id, output_ty: self.output_ty, kind: self.kind }
    }

    pub(crate) fn alloc<T>(&self, rt: Rt, value: T) -> BumpRc<T> {
        rt.0.alloc_in_node(self.id, value)
    }

    pub(crate) fn def_info(&self, rt: Rt) -> DefInfo {
        rt.0.get_node_def_info(self.id)
    }

    #[inline(always)]
    fn with_input_ty<I: ?Sized>(&self, input_ty: EventTy<I>) -> Node<I, Out, Kind> {
        Node { id: self.id, input_ty, output_ty: self.output_ty, kind: self.kind }
    }

    #[inline(always)]
    fn with_output_ty<O: ?Sized>(&self, output_ty: EventTy<O>) -> Node<In, O, Kind> {
        Node { id: self.id, input_ty: self.input_ty, output_ty, kind: self.kind }
    }
}

impl<In: ?Sized, Out: ?Sized, Kind> Node<In, Out, Kind> {
    #[inline(always)]
    fn with_kind<K>(self) -> Node<In, Out, K> {
        Node {
            id:        self.id,
            input_ty:  self.input_ty,
            output_ty: self.output_ty,
            kind:      PhantomData,
        }
    }

    #[inline(always)]
    fn kind_stream(self) -> Node<In, Out, KindStream> {
        self.with_kind()
    }

    #[inline(always)]
    fn kind_behavior(self) -> Node<In, Out, KindBehavior> {
        self.with_kind()
    }
}

impl<In: ?Sized, Out: ?Sized> Node<In, Out, KindStream> {
    #[inline(always)]
    pub(crate) fn stream(&self) -> Stream<Out> {
        self.emitter().stream()
    }
}

impl<In: ?Sized, Out: ?Sized + Consumable> Node<In, Out, KindStream> {
    #[inline(always)]
    #[track_caller]
    pub(crate) fn emit_event(&self, rt: Rt, value: &Out) {
        self.emitter().emit_event(rt, value)
    }
}

impl<In: ?Sized, Out: Data + Clone> Node<In, Out, KindBehavior> {
    #[inline(always)]
    #[track_caller]
    pub(crate) fn emit_event(&self, rt: Rt, value: &Out) {
        self.emitter().emit_event(rt, value)
    }
}

impl<In: ?Sized, Out: Data> Node<In, Out, KindBehavior> {
    #[inline(always)]
    pub(crate) fn behavior(&self) -> Behavior<Out> {
        self.emitter().behavior()
    }
}

impl<'a> NodeBuilder<'a, Unassigned, Unassigned, Unassigned> {
    fn new(rt: Rt<'a>, network_id: NetworkId, def: DefInfo) -> Self {
        let input_ty = EventTy::<Unassigned>(PhantomData);
        let output_ty = EventTy::<Unassigned>(PhantomData);
        let id = rt.0.new_node(network_id, def);
        let node = Node { id, input_ty, output_ty, kind: PhantomData };
        NodeBuilder { rt, network_id, node }
    }
}

impl<'a, In: ?Sized> NodeBuilder<'a, In, Unassigned, Unassigned> {
    #[inline(always)]
    pub fn with_output<T: ?Sized>(self) -> NodeBuilder<'a, In, T, KindStream> {
        let output_ty = EventTy::<T>(PhantomData);
        self.map_node(|n| n.kind_stream().with_output_ty(output_ty))
    }

    #[inline(always)]
    pub fn with_value<T: Data>(self, value: T) -> NodeBuilder<'a, In, T, KindBehavior> {
        let id = self.node.id;
        let output_ty = EventTy::<T>(PhantomData);
        let stored = self.rt.0.alloc_in_node(id, value);
        self.rt.0.write_behavior(id, stored);
        self.rt.0.behaviors.borrow_mut().insert(id, id);
        self.map_node(|n| n.kind_behavior().with_output_ty(output_ty))
    }
}

impl<'a, Out: ?Sized, Kind> NodeBuilder<'a, Unassigned, Out, Kind> {
    pub fn with_input<C>(self, consumer_impl: C) -> NodeBuilder<'a, C::Event, Out, Kind>
    where C: EventConsumer {
        let input_ty = EventTy::<C::Event>(PhantomData);
        let rt = self.rt.0;
        if let Some(network) = rt.networks.borrow().get(self.network_id) {
            rt.set_node_consumer(
                self.node.id,
                // Safety:
                // - The consumer is immediately registered to a node that receives events of type
                //   `C::Event`. We have just marked this node as receiving events of that type.
                // - Once the node output type is set, it is no longer possible to change it, as
                //   `with_input` method is no longer available and the [`NodeBuilder`] with
                //   unassigned type cannot be cloned.
                unsafe { C::Event::raw_consumer(&network.arena, consumer_impl, input_ty) },
            );
        }

        self.map_node(|n| n.with_input_ty(input_ty))
    }

    pub fn with_input_fn<T: Consumable + 'static + ?Sized, F>(
        self,
        f: F,
    ) -> NodeBuilder<'a, T, Out, Kind>
    where
        F: Fn(Rt, &T) + 'static,
    {
        let input_ty = EventTy::<T>(PhantomData);
        self.with_input(FnConsumer { input_ty, f })
    }
}
impl<'a, In: ?Sized, Out: ?Sized, Kind> NodeBuilder<'a, In, Out, Kind> {
    #[inline(always)]
    pub(crate) fn alloc<T>(&self, value: T) -> BumpRc<T>
    where Out: Data {
        self.node.alloc(self.rt, value)
    }

    #[inline(always)]
    pub(crate) fn with_attached<A: ?Sized + Attachable<In>>(self, src: Stream<A>) -> Self
    where In: Consumable {
        self.node.attach_in_rt(self.rt, src);
        self
    }
}

/// Internal typed representation of a node that is able to consume events of specific type.
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Copy(bound = ""), Debug(bound = ""))]
pub struct Consumer<T: ?Sized> {
    id:       NodeId,
    input_ty: EventTy<T>,
}

impl<T: ?Sized> Consumer<T> {
    pub(crate) fn attach_in_rt<A>(&self, rt: Rt, src: Stream<A>)
    where A: ?Sized + Attachable<T> {
        rt.0.register_target(src.id, self.id);
    }

    pub(crate) fn attach<A>(&self, src: Stream<A>)
    where A: ?Sized + Attachable<T> {
        with_runtime(|rt| self.attach_in_rt(rt, src));
    }
}

/// An output of a node. Is able to emit events to the network.
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Copy(bound = ""), Debug(bound = ""))]
pub struct Emitter<T: ?Sized, Kind = KindStream> {
    id:        NodeId,
    output_ty: EventTy<T>,
    kind:      PhantomData<Kind>,
}

impl<T: ?Sized, Kind> Emitter<T, Kind> {
    /// Creates stream that can be used to receive events emitted by this emitter.
    #[inline(always)]
    pub(crate) fn stream(self) -> Stream<T> {
        Stream { id: self.id, output_ty: self.output_ty }
    }
}

pub struct Erased(dyn Data);

impl<T: ?Sized> Emitter<T, KindStream> {
    #[inline(always)]
    #[track_caller]
    pub(crate) fn emit_event(self, rt: Rt, value: &T)
    where T: Consumable {
        rt.0.emit_event(self.id, value.into_dyn())
    }

    /// Safety:
    /// - The emitter can only be used to emit events of type `T`, but in type-erased form. Emitting
    ///   events of non-matching types is *undefined behavior*, because other parts of the system
    ///   will assume that all outgoing [`Node`] events match its declared `Out` type.
    pub(crate) unsafe fn into_dyn(self) -> Emitter<dyn Data> {
        Emitter { id: self.id, output_ty: EventTy(PhantomData), kind: self.kind }
    }
}

impl<T> Emitter<T, KindBehavior> {
    /// Creates behavior that can be used to receive values emitted by this emitter.
    #[inline(always)]
    pub(crate) fn behavior(self) -> Behavior<T> {
        Behavior { id: self.id, output_ty: self.output_ty }
    }

    #[track_caller]
    pub(crate) fn emit_event(self, rt: Rt, value: &T)
    where T: Data + Clone {
        let rt = rt.0;
        let Some(mut rc) = rt.mut_access_behavior(self.id) else { return };
        if BumpRc::make_write_dyn(&mut *rc, value.clone()).is_err() {
            panic!("Type of emitted behavior changed.");
        };
        drop(rc);

        rt.emit_event(self.id, value.as_dyn())
    }
}

// ==================
// === AsStream ===
// ==================

pub trait AsStream {
    type Event: ?Sized + AnyEvent;
    fn as_stream(&self) -> Stream<Self::Event>;
}

pub trait AsSameStreams<T> =
    AsStreams where <Self as AsStreams>::Streams: TupleAsArray<Item = Stream<T>>;

pub trait AsStreams {
    type Streams;
    fn as_streams(&self) -> Self::Streams;
    fn attach_dyn(streams: &Self::Streams, consumer: Consumer<dyn Data>);
    #[inline(always)]
    fn phantom(&self) -> PhantomData<Self> {
        PhantomData
    }
    /// A version of `attach_dyn` with phantom argument. Allows trait dispatch in methods that use
    /// `impl AsStreams` argument.
    #[inline(always)]
    fn impl_attach_dyn(
        _phantom: PhantomData<Self>,
        streams: &Self::Streams,
        consumer: Consumer<dyn Data>,
    ) {
        Self::attach_dyn(streams, consumer)
    }
}

impl AsStreams for () {
    type Streams = ();
    fn as_streams(&self) {
        ()
    }
    fn attach_dyn(_: &Self::Streams, _: Consumer<dyn Data>) {}
}

impl<T: ?Sized + AnyEvent> AsStream for Stream<T> {
    type Event = T;
    fn as_stream(&self) -> Stream<Self::Event> {
        *self
    }
}

impl<T: AsStream + Copy> AsStream for &T {
    type Event = T::Event;
    #[inline(always)]
    fn as_stream(&self) -> Stream<Self::Event> {
        T::as_stream(*self)
    }
}

impl<T> AsStreams for T
where T: AsStream
{
    type Streams = (Stream<T::Event>,);
    #[inline(always)]
    fn as_streams(&self) -> Self::Streams {
        (self.as_stream(),)
    }
    #[inline(always)]
    fn attach_dyn(streams: &Self::Streams, consumer: Consumer<dyn Data>) {
        consumer.attach(streams.0);
    }
}

macro_rules! impl_as_streams {
    ($($a:ident)+) => {
        impl<$($a),+> AsStreams for ($($a,)+)
        where
            $($a: AsStream),+
        {
            type Streams = ($(Stream<<$a as AsStream>::Event>,)+);

            #[allow(non_snake_case)]
            #[inline(always)]
            fn as_streams(&self) -> Self::Streams {
                let ($($a,)+) = self;
                ($($a.as_stream(),)+)
            }
            #[allow(non_snake_case)]
            #[inline(always)]
            fn attach_dyn(streams: &Self::Streams, consumer: Consumer<dyn Data>) {
                let ($($a,)+) = streams;
                $(consumer.attach(*$a);)+
            }
        }
    };
}
crate::tuple::impl_tuples!(f = [[impl_as_streams]]);

// ====================
// === AsBehavior ===
// ====================

pub trait AsBehavior {
    type Value: AnyBehavior;
    fn as_behavior(&self, rt: Rt) -> Behavior<Self::Value>;
}

impl<T: AsBehavior + Copy> AsBehavior for &T {
    type Value = T::Value;
    fn as_behavior(&self, rt: Rt) -> Behavior<Self::Value> {
        T::as_behavior(*self, rt)
    }
}

impl<T: Data + Clone + Default> AsBehavior for Stream<T> {
    type Value = T;
    fn as_behavior(&self, rt: Rt) -> Behavior<T> {
        let behavior: Option<Behavior<T>> = (|| {
            if let Some(behavior_id) = rt.0.behaviors.borrow().get(self.id) {
                return Some(Behavior { id: *behavior_id, output_ty: self.output_ty });
            }
            // Node is alive, therefore its network must also be alive. Let's create a temporary
            // network reference to create a sampler. Always use the same network that the node
            // was created in.
            let net_id = rt.0.nodes.borrow().get(self.id)?.network_id;
            let weak_net = WeakNetwork { id: net_id };
            let network = weak_net.upgrade(rt).unwrap();
            let sampler = NetworkSpan::new(&network, rt.0.get_node_def_info(self.id)).sampler(self);
            rt.0.behaviors.borrow_mut().insert(self.id, sampler.id);
            Some(sampler)
        })();
        behavior.unwrap_or_else(|| Behavior::null())
    }
}

pub trait AsBehaviors {
    type Behaviors: Clone + Copy + 'static;
    type Rcs: 'static;
    type Values: Clone + 'static;
    fn as_behaviors(&self, rt: Rt) -> Self::Behaviors;
    fn get(rt: Rt, behaviors: &Self::Behaviors) -> Option<Self::Rcs>;
    fn try_value(rt: Rt, behaviors: &Self::Behaviors) -> Option<Self::Values>;
    fn attach_dyn(rt: Rt, behaviors: &Self::Behaviors, consumer: Consumer<dyn Data>);
    #[inline(always)]
    fn phantom(&self) -> PhantomData<Self> {
        PhantomData
    }
    /// A version of `attach_dyn` with phantom argument. Allows trait dispatch in methods that use
    /// `impl AsBehaviors` argument.
    #[inline(always)]
    fn impl_attach_dyn(
        _phantom: PhantomData<Self>,
        rt: Rt,
        behaviors: &Self::Behaviors,
        consumer: Consumer<dyn Data>,
    ) {
        Self::attach_dyn(rt, behaviors, consumer)
    }
}

impl AsBehaviors for () {
    type Behaviors = ();
    type Rcs = ();
    type Values = ();
    fn as_behaviors(&self, _: Rt) -> Self::Behaviors {
        ()
    }
    fn get(_: Rt, _: &Self::Behaviors) -> Option<Self::Rcs> {
        Some(())
    }
    fn try_value(_: Rt, _: &Self::Behaviors) -> Option<Self::Rcs> {
        Some(())
    }
    fn attach_dyn(_: Rt, _: &Self::Behaviors, _: Consumer<dyn Data>) {}
}

impl<T> AsBehaviors for T
where T: AsBehavior
{
    type Behaviors = (Behavior<T::Value>,);
    type Rcs = (BumpRc<T::Value>,);
    type Values = (T::Value,);
    fn as_behaviors(&self, rt: Rt) -> Self::Behaviors {
        (self.as_behavior(rt),)
    }
    fn get(rt: Rt, behaviors: &Self::Behaviors) -> Option<Self::Rcs> {
        Some((behaviors.0.get_in_rt(rt)?,))
    }
    fn try_value(rt: Rt, behaviors: &Self::Behaviors) -> Option<Self::Values> {
        Some((behaviors.0.try_value_in_rt(rt)?,))
    }
    fn attach_dyn(rt: Rt, behaviors: &Self::Behaviors, consumer: Consumer<dyn Data>) {
        consumer.attach_in_rt(rt, behaviors.0.as_stream());
    }
}

macro_rules! impl_as_behaviors {
    ($($a:ident)+) => {
        #[allow(non_snake_case)]
        impl<$($a),+> AsBehaviors for ($($a,)+)
        where
            $($a: AsBehavior,)+
        {
            type Behaviors = ($(Behavior<$a::Value>,)+);
            type Rcs = ($(BumpRc<$a::Value>,)+);
            type Values = ($($a::Value,)+);
            fn as_behaviors(&self, rt: Rt) -> Self::Behaviors {
                let ($($a,)+) = self;
                ($($a.as_behavior(rt),)+)
            }
            fn get(rt: Rt, behaviors: &Self::Behaviors) -> Option<Self::Rcs> {
                let ($($a,)+) = behaviors;
                let borrow = rt.0.behavior_values.borrow();
                $(let $a = $a.get_from_map(&*borrow)?;)+
                Some(($($a.clone(),)+))
            }
            fn try_value(rt: Rt, behaviors: &Self::Behaviors) -> Option<Self::Values> {
                let ($($a,)+) = behaviors;
                let borrow = rt.0.behavior_values.borrow();
                $(let $a = $a.get_from_map(&*borrow)?;)+
                Some(($((&**$a).clone(),)+))
            }
            fn attach_dyn(rt: Rt, behaviors: &Self::Behaviors, consumer: Consumer<dyn Data>) {
                let ($($a,)+) = behaviors;
                $(consumer.attach_in_rt(rt, $a.as_stream());)+
            }
        }
    };
}
crate::tuple::impl_tuples!(f = [[impl_as_behaviors]]);

/// ===================
/// === OpaqueBrand ===
/// ===================

// Safety: Branded type must be used only once to create an `EraseToken` replacing a single generic
// type parameter. Use [`make_erased!`] to create an `EraseToken` of single generic parameter.
pub(crate) unsafe trait OpaqueBrand {}

pub(crate) struct OpaqueBranded<Brand: OpaqueBrand>(PhantomData<*mut Brand>, dyn Data);

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Copy(bound = ""))]
pub(crate) struct EraseToken<T: ?Sized, E: OpaqueBrand>(EventTy<T>, EventTy<OpaqueBranded<E>>);

impl<T: ?Sized, E: OpaqueBrand> EraseToken<T, E> {
    pub(crate) fn new() -> EraseToken<T, E> {
        EraseToken(EventTy(PhantomData), EventTy(PhantomData))
    }
}

impl<T: ?Sized + Data> Stream<T> {
    /// Convert this stream into a type-erased stream. Can be useful to avoid excessive
    /// monomorphization, when the actual type of the event doesn't have to be statically known.
    /// Use `reverse_erase` on a stream to convert it back.
    #[inline(always)]
    pub(crate) fn erase<E: OpaqueBrand>(self, tok: EraseToken<T, E>) -> Stream<OpaqueBranded<E>> {
        Stream { id: self.id, output_ty: tok.1 }
    }
}

impl<Brand: OpaqueBrand> Stream<OpaqueBranded<Brand>> {
    #[inline(always)]
    pub(crate) fn recover_erased<T: ?Sized>(&self, tok: EraseToken<T, Brand>) -> Stream<T> {
        Stream { id: self.id, output_ty: tok.0 }
    }
}

impl<T: ?Sized> Emitter<T, KindStream> {
    /// Convert this emitter into a type-erased emitter. Can be useful to avoid excessive
    /// monomorphization, when the actual type of the event doesn't have to be statically known.
    /// Use `reverse_erase` on a stream to convert it back.
    #[inline(always)]
    pub(crate) fn erase<Brand: OpaqueBrand>(
        self,
        tok: EraseToken<T, Brand>,
    ) -> Emitter<OpaqueBranded<Brand>> {
        Emitter { id: self.id, output_ty: tok.1, kind: self.kind }
    }
}

impl<Brand: OpaqueBrand> Emitter<OpaqueBranded<Brand>> {
    #[inline(always)]
    pub(crate) fn recover_erased<T: ?Sized>(&self, tok: EraseToken<T, Brand>) -> Emitter<T> {
        Emitter { id: self.id, output_ty: tok.0, kind: self.kind }
    }
}

macro_rules! make_erased {
    ($event_ty:ty) => {{
        enum ErasedBrand {}
        unsafe impl OpaqueBrand for ErasedBrand {}
        // Safety:
        // Type defined in macro cannot be named in user code. All erased tokens created using this
        // brand will be of type $event_ty.
        EraseToken::<$event_ty, ErasedBrand>::new()
    }};
}
pub(crate) use make_erased;
