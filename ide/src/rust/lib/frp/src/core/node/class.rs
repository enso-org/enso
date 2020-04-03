//! Defines FRP node related abstractions.

use crate::prelude::*;
use crate::debug::*;
use crate::node::id::*;
use crate::node::label::*;
use crate::data::*;



// =============
// === Types ===
// =============

alias! { no_docs
    NodeReflection         = { HasInputs + HasLabel + KnownOutputType }
    NodeDebug              = { Debug + GraphvizBuilder }
    AnyNode                = { NodeDebug + NodeReflection + HasId + HasDisplayId }
    AnyNodeWithKnownOutput = { AnyNode + KnownOutput }
    AnyBehaviorNode        = { AnyNodeWithKnownOutput + HasCurrentValue  }
    AnyEventNode           = { AnyNodeWithKnownOutput + HasEventTargets + EventEmitter }
}



// =========================
// === NodeAsTraitObject ===
// =========================

/// Type level association between FRP data type and a node's trait object. The associated type
/// differs depending on whether it is an event or behavior node, as they provide different APIs.
/// For example, behaviors allow lookup for the current value, which does not make sense in case
/// of events.
pub trait NodeAsTraitObjectForData {
    /// The trait object representing the node.
    type NodeAsTraitObject: AnyNodeWithKnownOutput + CloneRef;
}

/// Accessor. See docs of `NodeAsTraitObjectForData` to learn more.
pub type NodeAsTraitObject<T> = <T as NodeAsTraitObjectForData>::NodeAsTraitObject;


// === EventDynNode ===

/// Newtype wrapper for any event node.
#[derive(Debug,Derivative,Shrinkwrap)]
#[derivative(Clone(bound=""))]
pub struct EventDynNode<Out> {
    rc: Rc<dyn AnyEventNode<Output=EventData<Out>>>,
}

impl<Out:Value> NodeAsTraitObjectForData for EventData<Out> {
    type NodeAsTraitObject = EventDynNode<Out>;
}

impl<Out> ContentRef for EventDynNode<Out> { fn content(&self) -> &Self::Content { self.deref() } }
impl<Out> CloneRef   for EventDynNode<Out> {}
impl<Out> HasContent for EventDynNode<Out> {
    // TODO: Simplify after fixing https://github.com/rust-lang/rust/issues/68776
    type Content = <EventDynNode<Out> as Deref>::Target;
}

impl<Out:Value> KnownOutput for EventDynNode<Out> {
    type Output = EventData<Out>;
}


// === AnyBehaviorNode ===

/// Newtype wrapper for any behavior node.
#[derive(Debug,Derivative,Shrinkwrap)]
#[derivative(Clone(bound=""))]
pub struct BehaviorDynNode<Out> {
    rc: Rc<dyn AnyBehaviorNode<Output=BehaviorData<Out>>>,
}

impl<Out:Value> NodeAsTraitObjectForData for BehaviorData<Out> {
    type NodeAsTraitObject = BehaviorDynNode<Out>;
}

impl<Out> ContentRef for BehaviorDynNode<Out> { fn content(&self) -> &Self::Content { self.deref() } }
impl<Out> CloneRef   for BehaviorDynNode<Out> {}
impl<Out> HasContent for BehaviorDynNode<Out> {
    // TODO: Simplify after fixing https://github.com/rust-lang/rust/issues/68776
    type Content = <BehaviorDynNode<Out> as Deref>::Target;
}

impl<Out:Value> KnownOutput for BehaviorDynNode<Out> {
    type Output = BehaviorData<Out>;
}



// ============
// === Node ===
// ============

// === Types ===

/// Events represents discrete point in time. At a specific time, there can be at most one event.
/// Typical examples are mouse clicks, keyboard presses, status changes of the network connection.
pub type Event<T> = Node<EventData<T>>;

/// Behaviours represent a value over time that is always available. For example, mouse coordinates
/// vary in time and always have some value.
pub type Behavior <T> = Node<BehaviorData<T>>;


// === Definition ===

/// Node is used as a common types for frp operations. For example, `Event<T>` is just an alias to
/// `Node<EventData<T>>`.
#[derive(CloneRef,Derivative)]
#[derivative(Clone(bound=""))]
#[derivative(Debug(bound=""))]
pub struct Node<Out:NodeAsTraitObjectForData> {
    storage: NodeAsTraitObject<Out>,
}

impl<Out:Data> Node<Out> {
    /// Constructor.
    pub fn new(storage:NodeAsTraitObject<Out>) -> Self {
        Self {storage}
    }
}


// === Type Deps ===

impl<Out:Data> KnownOutput for Node<Out> { type Output  = Out; }
impl<Out:Data> HasContent  for Node<Out> { type Content = NodeAsTraitObject<Out>; }
impl<Out:Data> ContentRef  for Node<Out> {
    fn content(&self) -> &Self::Content {
        &self.storage
    }
}


// === Instances ===

impl<Out:Data> Deref for Node<Out> {
    type Target = NodeAsTraitObject<Out>;
    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}


// === Construction ===

impl<Out:Data> From<&Node<Out>> for Node<Out> {
    fn from(t:&Node<Out>) -> Self {
        t.clone_ref()
    }
}

impl<Storage,Out:Value>
From<&Storage> for Behavior<Out>
    where Storage : AnyBehaviorNode<Output=BehaviorData<Out>> + Clone + 'static {
    fn from(storage:&Storage) -> Self {
        Self::new(BehaviorDynNode{rc:Rc::new(storage.clone())})
    }
}

impl<Storage,Out:Value>
From<&Storage> for Event<Out>
    where Storage : AnyEventNode<Output=EventData<Out>> + Clone + 'static {
    fn from(storage:&Storage) -> Self {
        Self::new(EventDynNode{rc:Rc::new(storage.clone())})
    }
}


// === AddTarget ===

/// Abstraction for adding a target to a given node. Nodes which carry behaviors do not need to
/// perform any operation here, while event streams want to register the nodes they want to send
/// notifications to.
pub trait AddTarget<T> {
    /// Adds a node as a target of the current flow.
    fn add_target(&self,t:&T);
}

impl<S,T:Value> AddTarget<S> for Event<T>
    where for<'t> &'t S : Into<AnyEventConsumer<EventData<T>>> {
    fn add_target(&self,t:&S) {
        self.storage.add_event_target(t.into())
    }
}

impl<S,T:Value> AddTarget<S> for Behavior<T> {
    fn add_target(&self,_:&S) {}
}



// =========================
// === NodeWithAnyOutput ===
// =========================

/// A type for any possible node type. It hides the result type of a node, which makes it the most
/// generic node type out there.
#[derive(Debug,Shrinkwrap)]
pub struct NodeWithAnyOutput {
    rc: Rc<dyn AnyNode>,
}


// === Instances ===

impls! { [Out:Data+'static] From <&Node<Out>> for NodeWithAnyOutput { |t| t.clone_ref().into() } }
impls! { [Out:Data+'static] From  <Node<Out>> for NodeWithAnyOutput { |t| Self {rc:Rc::new(t)} } }

impl KnownOutputType for NodeWithAnyOutput {
    fn output_type(&self) -> DataType {
        self.rc.output_type()
    }

    fn output_type_value_name(&self) -> String {
        self.rc.output_type_value_name()
    }
}



// =====================
// === EventConsumer ===
// =====================

// === Definition ===

/// Abstraction for nodes which are able to consume events.
pub trait EventConsumer: KnownEventInput + Debug {
    /// Function called on every new received event.
    fn on_event(&self, input:&Content<Self::EventInput>);
}


// === AnyEventConsumer ===

/// Abstraction for any node which consumes events of a given type.
#[derive(Clone,Debug,Shrinkwrap)]
pub struct AnyEventConsumer<In> {
    raw: Rc<dyn EventConsumer<EventInput=In>>,
}

impl<In:Data> AnyEventConsumer<In> {
    /// Constructor.
    pub fn new<A:EventConsumer<EventInput=In>+'static>(a:A) -> Self {
        let raw = Rc::new(a);
        Self {raw}
    }
}

impl<T,In> From<&T> for AnyEventConsumer<In>
    where T  : EventConsumer<EventInput=In> + Clone + 'static,
          In : Data {
    fn from(t:&T) -> Self {
        Self::new(t.clone())
    }
}



// ====================
// === EventEmitter ===
// ====================

// === Definition ===

/// Abstraction for nodes which are able to emit events.
pub trait EventEmitter: KnownOutput {
    /// Function for emitting new events.
    fn emit_event(&self, event:&Content<Self::Output>);
}

impl<T> EventEmitter for T
    where T:ContentRef+KnownOutput, Content<T>:EventEmitter<Output=Output<Self>> {
    fn emit_event(&self, event:&Content<Self::Output>) {
        self.content().emit_event(event)
    }
}


// === EventEmitterPoly ===

/// Polymorphic version of `EventEmitter`. Please note that `EventEmitter` could not be implemented
/// this way as we want to use it in a trait object, so all its methods have to be monomorphic.
pub trait EventEmitterPoly : KnownOutput where Output<Self>:HasContent {
    /// Function for emitting new events.
    fn emit<E:ToRef<Content<Self::Output>>>(&self, event:E);
}

impl<T:EventEmitter> EventEmitterPoly for T {
    fn emit<E:ToRef<Content<Self::Output>>>(&self, event:E) {
        self.emit_event(event.to_ref())
    }
}
