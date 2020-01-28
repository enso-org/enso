//! This module implements an Functional Reactive Programming system. It is an advanced event
//! handling framework which allows describing events and actions by creating declarative event
//! flow diagrams.
//!
//! **WARNING**
//! Please note that this file is under heavy development so it may be missing proper docs here and
//! there and the API can drastically change day after day.

use crate::prelude::*;



// ==============
// === Macros ===
// ==============

macro_rules! alias {
    ($( $(#$meta:tt)* $name:ident = {$($tok:tt)*} )*) => {$(
        $(#$meta)*
        pub trait $name: $($tok)* {}
        impl<T:$($tok)*> $name for T {}
    )*}
}



// ===============
// === Wrapper ===
// ===============

/// Trait for objects which wrap values.
///
/// Please note that this implements safe wrappers, so the object - value relation must be
/// bijective.
pub trait Wrapper {

    /// The wrapped value type.
    type Content;

    /// Wraps the value and returns the wrapped type.
    fn wrap(t:Self::Content) -> Self;

    /// Unwraps this type to get the inner value.
    fn unwrap(&self) -> &Self::Content;
}

/// Accessor for the wrapped value.
pub type Unwrap<T> = <T as Wrapper>::Content;

/// Wraps the value and returns the wrapped type.
pub fn wrap<T:Wrapper>(t:T::Content) -> T {
    T::wrap(t)
}

/// Unwraps this type to get the inner value.
pub fn unwrap<T:Wrapper>(t:&T) -> &T::Content {
    T::unwrap(t)
}



// ===============
// === Message ===
// ===============

// === Types ===

alias! {
    /// Message is a data send between FRP nodes.
    /// There are two important message implementation – the `BehaviorMessage` and `EventMessage`.
    Message = { MessageValue + ValueWrapper + KnownNodeStorage }

    /// Abstraction for a value carried by a message.
    MessageValue = { Clone + Debug + Default + 'static }
}

/// Accessor to a value of a given message. For example, `Value<Behavior<i32>>` resolves to `i32`.
pub type Value<T> = Unwrap<T>;

/// Alias to `Wrapper` with the inner type being `Debug`.
pub trait ValueWrapper = Wrapper where Unwrap<Self>:Debug;


// === Definition ===

/// A newtype containing a value of an event.
#[derive(Clone,Copy,Debug,Default)]
pub struct EventMessage<T>(T);

/// A newtype containing a value of a behavior.
#[derive(Clone,Copy,Debug,Default)]
pub struct BehaviorMessage<T>(T);


// === API ===

impl<T:Clone> EventMessage<T> {
    /// Get the unwrapped value of this message.
    pub fn value(&self) -> T {
        self.unwrap().clone()
    }
}

impl<T:Clone> BehaviorMessage<T> {
    /// Get the unwrapped value of this message.
    pub fn value(&self) -> T {
        self.unwrap().clone()
    }
}


// === Wrappers ===

impl Wrapper for () {
    type Content = ();
    fn wrap   (_:())  -> Self {}
    fn unwrap (&self) -> &()  { self }
}

impl<T> Wrapper for EventMessage<T> {
    type Content = T;
    fn wrap   (t:T)   -> Self { EventMessage(t) }
    fn unwrap (&self) -> &T   { &self.0 }
}

impl<T> Wrapper for BehaviorMessage<T> {
    type Content = T;
    fn wrap   (t:T)   -> Self { BehaviorMessage(t) }
    fn unwrap (&self) -> &T   { &self.0 }
}



// ======================
// === Input / Output ===
// ======================

/// Event input associated type. Please note that FRP nodes can have maximum one event input.
/// In such a case this trait points to it.
pub trait KnownEventInput {
    /// The event input type.
    type EventInput : Message;
}

/// Event input accessor.
pub type EventInput<T> = <T as KnownEventInput>::EventInput;


/// Each FRP node has a single node, which type is described by this trait.
pub trait KnownOutput {
    /// The output type.
    type Output : Message;
}

/// Node output accessor.
pub type Output<T> = <T as KnownOutput>::Output;



// ===================
// === NodeStorage ===
// ===================

/// Type level abstraction for node internal storage.
pub trait KnownNodeStorage {
    /// The node storage type.
    type NodeStorage: CloneRef + Debug;
}

/// Internal node storage type accessor.
pub type NodeStorage<T> = <T as KnownNodeStorage>::NodeStorage;

impl KnownNodeStorage for () {
    type NodeStorage = ();
}


// === EventNodeStorage ===

/// Event node operations.
pub trait EventNodeStorage: KnownOutput + Debug {
    /// Registers a new event target. Whenever a new event arrives it will be transmitted to all
    /// registered targets.
    fn add_event_target(&self, target:AnyEventConsumer<Output<Self>>);
}

impl<Out> KnownNodeStorage for EventMessage<Out> {
    type NodeStorage = Rc<dyn EventNodeStorage<Output=EventMessage<Out>>>;
}


// === BehaviorNodeStorage ===

/// Behavior node operations.
pub trait BehaviorNodeStorage: KnownOutput + Debug {
    /// Returns the current value of the behavior.
    fn current_value(&self) -> Value<Output<Self>>;
}

impl<Out> KnownNodeStorage for BehaviorMessage<Out> {
    type NodeStorage = Rc<dyn BehaviorNodeStorage<Output=BehaviorMessage<Out>>>;
}



// ============
// === Node ===
// ============

// === Types ===

/// The type of any FRP node which produces event messages. Having a reference to a node is like
/// having a reference to network endpoint which transmits messages of a given type. Thus, it is a
/// nice mental simplification to think about it just like about an event (stream).
pub type Event<T> = Node<EventMessage<T>>;

/// The type of any FRP node which can be queried for behavior value. Having a reference to a node
/// is like having a reference to network endpoint which transmits messages of a given type. Thus,
/// it is a nice mental simplification to think about it just like about a behavior.
pub type Behavior <T> = Node<BehaviorMessage<T>>;


// === Definition ===

/// Node is used as a common types for frp operations. For example, `Event<T>` is just an alias to
/// `Node<EventMessage<T>>`.
#[derive(Debug)]
pub struct Node<Out:KnownNodeStorage> {
    storage: NodeStorage<Out>,
}

impl<Out:Message> Node<Out> {
    /// Constructor.
    pub fn new(storage:NodeStorage<Out>) -> Self {
        Self {storage}
    }
}


// === Instances ===

impl<Out:Message> KnownOutput for Node<Out> { type Output = Out; }

impl<Out:KnownNodeStorage> Deref for Node<Out> {
    type Target = NodeStorage<Out>;
    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}

impl<Out:KnownNodeStorage> Clone for Node<Out> {
    fn clone(&self) -> Self {
        let storage = self.storage.clone();
        Self {storage}
    }
}

impl<Out:KnownNodeStorage> CloneRef for Node<Out> {
    fn clone_ref(&self) -> Self {
        let storage = self.storage.clone_ref();
        Self {storage}
    }
}

impl<Out:KnownNodeStorage> From<&Node<Out>> for Node<Out> {
    fn from(t:&Node<Out>) -> Self {
        t.clone_ref()
    }
}


// === Construction ===

impl<Storage,Out> From<&Storage> for Node<BehaviorMessage<Out>>
where Storage : BehaviorNodeStorage<Output=BehaviorMessage<Out>> + Clone + 'static,
      Out     : MessageValue {
    fn from(storage:&Storage) -> Self {
        Self::new(Rc::new(storage.clone()))
    }
}


impl<Storage,Out> From<&Storage> for Node<EventMessage<Out>>
where Storage : EventNodeStorage<Output=EventMessage<Out>> + Clone + 'static,
      Out     : MessageValue {
    fn from(storage:&Storage) -> Self {
        Self::new(Rc::new(storage.clone()))
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

impl<S,T> AddTarget<S> for Node<EventMessage<T>>
    where for<'t> &'t S : Into<AnyEventConsumer<EventMessage<T>>> {
    fn add_target(&self,t:&S) {
        self.add_event_target(t.into())
    }
}

impl<S,T> AddTarget<S> for Node<BehaviorMessage<T>> {
    fn add_target(&self,_:&S) {}
}



// ===================
// === NodeWrapper ===
// ===================

// === NodeWrapper ===

/// `NodeWrapper` is an outer layer for every FRP node. For example, the `Source<Out>` node is just
/// an alias to `NodeWrapper<SourceData<Out>>`, where `SourceData` is it's internal representation.
/// This struct bundles each node with information about target edges. Although the edges are used
/// only to send events, they are bundled to every node type in order to keep the implementation
/// simple.
pub type NodeWrapper<Shape> = NodeWrapperTemplate<Shape,Output<Shape>>;

impl<Shape:KnownOutput> NodeWrapper<Shape> {
    /// Constructor.
    pub fn construct(shape:Shape) -> Self {
        let data = NodeWrapperTemplateData::construct(shape);
        let rc   = Rc::new(RefCell::new(data));
        Self {rc}
    }
}

impl<Shape:KnownOutput> NodeWrapper<Shape> {
    /// Sends an event to all the children.
    pub fn emit_event(&self, event:&Output<Shape>) {
        self.rc.borrow().targets.iter().for_each(|target| {
            target.on_event(event)
        })
    }
}

impl<Shape:KnownOutput + Debug>
EventNodeStorage for NodeWrapper<Shape>
    where Output<Self>:'static, Output<Shape>:Message {
    fn add_event_target(&self, target:AnyEventConsumer<Output<Self>>) {
        self.rc.borrow_mut().targets.push(target);
    }
}


impl<Shape:BehaviorNodeStorage + Debug>
BehaviorNodeStorage for NodeWrapper<Shape>
    where Output<Shape>:Message {
    fn current_value(&self) -> Value<Output<Self>> {
        self.rc.borrow().shape.current_value()
    }
}


// === NodeWrapperTemplate ===

/// Internal representation for `NodeWrapper`.
#[derive(Debug,Derivative)]
#[derivative(Default(bound="Shape:Default"))]
#[derivative(Clone(bound=""))]
pub struct NodeWrapperTemplate<Shape,Out> {
    rc: Rc<RefCell<NodeWrapperTemplateData<Shape,Out>>>,
}

impl<Shape,Out:Message> KnownOutput for NodeWrapperTemplate<Shape,Out> {
    type Output = Out;
}

impl<Shape:KnownEventInput,Out> KnownEventInput for NodeWrapperTemplate<Shape,Out>
    where Value<EventInput<Shape>> : Debug {
    type EventInput = EventInput<Shape>;
}

impl<Shape,Out> CloneRef for NodeWrapperTemplate<Shape,Out> {}



// === NodeWrapperTemplateData ===

/// Internal representation for `NodeWrapperTemplate`.
#[derive(Debug,Derivative)]
#[derivative(Default(bound="Shape:Default"))]
pub struct NodeWrapperTemplateData<Shape,Out> {
    shape   : Shape,
    targets : Vec<AnyEventConsumer<Out>>,
}

impl<Shape,Out> NodeWrapperTemplateData<Shape,Out> {
    /// Constructor.
    pub fn construct(shape:Shape) -> Self {
        let targets = default();
        Self {shape,targets}
    }
}



// =====================
// === EventConsumer ===
// =====================

// === Definition ===

/// Abstraction for nodes which are able to consume events.
pub trait EventConsumer: KnownEventInput + Debug {
    /// Function called on every new received event.
    fn on_event(&self, input:&Self::EventInput);
}


// === AnyEventConsumer ===

/// Abstraction for any node which consumes events of a given type.
#[derive(Clone,Debug,Shrinkwrap)]
pub struct AnyEventConsumer<In> {
    raw: Rc<dyn EventConsumer<EventInput=In>>,
}

impl<In:Message> AnyEventConsumer<In> {
    /// Constructor.
    pub fn new<A:EventConsumer<EventInput=In>+'static>(a:A) -> Self {
        let raw = Rc::new(a);
        Self {raw}
    }
}

impl<T,In> From<&T> for AnyEventConsumer<In>
where T  : EventConsumer<EventInput=In> + Clone + 'static,
      In : Message {
    fn from(t:&T) -> Self {
        Self::new(t.clone())
    }
}



// =========================
// === Inference Helpers ===
// =========================

/// Message product type-level inference guidance.
pub trait Infer<T> {
    /// Inference results.
    type Result;
}

/// Accessor for inferred type.
pub type Inferred<T,X> = <X as Infer<T>>::Result;


// === Rules ===

macro_rules! inference_rules {
    ($( $pat:tt => $result:ident )*) => {$(
        inference_rule! { $pat => $result }
    )*}

}

macro_rules! inference_rule {
    ( $t1:ident => $result:ident ) => {
        impl<X,T1> Infer <$t1<T1>> for X { type Result = $result<X>; }
    };

    ( ($t1:ident) => $result:ident ) => {
        impl<X,T1> Infer <$t1<T1>> for X { type Result = $result<X>; }
    };

    ( ($t1:ident, $t2:ident) => $result:ident ) => {
        impl<X,T1,T2> Infer <($t1<T1>,$t2<T2>)> for X { type Result = $result<X>; }
    };

    ( ($t1:ident, $t2:ident, $t3:ident) => $result:ident ) => {
        impl<X,T1,T2,T3> Infer <($t1<T1>,$t2<T2>,$t3<T3>)> for X { type Result = $result<X>; }
    };
}

inference_rules! {
    EventMessage    => EventMessage
    BehaviorMessage => BehaviorMessage

    (EventMessage    , EventMessage   ) => EventMessage
    (BehaviorMessage , EventMessage   ) => EventMessage
    (EventMessage    , BehaviorMessage) => EventMessage
    (BehaviorMessage , BehaviorMessage) => EventMessage
}



// =================================================================================================
// === FRP Nodes ===================================================================================
// =================================================================================================

// ==============
// === Source ===
// ==============

// === Storage ===

/// Internal source storage accessor.
pub type SourceStorage<T> = <T as KnownSourceStorage>::SourceStorage;

/// Internal source storage type.
pub trait KnownSourceStorage {
    /// The result type.
    type SourceStorage : Default;
}

impl<T>         KnownSourceStorage for EventMessage   <T> {type SourceStorage = ();}
impl<T:Default> KnownSourceStorage for BehaviorMessage<T> {type SourceStorage = BehaviorMessage<T>;}


// === Definition ===

/// Source is a begin point in the FRP network. It is able to emit events or initialize behaviors.
type Source<Out> = NodeWrapper<SourceData<Out>>;

/// Internal definition of the source FRP node.
#[derive(Derivative)]
#[derivative(Default (bound="SourceStorage<Out>:Default"))]
#[derivative(Debug   (bound="SourceStorage<Out>:Debug"))]
pub struct SourceData<Out:KnownSourceStorage> {
    storage: SourceStorage<Out>
}

impl<Out> KnownOutput for SourceData<Out>
where Out : KnownSourceStorage + Message {
    type Output = Out;
}

impl<Out> Source<Out>
where Out : KnownSourceStorage + Message {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

impl<Out> BehaviorNodeStorage for SourceData<BehaviorMessage<Out>>
where Out : MessageValue {
    fn current_value(&self) -> Out {
        self.storage.value()
    }
}



// ==============
// === Lambda ===
// ==============

/// Transforms input data with the provided function. Lambda accepts a single input and outputs
/// message of the same type as the input message.
pub type Lambda<In,Out> = NodeWrapper<LambdaShape<In,Out>>;

/// Internal representation of `Lambda`.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct LambdaShape<In:Message,Out:Message> {
    source : Node<In>,
    #[derivative(Debug="ignore")]
    func : Rc<dyn Fn(&Value<In>) -> Out>,
}


// === Instances ===

impl<In:Message,Out:Message> KnownEventInput for LambdaShape<In,Out> { type EventInput = In;  }
impl<In:Message,Out:Message> KnownOutput     for LambdaShape<In,Out> { type Output     = Out; }


// === Constructor ===

/// Constructor abstraction. Used only to satisfy Rust type system.
pub trait LambdaNew<Source,Func> {
    /// Constructor.
    fn new(source:Source,f:Func) -> Self;
}

impl<In,OutVal,Func,Source> LambdaNew<Source,Func> for Lambda<In,Inferred<In,OutVal>>
where In       : Message,
      OutVal   : Infer<In>,
      Func     : 'static + Fn(&Value<In>) -> OutVal,
      Source   : Into<Node<In>>,
      Node<In> : AddTarget<Self>,
      Inferred<In,OutVal> : Message<Content=OutVal> {
    fn new (source:Source, f:Func) -> Self {
        let source     = source.into();
        let source_ref = source.clone();
        let shape      = LambdaShape::new(source,f);
        let this       = Self::construct(shape);
        source_ref.add_target(&this);
        this
    }
}

impl<In:Message,Out:Message> LambdaShape<In,Out> {
    /// Constructor.
    pub fn new<Func,Source>(source:Source, f:Func) -> Self
    where Func   : 'static + Fn(&Value<In>) -> Value<Out>,
          Source : Into<Node<In>> {
        let source = source.into();
        let func   = Rc::new(move |t:&Value<In>| { wrap(f(t)) });
        Self {source,func}
    }
}

impl<In:Message,Out:Message> EventConsumer for Lambda<In,Out> {
    fn on_event(&self, input:&Self::EventInput) {
        println!("GOT {:?}",input);
        let output = (self.rc.borrow().shape.func)(unwrap(input));
        self.emit_event(&output);
    }
}



// ===============
// === Lambda2 ===
// ===============

/// Transforms input data with the provided function. `Lambda2` accepts two inputs. If at least one
/// of the inputs was event, the output message will be event as well. In case both inputs were
/// behavior, a new behavior will be produced.
pub type Lambda2<In1,In2,Out> = NodeWrapper<Lambda2Shape<In1,In2,Out>>;

/// Internal representation for `Lambda2`.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Lambda2Shape<In1:Message,In2:Message,Out:Message> {
    source1 : Node<In1>,
    source2 : Node<In2>,
    #[derivative(Debug="ignore")]
    func : Rc<dyn Fn(&Value<In1>,&Value<In2>) -> Out>,
}

impl<In1:Message,In2:Message,Out:Message>
Lambda2Shape<In1,In2,Out> {
    /// Constructor.
    pub fn new
    < F:'static + Fn(&Value<In1>,&Value<In2>) -> Value<Out>
    , Source1:Into<Node<In1>>
    , Source2:Into<Node<In2>>
    >
    (source1:Source1, source2:Source2, f:F) -> Self {
        let source1 = source1.into();
        let source2 = source2.into();
        let func    = Rc::new(move |a:&Value<In1>,b:&Value<In2>| { wrap(f(a,b)) });
        Self {source1,source2,func}
    }
}


// === Instances ===

impl<In1,In2,Out> KnownEventInput for Lambda2Shape<EventMessage<In1>,BehaviorMessage<In2>,Out>
where In1:MessageValue,In2:MessageValue,Out:Message {
    type EventInput = EventMessage<In1>;
}

impl<In1,In2,Out> KnownEventInput for Lambda2Shape<BehaviorMessage<In1>,EventMessage<In2>,Out>
where In1:MessageValue,In2:MessageValue,Out:Message {
    type EventInput = EventMessage<In2>;
}

impl<In1,In2,Out> KnownOutput for Lambda2Shape<In1,In2,Out>
where In1:Message,In2:Message,Out:Message {
    type Output = Out;
}


// === Construction ===

/// Constructor abstraction. Used only to satisfy Rust type system.
pub trait Lambda2New<Source1,Source2,Function> {
    /// Constructor.
    fn new(source:Source1, source2:Source2,f:Function) -> Self;
}

impl<In1,In2,OutVal,Source1,Source2,Function>
Lambda2New<Source1,Source2,Function> for Lambda2<In1,In2,Inferred<(In1,In2),OutVal>>
where In1       :  Message,
      In2       :  Message,
      OutVal    :  Infer<(In1,In2)>,
      Source1   :  Into<Node<In1>>,
      Source2   :  Into<Node<In2>>,
      Function  :  'static + Fn(&Value<In1>,&Value<In2>) -> OutVal,
      Node<In1> : AddTarget<Self>,
      Node<In2> : AddTarget<Self>,
      Inferred<(In1,In2),OutVal> : Message<Content=OutVal> {
    fn new (source1:Source1, source2:Source2, f:Function) -> Self {
        let source1     = source1.into();
        let source2     = source2.into();
        let source1_ref = source1.clone();
        let source2_ref = source2.clone();
        let shape       = Lambda2Shape::new(source1,source2,f);
        let this        = Self::construct(shape);
        source1_ref.add_target(&this);
        source2_ref.add_target(&this);
        this
    }
}

impl<In1,In2,Out> EventConsumer for Lambda2<EventMessage<In1>,BehaviorMessage<In2>,Out>
where In1:MessageValue, In2:MessageValue, Out:Message {
    fn on_event(&self, event:&Self::EventInput) {
        println!("GOT {:?}",event);
        let value2 = self.rc.borrow().shape.source2.current_value();
        let output = (self.rc.borrow().shape.func)(&event.value(),&value2);
        self.emit_event(&output);
    }
}

impl<In1,In2,Out> EventConsumer for Lambda2<BehaviorMessage<In1>,EventMessage<In2>,Out>
where In1:MessageValue, In2:MessageValue, Out:Message {
    fn on_event(&self, event:&Self::EventInput) {
        println!("GOT {:?}",event);
        let value1 = self.rc.borrow().shape.source1.current_value();
        let output = (self.rc.borrow().shape.func)(&value1,&event.value());
        self.emit_event(&output);
    }
}



// =================================================================================================
// === Examples ====================================================================================
// =================================================================================================

#[allow(missing_docs)]
mod tests {
    use super::*;

    // ================
    // === Position ===
    // ================

    #[derive(Clone,Copy,Debug,Default)]
    pub struct Position {
        x:i32,
        y:i32,
    }

    impl Position {
        pub fn new(x:i32, y:i32) -> Self {
            Self {x,y}
        }
    }



    // ============
    // === Test ===
    // ============

    #[allow(unused_variables)]
    pub fn test () {
        println!("\n\n\n--- FRP ---\n");


        let mouse_position = Source::<BehaviorMessage<Position>>::new();

        let e1 = Source::<EventMessage<i32>>::new();

        let n1  = Lambda::new(&e1, |i| { i+1 });
        let nn1: Event<i32> = (&n1).into();
        let n2 = Lambda::new(&nn1, |i| { i*2 });

        let n3: Lambda<BehaviorMessage<Position>, BehaviorMessage<Position>> =
            Lambda::new(&mouse_position, |t| { *t });


        let n3 = Lambda2::new(&n1,&mouse_position, |e,b| { *e });

        //  let n3 = Lambda2::new(&n1,&n2,|i,j| {i * j});

        e1.emit_event(&EventMessage(7));

    }
}
pub use tests::*;
