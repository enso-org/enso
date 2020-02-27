//! This module defines abstraction for FRP data types.

use crate::prelude::*;
use crate::NodeAsTraitObjectForData;
use crate::AnyEventConsumer;
use crate::NodeWithAnyOutput;


// =============
// === Value ===
// =============

alias! {
    /// Abstraction for a value carried by the data sent between FRP nodes.
    Value = { Clone + Debug + Default + 'static }
}

/// Trait for every FRP data which contains valid FRP value.
pub trait KnownValue : HasContent {
    /// The raw value of the data.
    fn value(&self) -> Content<Self>;
}



// ============
// === Data ===
// ============

// === Types ===

alias! {
    /// Data is information sent between FRP nodes. There are two possible data types:
    /// `BehaviorData` and `EventData`.
    Data = { Value + DebugWrapper + NodeAsTraitObjectForData + PhantomInto<DataType> }
}


/// A newtype containing a value of an event.
#[derive(Clone,Copy,Debug,Default)]
pub struct EventData<T>(pub T);

/// A newtype containing a value of a behavior.
#[derive(Clone,Copy,Debug,Default)]
pub struct BehaviorData<T>(pub T);

/// Alias to `Wrapper` with the inner type being `Debug`.
pub trait DebugWrapper = Wrapper where Content<Self> : Default + Debug;


// === DataType ===

/// A value-level information about the data type.
#[derive(Clone,Debug,Copy)]
#[allow(missing_docs)]
pub enum DataType {Event,Behavior}

impls!{[T] PhantomFrom<EventData<T>>    for DataType { Self::Event    }}
impls!{[T] PhantomFrom<BehaviorData<T>> for DataType { Self::Behavior }}


// === Instances ===

impl<T:Clone> KnownValue for EventData<T> {
    fn value(&self) -> T {
        self.content().clone()
    }
}

impl<T:Clone> KnownValue for BehaviorData<T> {
    fn value(&self) -> T {
        self.content().clone()
    }
}


// === Wrappers ===

impl<T> HasContent for EventData<T> { type Content = T; }
impl<T> Wrap       for EventData<T> { fn wrap    (t:T)   -> Self { EventData(t) } }
impl<T> ContentRef for EventData<T> { fn content (&self) -> &T   { &self.0 } }

impl<T> HasContent for BehaviorData<T> { type Content = T; }
impl<T> Wrap       for BehaviorData<T> { fn wrap    (t:T)   -> Self { BehaviorData(t) } }
impl<T> ContentRef for BehaviorData<T> { fn content (&self) -> &T   { &self.0 } }



// =============
// === Input ===
// =============

/// Event input associated type. Please note that FRP nodes can have maximum one event input.
/// In such a case this trait points to it.
pub trait KnownEventInput {
    /// The event input type.
    type EventInput : Data;
}

/// Event input accessor.
pub type EventInput<T> = <T as KnownEventInput>::EventInput;

/// Provides a list of all inputs to a node.
pub trait HasInputs {
    /// Accessor.
    fn inputs(&self) -> Vec<NodeWithAnyOutput>;
}

impl<T> HasInputs for T
where T:ContentRef, Content<T>:HasInputs {
    fn inputs(&self) -> Vec<NodeWithAnyOutput> {
        self.content().inputs()
    }
}



// ==============
// === Output ===
// ==============

// === Definition ===

/// Each FRP node has a single node, which type is described by this trait.
pub trait KnownOutput {
    /// The output type.
    type Output : Data;
}

/// Node output accessor.
pub type Output<T> = <T as KnownOutput>::Output;

impl<T:?Sized+KnownOutput> KnownOutput for Rc<T>
where Output<T> : Data {
    type Output = Output<T>;
}



// === Traits ===

/// Trait for nodes which can register an event target.
pub trait HasEventTargets : KnownOutput {
    /// Registers a new event target.
    fn add_event_target(&self, target:AnyEventConsumer<Output<Self>>);
}

/// Trait for nodes which remember the current value.
pub trait HasCurrentValue : KnownOutput {
    /// Gets the current value of the node.
    fn current_value(&self) -> Content<Output<Self>>;
}


// === KnownOutputType ===

/// Value-level information about the node output type. Used mainly for debugging purposes.
#[allow(missing_docs)]
pub trait KnownOutputType {
    fn output_type            (&self) -> DataType;
    fn output_type_value_name (&self) -> String;
}

impl<T:KnownOutput> KnownOutputType for T
where Output<Self> : Data {
    fn output_type(&self) -> DataType {
        PhantomData::<Output<Self>>.into()
    }

    fn output_type_value_name(&self) -> String {
        let qual  = type_name::<Output<Self>>();
        let param = qual.split('<').skip(1).collect::<String>();
        let param = &param[0..param.len()-1];
        let param = param.rsplit("::").collect::<Vec<_>>()[0];
        param.into()
    }
}
