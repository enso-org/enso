//! Root module for FRP Dynamic node types. The Dynamic type is a generalization of Event and
//! Behavior and is very easy to work with. You should use this type in most (all?) cases in your
//! FRP flows.

use crate::prelude::*;

use crate::data::*;
use crate::node::*;
use crate::nodes::prim::*;
use crate::nodes::lambda::*;



// ======================
// === RefinedDynamic ===
// ======================

/// Similar to `Dynamic` but with a known type of the `event` component. In most cases using
/// `Dynamic` is just fine. Sometimes however, you want to use a non-generic utilities of nodes,
/// like initializing a recursive one. By using `RefinedDynamic` you do not lose the information
/// about the specific shape of the node and you can access all of its methods directly.
#[derive(Debug,Derivative)]
#[derivative(Clone(bound="Event:Clone"))]
pub struct RefinedDynamic<Event>
    where Event                  : KnownOutput,
          Output<Event>          : HasContent,
          Content<Output<Event>> : Value {
    /// The underlying dynamic.
    pub dynamic : Dynamic<Content<Output<Event>>>,
    /// The event with a known type. This is the same event as `dynamic.event`.
    pub event : Event,
}

impl<Event> Deref for RefinedDynamic<Event>
    where Event                  : KnownOutput,
          Output<Event>          : HasContent,
          Content<Output<Event>> : Value {
    type Target = Dynamic<Content<Output<Event>>>;
    fn deref(&self) -> &Self::Target {
        &self.dynamic
    }
}

impl<E> From<&E> for RefinedDynamic<E>
    where E                  : CloneRef + KnownOutput,
          for <'t> &'t E     : Into<Event<Content<Output<E>>>>,
          Output<E>          : HasContent,
          Content<Output<E>> : Value {
    fn from(event:&E) -> Self {
        let event2  = event.into();
        let event   = event.clone_ref();
        let dynamic = event2.into();
        Self {event,dynamic}
    }
}

impl<Out:Value> RefinedDynamic<Recursive<EventData<Out>>> {
    /// Initialize the recursive `Dynamic` with a value. You need to perform this operation before
    /// running the FRP graph.
    pub fn initialize(&self, target:&Dynamic<Out>) {
        self.event.initialize(&target.event);
        self.event.set_display_id(target.event.display_id());
        self.behavior.set_display_id(target.event.display_id());
    }
}



// ===============
// === Dynamic ===
// ===============

/// The `Dynamic` type is an `Event` with an associated `Behavior`. You can assume that the
/// behavior just always holds the last event value.
#[derive(CloneRef,Derivative)]
#[derivative(Clone(bound=""))]
pub struct Dynamic<Out:Value> {
    /// The `Event` component.
    pub event : Event<Out>,
    /// The `Behavior` component.
    pub behavior : Behavior<Out>,
}

impl<Out:Value> Debug for Dynamic<Out> {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Dynamic")
    }
}


// === Constructors ===

impl<Out:Value> Dynamic<Out> {
    /// Constructor.
    pub fn new<E,B>(event:E, behavior:B) -> Self
        where E:Into<Event<Out>>, B:Into<Behavior<Out>> {
        let event    = event.into();
        let behavior = behavior.into();
        Self {event,behavior}
    }

    /// Creates a new FRP source node.
    pub fn source<Label>(label:Label) -> Self
        where Label : Into<CowString> {
        let event = Source::<EventData<Out>>::new_named(label);
        (&event).into()
    }

    /// Create a new node which will be a placeholder (reference) to another node. Please note that
    /// this node has to be initialized before the FRP network is run.
    pub fn recursive<Label>(label:Label) -> RefinedDynamic<Recursive<EventData<Out>>>
        where Label : Into<CowString> {
        (&Recursive::<EventData<Out>>::new_named(label)).into()
    }
}


// === Modifiers ===

impl<Out:Value> Dynamic<Out> {
    /// Create a new node which drops the incoming event and emits a new event with the constant
    /// value.
    pub fn constant<Label,T>(&self, label:Label, value:T) -> Dynamic<T>
        where Label:Into<CowString>, T:Value {
        self.map(label,move |_| value.clone())
    }

    /// Creates a new node which merges two event streams. The output event will be emitted
    /// whenever one of the streams emit an event.
    pub fn merge<Label>(&self, label:Label, that:&Dynamic<Out>) -> Self
        where Label:Into<CowString> {
        (&Merge::new_named(label,&self.event,&that.event)).into()
    }

    /// Creates a new node which emits `true`, `false`, `true`, `false`, ... on every incoming
    /// event.
    pub fn toggle<Label>(&self, label:Label) -> Dynamic<bool>
        where Label:Into<CowString> {
        (&Toggle::new_named(label,&self.event)).into()
    }

    /// Remembers the value and emits the last one.
    pub fn previous<Label,T>(&self, label:Label) -> Self
        where Label : Into<CowString>,
              T     : Value {
        (&Previous::new_named(label,&self.event)).into()
    }

    /// Creates a new node which passes the incoming event only if its second input is `true`.
    pub fn gate<Label>(&self, label:Label, that:&Dynamic<bool>) -> Self
        where Label:Into<CowString> {
        (&Gate::new_named(label,that,self)).into()
    }

    /// Creates a node which samples this behavior on every incoming argument event. The incoming
    /// event is dropped and a new event with the behavior value is emitted.
    pub fn sample<Label,T>(&self, label:Label, that:&Dynamic<T>) -> Self
        where Label : Into<CowString>,
              T     : Value {
        (&Sample::new_named(label,&self.behavior,that)).into()
    }

    /// Creates a node which maps the current value with the provided lambda. This is one of the
    /// most powerful utilities, however, you should try not to use it too often. The reason is that
    /// it also makes optimizations impossible, as lambdas are like "black-boxes" for the FRP
    /// engine.
    pub fn map<Label,F,R>(&self, label:Label, f:F) -> Dynamic<R>
        where Label : Into<CowString>,
              R     : Value,
              F     : 'static + Fn(&Out) -> R {
        (&Lambda::new_named(label,&self.event,f)).into()
    }

    /// Creates a node which maps the current value with the provided lambda. This is one of the
    /// most powerful utilities, however, you should try not to use it too often. The reason is that
    /// it also makes optimizations impossible, as lambdas are like "black-boxes" for the FRP
    /// engine.
    pub fn map2<Label,T,F,R>(&self, label:Label, that:&Dynamic<T>, f:F) -> Dynamic<R>
        where Label : Into<CowString>,
              T     : Value,
              R     : Value,
              F     : 'static + Fn(&Out,&T) -> R {
        (&Lambda2::new_named(label,&self.event,that,f)).into()
    }
}


// === Debug ===

impl<Out:Value+Eq> Dynamic<Out> {
    /// Creates a new node which passes the incoming event trough and panics if it was not equal to
    /// the given value.
    pub fn assert_eq<Label>(&self, label:Label) -> RefinedDynamic<AssertEq<EventData<Out>>>
        where Label:Into<CowString> {
        (&AssertEq::new_named(label,&self.event)).into()
    }
}



// === Instances ===

impl<Out:Value, T:Into<Event<Out>>> From<T> for Dynamic<Out> {
    fn from(t:T) -> Self {
        let event    = t.into();
        let behavior = Hold :: new_named(event.label(),&event);
        behavior.set_display_id(event.display_id());
        let event    = (&event).into();
        let behavior = (&behavior).into();
        Dynamic {event,behavior}
    }
}

impl<Out:Value> From<&Dynamic<Out>> for Event<Out> {
    fn from(t:&Dynamic<Out>) -> Self {
        t.event.clone_ref()
    }
}

impl<Out:Value> From<&Dynamic<Out>> for Behavior<Out> {
    fn from(t:&Dynamic<Out>) -> Self {
        t.behavior.clone_ref()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::frp_def;

    #[test]
    #[should_panic]
    fn assert() {
        frp_def! { source = source::<i32>() }
        frp_def! { check  = source.assert_eq() }
        check.event.expect(1);
        source.event.emit(2);
    }

    #[test]
    fn constant() {
        frp_def! { source   = source::<i32>() }
        frp_def! { constant = source.constant(7) }
        frp_def! { check    = constant.assert_eq() }
        check.event.expect(7);
        source.event.emit(0);
        source.event.emit(7);
        source.event.emit(1);
        assert_eq!(check.event.success_count(),3);
    }

    #[test]
    fn merge() {
        frp_def! { source1 = source::<i32>() }
        frp_def! { source2 = source::<i32>() }
        frp_def! { merge   = source1.merge(&source2) }
        frp_def! { check   = merge.assert_eq() }
        check.event.expect(1);
        source1.event.emit(1);
        source2.event.emit(1);
        check.event.expect(2);
        source1.event.emit(2);
        source2.event.emit(2);
        assert_eq!(check.event.success_count(),4);
    }

    #[test]
    fn toggle() {
        frp_def! { source = source::<i32>() }
        frp_def! { toggle = source.toggle() }
        frp_def! { check  = toggle.assert_eq() }
        check.event.expect(true);
        source.event.emit(0);
        check.event.expect(false);
        source.event.emit(0);
        check.event.expect(true);
        source.event.emit(0);
        assert_eq!(check.event.success_count(),3);
    }

    #[test]
    fn gate() {
        frp_def! { source1 = source::<i32>() }
        frp_def! { source2 = source::<bool>() }
        frp_def! { gate    = source1.gate(&source2) }
        frp_def! { check   = gate.assert_eq() }
        source1.event.emit(0);
        assert_eq!(check.event.success_count(),0);
        source2.event.emit(true);
        source1.event.emit(0);
        assert_eq!(check.event.success_count(),1);
        source2.event.emit(false);
        source1.event.emit(0);
        assert_eq!(check.event.success_count(),1);
    }

    #[test]
    fn sample() {
        frp_def! { source1 = source::<i32>() }
        frp_def! { source2 = source::<i32>() }
        frp_def! { sample  = source1.sample(&source2) }
        frp_def! { check   = sample.assert_eq() }
        check.event.expect(1);
        source1.event.emit(1);
        assert_eq!(check.event.success_count(),0);
        source2.event.emit(0);
        assert_eq!(check.event.success_count(),1);
        source2.event.emit(0);
        assert_eq!(check.event.success_count(),2);
    }

    #[test]
    fn map() {
        frp_def! { source = source::<i32>() }
        frp_def! { map    = source.map(|t| {t+1}) }
        frp_def! { check  = map.assert_eq() }
        check.event.expect(1);
        source.event.emit(0);
        check.event.expect(2);
        source.event.emit(1);
        assert_eq!(check.event.success_count(),2);
    }
}
