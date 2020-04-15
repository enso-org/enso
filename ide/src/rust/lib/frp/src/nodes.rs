//! Definition of all FRP stream nodes.
//!
//! Please note that the documentation is provided for methods of `Network`, as this is considered
//! to be the public API. The same documentation applies to node definitions below.
#![allow(missing_docs)]
#![allow(clippy::type_complexity)]

use crate::prelude::*;

use crate::data::watch;
use crate::network::*;
use crate::node::*;
use crate::stream::EventOutput;
use crate::stream::Stream;
use crate::stream;



// ========================
// === Network Node API ===
// ========================

impl Network {
    /// Begin point in the FRP network. It does not accept inputs, but it is able to emit events.
    /// Often it is used to indicate that something happened, like a button was pressed. In such a
    /// case its type parameter is set to an empty tuple.
    pub fn source<T:Data>(&self, label:Label) -> Source<T> {
        self.register_raw(OwnedSource::new(label))
    }

    /// Begin point in the FRP network. Specialized version of `source`.
    pub fn source_(&self, label:Label) -> Source {
        self.register_raw(OwnedSource::new(label))
    }

    /// Remember the last event value and allow sampling it anytime.
    pub fn sampler<S,T>(&self, label:Label, source:&S) -> Sampler<T>
    where S:EventOutput<Output=T>, T:Data {
        self.register_raw(OwnedSampler::new(label,source))
    }

    /// Print the incoming events to console and pass them to output.
    pub fn trace<S,T>(&self, label:Label, source:&S) -> Stream<T>
    where S:EventOutput<Output=T>, T:Data {
        self.register(OwnedTrace::new(label,source))
    }

    /// Emits `true`, `false`, `true`, `false`, ... on every incoming event.
    pub fn toggle<S:EventOutput>(&self, label:Label, source:&S) -> Stream<bool> {
        self.register(OwnedToggle::new(label,source))
    }

    /// OwnedCount the incoming events.
    pub fn count<S:EventOutput>(&self, label:Label, source:&S) -> Stream<usize> {
        self.register(OwnedCount::new(label,source))
    }

    /// Replaces the incoming event with the predefined value.
    pub fn constant<S,T> (&self, label:Label, source:&S, value:T) -> Stream<T>
    where S:EventOutput, T:Data {
        self.register(OwnedConstant::new(label,source,value))
    }

    /// Remembers the value of the input stream and outputs the previously received one.
    pub fn previous<S,T> (&self, label:Label, source:&S) -> Stream<T>
    where S:EventOutput<Output=T>, T:Data {
        self.register(OwnedPrevious::new(label,source))
    }

    /// Samples the first stream (behavior) on every incoming event of the second stream. The
    /// incoming event is dropped and a new event with the behavior's value is emitted.
    pub fn sample<E:EventOutput,B:EventOutput>
    (&self, label:Label, behavior:&B, event:&E) -> Stream<Output<B>> {
        self.register(OwnedSample::new(label,behavior,event))
    }

    /// Passes the incoming event of the fisr stream only if the value of the second stream is true.
    pub fn gate<T,E,B>(&self, label:Label, event:&E, behavior:&B) -> Stream<Output<E>>
    where T:Data, E:EventOutput<Output=T>, B:EventOutput<Output=bool> {
        self.register(OwnedGate::new(label,event,behavior))
    }


    // === Merge ===

    /// Merges multiple input streams into a single output stream. All input streams have to share
    /// the same output data type. Please note that `gather` can be used to create recursive FRP
    /// networks by creating an empty merge and using the `attach` method to attach new streams to
    /// it. When a recursive network is created, `gather` breaks the cycle. After passing the first
    /// event, no more events will be passed till the end of the current FRP network resolution.
    pub fn gather<T:Data>(&self, label:Label) -> Merge<T> {
        self.register_raw(OwnedMerge::new(label))
    }

    /// Merges multiple input streams into a single output stream. All input streams have to share
    /// the same output data type.
    pub fn merge<T1,T2,T:Data>(&self, label:Label, t1:&T1, t2:&T2) -> Stream<T>
    where T1:EventOutput<Output=T>, T2:EventOutput<Output=T> {
        self.register(OwnedMerge::new2(label,t1,t2))
    }

    /// Specialized version of `merge`.
    pub fn merge2<T1,T2,T:Data>(&self, label:Label, t1:&T1, t2:&T2) -> Stream<T>
    where T1:EventOutput<Output=T>, T2:EventOutput<Output=T> {
        self.register(OwnedMerge::new2(label,t1,t2))
    }

    /// Specialized version of `merge`.
    pub fn merge3<T1,T2,T3,T:Data>(&self, label:Label, t1:&T1, t2:&T2, t3:&T3) -> Stream<T>
    where T1:EventOutput<Output=T>, T2:EventOutput<Output=T>, T3:EventOutput<Output=T> {
        self.register(OwnedMerge::new3(label,t1,t2,t3))
    }

    /// Specialized version of `merge`.
    pub fn merge4<T1,T2,T3,T4,T:Data>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4) -> Stream<T>
    where T1:EventOutput<Output=T>,
          T2:EventOutput<Output=T>,
          T3:EventOutput<Output=T>,
          T4:EventOutput<Output=T> {
        self.register(OwnedMerge::new4(label,t1,t2,t3,t4))
    }


    // === Zip ===

    /// Merges input streams into a stream containing values from all of them. On event from any of
    /// the input streams, all streams are sampled and the final event is produced.
    pub fn zip<T1,T2>(&self, label:Label, t1:&T1, t2:&T2) -> Stream<(Output<T1>,Output<T2>)>
    where T1:EventOutput, T2:EventOutput {
        self.register(OwnedZip2::new(label,t1,t2))
    }

    /// Specialized version of `zip`.
    pub fn zip2<T1,T2>(&self, label:Label, t1:&T1, t2:&T2) -> Stream<(Output<T1>,Output<T2>)>
    where T1:EventOutput, T2:EventOutput {
        self.register(OwnedZip2::new(label,t1,t2))
    }

    /// Specialized version of `zip`.
    pub fn zip3<T1,T2,T3>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3) -> Stream<(Output<T1>,Output<T2>,Output<T3>)>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
        self.register(OwnedZip3::new(label,t1,t2,t3))
    }

    /// Specialized version of `zip`.
    pub fn zip4<T1,T2,T3,T4>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4)
    -> Stream<(Output<T1>,Output<T2>,Output<T3>,Output<T4>)>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
        self.register(OwnedZip4::new(label,t1,t2,t3,t4))
    }


    // === OwnedMap ===

    /// On every event from the first input stream, sample all other input streams and run the
    /// provided function on all gathered values. If you want to run the function on event from any
    /// input stream, use the `apply` function family instead.
    pub fn map<S,F,T>(&self, label:Label, source:&S, f:F) -> Stream<T>
    where S:EventOutput, T:Data, F:'static+Fn(&Output<S>)->T {
        self.register(OwnedMap::new(label,source,f))
    }

    /// Specialized version of `map`.
    pub fn map2<T1,T2,F,T>(&self, label:Label, t1:&T1, t2:&T2, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->T {
        self.register(OwnedMap2::new(label,t1,t2,f))
    }

    /// Specialized version of `map`.
    pub fn map3<T1,T2,T3,F,T>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->T {
        self.register(OwnedMap3::new(label,t1,t2,t3,f))
    }

    /// Specialized version of `map`.
    pub fn map4<T1,T2,T3,T4,F,T>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->T {
        self.register(OwnedMap4::new(label,t1,t2,t3,t4,f))
    }


    // === Apply ===

    /// On every input event sample all input streams and run the provided function on all gathered
    /// values. If you want to run the function only on event on the first input, use the `map`
    /// function family instead.
    pub fn apply2<T1,T2,F,T>(&self, label:Label, t1:&T1, t2:&T2, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->T {
        self.register(OwnedApply2::new(label,t1,t2,f))
    }

    /// Specialized version `apply`.
    pub fn apply3<T1,T2,T3,F,T>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->T {
        self.register(OwnedApply3::new(label,t1,t2,t3,f))
    }

    /// Specialized version `apply`.
    pub fn apply4<T1,T2,T3,T4,F,T>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->T {
        self.register(OwnedApply4::new(label,t1,t2,t3,t4,f))
    }
}



// =================================================================================================
// === Nodes Definitions ===========================================================================
// =================================================================================================


pub fn watch_stream<T:EventOutput>(target:&T) -> watch::Ref<T> {
    let target = target.clone_ref();
    let handle = target.register_watch();
    watch::Ref::new(target,handle)
}


// ==============
// === Source ===
// ==============

#[derive(Debug)]
pub struct SourceData  <Out=()> { phantom:PhantomData<Out> }
pub type   OwnedSource <Out=()> = stream::Node     <SourceData<Out>>;
pub type   Source      <Out=()> = stream::WeakNode <SourceData<Out>>;

impl<Out:Data> HasOutput for SourceData<Out> {
    type Output = Out;
}

impl<Out:Data> OwnedSource<Out> {
    /// Constructor.
    pub fn new(label:Label) -> Self {
        let phantom    = default();
        let definition = SourceData {phantom};
        Self::construct(label,definition)
    }
}

impl<Out:Data> Source<Out> {
    /// Emit new event.
    pub fn emit<T:ToRef<Out>>(&self, value:T) {
        self.emit_event(value.to_ref())
    }
}

impl<Out:Data> FnOnce<(Out,)> for Source<Out> {
    type Output = ();
    extern "rust-call" fn call_once(self, args:(Out,)) {
        self.emit(args.0)
    }
}

impl<Out:Data> FnMut<(Out,)> for Source<Out> {
    extern "rust-call" fn call_mut(&mut self, args:(Out,)) {
        self.emit(args.0)
    }
}

impl<Out:Data> Fn<(Out,)> for Source<Out> {
    extern "rust-call" fn call(&self, args:(Out,)) {
        self.emit(args.0)
    }
}



// ===============
// === Sampler ===
// ===============

#[derive(Debug)]
pub struct SamplerData  <Out=()> { value:RefCell<Out> }
pub type   OwnedSampler <Out=()> = stream::Node     <SamplerData<Out>>;
pub type   Sampler      <Out=()> = stream::WeakNode <SamplerData<Out>>;

impl<Out:Data> HasOutput for SamplerData<Out> {
    type Output = Out;
}

impl<Out:Data> OwnedSampler<Out> {
    /// Constructor.
    pub fn new<T1>(label:Label, source:&T1) -> Self
    where T1:EventOutput<Output=Out> {
        let value      = default();
        let definition = SamplerData {value};
        Self::construct_and_connect(label,source,definition)
    }
}

impl<Out:Data> Sampler<Out> {
    /// OwnedSample the value.
    pub fn value(&self) -> Out {
        self.upgrade().map(|t| t.value.borrow().clone()).unwrap_or_default()
    }
}

impl<Out:Data> stream::EventConsumer<Out> for OwnedSampler<Out> {
    fn on_event(&self, event:&Out) {
        *self.value.borrow_mut() = event.clone();
        self.emit_event(event);
    }
}



// =============
// === Trace ===
// =============

#[derive(Clone,Debug)]
pub struct TraceData  <Out> { phantom:PhantomData<Out> }
pub type   OwnedTrace <Out> = stream::Node     <TraceData<Out>>;
pub type   Trace      <Out> = stream::WeakNode <TraceData<Out>>;

impl<Out:Data> HasOutput for TraceData<Out> {
    type Output = Out;
}

impl<Out:Data> OwnedTrace<Out> {
    /// Constructor.
    pub fn new<T1>(label:Label, source:&T1) -> Self
    where T1:EventOutput<Output=Out> {
        let phantom = default();
        let def     = TraceData {phantom};
        Self::construct_and_connect(label,source,def)
    }
}

impl<Out:Data> stream::EventConsumer<Out> for OwnedTrace<Out> {
    fn on_event(&self, event:&Out) {
        println!("[FRP] {}: {:?}", self.label(), event);
        self.emit_event(event);
    }
}



// ==============
// === Toggle ===
// ==============

#[derive(Debug)]
pub struct ToggleData { value:Cell<bool> }
pub type   OwnedToggle = stream::Node     <ToggleData>;
pub type   Toggle      = stream::WeakNode <ToggleData>;

impl HasOutput for ToggleData {
    type Output = bool;
}

impl OwnedToggle {
    /// Constructor.
    pub fn new<T1:EventOutput>(label:Label, source:&T1) -> Self {
        Self::new_with(label,source,default())
    }

    /// Constructor with explicit start value.
    pub fn new_with<T1:EventOutput>(label:Label, source:&T1, init:bool) -> Self {
        let value = Cell::new(init);
        let def   = ToggleData {value};
        Self::construct_and_connect(label,source,def)
    }
}

impl<T> stream::EventConsumer<T> for OwnedToggle {
    fn on_event(&self, _:&T) {
        let value = !self.value.get();
        self.value.set(value);
        self.emit_event(&value);
    }
}



// =============
// === Count ===
// =============

#[derive(Debug)]
pub struct CountData { value:Cell<usize> }
pub type   OwnedCount = stream::Node     <CountData>;
pub type   Count      = stream::WeakNode <CountData>;

impl HasOutput for CountData {
    type Output = usize;
}

impl OwnedCount {
    /// Constructor.
    pub fn new<T1>(label:Label, source:&T1) -> Self
    where T1:EventOutput {
        let value = default();
        let def   = CountData {value};
        Self::construct_and_connect(label,source,def)
    }
}

impl<T> stream::EventConsumer<T> for OwnedCount {
    fn on_event(&self, _:&T) {
        let value = self.value.get() + 1;
        self.value.set(value);
        self.emit_event(&value);
    }
}



// ================
// === Constant ===
// ================

#[derive(Debug)]
pub struct ConstantData  <Out=()> { value:Out }
pub type   OwnedConstant <Out=()> = stream::Node     <ConstantData<Out>>;
pub type   Constant      <Out=()> = stream::WeakNode <ConstantData<Out>>;

impl<Out:Data> HasOutput for ConstantData<Out> {
    type Output = Out;
}

impl<Out:Data> OwnedConstant<Out> {
    /// Constructor.
    pub fn new<S>(label:Label, source:&S, value:Out) -> Self
    where S:EventOutput {
        let def = ConstantData {value};
        Self::construct_and_connect(label,source,def)
    }
}

impl<Out:Data,T> stream::EventConsumer<T> for OwnedConstant<Out> {
    fn on_event(&self, _:&T) {
        self.emit_event(&self.value);
    }
}



// ================
// === Previous ===
// ================

#[derive(Debug)]
pub struct PreviousData  <Out=()> { previous:RefCell<Out> }
pub type   OwnedPrevious <Out=()> = stream::Node     <PreviousData<Out>>;
pub type   Previous      <Out=()> = stream::WeakNode <PreviousData<Out>>;

impl<Out:Data> HasOutput for PreviousData<Out> {
    type Output = Out;
}

impl<Out:Data> OwnedPrevious<Out> {
    /// Constructor.
    pub fn new<S>(label:Label, source:&S) -> Self
        where S:EventOutput<Output=Out> {
        let previous = default();
        let def      = PreviousData {previous};
        Self::construct_and_connect(label,source,def)
    }
}

impl<Out:Data> stream::EventConsumer<Out> for OwnedPrevious<Out> {
    fn on_event(&self, event:&Out) {
        let previous = mem::replace(&mut *self.previous.borrow_mut(),event.clone());
        self.emit_event(&previous);
    }
}



// ==============
// === Sample ===
// ==============

#[derive(Debug)]
pub struct SampleData  <T1> { behavior:watch::Ref<T1> }
pub type   OwnedSample <T1> = stream::Node     <SampleData<T1>>;
pub type   Sample      <T1> = stream::WeakNode <SampleData<T1>>;

impl<T1:HasOutput> HasOutput for SampleData<T1> {
    type Output = Output<T1>;
}

impl<T1:EventOutput> OwnedSample<T1> {
    /// Constructor.
    pub fn new<Event:EventOutput>(label:Label, behavior:&T1, event:&Event) -> Self {
        let behavior   = watch_stream(behavior);
        let definition = SampleData {behavior};
        Self::construct_and_connect(label,event,definition)
    }
}

impl<T,T1:EventOutput> stream::EventConsumer<T> for OwnedSample<T1> {
    fn on_event(&self, _:&T) {
        self.emit_event(&self.behavior.value());
    }
}

impl<B> stream::InputBehaviors for SampleData<B>
where B:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.behavior)]
    }
}



// ============
// === Gate ===
// ============

#[derive(Debug)]
pub struct GateData  <T1,Out=()> { behavior:watch::Ref<T1>, phantom:PhantomData<Out> }
pub type   OwnedGate <T1,Out=()> = stream::Node     <GateData<T1,Out>>;
pub type   Gate      <T1,Out=()> = stream::WeakNode <GateData<T1,Out>>;

impl<T1,Out:Data> HasOutput for GateData<T1,Out> {
    type Output = Out;
}

impl<T1,Out> OwnedGate<T1,Out>
where Out:Data, T1:EventOutput<Output=bool> {
    /// Constructor.
    pub fn new<E>(label:Label, event:&E, behavior:&T1) -> Self
    where E:EventOutput<Output=Out> {
        let behavior   = watch_stream(behavior);
        let phantom    = default();
        let definition = GateData {behavior,phantom};
        Self::construct_and_connect(label,event,definition)
    }
}

impl<T1,Out> stream::EventConsumer<Out> for OwnedGate<T1,Out>
where Out:Data, T1:EventOutput<Output=bool> {
    fn on_event(&self, event:&Out) {
        if self.behavior.value() {
            self.emit_event(event)
        }
    }
}

impl<T1,Out> stream::InputBehaviors for GateData<T1,Out>
where T1:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.behavior)]
    }
}



// =============
// === Merge ===
// =============

#[derive(Debug)]
pub struct MergeData  <Out=()> { phantom:PhantomData<Out>, during_call:Cell<bool> }
pub type   OwnedMerge <Out=()> = stream::Node     <MergeData<Out>>;
pub type   Merge      <Out=()> = stream::WeakNode <MergeData<Out>>;

impl<Out:Data> HasOutput for MergeData<Out> {
    type Output = Out;
}

impl<Out:Data> OwnedMerge<Out> {
    /// Constructor.
    pub fn new(label:Label) -> Self {
        let phantom     = default();
        let during_call = default();
        let def         = MergeData {phantom,during_call};
        Self::construct(label,def)
    }

    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<S>(self, stream:&S) -> Self
        where S:EventOutput<Output=Out> {
        stream.register_target(self.downgrade().into());
        self
    }

    /// Constructor for 1 input stream.
    pub fn new1<T1>(label:Label, t1:&T1) -> Self
        where T1:EventOutput<Output=Out> {
        Self::new(label).with(t1)
    }

    /// Constructor for 2 input streams.
    pub fn new2<T1,T2>(label:Label, t1:&T1, t2:&T2) -> Self
        where T1:EventOutput<Output=Out>,
              T2:EventOutput<Output=Out> {
        Self::new(label).with(t1).with(t2)
    }

    /// Constructor for 3 input streams.
    pub fn new3<T1,T2,T3>(label:Label, t1:&T1, t2:&T2, t3:&T3) -> Self
        where T1:EventOutput<Output=Out>,
              T2:EventOutput<Output=Out>,
              T3:EventOutput<Output=Out> {
        Self::new(label).with(t1).with(t2).with(t3)
    }

    /// Constructor for 4 input streams.
    pub fn new4<T1,T2,T3,T4>(label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4) -> Self
        where T1:EventOutput<Output=Out>,
              T2:EventOutput<Output=Out>,
              T3:EventOutput<Output=Out>,
              T4:EventOutput<Output=Out> {
        Self::new(label).with(t1).with(t2).with(t3).with(t4)
    }
}

impl<Out:Data> Merge<Out> {
    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T1>(self, source:&T1) -> Self
    where T1:EventOutput<Output=Out> {
        source.register_target(self.clone_ref().into());
        self
    }

    /// Attach new source to this node.
    pub fn attach<T1>(&self, source:&T1)
    where T1:EventOutput<Output=Out> {
        source.register_target(self.into());
    }
}

impl<Out:Data> stream::EventConsumer<Out> for OwnedMerge<Out> {
    fn on_event(&self, event:&Out) {
        self.emit_event(event);
    }
}



// ============
// === Zip2 ===
// ============

#[derive(Debug)]
pub struct Zip2Data  <T1,T2> { source1:watch::Ref<T1>, source2:watch::Ref<T2> }
pub type   OwnedZip2 <T1,T2> = stream::Node     <Zip2Data<T1,T2>>;
pub type   Zip2      <T1,T2> = stream::WeakNode <Zip2Data<T1,T2>>;

impl<T1,T2> HasOutput for Zip2Data<T1,T2>
    where T1:EventOutput, T2:EventOutput {
    type Output = (Output<T1>,Output<T2>);
}

impl<T1,T2> OwnedZip2<T1,T2>
    where T1:EventOutput, T2:EventOutput {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2) -> Self {
        let source1 = watch_stream(t1);
        let source2 = watch_stream(t2);
        let def   = Zip2Data {source1,source2};
        let this  = Self::construct(label,def);
        let weak  = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.into());
        this
    }
}

impl<T1,T2,Out> stream::EventConsumer<Out> for OwnedZip2<T1,T2>
    where T1:EventOutput, T2:EventOutput {
    fn on_event(&self, _:&Out) {
        let value1 = self.source1.value();
        let value2 = self.source2.value();
        self.emit_event(&(value1,value2));
    }
}

impl<T1,T2> stream::InputBehaviors for Zip2Data<T1,T2>
    where T1:EventOutput, T2:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::mixed(&self.source1), Link::mixed(&self.source2)]
    }
}



// ============
// === Zip3 ===
// ============

#[derive(Debug)]
pub struct Zip3Data  <T1,T2,T3>
    { source1:watch::Ref<T1>, source2:watch::Ref<T2>, source3:watch::Ref<T3> }
pub type   OwnedZip3 <T1,T2,T3> = stream::Node     <Zip3Data<T1,T2,T3>>;
pub type   Zip3      <T1,T2,T3> = stream::WeakNode <Zip3Data<T1,T2,T3>>;

impl<T1,T2,T3> HasOutput for Zip3Data<T1,T2,T3>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
    type Output = (Output<T1>,Output<T2>,Output<T3>);
}

impl<T1,T2,T3> OwnedZip3<T1,T2,T3>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2, t3:&T3) -> Self {
        let source1 = watch_stream(t1);
        let source2 = watch_stream(t2);
        let source3 = watch_stream(t3);
        let def   = Zip3Data {source1,source2,source3};
        let this  = Self::construct(label,def);
        let weak  = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.into());
        this
    }
}

impl<T1,T2,T3,Out> stream::EventConsumer<Out> for OwnedZip3<T1,T2,T3>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
    fn on_event(&self, _:&Out) {
        let value1 = self.source1.value();
        let value2 = self.source2.value();
        let value3 = self.source3.value();
        self.emit_event(&(value1,value2,value3));
    }
}

impl<T1,T2,T3> stream::InputBehaviors for Zip3Data<T1,T2,T3>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::mixed(&self.source1), Link::mixed(&self.source2), Link::mixed(&self.source3)]
    }
}



// ============
// === Zip4 ===
// ============

#[derive(Debug)]
pub struct Zip4Data <T1,T2,T3,T4>
    {source1:watch::Ref<T1>, source2:watch::Ref<T2>, source3:watch::Ref<T3>, source4:watch::Ref<T4>}
pub type   OwnedZip4 <T1,T2,T3,T4> = stream::Node     <Zip4Data<T1,T2,T3,T4>>;
pub type   WeakZip4  <T1,T2,T3,T4> = stream::WeakNode <Zip4Data<T1,T2,T3,T4>>;

impl<T1,T2,T3,T4> HasOutput for Zip4Data<T1,T2,T3,T4>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
    type Output = (Output<T1>,Output<T2>,Output<T3>,Output<T4>);
}

impl<T1,T2,T3,T4> OwnedZip4<T1,T2,T3,T4>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4) -> Self {
        let source1 = watch_stream(t1);
        let source2 = watch_stream(t2);
        let source3 = watch_stream(t3);
        let source4 = watch_stream(t4);
        let def   = Zip4Data {source1,source2,source3,source4};
        let this  = Self::construct(label,def);
        let weak  = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.clone_ref().into());
        t4.register_target(weak.into());
        this
    }
}

impl<T1,T2,T3,T4,Out> stream::EventConsumer<Out> for OwnedZip4<T1,T2,T3,T4>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
    fn on_event(&self, _:&Out) {
        let value1 = self.source1.value();
        let value2 = self.source2.value();
        let value3 = self.source3.value();
        let value4 = self.source4.value();
        self.emit_event(&(value1,value2,value3,value4));
    }
}

impl<T1,T2,T3,T4> stream::InputBehaviors for Zip4Data<T1,T2,T3,T4>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![ Link::mixed(&self.source1)
            , Link::mixed(&self.source2)
            , Link::mixed(&self.source3)
            , Link::mixed(&self.source4)
            ]
    }
}



// ===========
// === Map ===
// ===========

pub struct MapData  <V1,F> { phantom:PhantomData<V1>, function:F }
pub type   OwnedMap <V1,F> = stream::Node     <MapData<V1,F>>;
pub type   Map      <V1,F> = stream::WeakNode <MapData<V1,F>>;

impl<V1,F,Out> HasOutput for MapData<V1,F>
where Out:Data, F:'static+Fn(&V1)->Out {
    type Output = Out;
}

impl<V1,F,Out> OwnedMap<V1,F>
where V1:Data, Out:Data, F:'static+Fn(&V1)->Out {
    /// Constructor.
    pub fn new<T1>(label:Label, t1:&T1, function:F) -> Self
    where T1:EventOutput<Output=V1> {
        let phantom    = default();
        let definition = MapData {phantom,function};
        Self::construct_and_connect(label,t1,definition)
    }
}

impl<V1,F,Out> stream::EventConsumer<V1> for OwnedMap<V1,F>
where V1:Data, Out:Data, F:'static+Fn(&V1)->Out {
    fn on_event(&self, value:&V1) {
        let out = (self.function)(value);
        self.emit_event(&out);
    }
}

impl<V1,F> Debug for MapData<V1,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"MapData")
    }
}



// ============
// === Map2 ===
// ============

pub struct Map2Data  <V1,T2,F> { phantom:PhantomData<V1>, source2:watch::Ref<T2>, function:F }
pub type   OwnedMap2 <V1,T2,F> = stream::Node     <Map2Data<V1,T2,F>>;
pub type   Map2      <V1,T2,F> = stream::WeakNode <Map2Data<V1,T2,F>>;

impl<V1,T2,F,Out> HasOutput for Map2Data<V1,T2,F>
where V1:Data, T2:EventOutput, Out:Data, F:'static+Fn(&V1,&Output<T2>)->Out {
    type Output = Out;
}

impl<V1,T2,F,Out> OwnedMap2<V1,T2,F>
where V1:Data, T2:EventOutput, Out:Data, F:'static+Fn(&V1,&Output<T2>)->Out {
    /// Constructor.
    pub fn new<T1>(label:Label, t1:&T1, t2:&T2, function:F) -> Self
    where T1:EventOutput<Output=V1> {
        let phantom = default();
        let source2 = watch_stream(t2);
        let def     = Map2Data {phantom,source2,function};
        let this    = Self::construct(label,def);
        let weak    = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<V1,T2,F,Out> stream::EventConsumer<V1> for OwnedMap2<V1,T2,F>
where V1:Data, T2:EventOutput, Out:Data, F:'static+Fn(&V1,&Output<T2>)->Out {
    fn on_event(&self, value1:&V1) {
        let value2 = self.source2.value();
        let out    = (self.function)(&value1,&value2);
        self.emit_event(&out);
    }
}

impl<V1,T2,F> Debug for Map2Data<V1,T2,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Map2Data")
    }
}

impl<V1,T2,F> stream::InputBehaviors for Map2Data<V1,T2,F>
where V1:Data, T2:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.source2)]
    }
}



// ============
// === Map3 ===
// ============

pub struct Map3Data <V1,T2,T3,F>
    { phantom:PhantomData<V1>, source2:watch::Ref<T2>, source3:watch::Ref<T3>, function:F }
pub type   OwnedMap3 <V1,T2,T3,F> = stream::Node     <Map3Data<V1,T2,T3,F>>;
pub type   Map3      <V1,T2,T3,F> = stream::WeakNode <Map3Data<V1,T2,T3,F>>;

impl<V1,T2,T3,F,Out> HasOutput for Map3Data<V1,T2,T3,F>
where V1:Data, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&V1,&Output<T2>,&Output<T3>)->Out {
    type Output = Out;
}

impl<V1,T2,T3,F,Out> OwnedMap3<V1,T2,T3,F>
where V1:Data, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&V1,&Output<T2>,&Output<T3>)->Out {
    /// Constructor.
    pub fn new<T1>(label:Label, t1:&T1, t2:&T2, t3:&T3, function:F) -> Self
    where T1:EventOutput<Output=V1> {
        let phantom = default();
        let source2 = watch_stream(t2);
        let source3 = watch_stream(t3);
        let def   = Map3Data {phantom,source2,source3,function};
        let this  = Self::construct(label,def);
        let weak  = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<V1,T2,T3,F,Out> stream::EventConsumer<V1> for OwnedMap3<V1,T2,T3,F>
where V1:Data, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&V1,&Output<T2>,&Output<T3>)->Out {
    fn on_event(&self, value1:&V1) {
        let value2 = self.source2.value();
        let value3 = self.source3.value();
        let out    = (self.function)(&value1,&value2,&value3);
        self.emit_event(&out);
    }
}

impl<V1,T2,T3,F> Debug for Map3Data<V1,T2,T3,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Map3Data")
    }
}

impl<V1,T2,T3,F> stream::InputBehaviors for Map3Data<V1,T2,T3,F>
    where V1:Data, T2:EventOutput, T3:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.source2), Link::behavior(&self.source3)]
    }
}



// ============
// === Map4 ===
// ============

pub struct Map4Data <V1,T2,T3,T4,F>
    { phantom:PhantomData<V1>
    , source2:watch::Ref<T2>, source3:watch::Ref<T3>, source4:watch::Ref<T4>, function:F }
pub type   OwnedMap4 <V1,T2,T3,T4,F> = stream::Node     <Map4Data<V1,T2,T3,T4,F>>;
pub type   Map4      <V1,T2,T3,T4,F> = stream::WeakNode <Map4Data<V1,T2,T3,T4,F>>;

impl<V1,T2,T3,T4,F,Out> HasOutput for Map4Data<V1,T2,T3,T4,F>
    where V1:Data, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
          F:'static+Fn(&V1,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    type Output = Out;
}

impl<V1,T2,T3,T4,F,Out> OwnedMap4<V1,T2,T3,T4,F>
    where V1:Data, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
          F:'static+Fn(&V1,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    /// Constructor.
    pub fn new<T1>(label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4, function:F) -> Self
    where T1:EventOutput<Output=V1> {
        let phantom = default();
        let source2 = watch_stream(t2);
        let source3 = watch_stream(t3);
        let source4 = watch_stream(t4);
        let def     = Map4Data {phantom,source2,source3,source4,function};
        let this    = Self::construct(label,def);
        let weak    = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<V1,T2,T3,T4,F,Out> stream::EventConsumer<V1> for OwnedMap4<V1,T2,T3,T4,F>
    where V1:Data, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
          F:'static+Fn(&V1,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    fn on_event(&self, value1:&V1) {
        let value2 = self.source2.value();
        let value3 = self.source3.value();
        let value4 = self.source4.value();
        let out    = (self.function)(&value1,&value2,&value3,&value4);
        self.emit_event(&out);
    }
}

impl<V1,T2,T3,T4,F> stream::InputBehaviors for Map4Data<V1,T2,T3,T4,F>
where V1:Data, T2:EventOutput, T3:EventOutput, T4:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![ Link::behavior(&self.source2)
            , Link::behavior(&self.source3)
            , Link::behavior(&self.source4)
            ]
    }
}

impl<V1,T2,T3,T4,F> Debug for Map4Data<V1,T2,T3,T4,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Map4Data")
    }
}



// ==============
// === Apply2 ===
// ==============

pub struct Apply2Data  <T1,T2,F> { source1:watch::Ref<T1>, source2:watch::Ref<T2>, function:F }
pub type   OwnedApply2 <T1,T2,F> = stream::Node     <Apply2Data<T1,T2,F>>;
pub type   Apply2      <T1,T2,F> = stream::WeakNode <Apply2Data<T1,T2,F>>;

impl<T1,T2,F,Out> HasOutput for Apply2Data<T1,T2,F>
where T1:EventOutput, T2:EventOutput, Out:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->Out {
    type Output = Out;
}

impl<T1,T2,F,Out> OwnedApply2<T1,T2,F>
where T1:EventOutput, T2:EventOutput, Out:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->Out {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2, function:F) -> Self {
        let source1 = watch_stream(t1);
        let source2 = watch_stream(t2);
        let def     = Apply2Data {source1,source2,function};
        let this    = Self::construct(label,def);
        let weak    = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.into());
        this
    }
}

impl<T1,T2,F,Out,T> stream::EventConsumer<T> for OwnedApply2<T1,T2,F>
where T1:EventOutput, T2:EventOutput, Out:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->Out {
    fn on_event(&self, _:&T) {
        let value1 = self.source1.value();
        let value2 = self.source2.value();
        let out    = (self.function)(&value1,&value2);
        self.emit_event(&out);
    }
}

impl<T1,T2,F> Debug for Apply2Data<T1,T2,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Apply2Data")
    }
}



// ==============
// === Apply3 ===
// ==============

pub struct Apply3Data <T1,T2,T3,F>
    { source1:watch::Ref<T1>, source2:watch::Ref<T2>, source3:watch::Ref<T3>, function:F }
pub type   OwnedApply3 <T1,T2,T3,F> = stream::Node     <Apply3Data<T1,T2,T3,F>>;
pub type   Apply3      <T1,T2,T3,F> = stream::WeakNode <Apply3Data<T1,T2,T3,F>>;

impl<T1,T2,T3,F,Out> HasOutput for Apply3Data<T1,T2,T3,F>
where T1:EventOutput, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->Out {
    type Output = Out;
}

impl<T1,T2,T3,F,Out> OwnedApply3<T1,T2,T3,F>
where T1:EventOutput, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->Out {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2, t3:&T3, function:F) -> Self {
        let source1 = watch_stream(t1);
        let source2 = watch_stream(t2);
        let source3 = watch_stream(t3);
        let def     = Apply3Data {source1,source2,source3,function};
        let this    = Self::construct(label,def);
        let weak    = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.into());
        this
    }
}

impl<T1,T2,T3,F,Out,T> stream::EventConsumer<T> for OwnedApply3<T1,T2,T3,F>
where T1:EventOutput, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->Out {
    fn on_event(&self, _:&T) {
        let value1 = self.source1.value();
        let value2 = self.source2.value();
        let value3 = self.source3.value();
        let out    = (self.function)(&value1,&value2,&value3);
        self.emit_event(&out);
    }
}

impl<T1,T2,T3,F> Debug for Apply3Data<T1,T2,T3,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Apply3Data")
    }
}



// ==============
// === Apply4 ===
// ==============

pub struct Apply4Data <T1,T2,T3,T4,F>
    { source1:watch::Ref<T1>, source2:watch::Ref<T2>, source3:watch::Ref<T3>, source4:watch::Ref<T4>
    , function:F }
pub type   OwnedApply4 <T1,T2,T3,T4,F> = stream::Node     <Apply4Data<T1,T2,T3,T4,F>>;
pub type   Apply4      <T1,T2,T3,T4,F> = stream::WeakNode <Apply4Data<T1,T2,T3,T4,F>>;

impl<T1,T2,T3,T4,F,Out> HasOutput for Apply4Data<T1,T2,T3,T4,F>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    type Output = Out;
}

impl<T1,T2,T3,T4,F,Out> OwnedApply4<T1,T2,T3,T4,F>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4, function:F) -> Self {
        let source1 = watch_stream(t1);
        let source2 = watch_stream(t2);
        let source3 = watch_stream(t3);
        let source4 = watch_stream(t4);
        let def     = Apply4Data {source1,source2,source3,source4,function};
        let this    = Self::construct(label,def);
        let weak    = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.clone_ref().into());
        t4.register_target(weak.into());
        this
    }
}

impl<T1,T2,T3,T4,F,Out,T> stream::EventConsumer<T> for OwnedApply4<T1,T2,T3,T4,F>
where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
      F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    fn on_event(&self, _:&T) {
        let value1 = self.source1.value();
        let value2 = self.source2.value();
        let value3 = self.source3.value();
        let value4 = self.source4.value();
        let out    = (self.function)(&value1,&value2,&value3,&value4);
        self.emit_event(&out);
    }
}

impl<T1,T2,T3,T4,F> Debug for Apply4Data<T1,T2,T3,T4,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Apply4Data")
    }
}
