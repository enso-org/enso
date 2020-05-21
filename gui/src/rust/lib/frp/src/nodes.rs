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
use crate::stream::OwnedStream;
use crate::stream;
use enso_generics as generics;
use enso_generics::traits::*;



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
    pub fn sampler<T,Out>(&self, label:Label, source:&T) -> Sampler<Out>
    where T:EventOutput<Output=Out>, Out:Data {
        self.register_raw(OwnedSampler::new(label,source))
    }

    /// Print the incoming events to console and pass them to output.
    pub fn trace<T:EventOutput>(&self, label:Label, source:&T) -> Stream<Output<T>> {
        self.register(OwnedTrace::new(label,source))
    }

    /// Emits `true`, `false`, `true`, `false`, ... on every incoming event.
    pub fn toggle<T:EventOutput>(&self, label:Label, source:&T) -> Stream<bool> {
        self.register(OwnedToggle::new(label,source))
    }

    /// Emits `true`, `false`, `true`, `false`, ... on every incoming event. Initialized with true
    /// value.
    pub fn toggle_true<T:EventOutput>(&self, label:Label, source:&T) -> Stream<bool> {
        self.register(OwnedToggle::new_with(label,source,true))
    }

    /// Count the incoming events.
    pub fn count<T:EventOutput>(&self, label:Label, source:&T) -> Stream<usize> {
        self.register(OwnedCount::new(label,source))
    }

    /// Replaces the incoming event with the predefined value.
    pub fn constant<X:Data,T:EventOutput> (&self, label:Label, source:&T, value:X) -> Stream<X> {
        self.register(OwnedConstant::new(label,source,value))
    }

    /// Remembers the value of the input stream and outputs the previously received one.
    pub fn previous<T:EventOutput> (&self, label:Label, source:&T) -> Stream<Output<T>> {
        self.register(OwnedPrevious::new(label,source))
    }

    /// Samples the first stream (behavior) on every incoming event of the second stream. The
    /// incoming event is dropped and a new event with the behavior's value is emitted.
    pub fn sample<T1:EventOutput,T2:EventOutput>
    (&self, label:Label, behavior:&T1, event:&T2) -> Stream<Output<T1>> {
        self.register(OwnedSample::new(label,behavior,event))
    }

    /// Passes the incoming event of the fisr stream only if the value of the second stream is true.
    pub fn gate<T1,T2>(&self, label:Label, event:&T1, behavior:&T2) -> Stream<Output<T1>>
        where T1:EventOutput, T2:EventOutput<Output=bool> {
        self.register(OwnedGate::new(label,event,behavior))
    }

    /// Like `gate` but passes the value when the condition is `false`.
    pub fn gate_not<T1,T2>(&self, label:Label, event:&T1, behavior:&T2) -> Stream<Output<T1>>
        where T1:EventOutput, T2:EventOutput<Output=bool> {
        self.register(OwnedGateNot::new(label,event,behavior))
    }

    pub fn iter<T1,X>(&self, label:Label, event:&T1) -> Stream<X>
        where T1:EventOutput, for<'t> &'t T1::Output:IntoIterator<Item=&'t X>, X:Data {
        self.register(OwnedIter::new(label,event))
    }

    pub fn _0<T1>(&self, label:Label, event:&T1) -> Stream<generics::ItemAt0<Output<T1>>>
        where T1:EventOutput, T1::Output:generics::GetItemAt0, generics::ItemAt0<T1::Output>:Data {
        self.register(OwnedGet0::new(label,event))
    }

    pub fn _1<T1>(&self, label:Label, event:&T1) -> Stream<generics::ItemAt1<Output<T1>>>
        where T1:EventOutput, T1::Output:generics::GetItemAt1, generics::ItemAt1<T1::Output>:Data {
        self.register(OwnedGet1::new(label,event))
    }

    pub fn _2<T1>(&self, label:Label, event:&T1) -> Stream<generics::ItemAt2<Output<T1>>>
        where T1:EventOutput, T1::Output:generics::GetItemAt2, generics::ItemAt2<T1::Output>:Data {
        self.register(OwnedGet2::new(label,event))
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


    // === Merge_ ===

    /// Like `gather` but drops the incoming data. You can attach streams of different types.
    pub fn gather_(&self, label:Label) -> Merge_ {
        self.register_raw(OwnedMerge_::new(label))
    }

    /// Like `merge` but drops the incoming data. You can attach streams of different types.
    pub fn merge_<T1,T2>(&self, label:Label, t1:&T1, t2:&T2) -> Stream<()>
    where T1:EventOutput, T2:EventOutput {
        self.register(OwnedMerge_::new2(label,t1,t2))
    }

    /// Specialized version of `merge_`.
    pub fn merge2_<T1,T2>(&self, label:Label, t1:&T1, t2:&T2) -> Stream<()>
    where T1:EventOutput, T2:EventOutput {
        self.register(OwnedMerge_::new2(label,t1,t2))
    }

    /// Specialized version of `merge_`.
    pub fn merge3_<T1,T2,T3>(&self, label:Label, t1:&T1, t2:&T2, t3:&T3) -> Stream<()>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
        self.register(OwnedMerge_::new3(label,t1,t2,t3))
    }

    /// Specialized version of `merge_`.
    pub fn merge4_<T1,T2,T3,T4>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4) -> Stream<()>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
        self.register(OwnedMerge_::new4(label,t1,t2,t3,t4))
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


    // === Map ===

    /// On every event from the first input stream, sample all other input streams and run the
    /// provided function on all gathered values. If you want to run the function on event from any
    /// input stream, use the `zip_with` function family instead.
    pub fn map<T,F,Out>(&self, label:Label, source:&T, f:F) -> Stream<Out>
    where T:EventOutput, Out:Data, F:'static+Fn(&Output<T>)->Out {
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
    pub fn zip_with<T1,T2,F,T>(&self, label:Label, t1:&T1, t2:&T2, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->T {
        self.register(OwnedApply2::new(label,t1,t2,f))
    }

    /// Specialized version `apply`.
    pub fn zip_with3<T1,T2,T3,F,T>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->T {
        self.register(OwnedApply3::new(label,t1,t2,t3,f))
    }

    /// Specialized version `apply`.
    pub fn zip_with4<T1,T2,T3,T4,F,T>
    (&self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4, f:F) -> Stream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->T {
        self.register(OwnedApply4::new(label,t1,t2,t3,t4,f))
    }
}



// ========================
// === Dynamic Node API ===
// ========================

/// This is a phantom structure used by macros to create dynamic FRP graphs. It exposes the same
/// API as `Network` in order to reuse macro code for both network and dynamic modes.
#[derive(Clone,Copy,Debug,Default)]
pub struct DynamicNetwork {}

impl DynamicNetwork {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

/// See docs of `Network` to learn about the methods.
impl DynamicNetwork {
    pub fn source<T:Data>(self, label:Label) -> OwnedSource<T> {
        OwnedSource::new(label)
    }

    pub fn source_(self, label:Label) -> OwnedSource {
        OwnedSource::new(label)
    }

    pub fn sampler<T,Out>(self, label:Label, source:&T) -> OwnedSampler<Out>
        where T:EventOutput<Output=Out>, Out:Data {
        OwnedSampler::new(label,source)
    }

    pub fn trace<T:EventOutput>(self, label:Label, source:&T) -> OwnedStream<Output<T>> {
        OwnedTrace::new(label,source).into()
    }

    pub fn toggle<T:EventOutput>(self, label:Label, source:&T) -> OwnedStream<bool> {
        OwnedToggle::new(label,source).into()
    }

    pub fn count<T:EventOutput>(self, label:Label, source:&T) -> OwnedCount<T> {
        OwnedCount::new(label,source)
    }

    pub fn constant<X:Data,T:EventOutput> (self, label:Label, source:&T, value:X) -> OwnedStream<X> {
        OwnedConstant::new(label,source,value).into()
    }

    pub fn previous<T:EventOutput> (self, label:Label, source:&T) -> OwnedStream<Output<T>> {
        OwnedPrevious::new(label,source).into()
    }

    pub fn sample<T1:EventOutput,T2:EventOutput>
    (self, label:Label, behavior:&T1, event:&T2) -> OwnedStream<Output<T1>> {
        OwnedSample::new(label,behavior,event).into()
    }

    pub fn gate<T1,T2>(self, label:Label, event:&T1, behavior:&T2) -> OwnedStream<Output<T1>>
    where T1:EventOutput, T2:EventOutput<Output=bool> {
        OwnedGate::new(label,event,behavior).into()
    }

    pub fn gate_not<T1,T2>(self, label:Label, event:&T1, behavior:&T2) -> OwnedStream<Output<T1>>
    where T1:EventOutput, T2:EventOutput<Output=bool> {
        OwnedGateNot::new(label,event,behavior).into()
    }

    pub fn iter<T1,X>(self, label:Label, event:&T1) -> OwnedStream<X>
    where T1:EventOutput, for<'t> &'t T1::Output:IntoIterator<Item=&'t X>, X:Data {
        OwnedIter::new(label,event).into()
    }

    pub fn _0<T1>(self, label:Label, event:&T1) -> OwnedStream<generics::ItemAt0<Output<T1>>>
    where T1:EventOutput, T1::Output:generics::GetItemAt0, generics::ItemAt0<T1::Output>:Data {
        OwnedGet0::new(label,event).into()
    }

    pub fn _1<T1>(self, label:Label, event:&T1) -> OwnedStream<generics::ItemAt1<Output<T1>>>
    where T1:EventOutput, T1::Output:generics::GetItemAt1, generics::ItemAt1<T1::Output>:Data {
        OwnedGet1::new(label,event).into()
    }

    pub fn _2<T1>(self, label:Label, event:&T1) -> OwnedStream<generics::ItemAt2<Output<T1>>>
    where T1:EventOutput, T1::Output:generics::GetItemAt2, generics::ItemAt2<T1::Output>:Data {
        OwnedGet2::new(label,event).into()
    }


    // === Merge ===

    pub fn gather<T:Data>(self, label:Label) -> OwnedMerge<T> {
        OwnedMerge::new(label)
    }

    pub fn merge<T1,T2,T:Data>(self, label:Label, t1:&T1, t2:&T2) -> OwnedStream<T>
    where T1:EventOutput<Output=T>, T2:EventOutput<Output=T> {
        OwnedMerge::new2(label,t1,t2).into()
    }

    pub fn merge2<T1,T2,T:Data>(self, label:Label, t1:&T1, t2:&T2) -> OwnedStream<T>
    where T1:EventOutput<Output=T>, T2:EventOutput<Output=T> {
        OwnedMerge::new2(label,t1,t2).into()
    }

    pub fn merge3<T1,T2,T3,T:Data>(self, label:Label, t1:&T1, t2:&T2, t3:&T3) -> OwnedStream<T>
    where T1:EventOutput<Output=T>, T2:EventOutput<Output=T>, T3:EventOutput<Output=T> {
        OwnedMerge::new3(label,t1,t2,t3).into()
    }

    pub fn merge4<T1,T2,T3,T4,T:Data>
    (self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4) -> OwnedStream<T>
    where T1:EventOutput<Output=T>,
              T2:EventOutput<Output=T>,
              T3:EventOutput<Output=T>,
              T4:EventOutput<Output=T> {
        OwnedMerge::new4(label,t1,t2,t3,t4).into()
    }


    // === Merge_ ===

    pub fn gather_(self, label:Label) -> OwnedMerge_ {
        OwnedMerge_::new(label)
    }

    pub fn merge_<T1,T2>(self, label:Label, t1:&T1, t2:&T2) -> OwnedStream<()>
    where T1:EventOutput, T2:EventOutput {
        OwnedMerge_::new2(label,t1,t2).into()
    }

    pub fn merge2_<T1,T2>(self, label:Label, t1:&T1, t2:&T2) -> OwnedStream<()>
    where T1:EventOutput, T2:EventOutput {
        OwnedMerge_::new2(label,t1,t2).into()
    }

    pub fn merge3_<T1,T2,T3>(self, label:Label, t1:&T1, t2:&T2, t3:&T3) -> OwnedStream<()>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
        OwnedMerge_::new3(label,t1,t2,t3).into()
    }

    pub fn merge4_<T1,T2,T3,T4>
    (self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4) -> OwnedStream<()>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
        OwnedMerge_::new4(label,t1,t2,t3,t4).into()
    }


    // === Zip ===

    pub fn zip<T1,T2>(self, label:Label, t1:&T1, t2:&T2) -> OwnedStream<(Output<T1>,Output<T2>)>
    where T1:EventOutput, T2:EventOutput {
        OwnedZip2::new(label,t1,t2).into()
    }

    pub fn zip2<T1,T2>(self, label:Label, t1:&T1, t2:&T2) -> OwnedStream<(Output<T1>,Output<T2>)>
    where T1:EventOutput, T2:EventOutput {
        OwnedZip2::new(label,t1,t2).into()
    }

    pub fn zip3<T1,T2,T3>
    (self, label:Label, t1:&T1, t2:&T2, t3:&T3) -> OwnedStream<(Output<T1>,Output<T2>,Output<T3>)>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
        OwnedZip3::new(label,t1,t2,t3).into()
    }

    pub fn zip4<T1,T2,T3,T4>
    (self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4)
     -> OwnedStream<(Output<T1>,Output<T2>,Output<T3>,Output<T4>)>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
        OwnedZip4::new(label,t1,t2,t3,t4).into()
    }


    // === Map ===

    pub fn map<T,F,Out>(self, label:Label, source:&T, f:F) -> OwnedStream<Out>
    where T:EventOutput, Out:Data, F:'static+Fn(&Output<T>)->Out {
        OwnedMap::new(label,source,f).into()
    }

    pub fn map2<T1,T2,F,T>(self, label:Label, t1:&T1, t2:&T2, f:F) -> OwnedStream<T>
    where T1:EventOutput, T2:EventOutput, T:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->T {
        OwnedMap2::new(label,t1,t2,f).into()
    }

    pub fn map3<T1,T2,T3,F,T>
    (self, label:Label, t1:&T1, t2:&T2, t3:&T3, f:F) -> OwnedStream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->T {
        OwnedMap3::new(label,t1,t2,t3,f).into()
    }

    pub fn map4<T1,T2,T3,T4,F,T>
    (self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4, f:F) -> OwnedStream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->T {
        OwnedMap4::new(label,t1,t2,t3,t4,f).into()
    }


    // === Apply ===

    pub fn apply2<T1,T2,F,T>(self, label:Label, t1:&T1, t2:&T2, f:F) -> OwnedStream<T>
    where T1:EventOutput, T2:EventOutput, T:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->T {
        OwnedApply2::new(label,t1,t2,f).into()
    }

    pub fn apply3<T1,T2,T3,F,T>
    (self, label:Label, t1:&T1, t2:&T2, t3:&T3, f:F) -> OwnedStream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->T {
        OwnedApply3::new(label,t1,t2,t3,f).into()
    }

    pub fn apply4<T1,T2,T3,T4,F,T>
    (self, label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4, f:F) -> OwnedStream<T>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, T:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->T {
        OwnedApply4::new(label,t1,t2,t3,t4,f).into()
    }
}



// =================================================================================================
// === Nodes Definitions ===========================================================================
// =================================================================================================

fn watch_stream<T:EventOutput>(target:&T) -> watch::Ref<T> {
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

impl<Out:Data> OwnedSource<Out> {
    /// Emit new event.
    pub fn emit<T:ToRef<Out>>(&self, value:T) {
        self.emit_event(value.to_ref())
    }
}

impl<Out:Data> Source<Out> {
    /// Emit new event.
    pub fn emit<T:ToRef<Out>>(&self, value:T) {
        self.emit_event(value.to_ref())
    }
}



// ===============
// === Sampler ===
// ===============

#[derive(Debug)]
pub struct SamplerData  <Out=()> { source:Box<dyn Any>, value:RefCell<Out> }
pub type   OwnedSampler <Out=()> = stream::Node     <SamplerData<Out>>;
pub type   Sampler      <Out=()> = stream::WeakNode <SamplerData<Out>>;

impl<Out:Data> HasOutput for SamplerData<Out> {
    type Output = Out;
}

impl<Out:Data> OwnedSampler<Out> {
    /// Constructor.
    pub fn new<T1>(label:Label, src:&T1) -> Self
    where T1:EventOutput<Output=Out> {
        let source     = Box::new(src.clone_ref());
        let value      = default();
        let definition = SamplerData {source,value};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<Out:Data> OwnedSampler<Out> {
    /// Sample the value.
    pub fn value(&self) -> Out {
        self.value.borrow().clone()
    }
}

impl<Out:Data> Sampler<Out> {
    /// Sample the value.
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
pub struct TraceData  <T> { source:T }
pub type   OwnedTrace <T> = stream::Node     <TraceData<T>>;
pub type   Trace      <T> = stream::WeakNode <TraceData<T>>;

impl<T:EventOutput> HasOutput for TraceData<T> {
    type Output = Output<T>;
}

impl<T:EventOutput> OwnedTrace<T> {
    /// Constructor.
    pub fn new(label:Label, src:&T) -> Self {
        let source = src.clone_ref();
        let def   = TraceData {source};
        Self::construct_and_connect(label,src,def)
    }
}

impl<T:EventOutput> stream::EventConsumer<Output<T>> for OwnedTrace<T> {
    fn on_event(&self, event:&Output<T>) {
        println!("[FRP] {}: {:?}", self.label(), event);
        self.emit_event(event);
    }
}



// ==============
// === Toggle ===
// ==============

#[derive(Debug)]
pub struct ToggleData  <T>  { source:T, value:Cell<bool> }
pub type   OwnedToggle <T> = stream::Node     <ToggleData<T>>;
pub type   Toggle      <T> = stream::WeakNode <ToggleData<T>>;

impl<T> HasOutput for ToggleData<T> {
    type Output = bool;
}

impl<T:EventOutput> OwnedToggle<T> {
    /// Constructor.
    pub fn new(label:Label, source:&T) -> Self {
        Self::new_with(label,source,default())
    }

    /// Constructor with explicit start value.
    pub fn new_with(label:Label, src:&T, init:bool) -> Self {
        let source = src.clone_ref();
        let value  = Cell::new(init);
        let def    = ToggleData {source,value};
        Self::construct_and_connect_with_init_value(label,src,def,init)
    }
}

impl<T:EventOutput> stream::EventConsumer<Output<T>> for OwnedToggle<T> {
    fn on_event(&self, _:&Output<T>) {
        let value = !self.value.get();
        self.value.set(value);
        self.emit_event(&value);
    }
}



// =============
// === Count ===
// =============

#[derive(Debug)]
pub struct CountData  <T> { source:T, value:Cell<usize> }
pub type   OwnedCount <T> = stream::Node     <CountData<T>>;
pub type   Count      <T> = stream::WeakNode <CountData<T>>;

impl<T> HasOutput for CountData<T> {
    type Output = usize;
}

impl<T:EventOutput> OwnedCount<T> {
    /// Constructor.
    pub fn new(label:Label, src:&T) -> Self {
        let source = src.clone_ref();
        let value  = default();
        let def    = CountData {source,value};
        Self::construct_and_connect(label,src,def)
    }
}

impl<T:EventOutput> stream::EventConsumer<Output<T>> for OwnedCount<T> {
    fn on_event(&self, _:&Output<T>) {
        let value = self.value.get() + 1;
        self.value.set(value);
        self.emit_event(&value);
    }
}



// ================
// === Constant ===
// ================

#[derive(Debug)]
pub struct ConstantData  <T,Out=()> { source:T, value:Out }
pub type   OwnedConstant <T,Out=()> = stream::Node     <ConstantData<T,Out>>;
pub type   Constant      <T,Out=()> = stream::WeakNode <ConstantData<T,Out>>;

impl<T,Out:Data> HasOutput for ConstantData<T,Out> {
    type Output = Out;
}

impl<T:EventOutput,Out:Data> OwnedConstant<T,Out> {
    /// Constructor.
    pub fn new(label:Label, src:&T, value:Out) -> Self {
        let source = src.clone_ref();
        let def    = ConstantData {source,value};
        Self::construct_and_connect(label,src,def)
    }
}

impl<T:EventOutput,Out:Data> stream::EventConsumer<Output<T>> for OwnedConstant<T,Out> {
    fn on_event(&self, _:&Output<T>) {
        self.emit_event(&self.value);
    }
}



// ================
// === Previous ===
// ================

#[derive(Debug)]
pub struct PreviousData  <T:EventOutput> { source:T, previous:RefCell<Output<T>> }
pub type   OwnedPrevious <T> = stream::Node     <PreviousData<T>>;
pub type   Previous      <T> = stream::WeakNode <PreviousData<T>>;

impl<T:EventOutput> HasOutput for PreviousData<T> {
    type Output = Output<T>;
}

impl<T:EventOutput> OwnedPrevious<T> {
    /// Constructor.
    pub fn new(label:Label, src:&T) -> Self {
        let source   = src.clone_ref();
        let previous = default();
        let def      = PreviousData {source,previous};
        Self::construct_and_connect(label,src,def)
    }
}

impl<T:EventOutput> stream::EventConsumer<Output<T>> for OwnedPrevious<T> {
    fn on_event(&self, event:&Output<T>) {
        let previous = mem::replace(&mut *self.previous.borrow_mut(),event.clone());
        self.emit_event(&previous);
    }
}



// ==============
// === Sample ===
// ==============

#[derive(Debug)]
pub struct SampleData  <T1,T2> { behavior:watch::Ref<T1>, event:T2 }
pub type   OwnedSample <T1,T2> = stream::Node     <SampleData<T1,T2>>;
pub type   Sample      <T1,T2> = stream::WeakNode <SampleData<T1,T2>>;

impl<T1:HasOutput,T2> HasOutput for SampleData<T1,T2> {
    type Output = Output<T1>;
}

impl<T1:EventOutput,T2:EventOutput> OwnedSample<T1,T2> {
    /// Constructor.
    pub fn new(label:Label, behavior:&T1, src:&T2) -> Self {
        let event      = src.clone_ref();
        let behavior   = watch_stream(behavior);
        let definition = SampleData {behavior,event};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<T1:EventOutput,T2:EventOutput> stream::EventConsumer<Output<T2>> for OwnedSample<T1,T2> {
    fn on_event(&self, _:&Output<T2>) {
        self.emit_event(&self.behavior.value());
    }
}

impl<T1:EventOutput,T2> stream::InputBehaviors for SampleData<T1,T2> {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.behavior)]
    }
}



// ============
// === Gate ===
// ============

#[derive(Debug)]
pub struct GateData  <T1,T2> { event:T1, behavior:watch::Ref<T2> }
pub type   OwnedGate <T1,T2> = stream::Node     <GateData<T1,T2>>;
pub type   Gate      <T1,T2> = stream::WeakNode <GateData<T1,T2>>;

impl<T1:EventOutput,T2> HasOutput for GateData<T1,T2> {
    type Output = Output<T1>;
}

impl<T1,T2> OwnedGate<T1,T2>
where T1:EventOutput,T2:EventOutput<Output=bool> {
    /// Constructor.
    pub fn new(label:Label, src:&T1, behavior:&T2) -> Self {
        let event      = src.clone_ref();
        let behavior   = watch_stream(behavior);
        let definition = GateData {event,behavior};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<T1,T2> stream::EventConsumer<Output<T1>> for OwnedGate<T1,T2>
where T1:EventOutput, T2:EventOutput<Output=bool> {
    fn on_event(&self, event:&Output<T1>) {
        if self.behavior.value() {
            self.emit_event(event)
        }
    }
}

impl<T1,T2> stream::InputBehaviors for GateData<T1,T2>
where T2:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.behavior)]
    }
}



// ===============
// === GateNot ===
// ===============

#[derive(Debug)]
pub struct GateNotData  <T1,T2> { event:T1, behavior:watch::Ref<T2> }
pub type   OwnedGateNot <T1,T2> = stream::Node     <GateNotData<T1,T2>>;
pub type   GateNot      <T1,T2> = stream::WeakNode <GateNotData<T1,T2>>;

impl<T1:EventOutput,T2> HasOutput for GateNotData<T1,T2> {
    type Output = Output<T1>;
}

impl<T1,T2> OwnedGateNot<T1,T2>
    where T1:EventOutput,T2:EventOutput<Output=bool> {
    /// Constructor.
    pub fn new(label:Label, src:&T1, behavior:&T2) -> Self {
        let event      = src.clone_ref();
        let behavior   = watch_stream(behavior);
        let definition = GateNotData {event,behavior};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<T1,T2> stream::EventConsumer<Output<T1>> for OwnedGateNot<T1,T2>
    where T1:EventOutput, T2:EventOutput<Output=bool> {
    fn on_event(&self, event:&Output<T1>) {
        if !self.behavior.value() {
            self.emit_event(event)
        }
    }
}

impl<T1,T2> stream::InputBehaviors for GateNotData<T1,T2>
    where T2:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.behavior)]
    }
}



// =============
// === Merge ===
// =============

#[derive(Debug)]
pub struct MergeData  <Out=()> { sources:Rc<RefCell<Vec<Box<dyn Any>>>>, phantom:PhantomData<Out> }
pub type   OwnedMerge <Out=()> = stream::Node     <MergeData<Out>>;
pub type   Merge      <Out=()> = stream::WeakNode <MergeData<Out>>;

impl<Out:Data> HasOutput for MergeData<Out> {
    type Output = Out;
}

impl<Out:Data> OwnedMerge<Out> {
    /// Constructor.
    pub fn new(label:Label) -> Self {
        let sources     = default();
        let phantom     = default();
        let def         = MergeData {sources,phantom};
        Self::construct(label,def)
    }

    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T>(self, source:&T) -> Self
    where T:EventOutput<Output=Out> {
        source.register_target(self.downgrade().into());
        self.sources.borrow_mut().push(Box::new(source.clone_ref()));
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
        self.upgrade().for_each(|t| t.sources.borrow_mut().push(Box::new(source.clone_ref())));
        self
    }

    /// Attach new source to this node.
    pub fn attach<T1>(&self, source:&T1)
    where T1:EventOutput<Output=Out> {
        source.register_target(self.into());
        self.upgrade().for_each(|t| t.sources.borrow_mut().push(Box::new(source.clone_ref())));
    }
}

impl<Out:Data> stream::EventConsumer<Out> for OwnedMerge<Out> {
    fn on_event(&self, event:&Out) {
        self.emit_event(event);
    }
}



// =============
// === Merge ===
// =============

#[derive(Debug)]
pub struct MergeData_ { sources:Rc<RefCell<Vec<Box<dyn Any>>>> }
pub type OwnedMerge_ = stream::Node     <MergeData_>;
pub type Merge_      = stream::WeakNode <MergeData_>;

impl HasOutput for MergeData_ {
    type Output = ();
}

impl OwnedMerge_ {
    /// Constructor.
    pub fn new(label:Label) -> Self {
        let sources = default();
        let def     = MergeData_ {sources};
        Self::construct(label,def)
    }

    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T>(self, source:&T) -> Self
    where T:EventOutput {
        source.register_target(self.downgrade().into());
        self.sources.borrow_mut().push(Box::new(source.clone_ref()));
        self
    }

    /// Constructor for 1 input stream.
    pub fn new1<T1>(label:Label, t1:&T1) -> Self
        where T1:EventOutput {
        Self::new(label).with(t1)
    }

    /// Constructor for 2 input streams.
    pub fn new2<T1,T2>(label:Label, t1:&T1, t2:&T2) -> Self
        where T1:EventOutput, T2:EventOutput {
        Self::new(label).with(t1).with(t2)
    }

    /// Constructor for 3 input streams.
    pub fn new3<T1,T2,T3>(label:Label, t1:&T1, t2:&T2, t3:&T3) -> Self
        where T1:EventOutput, T2:EventOutput, T3:EventOutput {
        Self::new(label).with(t1).with(t2).with(t3)
    }

    /// Constructor for 4 input streams.
    pub fn new4<T1,T2,T3,T4>(label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4) -> Self
        where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
        Self::new(label).with(t1).with(t2).with(t3).with(t4)
    }
}

impl Merge_ {
    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T1>(self, source:&T1) -> Self
    where T1:EventOutput {
        source.register_target(self.clone_ref().into());
        self.upgrade().for_each(|t| t.sources.borrow_mut().push(Box::new(source.clone_ref())));
        self
    }

    /// Attach new source to this node.
    pub fn attach<T1>(&self, source:&T1)
    where T1:EventOutput {
        source.register_target(self.into());
        self.upgrade().for_each(|t| t.sources.borrow_mut().push(Box::new(source.clone_ref())));
    }
}

impl<T> stream::EventConsumer<T> for OwnedMerge_ {
    fn on_event(&self, _:&T) {
        self.emit_event(&());
    }
}



// ============
// === Get0 ===
// ============

#[derive(Debug)]
pub struct Get0Data  <T1> { event:T1 }
pub type   OwnedGet0 <T1> = stream::Node     <Get0Data<T1>>;
pub type   Get0      <T1> = stream::WeakNode <Get0Data<T1>>;

impl<T1> HasOutput for Get0Data<T1>
where T1:EventOutput, T1::Output:generics::GetItemAt0, generics::ItemAt0<T1::Output>:Data {
    type Output = generics::ItemAt0<T1::Output>;
}

impl<T1> OwnedGet0<T1>
where T1:EventOutput, T1::Output:generics::GetItemAt0, generics::ItemAt0<T1::Output>:Data {
    /// Constructor.
    pub fn new(label:Label, src:&T1) -> Self {
        let event      = src.clone_ref();
        let definition = Get0Data {event};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<T1> stream::EventConsumer<Output<T1>> for OwnedGet0<T1>
where T1:EventOutput, T1::Output:generics::GetItemAt0, generics::ItemAt0<T1::Output>:Data {
    fn on_event(&self, event:&Output<T1>) {
        self.emit_event((*event)._0())
    }
}

impl<T1> stream::InputBehaviors for Get0Data<T1> {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![]
    }
}



// ============
// === Get1 ===
// ============

#[derive(Debug)]
pub struct Get1Data  <T1> { event:T1 }
pub type   OwnedGet1 <T1> = stream::Node     <Get1Data<T1>>;
pub type   Get1      <T1> = stream::WeakNode <Get1Data<T1>>;

impl<T1> HasOutput for Get1Data<T1>
    where T1:EventOutput, T1::Output:generics::GetItemAt1, generics::ItemAt1<T1::Output>:Data {
    type Output = generics::ItemAt1<T1::Output>;
}

impl<T1> OwnedGet1<T1>
    where T1:EventOutput, T1::Output:generics::GetItemAt1, generics::ItemAt1<T1::Output>:Data {
    /// Constructor.
    pub fn new(label:Label, src:&T1) -> Self {
        let event      = src.clone_ref();
        let definition = Get1Data {event};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<T1> stream::EventConsumer<Output<T1>> for OwnedGet1<T1>
    where T1:EventOutput, T1::Output:generics::GetItemAt1, generics::ItemAt1<T1::Output>:Data {
    fn on_event(&self, event:&Output<T1>) {
        self.emit_event((*event)._1())
    }
}

impl<T1> stream::InputBehaviors for Get1Data<T1> {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![]
    }
}


// ============
// === Get2 ===
// ============

#[derive(Debug)]
pub struct Get2Data  <T1> { event:T1 }
pub type   OwnedGet2 <T1> = stream::Node     <Get2Data<T1>>;
pub type   Get2      <T1> = stream::WeakNode <Get2Data<T1>>;

impl<T1> HasOutput for Get2Data<T1>
    where T1:EventOutput, T1::Output:generics::GetItemAt2, generics::ItemAt2<T1::Output>:Data {
    type Output = generics::ItemAt2<T1::Output>;
}

impl<T1> OwnedGet2<T1>
    where T1:EventOutput, T1::Output:generics::GetItemAt2, generics::ItemAt2<T1::Output>:Data {
    /// Constructor.
    pub fn new(label:Label, src:&T1) -> Self {
        let event      = src.clone_ref();
        let definition = Get2Data {event};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<T1> stream::EventConsumer<Output<T1>> for OwnedGet2<T1>
    where T1:EventOutput, T1::Output:generics::GetItemAt2, generics::ItemAt2<T1::Output>:Data {
    fn on_event(&self, event:&Output<T1>) {
        self.emit_event((*event)._2())
    }
}

impl<T1> stream::InputBehaviors for Get2Data<T1> {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![]
    }
}



// ============
// === Iter ===
// ============

#[derive(Debug)]
pub struct IterData  <T1> { event:T1 }
pub type   OwnedIter <T1> = stream::Node     <IterData<T1>>;
pub type   Iter      <T1> = stream::WeakNode <IterData<T1>>;

impl<T1,X> HasOutput for IterData<T1>
where T1:EventOutput, X:Data, for<'t> &'t T1::Output:IntoIterator<Item=&'t X> {
    type Output = X;
}

impl<T1,X> OwnedIter<T1>
where T1:EventOutput, X:Data, for<'t> &'t T1::Output:IntoIterator<Item=&'t X> {
    /// Constructor.
    pub fn new(label:Label, src:&T1) -> Self {
        let event      = src.clone_ref();
        let definition = IterData {event};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<T1,X> stream::EventConsumer<Output<T1>> for OwnedIter<T1>
where T1:EventOutput, X:Data, for<'t> &'t T1::Output:IntoIterator<Item=&'t X> {
    fn on_event(&self, event:&Output<T1>) {
        for val in event {
            self.emit_event(val)
        }
    }
}

impl<T1> stream::InputBehaviors for IterData<T1> {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![]
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

pub struct MapData  <T,F> { _source:T, function:F }
pub type   OwnedMap <T,F> = stream::Node     <MapData<T,F>>;
pub type   Map      <T,F> = stream::WeakNode <MapData<T,F>>;

impl<T,F,Out> HasOutput for MapData<T,F>
where T:EventOutput, Out:Data, F:'static+Fn(&Output<T>)->Out {
    type Output = Out;
}

impl<T,F,Out> OwnedMap<T,F>
where T:EventOutput, Out:Data, F:'static+Fn(&Output<T>)->Out {
    /// Constructor.
    pub fn new(label:Label, src:&T, function:F) -> Self {
        let _source    = src.clone_ref();
        let definition = MapData {_source,function};
        Self::construct_and_connect(label,src,definition)
    }
}

impl<T,F,Out> stream::EventConsumer<Output<T>> for OwnedMap<T,F>
where T:EventOutput, Out:Data, F:'static+Fn(&Output<T>)->Out {
    fn on_event(&self, value:&Output<T>) {
        let out = (self.function)(value);
        self.emit_event(&out);
    }
}

impl<T,F> Debug for MapData<T,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"MapData")
    }
}



// ============
// === Map2 ===
// ============

pub struct Map2Data  <T1,T2,F> { _source1:T1, source2:watch::Ref<T2>, function:F }
pub type   OwnedMap2 <T1,T2,F> = stream::Node     <Map2Data<T1,T2,F>>;
pub type   Map2      <T1,T2,F> = stream::WeakNode <Map2Data<T1,T2,F>>;

impl<T1,T2,F,Out> HasOutput for Map2Data<T1,T2,F>
where T1:EventOutput, T2:EventOutput, Out:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->Out {
    type Output = Out;
}

impl<T1,T2,F,Out> OwnedMap2<T1,T2,F>
where T1:EventOutput, T2:EventOutput, Out:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->Out {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2, function:F) -> Self {
        let _source1 = t1.clone_ref();
        let source2  = watch_stream(t2);
        let def      = Map2Data {_source1,source2,function};
        let this     = Self::construct(label,def);
        let weak     = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<T1,T2,F,Out> stream::EventConsumer<Output<T1>> for OwnedMap2<T1,T2,F>
where T1:EventOutput, T2:EventOutput, Out:Data, F:'static+Fn(&Output<T1>,&Output<T2>)->Out {
    fn on_event(&self, value1:&Output<T1>) {
        let value2 = self.source2.value();
        let out    = (self.function)(&value1,&value2);
        self.emit_event(&out);
    }
}

impl<T1,T2,F> Debug for Map2Data<T1,T2,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Map2Data")
    }
}

impl<T1,T2,F> stream::InputBehaviors for Map2Data<T1,T2,F>
where T1:EventOutput, T2:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.source2)]
    }
}



// ============
// === Map3 ===
// ============

pub struct Map3Data <T1,T2,T3,F>
    { _source1:T1, source2:watch::Ref<T2>, source3:watch::Ref<T3>, function:F }
pub type   OwnedMap3 <T1,T2,T3,F> = stream::Node     <Map3Data<T1,T2,T3,F>>;
pub type   Map3      <T1,T2,T3,F> = stream::WeakNode <Map3Data<T1,T2,T3,F>>;

impl<T1,T2,T3,F,Out> HasOutput for Map3Data<T1,T2,T3,F>
where T1:EventOutput, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->Out {
    type Output = Out;
}

impl<T1,T2,T3,F,Out> OwnedMap3<T1,T2,T3,F>
where T1:EventOutput, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->Out {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2, t3:&T3, function:F) -> Self {
        let _source1 = t1.clone_ref();
        let source2  = watch_stream(t2);
        let source3  = watch_stream(t3);
        let def      = Map3Data {_source1,source2,source3,function};
        let this     = Self::construct(label,def);
        let weak     = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<T1,T2,T3,F,Out> stream::EventConsumer<Output<T1>> for OwnedMap3<T1,T2,T3,F>
where T1:EventOutput, T2:EventOutput, T3:EventOutput, Out:Data,
      F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>)->Out {
    fn on_event(&self, value1:&Output<T1>) {
        let value2 = self.source2.value();
        let value3 = self.source3.value();
        let out    = (self.function)(&value1,&value2,&value3);
        self.emit_event(&out);
    }
}

impl<T1,T2,T3,F> Debug for Map3Data<T1,T2,T3,F> {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Map3Data")
    }
}

impl<T1,T2,T3,F> stream::InputBehaviors for Map3Data<T1,T2,T3,F>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.source2), Link::behavior(&self.source3)]
    }
}



// ============
// === Map4 ===
// ============

pub struct Map4Data <T1,T2,T3,T4,F>
    { _source1:T1
    , source2:watch::Ref<T2>, source3:watch::Ref<T3>, source4:watch::Ref<T4>, function:F }
pub type   OwnedMap4 <T1,T2,T3,T4,F> = stream::Node     <Map4Data<T1,T2,T3,T4,F>>;
pub type   Map4      <T1,T2,T3,T4,F> = stream::WeakNode <Map4Data<T1,T2,T3,T4,F>>;

impl<T1,T2,T3,T4,F,Out> HasOutput for Map4Data<T1,T2,T3,T4,F>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    type Output = Out;
}

impl<T1,T2,T3,T4,F,Out> OwnedMap4<T1,T2,T3,T4,F>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    /// Constructor.
    pub fn new(label:Label, t1:&T1, t2:&T2, t3:&T3, t4:&T4, function:F) -> Self {
        let _source1 = t1.clone_ref();
        let source2  = watch_stream(t2);
        let source3  = watch_stream(t3);
        let source4  = watch_stream(t4);
        let def      = Map4Data {_source1,source2,source3,source4,function};
        let this     = Self::construct(label,def);
        let weak     = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<T1,T2,T3,T4,F,Out> stream::EventConsumer<Output<T1>> for OwnedMap4<T1,T2,T3,T4,F>
    where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput, Out:Data,
          F:'static+Fn(&Output<T1>,&Output<T2>,&Output<T3>,&Output<T4>)->Out {
    fn on_event(&self, value1:&Output<T1>) {
        let value2 = self.source2.value();
        let value3 = self.source3.value();
        let value4 = self.source4.value();
        let out    = (self.function)(&value1,&value2,&value3,&value4);
        self.emit_event(&out);
    }
}

impl<T1,T2,T3,T4,F> stream::InputBehaviors for Map4Data<T1,T2,T3,T4,F>
where T1:EventOutput, T2:EventOutput, T3:EventOutput, T4:EventOutput {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![ Link::behavior(&self.source2)
            , Link::behavior(&self.source3)
            , Link::behavior(&self.source4)
            ]
    }
}

impl<T1,T2,T3,T4,F> Debug for Map4Data<T1,T2,T3,T4,F> {
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
