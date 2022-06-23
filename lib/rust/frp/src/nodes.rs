//! Definition of all FRP stream nodes.
//!
//! Please note that the documentation is provided for methods of `Network`, as this is considered
//! to be the public API. The same documentation applies to node definitions below.

// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]
#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

use crate::network::*;
use crate::node::*;
use crate::prelude::*;
use enso_generics::traits::*;

use crate::data::watch;
use crate::stream;
use crate::stream::CallStack;
use crate::stream::EventOutput;
use crate::stream::OwnedStream;
use crate::stream::Stream;
use crate::stream::ValueProvider;

use enso_generics as generics;



// ========================
// === Network Node API ===
// ========================

impl Network {
    /// Begin point in the FRP network. It does not accept inputs, but it is able to emit events.
    /// Often it is used to indicate that something happened, like a button was pressed. In such
    /// case its type parameter is set to an empty tuple.
    pub fn source<T: Data>(&self, label: Label) -> Source<T> {
        self.register_raw(OwnedSource::new(label))
    }

    /// Starting point in the FRP network. Specialized version of `source`.
    pub fn source_(&self, label: Label) -> Source {
        self.register_raw(OwnedSource::new(label))
    }

    /// Remember the last event value and allow sampling it anytime.
    pub fn sampler<T, Out>(&self, label: Label, src: &T) -> Sampler<Out>
    where
        T: EventOutput<Output = Out>,
        Out: Data, {
        self.register_raw(OwnedSampler::new(label, src))
    }

    /// Print the incoming events to console and pass them to output.
    pub fn trace<T: EventOutput>(&self, label: Label, src: &T) -> Stream<Output<T>> {
        self.register(OwnedTrace::new(label, src))
    }

    /// Profile the event resolution from this node onwards and log the result in the profiling
    /// framework.
    pub fn profile<T: EventOutput>(&self, label: Label, src: &T) -> Stream<Output<T>> {
        self.register(OwnedProfile::new(label, src))
    }

    /// Emits `true`, `false`, `true`, `false`, ... on every incoming event. Initialized with false
    /// value.
    pub fn toggle<T: EventOutput>(&self, label: Label, src: &T) -> Stream<bool> {
        self.register(OwnedToggle::new(label, src))
    }

    /// Emits `false`, `true`, `false`, `true`, ... on every incoming event. Initialized with true
    /// value.
    pub fn toggle_true<T: EventOutput>(&self, label: Label, src: &T) -> Stream<bool> {
        self.register(OwnedToggle::new_with(label, src, true))
    }

    /// Count the incoming events.
    pub fn count<T: EventOutput>(&self, label: Label, src: &T) -> Stream<usize> {
        self.register(OwnedCount::new(label, src))
    }

    /// Replace the incoming event with the predefined value.
    pub fn constant<X: Data, T: EventOutput>(&self, label: Label, src: &T, value: X) -> Stream<X> {
        self.register(OwnedConstant::new(label, src, value))
    }

    /// Remembers the value of the input stream and outputs the previously received one.
    pub fn previous<T: EventOutput>(&self, label: Label, src: &T) -> Stream<Output<T>> {
        self.register(OwnedPrevious::new(label, src))
    }

    /// Samples the first stream (behavior) on every incoming event of the second stream. The
    /// incoming event is dropped and a new event with the behavior's value is emitted.
    pub fn sample<T1: EventOutput, T2: EventOutput>(
        &self,
        label: Label,
        behavior: &T1,
        event: &T2,
    ) -> Stream<Output<T1>> {
        self.register(OwnedSample::new(label, behavior, event))
    }

    /// Passes the incoming event of the first stream only if the value of the second stream is
    /// true.
    pub fn gate<T1, T2>(&self, label: Label, event: &T1, behavior: &T2) -> Stream<Output<T1>>
    where
        T1: EventOutput,
        T2: EventOutput<Output = bool>, {
        self.register(OwnedGate::new(label, event, behavior))
    }

    /// Like `gate` but passes the value when the condition is `false`.
    pub fn gate_not<T1, T2>(&self, label: Label, event: &T1, behavior: &T2) -> Stream<Output<T1>>
    where
        T1: EventOutput,
        T2: EventOutput<Output = bool>, {
        self.register(OwnedGateNot::new(label, event, behavior))
    }

    pub fn unwrap<T, S>(&self, label: Label, event: &T) -> Stream<S>
    where
        T: EventOutput<Output = Option<S>>,
        S: Data, {
        self.register(OwnedUnwrap::new(label, event))
    }

    pub fn iter<T1, X>(&self, label: Label, event: &T1) -> Stream<X>
    where
        T1: EventOutput,
        for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
        X: Data, {
        self.register(OwnedIter::new(label, event))
    }

    pub fn fold<T1, X>(&self, label: Label, event: &T1) -> Stream<X>
    where
        T1: EventOutput,
        for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
        X: Data + Monoid, {
        self.register(OwnedFold::new(label, event))
    }

    pub fn _0<T1>(&self, label: Label, event: &T1) -> Stream<generics::ItemAt0<Output<T1>>>
    where
        T1: EventOutput,
        T1::Output: generics::GetItemAt0,
        generics::ItemAt0<T1::Output>: Data, {
        self.register(OwnedGet0::new(label, event))
    }

    pub fn _1<T1>(&self, label: Label, event: &T1) -> Stream<generics::ItemAt1<Output<T1>>>
    where
        T1: EventOutput,
        T1::Output: generics::GetItemAt1,
        generics::ItemAt1<T1::Output>: Data, {
        self.register(OwnedGet1::new(label, event))
    }

    pub fn _2<T1>(&self, label: Label, event: &T1) -> Stream<generics::ItemAt2<Output<T1>>>
    where
        T1: EventOutput,
        T1::Output: generics::GetItemAt2,
        generics::ItemAt2<T1::Output>: Data, {
        self.register(OwnedGet2::new(label, event))
    }

    /// Only if the input event has changed, emit the input event. This will hide multiple
    /// consecutive events with the same value.
    pub fn on_change<T, V>(&self, label: Label, t: &T) -> Stream<V>
    where
        T: EventOutput<Output = V>,
        V: Data + PartialEq, {
        let prev = self.previous(label, t);
        let changed = self.map2(label, t, &prev, |t1, t2| t1 != t2);
        self.gate(label, t, &changed)
    }


    // === Bool Utils ===

    /// Replace the incoming event with `true`.
    pub fn to_true<T: EventOutput>(&self, label: Label, src: &T) -> Stream<bool> {
        self.constant(label, src, true)
    }

    /// Replace the incoming event with `false`.
    pub fn to_false<T: EventOutput>(&self, label: Label, src: &T) -> Stream<bool> {
        self.constant(label, src, false)
    }

    /// Whenever the input event is `true`, emit the output event.
    pub fn on_true<T>(&self, label: Label, t: &T) -> Stream
    where T: EventOutput<Output = bool> {
        let t_ = self.constant(label, t, ());
        self.gate(label, &t_, t)
    }

    /// Whenever the input event is `false`, emit the output event.
    pub fn on_false<T>(&self, label: Label, t: &T) -> Stream
    where T: EventOutput<Output = bool> {
        let t_ = self.constant(label, t, ());
        self.gate_not(label, &t_, t)
    }

    /// Replace the incoming event from first input with `false` and from second with `true`.
    pub fn bool<T1, T2>(&self, label: Label, src1: &T1, src2: &T2) -> Stream<bool>
    where
        T1: EventOutput,
        T2: EventOutput, {
        let false_ = self.to_false(label, src1);
        let true_ = self.to_true(label, src2);
        self.any(label, &false_, &true_)
    }

    /// On every input event, output its negation.
    pub fn not<T>(&self, label: Label, src: &T) -> Stream<bool>
    where T: EventOutput<Output = bool> {
        self.map(label, src, |t| !t)
    }

    /// On every input event, sample all input streams and output their `or` value.
    pub fn or<T1, T2>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<bool>
    where
        T1: EventOutput<Output = bool>,
        T2: EventOutput<Output = bool>, {
        self.all_with(label, t1, t2, |a, b| *a || *b)
    }

    /// On every input event, sample all input streams and output their `and` value.
    pub fn and<T1, T2>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<bool>
    where
        T1: EventOutput<Output = bool>,
        T2: EventOutput<Output = bool>, {
        self.all_with(label, t1, t2, |a, b| *a && *b)
    }

    /// Redirect second or third input to the output when the value of the first input is `false` or
    /// `true` respectively. The redirection is persistent. The first input doesn't have to fire to
    /// propagate the events fromm second and third input streams. Moreover, when first input
    /// changes, an output event will be emitted with the updated value.
    pub fn switch<T1, T2, T3, T>(&self, label: Label, check: &T1, t2: &T2, t3: &T3) -> Stream<T>
    where
        T1: EventOutput<Output = bool>,
        T2: EventOutput<Output = T>,
        T3: EventOutput<Output = T>,
        T: Data, {
        self.all_with3(
            label,
            check,
            t2,
            t3,
            |check, t1, t2| if *check { t2.clone() } else { t1.clone() },
        )
    }


    // === Any ===

    /// Merges multiple input streams into a single output stream. All input streams have to share
    /// the same output data type. Please note that `any_mut` can be used to create recursive FRP
    /// networks by creating an empty `any` and using the `attach` method to attach new streams to
    /// it. When a recursive network is created, `any_mut` breaks the cycle. After passing the first
    /// event, no more events will be passed till the end of the current FRP network resolution.
    pub fn any_mut<T: Data>(&self, label: Label) -> Any<T> {
        self.register_raw(OwnedAny::new(label))
    }

    /// Merges multiple input streams into a single output stream. All input streams have to share
    /// the same output data type.
    pub fn any<T1, T2, T: Data>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>, {
        self.register(OwnedAny::new2(label, t1, t2))
    }

    /// Specialized version of `any`.
    pub fn any2<T1, T2, T: Data>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>, {
        self.register(OwnedAny::new2(label, t1, t2))
    }

    /// Specialized version of `any`.
    pub fn any3<T1, T2, T3, T: Data>(&self, label: Label, t1: &T1, t2: &T2, t3: &T3) -> Stream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>,
        T3: EventOutput<Output = T>, {
        self.register(OwnedAny::new3(label, t1, t2, t3))
    }

    /// Specialized version of `any`.
    pub fn any4<T1, T2, T3, T4, T: Data>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
    ) -> Stream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>,
        T3: EventOutput<Output = T>,
        T4: EventOutput<Output = T>,
    {
        self.register(OwnedAny::new4(label, t1, t2, t3, t4))
    }

    /// Specialized version of `any`.
    pub fn any5<T1, T2, T3, T4, T5, T: Data>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> Stream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>,
        T3: EventOutput<Output = T>,
        T4: EventOutput<Output = T>,
        T5: EventOutput<Output = T>,
    {
        self.register(OwnedAny::new5(label, t1, t2, t3, t4, t5))
    }


    // === Any_ ===

    /// Like `any_mut` but drops the incoming data. You can attach streams of different types.
    pub fn any_mut_(&self, label: Label) -> Any_ {
        self.register_raw(OwnedAny_::new(label))
    }

    /// Like `any` but drops the incoming data. You can attach streams of different types.
    pub fn any_<T1, T2>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<()>
    where
        T1: EventOutput,
        T2: EventOutput, {
        self.register(OwnedAny_::new2(label, t1, t2))
    }

    /// Specialized version of `any_`.
    pub fn any2_<T1, T2>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<()>
    where
        T1: EventOutput,
        T2: EventOutput, {
        self.register(OwnedAny_::new2(label, t1, t2))
    }

    /// Specialized version of `any_`.
    pub fn any3_<T1, T2, T3>(&self, label: Label, t1: &T1, t2: &T2, t3: &T3) -> Stream<()>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput, {
        self.register(OwnedAny_::new3(label, t1, t2, t3))
    }

    /// Specialized version of `any_`.
    pub fn any4_<T1, T2, T3, T4>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
    ) -> Stream<()>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
    {
        self.register(OwnedAny_::new4(label, t1, t2, t3, t4))
    }

    /// Specialized version of `any_`.
    pub fn any5_<T1, T2, T3, T4, T5>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> Stream<()>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
    {
        self.register(OwnedAny_::new5(label, t1, t2, t3, t4, t5))
    }


    // === All ===

    /// Merges input streams into a stream containing values from all of them. On event from any of
    /// the input streams, all streams are sampled and the final event is produced.
    pub fn all_mut<T: Data>(&self, label: Label) -> AllMut<T> {
        self.register_raw(OwnedAllMut::new(label))
    }

    pub fn all_vec2<Out, T1, T2>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<Vec<Out>>
    where
        Out: Data,
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>, {
        self.register(OwnedAllMut::new(label).with(t1).with(t2))
    }

    pub fn all_vec3<Out, T1, T2, T3>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
    ) -> Stream<Vec<Out>>
    where
        Out: Data,
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
    {
        self.register(OwnedAllMut::new(label).with(t1).with(t2).with(t3))
    }

    pub fn all_vec4<Out, T1, T2, T3, T4>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
    ) -> Stream<Vec<Out>>
    where
        Out: Data,
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>,
    {
        self.register(OwnedAllMut::new(label).with(t1).with(t2).with(t3).with(t4))
    }

    pub fn all_vec5<Out, T1, T2, T3, T4, T5>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> Stream<Vec<Out>>
    where
        Out: Data,
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>,
        T5: EventOutput<Output = Out>,
    {
        self.register(OwnedAllMut::new(label).with(t1).with(t2).with(t3).with(t4).with(t5))
    }

    pub fn all_vec6<Out, T1, T2, T3, T4, T5, T6>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        t6: &T6,
    ) -> Stream<Vec<Out>>
    where
        Out: Data,
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>,
        T5: EventOutput<Output = Out>,
        T6: EventOutput<Output = Out>,
    {
        self.register(OwnedAllMut::new(label).with(t1).with(t2).with(t3).with(t4).with(t5).with(t6))
    }

    pub fn all_vec7<Out, T1, T2, T3, T4, T5, T6, T7>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        t6: &T6,
        t7: &T7,
    ) -> Stream<Vec<Out>>
    where
        Out: Data,
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>,
        T5: EventOutput<Output = Out>,
        T6: EventOutput<Output = Out>,
        T7: EventOutput<Output = Out>,
    {
        self.register(
            OwnedAllMut::new(label).with(t1).with(t2).with(t3).with(t4).with(t5).with(t6).with(t7),
        )
    }

    pub fn all_vec8<Out, T1, T2, T3, T4, T5, T6, T7, T8>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        t6: &T6,
        t7: &T7,
        t8: &T8,
    ) -> Stream<Vec<Out>>
    where
        Out: Data,
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>,
        T5: EventOutput<Output = Out>,
        T6: EventOutput<Output = Out>,
        T7: EventOutput<Output = Out>,
        T8: EventOutput<Output = Out>,
    {
        self.register(
            OwnedAllMut::new(label)
                .with(t1)
                .with(t2)
                .with(t3)
                .with(t4)
                .with(t5)
                .with(t6)
                .with(t7)
                .with(t8),
        )
    }

    pub fn all_vec9<Out, T1, T2, T3, T4, T5, T6, T7, T8, T9>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        t6: &T6,
        t7: &T7,
        t8: &T8,
        t9: &T9,
    ) -> Stream<Vec<Out>>
    where
        Out: Data,
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>,
        T5: EventOutput<Output = Out>,
        T6: EventOutput<Output = Out>,
        T7: EventOutput<Output = Out>,
        T8: EventOutput<Output = Out>,
        T9: EventOutput<Output = Out>,
    {
        self.register(
            OwnedAllMut::new(label)
                .with(t1)
                .with(t2)
                .with(t3)
                .with(t4)
                .with(t5)
                .with(t6)
                .with(t7)
                .with(t8)
                .with(t9),
        )
    }

    /// Merges input streams into a stream containing values from all of them. On event from any of
    /// the input streams, all streams are sampled and the final event is produced.
    pub fn all<T1, T2>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<(Output<T1>, Output<T2>)>
    where
        T1: EventOutput,
        T2: EventOutput, {
        self.register(OwnedAll2::new(label, t1, t2))
    }

    /// Specialized version of `all`.
    pub fn all2<T1, T2>(&self, label: Label, t1: &T1, t2: &T2) -> Stream<(Output<T1>, Output<T2>)>
    where
        T1: EventOutput,
        T2: EventOutput, {
        self.register(OwnedAll2::new(label, t1, t2))
    }

    /// Specialized version of `all`.
    pub fn all3<T1, T2, T3>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
    ) -> Stream<(Output<T1>, Output<T2>, Output<T3>)>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
    {
        self.register(OwnedAll3::new(label, t1, t2, t3))
    }

    /// Specialized version of `all`.
    pub fn all4<T1, T2, T3, T4>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
    ) -> Stream<(Output<T1>, Output<T2>, Output<T3>, Output<T4>)>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
    {
        self.register(OwnedAll4::new(label, t1, t2, t3, t4))
    }

    /// Specialized version of `all`.
    pub fn all5<T1, T2, T3, T4, T5>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> Stream<(Output<T1>, Output<T2>, Output<T3>, Output<T4>, Output<T5>)>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
    {
        self.register(OwnedAll5::new(label, t1, t2, t3, t4, t5))
    }


    // === Filter ===

    /// Passes exactly those incoming events that satisfy the predicate `p`.
    pub fn filter<T, P>(&self, label: Label, src: &T, p: P) -> Stream<Output<T>>
    where
        T: EventOutput,
        P: 'static + Fn(&Output<T>) -> bool, {
        self.register(OwnedFilter::new(label, src, p))
    }


    // === FilterMap ===

    /// Applies the function `f` to the value of all incoming events. If the resulting `Option`
    /// caries a value then this value is passed on. Otherwise, nothing happens.
    pub fn filter_map<T, F, Out>(&self, label: Label, src: &T, f: F) -> Stream<Out>
    where
        T: EventOutput,
        Out: Data,
        F: 'static + Fn(&Output<T>) -> Option<Out>, {
        self.register(OwnedFilterMap::new(label, src, f))
    }


    // === Map ===

    /// On every event from the first input stream, sample all other input streams and run the
    /// provided function on all gathered values. If you want to run the function on event from any
    /// input stream, use the `all_with` function family instead.
    pub fn map<T, F, Out>(&self, label: Label, src: &T, f: F) -> Stream<Out>
    where
        T: EventOutput,
        Out: Data,
        F: 'static + Fn(&Output<T>) -> Out, {
        self.register(OwnedMap::new(label, src, f))
    }

    /// A shortcut for `.map(|v| Some(v.clone()))`.
    pub fn some<T>(&self, label: Label, src: &T) -> Stream<Option<Output<T>>>
    where T: EventOutput {
        self.map(label, src, |value| Some(value.clone()))
    }

    /// Specialized version of `map`.
    pub fn map2<T1, T2, F, T>(&self, label: Label, t1: &T1, t2: &T2, f: F) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>) -> T, {
        self.register(OwnedMap2::new(label, t1, t2, f))
    }

    /// Specialized version of `map`.
    pub fn map3<T1, T2, T3, F, T>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        f: F,
    ) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> T,
    {
        self.register(OwnedMap3::new(label, t1, t2, t3, f))
    }

    /// Specialized version of `map`.
    pub fn map4<T1, T2, T3, T4, F, T>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        f: F,
    ) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> T,
    {
        self.register(OwnedMap4::new(label, t1, t2, t3, t4, f))
    }


    // === AllWith ===

    /// On every input event sample all input streams and run the provided function on all gathered
    /// values. If you want to run the function only on event on the first input, use the `map`
    /// function family instead.
    pub fn all_with<T1, T2, F, T>(&self, label: Label, t1: &T1, t2: &T2, f: F) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>) -> T, {
        self.register(OwnedAllWith2::new(label, t1, t2, f))
    }

    /// Specialized version `all_with`.
    pub fn all_with3<T1, T2, T3, F, T>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        f: F,
    ) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> T,
    {
        self.register(OwnedAllWith3::new(label, t1, t2, t3, f))
    }

    /// Specialized version `all_with`.
    pub fn all_with4<T1, T2, T3, T4, F, T>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        f: F,
    ) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> T,
    {
        self.register(OwnedAllWith4::new(label, t1, t2, t3, t4, f))
    }

    /// Specialized version `all_with`.
    pub fn all_with5<T1, T2, T3, T4, T5, F, T>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        f: F,
    ) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>) -> T,
    {
        self.register(OwnedAllWith5::new(label, t1, t2, t3, t4, t5, f))
    }

    /// Specialized version `all_with`.
    pub fn all_with6<T1, T2, T3, T4, T5, T6, F, T>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        t6: &T6,
        f: F,
    ) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
        T6: EventOutput,
        T: Data,
        F: 'static
            + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>, &Output<T6>) -> T,
    {
        self.register(OwnedAllWith6::new(label, t1, t2, t3, t4, t5, t6, f))
    }

    /// Specialized version `all_with`.
    pub fn all_with8<T1, T2, T3, T4, T5, T6, T7, T8, F, T>(
        &self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        t6: &T6,
        t7: &T7,
        t8: &T8,
        f: F,
    ) -> Stream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
        T6: EventOutput,
        T7: EventOutput,
        T8: EventOutput,
        T: Data,
        F: 'static
            + Fn(
                &Output<T1>,
                &Output<T2>,
                &Output<T3>,
                &Output<T4>,
                &Output<T5>,
                &Output<T6>,
                &Output<T7>,
                &Output<T8>,
            ) -> T,
    {
        self.register(OwnedAllWith8::new(label, t1, t2, t3, t4, t5, t6, t7, t8, f))
    }


    // === Repeat ===

    /// Repeat node listens for input events of type [`usize`] and emits events in number equal to
    /// the input event value.
    pub fn repeat<T>(&self, label: Label, src: &T) -> Stream<()>
    where T: EventOutput<Output = usize> {
        self.register(OwnedRepeat::new(label, src))
    }
}



// ========================
// === Dynamic Node API ===
// ========================

/// This is a phantom structure used by macros to create dynamic FRP graphs. It exposes the same
/// API as `Network` in order to reuse macro code for both network and dynamic modes.
#[derive(Clone, Copy, Debug, Default)]
pub struct DynamicNetwork {}

impl DynamicNetwork {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

/// See docs of `Network` to learn about the methods.
impl DynamicNetwork {
    pub fn source<T: Data>(self, label: Label) -> OwnedSource<T> {
        OwnedSource::new(label)
    }

    pub fn source_(self, label: Label) -> OwnedSource {
        OwnedSource::new(label)
    }

    pub fn sampler<T, Out>(self, label: Label, src: &T) -> OwnedSampler<Out>
    where
        T: EventOutput<Output = Out>,
        Out: Data, {
        OwnedSampler::new(label, src)
    }

    pub fn trace<T: EventOutput>(self, label: Label, src: &T) -> OwnedStream<Output<T>> {
        OwnedTrace::new(label, src).into()
    }

    pub fn profile<T: EventOutput>(self, label: Label, src: &T) -> OwnedStream<Output<T>> {
        OwnedProfile::new(label, src).into()
    }

    pub fn toggle<T: EventOutput>(self, label: Label, src: &T) -> OwnedStream<bool> {
        OwnedToggle::new(label, src).into()
    }

    pub fn count<T: EventOutput>(self, label: Label, src: &T) -> OwnedCount<T> {
        OwnedCount::new(label, src)
    }

    pub fn constant<X: Data, T: EventOutput>(
        self,
        label: Label,
        src: &T,
        value: X,
    ) -> OwnedStream<X> {
        OwnedConstant::new(label, src, value).into()
    }

    pub fn previous<T: EventOutput>(self, label: Label, src: &T) -> OwnedStream<Output<T>> {
        OwnedPrevious::new(label, src).into()
    }

    pub fn sample<T1: EventOutput, T2: EventOutput>(
        self,
        label: Label,
        behavior: &T1,
        event: &T2,
    ) -> OwnedStream<Output<T1>> {
        OwnedSample::new(label, behavior, event).into()
    }

    pub fn gate<T1, T2>(self, label: Label, event: &T1, behavior: &T2) -> OwnedStream<Output<T1>>
    where
        T1: EventOutput,
        T2: EventOutput<Output = bool>, {
        OwnedGate::new(label, event, behavior).into()
    }

    pub fn gate_not<T1, T2>(
        self,
        label: Label,
        event: &T1,
        behavior: &T2,
    ) -> OwnedStream<Output<T1>>
    where
        T1: EventOutput,
        T2: EventOutput<Output = bool>,
    {
        OwnedGateNot::new(label, event, behavior).into()
    }

    pub fn iter<T1, X>(self, label: Label, event: &T1) -> OwnedStream<X>
    where
        T1: EventOutput,
        for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
        X: Data, {
        OwnedIter::new(label, event).into()
    }

    pub fn fold<T1, X>(self, label: Label, event: &T1) -> OwnedStream<X>
    where
        T1: EventOutput,
        for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
        X: Data + Monoid, {
        OwnedFold::new(label, event).into()
    }

    pub fn _0<T1>(self, label: Label, event: &T1) -> OwnedStream<generics::ItemAt0<Output<T1>>>
    where
        T1: EventOutput,
        T1::Output: generics::GetItemAt0,
        generics::ItemAt0<T1::Output>: Data, {
        OwnedGet0::new(label, event).into()
    }

    pub fn _1<T1>(self, label: Label, event: &T1) -> OwnedStream<generics::ItemAt1<Output<T1>>>
    where
        T1: EventOutput,
        T1::Output: generics::GetItemAt1,
        generics::ItemAt1<T1::Output>: Data, {
        OwnedGet1::new(label, event).into()
    }

    pub fn _2<T1>(self, label: Label, event: &T1) -> OwnedStream<generics::ItemAt2<Output<T1>>>
    where
        T1: EventOutput,
        T1::Output: generics::GetItemAt2,
        generics::ItemAt2<T1::Output>: Data, {
        OwnedGet2::new(label, event).into()
    }


    // === Any ===

    pub fn any_mut<T: Data>(self, label: Label) -> OwnedAny<T> {
        OwnedAny::new(label)
    }

    pub fn any<T1, T2, T: Data>(self, label: Label, t1: &T1, t2: &T2) -> OwnedStream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>, {
        OwnedAny::new2(label, t1, t2).into()
    }

    pub fn any2<T1, T2, T: Data>(self, label: Label, t1: &T1, t2: &T2) -> OwnedStream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>, {
        OwnedAny::new2(label, t1, t2).into()
    }

    pub fn any3<T1, T2, T3, T: Data>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
    ) -> OwnedStream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>,
        T3: EventOutput<Output = T>,
    {
        OwnedAny::new3(label, t1, t2, t3).into()
    }

    pub fn any4<T1, T2, T3, T4, T: Data>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
    ) -> OwnedStream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>,
        T3: EventOutput<Output = T>,
        T4: EventOutput<Output = T>,
    {
        OwnedAny::new4(label, t1, t2, t3, t4).into()
    }

    pub fn any5<T1, T2, T3, T4, T5, T: Data>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> OwnedStream<T>
    where
        T1: EventOutput<Output = T>,
        T2: EventOutput<Output = T>,
        T3: EventOutput<Output = T>,
        T4: EventOutput<Output = T>,
        T5: EventOutput<Output = T>,
    {
        OwnedAny::new5(label, t1, t2, t3, t4, t5).into()
    }


    // === Any_ ===

    pub fn any_mut_(self, label: Label) -> OwnedAny_ {
        OwnedAny_::new(label)
    }

    pub fn any_<T1, T2>(self, label: Label, t1: &T1, t2: &T2) -> OwnedStream<()>
    where
        T1: EventOutput,
        T2: EventOutput, {
        OwnedAny_::new2(label, t1, t2).into()
    }

    pub fn any2_<T1, T2>(self, label: Label, t1: &T1, t2: &T2) -> OwnedStream<()>
    where
        T1: EventOutput,
        T2: EventOutput, {
        OwnedAny_::new2(label, t1, t2).into()
    }

    pub fn any3_<T1, T2, T3>(self, label: Label, t1: &T1, t2: &T2, t3: &T3) -> OwnedStream<()>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput, {
        OwnedAny_::new3(label, t1, t2, t3).into()
    }

    pub fn any4_<T1, T2, T3, T4>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
    ) -> OwnedStream<()>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
    {
        OwnedAny_::new4(label, t1, t2, t3, t4).into()
    }

    pub fn any5_<T1, T2, T3, T4, T5>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> OwnedStream<()>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
    {
        OwnedAny_::new5(label, t1, t2, t3, t4, t5).into()
    }


    // === All ===

    pub fn all<T1, T2>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
    ) -> OwnedStream<(Output<T1>, Output<T2>)>
    where
        T1: EventOutput,
        T2: EventOutput,
    {
        OwnedAll2::new(label, t1, t2).into()
    }

    pub fn all2<T1, T2>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
    ) -> OwnedStream<(Output<T1>, Output<T2>)>
    where
        T1: EventOutput,
        T2: EventOutput,
    {
        OwnedAll2::new(label, t1, t2).into()
    }

    pub fn all3<T1, T2, T3>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
    ) -> OwnedStream<(Output<T1>, Output<T2>, Output<T3>)>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
    {
        OwnedAll3::new(label, t1, t2, t3).into()
    }

    pub fn all4<T1, T2, T3, T4>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
    ) -> OwnedStream<(Output<T1>, Output<T2>, Output<T3>, Output<T4>)>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
    {
        OwnedAll4::new(label, t1, t2, t3, t4).into()
    }

    pub fn all5<T1, T2, T3, T4, T5>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> OwnedStream<(Output<T1>, Output<T2>, Output<T3>, Output<T4>, Output<T5>)>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
    {
        OwnedAll5::new(label, t1, t2, t3, t4, t5).into()
    }


    // === Filter ===
    pub fn filter<T, P>(self, label: Label, src: &T, p: P) -> Stream<Output<T>>
    where
        T: EventOutput,
        P: 'static + Fn(&Output<T>) -> bool, {
        OwnedFilter::new(label, src, p).into()
    }


    // === FilterMap ===

    pub fn filter_map<T, F, Out>(self, label: Label, src: &T, f: F) -> OwnedStream<Out>
    where
        T: EventOutput,
        Out: Data,
        F: 'static + Fn(&Output<T>) -> Option<Out>, {
        OwnedFilterMap::new(label, src, f).into()
    }


    // === Map ===

    pub fn map<T, F, Out>(self, label: Label, src: &T, f: F) -> OwnedStream<Out>
    where
        T: EventOutput,
        Out: Data,
        F: 'static + Fn(&Output<T>) -> Out, {
        OwnedMap::new(label, src, f).into()
    }

    pub fn map2<T1, T2, F, T>(self, label: Label, t1: &T1, t2: &T2, f: F) -> OwnedStream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>) -> T, {
        OwnedMap2::new(label, t1, t2, f).into()
    }

    pub fn map3<T1, T2, T3, F, T>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        f: F,
    ) -> OwnedStream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> T,
    {
        OwnedMap3::new(label, t1, t2, t3, f).into()
    }

    pub fn map4<T1, T2, T3, T4, F, T>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        f: F,
    ) -> OwnedStream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> T,
    {
        OwnedMap4::new(label, t1, t2, t3, t4, f).into()
    }


    // === AllWith ===

    pub fn apply2<T1, T2, F, T>(self, label: Label, t1: &T1, t2: &T2, f: F) -> OwnedStream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>) -> T, {
        OwnedAllWith2::new(label, t1, t2, f).into()
    }

    pub fn apply3<T1, T2, T3, F, T>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        f: F,
    ) -> OwnedStream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> T,
    {
        OwnedAllWith3::new(label, t1, t2, t3, f).into()
    }

    pub fn apply4<T1, T2, T3, T4, F, T>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        f: F,
    ) -> OwnedStream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> T,
    {
        OwnedAllWith4::new(label, t1, t2, t3, t4, f).into()
    }

    pub fn apply5<T1, T2, T3, T4, T5, F, T>(
        self,
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        f: F,
    ) -> OwnedStream<T>
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
        T: Data,
        F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>) -> T,
    {
        OwnedAllWith5::new(label, t1, t2, t3, t4, t5, f).into()
    }
}



// =================================================================================================
// === Nodes Definitions ===========================================================================
// =================================================================================================

fn watch_stream<T: EventOutput>(target: &T) -> watch::Ref<T> {
    let target = target.clone_ref();
    let handle = target.register_watch();
    watch::Ref::new(target, handle)
}


// ==============
// === Source ===
// ==============

#[derive(Debug)]
pub struct SourceData<Out = ()> {
    phantom: PhantomData<Out>,
}
pub type OwnedSource<Out = ()> = stream::Node<SourceData<Out>>;
pub type Source<Out = ()> = stream::WeakNode<SourceData<Out>>;

impl<Out: Data> HasOutput for SourceData<Out> {
    type Output = Out;
}

impl<Out: Data> OwnedSource<Out> {
    /// Constructor.
    pub fn new(label: Label) -> Self {
        let phantom = default();
        let definition = SourceData { phantom };
        Self::construct(label, definition)
    }
}

impl<Out: Data> OwnedSource<Out> {
    /// Emit new event.
    pub fn emit<T: IntoParam<Out>>(&self, value: T) {
        self.emit_event(&default(), &value.into_param())
    }
}

impl<Out: Data> Source<Out> {
    /// Emit new event.
    pub fn emit<T: IntoParam<Out>>(&self, value: T) {
        self.emit_event(&default(), &value.into_param())
    }
}

/// The parameter of FRP system. It allows passing wide range of values to the `emit` function for
/// easy of use.
#[allow(missing_docs)]
pub trait IntoParam<T> {
    fn into_param(self) -> T;
}
impl<T> IntoParam<T> for T {
    fn into_param(self) -> T {
        self
    }
}
impl<T: Clone> IntoParam<T> for &T {
    fn into_param(self) -> T {
        self.clone()
    }
}
impl<T> IntoParam<Option<T>> for T {
    fn into_param(self) -> Option<T> {
        Some(self)
    }
}
impl<T: Clone> IntoParam<Option<T>> for &T {
    fn into_param(self) -> Option<T> {
        Some(self.clone())
    }
}
impl IntoParam<String> for &str {
    fn into_param(self) -> String {
        self.into()
    }
}



// ===============
// === Sampler ===
// ===============

#[derive(Debug)]
pub struct SamplerData<Out = ()> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    src:        Box<dyn std::any::Any>,
    value:      RefCell<Out>,
    /// Used to cache the output even if no connection is present YET. Consider the following
    /// example: first we create `Sampler`, then we emit value to it, then we connect it to some
    /// output FRP network, Without `self_watch` the value would not be cached internally, and thus
    /// would not be available to the new sub-network until re-emitted.
    self_watch: RefCell<Option<Box<dyn std::any::Any>>>,
}
pub type OwnedSampler<Out = ()> = stream::Node<SamplerData<Out>>;
pub type Sampler<Out = ()> = stream::WeakNode<SamplerData<Out>>;

impl<Out: Data> HasOutput for SamplerData<Out> {
    type Output = Out;
}

impl<Out: Data> OwnedSampler<Out> {
    /// Constructor.
    pub fn new<T1>(label: Label, src1: &T1) -> Self
    where T1: EventOutput<Output = Out> {
        let src = Box::new(src1.clone_ref());
        let value = default();
        let self_watch = default();
        let definition = SamplerData { src, value, self_watch };
        let out = Self::construct_and_connect(label, src1, definition);
        *out.self_watch.borrow_mut() = Some(Box::new(watch_stream(&out)));
        out
    }
}

impl<Out: Data> OwnedSampler<Out> {
    /// Sample the value.
    pub fn value(&self) -> Out {
        self.value.borrow().clone()
    }
}

impl<Out: Data> Sampler<Out> {
    /// Sample the value.
    pub fn value(&self) -> Out {
        self.upgrade().map(|t| t.value.borrow().clone()).unwrap_or_default()
    }
}

impl<Out: Data> stream::EventConsumer<Out> for OwnedSampler<Out> {
    fn on_event(&self, stack: CallStack, event: &Out) {
        *self.value.borrow_mut() = event.clone();
        self.emit_event(stack, event);
    }
}



// =============
// === Trace ===
// =============

#[derive(Clone, Debug)]
pub struct TraceData<T> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    src: T,
}
pub type OwnedTrace<T> = stream::Node<TraceData<T>>;
pub type Trace<T> = stream::WeakNode<TraceData<T>>;

impl<T: EventOutput> HasOutput for TraceData<T> {
    type Output = Output<T>;
}

impl<T: EventOutput> OwnedTrace<T> {
    /// Constructor.
    pub fn new(label: Label, src1: &T) -> Self {
        let src = src1.clone_ref();
        let def = TraceData { src };
        Self::construct_and_connect(label, src1, def)
    }
}

impl<T: EventOutput> stream::EventConsumer<Output<T>> for OwnedTrace<T> {
    fn on_event(&self, stack: CallStack, event: &Output<T>) {
        ERROR!("[FRP] {self.label()}: {event:?}");
        DEBUG!("[FRP] {stack}");
        self.emit_event(stack, event);
    }
}



// ===============
// === Profile ===
// ===============

#[derive(Clone, Debug)]
pub struct ProfileData<T> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    src: T,
}
pub type OwnedProfile<T> = stream::Node<ProfileData<T>>;
pub type Profile<T> = stream::WeakNode<ProfileData<T>>;

impl<T: EventOutput> HasOutput for ProfileData<T> {
    type Output = Output<T>;
}

impl<T: EventOutput> OwnedProfile<T> {
    /// Constructor.
    pub fn new(label: Label, src1: &T) -> Self {
        let src = src1.clone_ref();
        let def = ProfileData { src };
        Self::construct_and_connect(label, src1, def)
    }
}

impl<T: EventOutput> stream::EventConsumer<Output<T>> for OwnedProfile<T> {
    fn on_event(&self, stack: CallStack, event: &Output<T>) {
        use profiler::internal::StartState;

        let label = profiler::internal::Label(self.label());
        let parent = profiler::internal::EventId::implicit();
        let now = Some(profiler::internal::Timestamp::now());
        let profiler = profiler::Debug::start(parent, label, now, StartState::Active);
        self.emit_event(stack, event);
        profiler.finish();
    }
}



// ==============
// === Toggle ===
// ==============

#[derive(Debug)]
pub struct ToggleData<T> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    src:   T,
    value: Cell<bool>,
}
pub type OwnedToggle<T> = stream::Node<ToggleData<T>>;
pub type Toggle<T> = stream::WeakNode<ToggleData<T>>;

impl<T> HasOutput for ToggleData<T> {
    type Output = bool;
}

impl<T: EventOutput> OwnedToggle<T> {
    /// Constructor.
    pub fn new(label: Label, src: &T) -> Self {
        Self::new_with(label, src, default())
    }

    /// Constructor with explicit start value.
    pub fn new_with(label: Label, src1: &T, init: bool) -> Self {
        let src = src1.clone_ref();
        let value = Cell::new(init);
        let def = ToggleData { src, value };
        Self::construct_and_connect_with_init_value(label, src1, def, init)
    }
}

impl<T: EventOutput> stream::EventConsumer<Output<T>> for OwnedToggle<T> {
    fn on_event(&self, stack: CallStack, _: &Output<T>) {
        let value = !self.value.get();
        self.value.set(value);
        self.emit_event(stack, &value);
    }
}



// =============
// === Count ===
// =============

#[derive(Debug)]
pub struct CountData<T> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    src:   T,
    value: Cell<usize>,
}
pub type OwnedCount<T> = stream::Node<CountData<T>>;
pub type Count<T> = stream::WeakNode<CountData<T>>;

impl<T> HasOutput for CountData<T> {
    type Output = usize;
}

impl<T: EventOutput> OwnedCount<T> {
    /// Constructor.
    pub fn new(label: Label, src1: &T) -> Self {
        let src = src1.clone_ref();
        let value = default();
        let def = CountData { src, value };
        Self::construct_and_connect(label, src1, def)
    }
}

impl<T: EventOutput> stream::EventConsumer<Output<T>> for OwnedCount<T> {
    fn on_event(&self, stack: CallStack, _: &Output<T>) {
        let value = self.value.get() + 1;
        self.value.set(value);
        self.emit_event(stack, &value);
    }
}



// ================
// === Constant ===
// ================

#[derive(Debug)]
pub struct ConstantData<T, Out = ()> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    src:   T,
    value: Out,
}
pub type OwnedConstant<T, Out = ()> = stream::Node<ConstantData<T, Out>>;
pub type Constant<T, Out = ()> = stream::WeakNode<ConstantData<T, Out>>;

impl<T, Out: Data> HasOutput for ConstantData<T, Out> {
    type Output = Out;
}

impl<T: EventOutput, Out: Data> OwnedConstant<T, Out> {
    /// Constructor.
    pub fn new(label: Label, src1: &T, value: Out) -> Self {
        let src = src1.clone_ref();
        let def = ConstantData { src, value };
        Self::construct_and_connect(label, src1, def)
    }
}

impl<T: EventOutput, Out: Data> stream::EventConsumer<Output<T>> for OwnedConstant<T, Out> {
    fn on_event(&self, stack: CallStack, _: &Output<T>) {
        self.emit_event(stack, &self.value);
    }
}



// ================
// === Previous ===
// ================

#[derive(Debug)]
pub struct PreviousData<T: EventOutput> {
    #[allow(dead_code)]
    src:      T,
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    previous: RefCell<Output<T>>,
}
pub type OwnedPrevious<T> = stream::Node<PreviousData<T>>;
pub type Previous<T> = stream::WeakNode<PreviousData<T>>;

impl<T: EventOutput> HasOutput for PreviousData<T> {
    type Output = Output<T>;
}

impl<T: EventOutput> OwnedPrevious<T> {
    /// Constructor.
    pub fn new(label: Label, src1: &T) -> Self {
        let src = src1.clone_ref();
        let previous = default();
        let def = PreviousData { src, previous };
        Self::construct_and_connect(label, src1, def)
    }
}

impl<T: EventOutput> stream::EventConsumer<Output<T>> for OwnedPrevious<T> {
    fn on_event(&self, stack: CallStack, event: &Output<T>) {
        let previous = mem::replace(&mut *self.previous.borrow_mut(), event.clone());
        self.emit_event(stack, &previous);
    }
}



// ==============
// === Sample ===
// ==============

#[derive(Debug)]
pub struct SampleData<T1, T2> {
    behavior: watch::Ref<T1>,
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    event:    T2,
}
pub type OwnedSample<T1, T2> = stream::Node<SampleData<T1, T2>>;
pub type Sample<T1, T2> = stream::WeakNode<SampleData<T1, T2>>;

impl<T1: HasOutput, T2> HasOutput for SampleData<T1, T2> {
    type Output = Output<T1>;
}

impl<T1: EventOutput, T2: EventOutput> OwnedSample<T1, T2> {
    /// Constructor.
    pub fn new(label: Label, behavior: &T1, src: &T2) -> Self {
        let event = src.clone_ref();
        let behavior = watch_stream(behavior);
        let definition = SampleData { behavior, event };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T1: EventOutput, T2: EventOutput> stream::EventConsumer<Output<T2>> for OwnedSample<T1, T2> {
    fn on_event(&self, stack: CallStack, _: &Output<T2>) {
        self.emit_event(stack, &self.behavior.value());
    }
}

impl<T1: EventOutput, T2> stream::InputBehaviors for SampleData<T1, T2> {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.behavior)]
    }
}



// ============
// === Gate ===
// ============

#[derive(Debug)]
pub struct GateData<T1, T2> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    event:    T1,
    behavior: watch::Ref<T2>,
}
pub type OwnedGate<T1, T2> = stream::Node<GateData<T1, T2>>;
pub type Gate<T1, T2> = stream::WeakNode<GateData<T1, T2>>;

impl<T1: EventOutput, T2> HasOutput for GateData<T1, T2> {
    type Output = Output<T1>;
}

impl<T1, T2> OwnedGate<T1, T2>
where
    T1: EventOutput,
    T2: EventOutput<Output = bool>,
{
    /// Constructor.
    pub fn new(label: Label, src: &T1, behavior: &T2) -> Self {
        let event = src.clone_ref();
        let behavior = watch_stream(behavior);
        let definition = GateData { event, behavior };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T1, T2> stream::EventConsumer<Output<T1>> for OwnedGate<T1, T2>
where
    T1: EventOutput,
    T2: EventOutput<Output = bool>,
{
    fn on_event(&self, stack: CallStack, event: &Output<T1>) {
        if self.behavior.value() {
            self.emit_event(stack, event)
        }
    }
}

impl<T1, T2> stream::InputBehaviors for GateData<T1, T2>
where T2: EventOutput
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.behavior)]
    }
}



// ===============
// === GateNot ===
// ===============

#[derive(Debug)]
pub struct GateNotData<T1, T2> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    event:    T1,
    behavior: watch::Ref<T2>,
}
pub type OwnedGateNot<T1, T2> = stream::Node<GateNotData<T1, T2>>;
pub type GateNot<T1, T2> = stream::WeakNode<GateNotData<T1, T2>>;

impl<T1: EventOutput, T2> HasOutput for GateNotData<T1, T2> {
    type Output = Output<T1>;
}

impl<T1, T2> OwnedGateNot<T1, T2>
where
    T1: EventOutput,
    T2: EventOutput<Output = bool>,
{
    /// Constructor.
    pub fn new(label: Label, src: &T1, behavior: &T2) -> Self {
        let event = src.clone_ref();
        let behavior = watch_stream(behavior);
        let definition = GateNotData { event, behavior };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T1, T2> stream::EventConsumer<Output<T1>> for OwnedGateNot<T1, T2>
where
    T1: EventOutput,
    T2: EventOutput<Output = bool>,
{
    fn on_event(&self, stack: CallStack, event: &Output<T1>) {
        if !self.behavior.value() {
            self.emit_event(stack, event)
        }
    }
}

impl<T1, T2> stream::InputBehaviors for GateNotData<T1, T2>
where T2: EventOutput
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.behavior)]
    }
}



// ==============
// === Unwrap ===
// ==============

#[derive(Debug)]
pub struct UnwrapData<T: EventOutput> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    src: T,
}
pub type OwnedUnwrap<T> = stream::Node<UnwrapData<T>>;
pub type Unwrap<T> = stream::WeakNode<UnwrapData<T>>;

impl<T, S> HasOutput for UnwrapData<T>
where
    T: EventOutput<Output = Option<S>>,
    S: Data,
{
    type Output = S;
}

impl<T, S> OwnedUnwrap<T>
where
    T: EventOutput<Output = Option<S>>,
    S: Data,
{
    /// Constructor.
    pub fn new(label: Label, src1: &T) -> Self {
        let src = src1.clone_ref();
        let def = UnwrapData { src };
        Self::construct_and_connect(label, src1, def)
    }
}

impl<T, S> stream::EventConsumer<Output<T>> for OwnedUnwrap<T>
where
    T: EventOutput<Output = Option<S>>,
    S: Data,
{
    fn on_event(&self, stack: CallStack, event: &Output<T>) {
        if let Some(t) = event {
            self.emit_event(stack, t)
        }
    }
}



// ===========
// === Any ===
// ===========

#[derive(Debug)]
pub struct AnyData<Out = ()> {
    srcs:    Rc<RefCell<Vec<Box<dyn std::any::Any>>>>,
    phantom: PhantomData<Out>,
}
pub type OwnedAny<Out = ()> = stream::Node<AnyData<Out>>;
/// Please refer to `any_mut` docs to learn more.
pub type Any<Out = ()> = stream::WeakNode<AnyData<Out>>;

impl<Out: Data> HasOutput for AnyData<Out> {
    type Output = Out;
}

impl<Out: Data> OwnedAny<Out> {
    /// Constructor.
    pub fn new(label: Label) -> Self {
        let srcs = default();
        let phantom = default();
        let def = AnyData { srcs, phantom };
        Self::construct(label, def)
    }

    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T>(self, src: &T) -> Self
    where T: EventOutput<Output = Out> {
        src.register_target(self.downgrade().into());
        self.srcs.borrow_mut().push(Box::new(src.clone_ref()));
        self
    }

    /// Constructor for 1 input stream.
    pub fn new1<T1>(label: Label, t1: &T1) -> Self
    where T1: EventOutput<Output = Out> {
        Self::new(label).with(t1)
    }

    /// Constructor for 2 input streams.
    pub fn new2<T1, T2>(label: Label, t1: &T1, t2: &T2) -> Self
    where
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>, {
        Self::new(label).with(t1).with(t2)
    }

    /// Constructor for 3 input streams.
    pub fn new3<T1, T2, T3>(label: Label, t1: &T1, t2: &T2, t3: &T3) -> Self
    where
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>, {
        Self::new(label).with(t1).with(t2).with(t3)
    }

    /// Constructor for 4 input streams.
    pub fn new4<T1, T2, T3, T4>(label: Label, t1: &T1, t2: &T2, t3: &T3, t4: &T4) -> Self
    where
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>, {
        Self::new(label).with(t1).with(t2).with(t3).with(t4)
    }

    /// Constructor for 5 input streams.
    pub fn new5<T1, T2, T3, T4, T5>(
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> Self
    where
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>,
        T5: EventOutput<Output = Out>,
    {
        Self::new(label).with(t1).with(t2).with(t3).with(t4).with(t5)
    }

    /// Emit new event. It's questionable if this node type should expose the `emit` functionality,
    /// but the current usage patterns proven it is a very handy utility. This node is used to
    /// define sources of frp output streams. Sources allow multiple streams to be attached and
    /// sometimes emitting events directly from the model is the cleanest solution possible.
    pub fn emit<T: IntoParam<Out>>(&self, value: T) {
        self.emit_event(&default(), &value.into_param())
    }
}

impl<Out: Data> Any<Out> {
    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T1>(self, src: &T1) -> Self
    where T1: EventOutput<Output = Out> {
        src.register_target(self.clone_ref().into());
        self.upgrade().for_each(|t| t.srcs.borrow_mut().push(Box::new(src.clone_ref())));
        self
    }

    /// Attach new src to this node.
    pub fn attach<T1>(&self, src: &T1)
    where T1: EventOutput<Output = Out> {
        src.register_target(self.into());
        self.upgrade().for_each(|t| t.srcs.borrow_mut().push(Box::new(src.clone_ref())));
    }

    /// Emit new event. It's questionable if this node type should expose the `emit` functionality,
    /// but the current usage patterns proven it is a very handy utility. This node is used to
    /// define sources of frp output streams. Sources allow multiple streams to be attached and
    /// sometimes emitting events directly from the model is the cleanest solution possible.
    pub fn emit<T: IntoParam<Out>>(&self, value: T) {
        self.emit_event(&default(), &value.into_param())
    }
}

impl<Out: Data> stream::EventConsumer<Out> for OwnedAny<Out> {
    fn on_event(&self, stack: CallStack, event: &Out) {
        self.emit_event(stack, event);
    }
}



// ============
// === Any_ ===
// ============

#[derive(Debug)]
pub struct AnyData_ {
    srcs: Rc<RefCell<Vec<Box<dyn std::any::Any>>>>,
}
pub type OwnedAny_ = stream::Node<AnyData_>;
pub type Any_ = stream::WeakNode<AnyData_>;

impl HasOutput for AnyData_ {
    type Output = ();
}

impl OwnedAny_ {
    /// Constructor.
    pub fn new(label: Label) -> Self {
        let srcs = default();
        let def = AnyData_ { srcs };
        Self::construct(label, def)
    }

    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T>(self, src: &T) -> Self
    where T: EventOutput {
        src.register_target(self.downgrade().into());
        self.srcs.borrow_mut().push(Box::new(src.clone_ref()));
        self
    }

    /// Constructor for 1 input stream.
    pub fn new1<T1>(label: Label, t1: &T1) -> Self
    where T1: EventOutput {
        Self::new(label).with(t1)
    }

    /// Constructor for 2 input streams.
    pub fn new2<T1, T2>(label: Label, t1: &T1, t2: &T2) -> Self
    where
        T1: EventOutput,
        T2: EventOutput, {
        Self::new(label).with(t1).with(t2)
    }

    /// Constructor for 3 input streams.
    pub fn new3<T1, T2, T3>(label: Label, t1: &T1, t2: &T2, t3: &T3) -> Self
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput, {
        Self::new(label).with(t1).with(t2).with(t3)
    }

    /// Constructor for 4 input streams.
    pub fn new4<T1, T2, T3, T4>(label: Label, t1: &T1, t2: &T2, t3: &T3, t4: &T4) -> Self
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput, {
        Self::new(label).with(t1).with(t2).with(t3).with(t4)
    }

    /// Constructor for 5 input streams.
    pub fn new5<T1, T2, T3, T4, T5>(
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> Self
    where
        T1: EventOutput,
        T2: EventOutput,
        T3: EventOutput,
        T4: EventOutput,
        T5: EventOutput,
    {
        Self::new(label).with(t1).with(t2).with(t3).with(t4).with(t5)
    }
}

impl Any_ {
    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T1>(self, src: &T1) -> Self
    where T1: EventOutput {
        src.register_target(self.clone_ref().into());
        self.upgrade().for_each(|t| t.srcs.borrow_mut().push(Box::new(src.clone_ref())));
        self
    }

    /// Attach new src to this node.
    pub fn attach<T1>(&self, src: &T1)
    where T1: EventOutput {
        src.register_target(self.into());
        self.upgrade().for_each(|t| t.srcs.borrow_mut().push(Box::new(src.clone_ref())));
    }
}

impl<T> stream::EventConsumer<T> for OwnedAny_ {
    fn on_event(&self, stack: CallStack, _: &T) {
        self.emit_event(stack, &());
    }
}



// ============
// === Get0 ===
// ============

#[derive(Debug)]
pub struct Get0Data<T1> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    event: T1,
}
pub type OwnedGet0<T1> = stream::Node<Get0Data<T1>>;
pub type Get0<T1> = stream::WeakNode<Get0Data<T1>>;

impl<T1> HasOutput for Get0Data<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt0,
    generics::ItemAt0<T1::Output>: Data,
{
    type Output = generics::ItemAt0<T1::Output>;
}

impl<T1> OwnedGet0<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt0,
    generics::ItemAt0<T1::Output>: Data,
{
    /// Constructor.
    pub fn new(label: Label, src: &T1) -> Self {
        let event = src.clone_ref();
        let definition = Get0Data { event };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T1> stream::EventConsumer<Output<T1>> for OwnedGet0<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt0,
    generics::ItemAt0<T1::Output>: Data,
{
    fn on_event(&self, stack: CallStack, event: &Output<T1>) {
        self.emit_event(stack, (*event)._0())
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
pub struct Get1Data<T1> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    event: T1,
}
pub type OwnedGet1<T1> = stream::Node<Get1Data<T1>>;
pub type Get1<T1> = stream::WeakNode<Get1Data<T1>>;

impl<T1> HasOutput for Get1Data<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt1,
    generics::ItemAt1<T1::Output>: Data,
{
    type Output = generics::ItemAt1<T1::Output>;
}

impl<T1> OwnedGet1<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt1,
    generics::ItemAt1<T1::Output>: Data,
{
    /// Constructor.
    pub fn new(label: Label, src: &T1) -> Self {
        let event = src.clone_ref();
        let definition = Get1Data { event };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T1> stream::EventConsumer<Output<T1>> for OwnedGet1<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt1,
    generics::ItemAt1<T1::Output>: Data,
{
    fn on_event(&self, stack: CallStack, event: &Output<T1>) {
        self.emit_event(stack, (*event)._1())
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
pub struct Get2Data<T1> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    event: T1,
}
pub type OwnedGet2<T1> = stream::Node<Get2Data<T1>>;
pub type Get2<T1> = stream::WeakNode<Get2Data<T1>>;

impl<T1> HasOutput for Get2Data<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt2,
    generics::ItemAt2<T1::Output>: Data,
{
    type Output = generics::ItemAt2<T1::Output>;
}

impl<T1> OwnedGet2<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt2,
    generics::ItemAt2<T1::Output>: Data,
{
    /// Constructor.
    pub fn new(label: Label, src: &T1) -> Self {
        let event = src.clone_ref();
        let definition = Get2Data { event };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T1> stream::EventConsumer<Output<T1>> for OwnedGet2<T1>
where
    T1: EventOutput,
    T1::Output: generics::GetItemAt2,
    generics::ItemAt2<T1::Output>: Data,
{
    fn on_event(&self, stack: CallStack, event: &Output<T1>) {
        self.emit_event(stack, (*event)._2())
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
pub struct IterData<T1> {
    #[allow(dead_code)]
    event: T1,
}
pub type OwnedIter<T1> = stream::Node<IterData<T1>>;
pub type Iter<T1> = stream::WeakNode<IterData<T1>>;

impl<T1, X> HasOutput for IterData<T1>
where
    T1: EventOutput,
    X: Data,
    for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
{
    type Output = X;
}

impl<T1, X> OwnedIter<T1>
where
    T1: EventOutput,
    X: Data,
    for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
{
    /// Constructor.
    pub fn new(label: Label, src: &T1) -> Self {
        let event = src.clone_ref();
        let definition = IterData { event };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T1, X> stream::EventConsumer<Output<T1>> for OwnedIter<T1>
where
    T1: EventOutput,
    X: Data,
    for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
{
    fn on_event(&self, stack: CallStack, event: &Output<T1>) {
        for val in event {
            self.emit_event(stack, val)
        }
    }
}

impl<T1> stream::InputBehaviors for IterData<T1> {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![]
    }
}



// ============
// === Fold ===
// ============

#[derive(Debug)]
pub struct FoldData<T1> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    event: T1,
}
pub type OwnedFold<T1> = stream::Node<FoldData<T1>>;
pub type Fold<T1> = stream::WeakNode<FoldData<T1>>;

impl<T1, X> HasOutput for FoldData<T1>
where
    T1: EventOutput,
    X: Data,
    for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
{
    type Output = X;
}

impl<T1, X> OwnedFold<T1>
where
    T1: EventOutput,
    X: Data + Monoid,
    for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
{
    /// Constructor.
    pub fn new(label: Label, src: &T1) -> Self {
        let event = src.clone_ref();
        let definition = FoldData { event };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T1, X> stream::EventConsumer<Output<T1>> for OwnedFold<T1>
where
    T1: EventOutput,
    X: Data + Monoid,
    for<'t> &'t T1::Output: IntoIterator<Item = &'t X>,
{
    fn on_event(&self, stack: CallStack, event: &Output<T1>) {
        self.emit_event(stack, &event.into_iter().fold(default(), |t, s| t.concat(s)))
    }
}

impl<T1> stream::InputBehaviors for FoldData<T1> {
    fn input_behaviors(&self) -> Vec<Link> {
        vec![]
    }
}



// ==============
// === AllMut ===
// ==============

pub struct AllMutData<Out = ()> {
    srcs:    Rc<RefCell<Vec<Box<dyn ValueProvider<Output = Out>>>>>,
    watches: Rc<RefCell<Vec<Box<dyn std::any::Any>>>>,
    phantom: PhantomData<Out>,
}
pub type OwnedAllMut<Out = ()> = stream::Node<AllMutData<Out>>;
pub type AllMut<Out = ()> = stream::WeakNode<AllMutData<Out>>;

impl<Out> Debug for AllMutData<Out> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AllMutData")
    }
}

impl<Out: Data> HasOutput for AllMutData<Out> {
    type Output = Vec<Out>;
}

impl<Out: Data> OwnedAllMut<Out> {
    /// Constructor.
    pub fn new(label: Label) -> Self {
        let srcs = default();
        let watches = default();
        let phantom = default();
        let def = AllMutData { srcs, watches, phantom };
        Self::construct(label, def)
    }

    /// Takes ownership of self and returns it with a new stream attached.
    pub fn with<T>(self, src: &T) -> Self
    where T: EventOutput<Output = Out> {
        src.register_target(self.downgrade().into());
        let watch = watch_stream(src);
        self.watches.borrow_mut().push(Box::new(watch));
        self.srcs.borrow_mut().push(Box::new(src.clone_ref()));
        self
    }

    /// Constructor for 1 input stream.
    pub fn new1<T1>(label: Label, t1: &T1) -> Self
    where T1: EventOutput<Output = Out> {
        Self::new(label).with(t1)
    }

    /// Constructor for 2 input streams.
    pub fn new2<T1, T2>(label: Label, t1: &T1, t2: &T2) -> Self
    where
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>, {
        Self::new(label).with(t1).with(t2)
    }

    /// Constructor for 3 input streams.
    pub fn new3<T1, T2, T3>(label: Label, t1: &T1, t2: &T2, t3: &T3) -> Self
    where
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>, {
        Self::new(label).with(t1).with(t2).with(t3)
    }

    /// Constructor for 4 input streams.
    pub fn new4<T1, T2, T3, T4>(label: Label, t1: &T1, t2: &T2, t3: &T3, t4: &T4) -> Self
    where
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>, {
        Self::new(label).with(t1).with(t2).with(t3).with(t4)
    }

    /// Constructor for 5 input streams.
    pub fn new5<T1, T2, T3, T4, T5>(
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
    ) -> Self
    where
        T1: EventOutput<Output = Out>,
        T2: EventOutput<Output = Out>,
        T3: EventOutput<Output = Out>,
        T4: EventOutput<Output = Out>,
        T5: EventOutput<Output = Out>,
    {
        Self::new(label).with(t1).with(t2).with(t3).with(t4).with(t5)
    }
}

impl<Out: Data> AllMut<Out> {
    /// Attach new src to this node.
    pub fn attach<T1>(&self, src: &T1)
    where T1: EventOutput<Output = Out> {
        src.register_target(self.into());
        let watch = watch_stream(src);
        self.upgrade().for_each(|t| {
            t.watches.borrow_mut().push(Box::new(watch));
            t.srcs.borrow_mut().push(Box::new(src.clone_ref()))
        });
    }

    pub fn with<T1>(self, src: &T1) -> Self
    where T1: EventOutput<Output = Out> {
        self.attach(src);
        self
    }
}

impl<Out: Data> stream::EventConsumer<Out> for OwnedAllMut<Out> {
    fn on_event(&self, stack: CallStack, _event: &Out) {
        let values = self.srcs.borrow().iter().map(|src| src.value()).collect();
        self.emit_event(stack, &values);
    }
}



// ============
// === All2 ===
// ============

#[derive(Debug)]
pub struct All2Data<T1, T2> {
    src1: watch::Ref<T1>,
    src2: watch::Ref<T2>,
}
pub type OwnedAll2<T1, T2> = stream::Node<All2Data<T1, T2>>;
pub type All2<T1, T2> = stream::WeakNode<All2Data<T1, T2>>;

impl<T1, T2> HasOutput for All2Data<T1, T2>
where
    T1: EventOutput,
    T2: EventOutput,
{
    type Output = (Output<T1>, Output<T2>);
}

impl<T1, T2> OwnedAll2<T1, T2>
where
    T1: EventOutput,
    T2: EventOutput,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let def = All2Data { src1, src2 };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.into());
        this
    }
}

impl<T1, T2, Out> stream::EventConsumer<Out> for OwnedAll2<T1, T2>
where
    T1: EventOutput,
    T2: EventOutput,
{
    fn on_event(&self, stack: CallStack, _: &Out) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        self.emit_event(stack, &(value1, value2));
    }
}

impl<T1, T2> stream::InputBehaviors for All2Data<T1, T2>
where
    T1: EventOutput,
    T2: EventOutput,
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::mixed(&self.src1), Link::mixed(&self.src2)]
    }
}



// ============
// === All3 ===
// ============

#[derive(Debug)]
pub struct All3Data<T1, T2, T3> {
    src1: watch::Ref<T1>,
    src2: watch::Ref<T2>,
    src3: watch::Ref<T3>,
}
pub type OwnedAll3<T1, T2, T3> = stream::Node<All3Data<T1, T2, T3>>;
pub type All3<T1, T2, T3> = stream::WeakNode<All3Data<T1, T2, T3>>;

impl<T1, T2, T3> HasOutput for All3Data<T1, T2, T3>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
{
    type Output = (Output<T1>, Output<T2>, Output<T3>);
}

impl<T1, T2, T3> OwnedAll3<T1, T2, T3>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, t3: &T3) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let def = All3Data { src1, src2, src3 };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, Out> stream::EventConsumer<Out> for OwnedAll3<T1, T2, T3>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
{
    fn on_event(&self, stack: CallStack, _: &Out) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        self.emit_event(stack, &(value1, value2, value3));
    }
}

impl<T1, T2, T3> stream::InputBehaviors for All3Data<T1, T2, T3>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::mixed(&self.src1), Link::mixed(&self.src2), Link::mixed(&self.src3)]
    }
}



// ============
// === All4 ===
// ============

#[derive(Debug)]
pub struct All4Data<T1, T2, T3, T4> {
    src1: watch::Ref<T1>,
    src2: watch::Ref<T2>,
    src3: watch::Ref<T3>,
    src4: watch::Ref<T4>,
}
pub type OwnedAll4<T1, T2, T3, T4> = stream::Node<All4Data<T1, T2, T3, T4>>;
pub type WeakAll4<T1, T2, T3, T4> = stream::WeakNode<All4Data<T1, T2, T3, T4>>;

impl<T1, T2, T3, T4> HasOutput for All4Data<T1, T2, T3, T4>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
{
    type Output = (Output<T1>, Output<T2>, Output<T3>, Output<T4>);
}

impl<T1, T2, T3, T4> OwnedAll4<T1, T2, T3, T4>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, t3: &T3, t4: &T4) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let src4 = watch_stream(t4);
        let def = All4Data { src1, src2, src3, src4 };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.clone_ref().into());
        t4.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, T4, Out> stream::EventConsumer<Out> for OwnedAll4<T1, T2, T3, T4>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
{
    fn on_event(&self, stack: CallStack, _: &Out) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let value4 = self.src4.value();
        self.emit_event(stack, &(value1, value2, value3, value4));
    }
}

impl<T1, T2, T3, T4> stream::InputBehaviors for All4Data<T1, T2, T3, T4>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![
            Link::mixed(&self.src1),
            Link::mixed(&self.src2),
            Link::mixed(&self.src3),
            Link::mixed(&self.src4),
        ]
    }
}



// ============
// === All5 ===
// ============

#[derive(Debug)]
pub struct All5Data<T1, T2, T3, T4, T5> {
    src1: watch::Ref<T1>,
    src2: watch::Ref<T2>,
    src3: watch::Ref<T3>,
    src4: watch::Ref<T4>,
    src5: watch::Ref<T5>,
}
pub type OwnedAll5<T1, T2, T3, T4, T5> = stream::Node<All5Data<T1, T2, T3, T4, T5>>;
pub type WeakAll5<T1, T2, T3, T4, T5> = stream::WeakNode<All5Data<T1, T2, T3, T4, T5>>;

impl<T1, T2, T3, T4, T5> HasOutput for All5Data<T1, T2, T3, T4, T5>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
{
    type Output = (Output<T1>, Output<T2>, Output<T3>, Output<T4>, Output<T5>);
}

impl<T1, T2, T3, T4, T5> OwnedAll5<T1, T2, T3, T4, T5>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let src4 = watch_stream(t4);
        let src5 = watch_stream(t5);
        let def = All5Data { src1, src2, src3, src4, src5 };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.clone_ref().into());
        t4.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, T4, T5, Out> stream::EventConsumer<Out> for OwnedAll5<T1, T2, T3, T4, T5>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
{
    fn on_event(&self, stack: CallStack, _: &Out) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let value4 = self.src4.value();
        let value5 = self.src5.value();
        self.emit_event(stack, &(value1, value2, value3, value4, value5));
    }
}

impl<T1, T2, T3, T4, T5> stream::InputBehaviors for All5Data<T1, T2, T3, T4, T5>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![
            Link::mixed(&self.src1),
            Link::mixed(&self.src2),
            Link::mixed(&self.src3),
            Link::mixed(&self.src4),
            Link::mixed(&self.src5),
        ]
    }
}



// ==============
// === Filter ===
// ==============

pub struct FilterData<T, P> {
    phantom:   PhantomData<T>,
    predicate: P,
}
pub type OwnedFilter<T, P> = stream::Node<FilterData<T, P>>;
pub type Filter<T, P> = stream::WeakNode<FilterData<T, P>>;

impl<T, P> HasOutput for FilterData<T, P>
where
    T: EventOutput,
    P: 'static + Fn(&Output<T>) -> bool,
{
    type Output = Output<T>;
}

impl<T, P> OwnedFilter<T, P>
where
    T: EventOutput,
    P: 'static + Fn(&Output<T>) -> bool,
{
    /// Constructor.
    pub fn new(label: Label, src: &T, predicate: P) -> Self {
        let definition = FilterData { phantom: PhantomData, predicate };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T, P> stream::EventConsumer<Output<T>> for OwnedFilter<T, P>
where
    T: EventOutput,
    P: 'static + Fn(&Output<T>) -> bool,
{
    fn on_event(&self, stack: CallStack, value: &Output<T>) {
        if (self.predicate)(value) {
            self.emit_event(stack, value);
        }
    }
}

impl<T, P> Debug for FilterData<T, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FilterData")
    }
}



// =================
// === FilterMap ===
// =================

pub struct FilterMapData<T, F> {
    phantom:  PhantomData<T>,
    function: F,
}
pub type OwnedFilterMap<T, F> = stream::Node<FilterMapData<T, F>>;
pub type FilterMap<T, F> = stream::WeakNode<FilterMapData<T, F>>;

impl<T, F, Out> HasOutput for FilterMapData<T, F>
where
    T: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T>) -> Option<Out>,
{
    type Output = Out;
}

impl<T, F, Out> OwnedFilterMap<T, F>
where
    T: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T>) -> Option<Out>,
{
    /// Constructor.
    pub fn new(label: Label, src: &T, function: F) -> Self {
        let definition = FilterMapData { phantom: PhantomData, function };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T, F, Out> stream::EventConsumer<Output<T>> for OwnedFilterMap<T, F>
where
    T: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T>) -> Option<Out>,
{
    fn on_event(&self, stack: CallStack, value: &Output<T>) {
        if let Some(out) = (self.function)(value) {
            self.emit_event(stack, &out);
        }
    }
}

impl<T, F> Debug for FilterMapData<T, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FilterMapData")
    }
}



// ===========
// === Map ===
// ===========

pub struct MapData<T, F> {
    phantom:  PhantomData<T>,
    function: F,
}
pub type OwnedMap<T, F> = stream::Node<MapData<T, F>>;
pub type Map<T, F> = stream::WeakNode<MapData<T, F>>;

impl<T, F, Out> HasOutput for MapData<T, F>
where
    T: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T>) -> Out,
{
    type Output = Out;
}

impl<T, F, Out> OwnedMap<T, F>
where
    T: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T>) -> Out,
{
    /// Constructor.
    pub fn new(label: Label, src: &T, function: F) -> Self {
        let definition = MapData { phantom: PhantomData, function };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T, F, Out> stream::EventConsumer<Output<T>> for OwnedMap<T, F>
where
    T: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T>) -> Out,
{
    fn on_event(&self, stack: CallStack, value: &Output<T>) {
        let out = (self.function)(value);
        self.emit_event(stack, &out);
    }
}

impl<T, F> Debug for MapData<T, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MapData")
    }
}



// ============
// === Map2 ===
// ============

pub struct Map2Data<T1, T2, F> {
    _src1:    T1,
    src2:     watch::Ref<T2>,
    function: F,
}
pub type OwnedMap2<T1, T2, F> = stream::Node<Map2Data<T1, T2, F>>;
pub type Map2<T1, T2, F> = stream::WeakNode<Map2Data<T1, T2, F>>;

impl<T1, T2, F, Out> HasOutput for Map2Data<T1, T2, F>
where
    T1: EventOutput,
    T2: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>) -> Out,
{
    type Output = Out;
}

impl<T1, T2, F, Out> OwnedMap2<T1, T2, F>
where
    T1: EventOutput,
    T2: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>) -> Out,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, function: F) -> Self {
        let _src1 = t1.clone_ref();
        let src2 = watch_stream(t2);
        let def = Map2Data { _src1, src2, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<T1, T2, F, Out> stream::EventConsumer<Output<T1>> for OwnedMap2<T1, T2, F>
where
    T1: EventOutput,
    T2: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>) -> Out,
{
    fn on_event(&self, stack: CallStack, value1: &Output<T1>) {
        let value2 = self.src2.value();
        let out = (self.function)(value1, &value2);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, F> Debug for Map2Data<T1, T2, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Map2Data")
    }
}

impl<T1, T2, F> stream::InputBehaviors for Map2Data<T1, T2, F>
where
    T1: EventOutput,
    T2: EventOutput,
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.src2)]
    }
}



// ============
// === Map3 ===
// ============

pub struct Map3Data<T1, T2, T3, F> {
    _src1:    T1,
    src2:     watch::Ref<T2>,
    src3:     watch::Ref<T3>,
    function: F,
}
pub type OwnedMap3<T1, T2, T3, F> = stream::Node<Map3Data<T1, T2, T3, F>>;
pub type Map3<T1, T2, T3, F> = stream::WeakNode<Map3Data<T1, T2, T3, F>>;

impl<T1, T2, T3, F, Out> HasOutput for Map3Data<T1, T2, T3, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> Out,
{
    type Output = Out;
}

impl<T1, T2, T3, F, Out> OwnedMap3<T1, T2, T3, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> Out,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, t3: &T3, function: F) -> Self {
        let _src1 = t1.clone_ref();
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let def = Map3Data { _src1, src2, src3, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, F, Out> stream::EventConsumer<Output<T1>> for OwnedMap3<T1, T2, T3, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> Out,
{
    fn on_event(&self, stack: CallStack, value1: &Output<T1>) {
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let out = (self.function)(value1, &value2, &value3);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, T3, F> Debug for Map3Data<T1, T2, T3, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Map3Data")
    }
}

impl<T1, T2, T3, F> stream::InputBehaviors for Map3Data<T1, T2, T3, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.src2), Link::behavior(&self.src3)]
    }
}



// ============
// === Map4 ===
// ============

pub struct Map4Data<T1, T2, T3, T4, F> {
    _src1:    T1,
    src2:     watch::Ref<T2>,
    src3:     watch::Ref<T3>,
    src4:     watch::Ref<T4>,
    function: F,
}
pub type OwnedMap4<T1, T2, T3, T4, F> = stream::Node<Map4Data<T1, T2, T3, T4, F>>;
pub type Map4<T1, T2, T3, T4, F> = stream::WeakNode<Map4Data<T1, T2, T3, T4, F>>;

impl<T1, T2, T3, T4, F, Out> HasOutput for Map4Data<T1, T2, T3, T4, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> Out,
{
    type Output = Out;
}

impl<T1, T2, T3, T4, F, Out> OwnedMap4<T1, T2, T3, T4, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> Out,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, t3: &T3, t4: &T4, function: F) -> Self {
        let _src1 = t1.clone_ref();
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let src4 = watch_stream(t4);
        let def = Map4Data { _src1, src2, src3, src4, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, T4, F, Out> stream::EventConsumer<Output<T1>> for OwnedMap4<T1, T2, T3, T4, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> Out,
{
    fn on_event(&self, stack: CallStack, value1: &Output<T1>) {
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let value4 = self.src4.value();
        let out = (self.function)(value1, &value2, &value3, &value4);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, T3, T4, F> stream::InputBehaviors for Map4Data<T1, T2, T3, T4, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
{
    fn input_behaviors(&self) -> Vec<Link> {
        vec![Link::behavior(&self.src2), Link::behavior(&self.src3), Link::behavior(&self.src4)]
    }
}

impl<T1, T2, T3, T4, F> Debug for Map4Data<T1, T2, T3, T4, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Map4Data")
    }
}



// ================
// === AllWith2 ===
// ================

pub struct AllWith2Data<T1, T2, F> {
    src1:     watch::Ref<T1>,
    src2:     watch::Ref<T2>,
    function: F,
}
pub type OwnedAllWith2<T1, T2, F> = stream::Node<AllWith2Data<T1, T2, F>>;
pub type AllWith2<T1, T2, F> = stream::WeakNode<AllWith2Data<T1, T2, F>>;

impl<T1, T2, F, Out> HasOutput for AllWith2Data<T1, T2, F>
where
    T1: EventOutput,
    T2: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>) -> Out,
{
    type Output = Out;
}

impl<T1, T2, F, Out> OwnedAllWith2<T1, T2, F>
where
    T1: EventOutput,
    T2: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>) -> Out,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, function: F) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let def = AllWith2Data { src1, src2, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.into());
        this
    }
}

impl<T1, T2, F, Out, T> stream::EventConsumer<T> for OwnedAllWith2<T1, T2, F>
where
    T1: EventOutput,
    T2: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>) -> Out,
{
    fn on_event(&self, stack: CallStack, _: &T) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let out = (self.function)(&value1, &value2);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, F> Debug for AllWith2Data<T1, T2, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AllWith2Data")
    }
}



// ================
// === AllWith3 ===
// ================

pub struct AllWith3Data<T1, T2, T3, F> {
    src1:     watch::Ref<T1>,
    src2:     watch::Ref<T2>,
    src3:     watch::Ref<T3>,
    function: F,
}
pub type OwnedAllWith3<T1, T2, T3, F> = stream::Node<AllWith3Data<T1, T2, T3, F>>;
pub type AllWith3<T1, T2, T3, F> = stream::WeakNode<AllWith3Data<T1, T2, T3, F>>;

impl<T1, T2, T3, F, Out> HasOutput for AllWith3Data<T1, T2, T3, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> Out,
{
    type Output = Out;
}

impl<T1, T2, T3, F, Out> OwnedAllWith3<T1, T2, T3, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> Out,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, t3: &T3, function: F) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let def = AllWith3Data { src1, src2, src3, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, F, Out, T> stream::EventConsumer<T> for OwnedAllWith3<T1, T2, T3, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>) -> Out,
{
    fn on_event(&self, stack: CallStack, _: &T) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let out = (self.function)(&value1, &value2, &value3);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, T3, F> Debug for AllWith3Data<T1, T2, T3, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AllWith3Data")
    }
}



// ================
// === AllWith4 ===
// ================

pub struct AllWith4Data<T1, T2, T3, T4, F> {
    src1:     watch::Ref<T1>,
    src2:     watch::Ref<T2>,
    src3:     watch::Ref<T3>,
    src4:     watch::Ref<T4>,
    function: F,
}
pub type OwnedAllWith4<T1, T2, T3, T4, F> = stream::Node<AllWith4Data<T1, T2, T3, T4, F>>;
pub type AllWith4<T1, T2, T3, T4, F> = stream::WeakNode<AllWith4Data<T1, T2, T3, T4, F>>;

impl<T1, T2, T3, T4, F, Out> HasOutput for AllWith4Data<T1, T2, T3, T4, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> Out,
{
    type Output = Out;
}

impl<T1, T2, T3, T4, F, Out> OwnedAllWith4<T1, T2, T3, T4, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> Out,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, t3: &T3, t4: &T4, function: F) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let src4 = watch_stream(t4);
        let def = AllWith4Data { src1, src2, src3, src4, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.clone_ref().into());
        t4.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, T4, F, Out, T> stream::EventConsumer<T> for OwnedAllWith4<T1, T2, T3, T4, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>) -> Out,
{
    fn on_event(&self, stack: CallStack, _: &T) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let value4 = self.src4.value();
        let out = (self.function)(&value1, &value2, &value3, &value4);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, T3, T4, F> Debug for AllWith4Data<T1, T2, T3, T4, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AllWith4Data")
    }
}



// ================
// === AllWith5 ===
// ================

pub struct AllWith5Data<T1, T2, T3, T4, T5, F> {
    src1:     watch::Ref<T1>,
    src2:     watch::Ref<T2>,
    src3:     watch::Ref<T3>,
    src4:     watch::Ref<T4>,
    src5:     watch::Ref<T5>,
    function: F,
}
pub type OwnedAllWith5<T1, T2, T3, T4, T5, F> = stream::Node<AllWith5Data<T1, T2, T3, T4, T5, F>>;
pub type AllWith5<T1, T2, T3, T4, T5, F> = stream::WeakNode<AllWith5Data<T1, T2, T3, T4, T5, F>>;

impl<T1, T2, T3, T4, T5, F, Out> HasOutput for AllWith5Data<T1, T2, T3, T4, T5, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>) -> Out,
{
    type Output = Out;
}

impl<T1, T2, T3, T4, T5, F, Out> OwnedAllWith5<T1, T2, T3, T4, T5, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>) -> Out,
{
    /// Constructor.
    pub fn new(label: Label, t1: &T1, t2: &T2, t3: &T3, t4: &T4, t5: &T5, function: F) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let src4 = watch_stream(t4);
        let src5 = watch_stream(t5);
        let def = AllWith5Data { src1, src2, src3, src4, src5, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.clone_ref().into());
        t4.register_target(weak.clone_ref().into());
        t5.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, T4, T5, F, Out, T> stream::EventConsumer<T>
    for OwnedAllWith5<T1, T2, T3, T4, T5, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    Out: Data,
    F: 'static + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>) -> Out,
{
    fn on_event(&self, stack: CallStack, _: &T) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let value4 = self.src4.value();
        let value5 = self.src5.value();
        let out = (self.function)(&value1, &value2, &value3, &value4, &value5);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, T3, T4, T5, F> Debug for AllWith5Data<T1, T2, T3, T4, T5, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AllWith5Data")
    }
}



// ================
// === AllWith6 ===
// ================

pub struct AllWith6Data<T1, T2, T3, T4, T5, T6, F> {
    src1:     watch::Ref<T1>,
    src2:     watch::Ref<T2>,
    src3:     watch::Ref<T3>,
    src4:     watch::Ref<T4>,
    src5:     watch::Ref<T5>,
    src6:     watch::Ref<T6>,
    function: F,
}
pub type OwnedAllWith6<T1, T2, T3, T4, T5, T6, F> =
    stream::Node<AllWith6Data<T1, T2, T3, T4, T5, T6, F>>;
pub type AllWith6<T1, T2, T3, T4, T5, T6, F> =
    stream::WeakNode<AllWith6Data<T1, T2, T3, T4, T5, T6, F>>;

impl<T1, T2, T3, T4, T5, T6, F, Out> HasOutput for AllWith6Data<T1, T2, T3, T4, T5, T6, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    T6: EventOutput,
    Out: Data,
    F: 'static
        + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>, &Output<T6>) -> Out,
{
    type Output = Out;
}

impl<T1, T2, T3, T4, T5, T6, F, Out> OwnedAllWith6<T1, T2, T3, T4, T5, T6, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    T6: EventOutput,
    Out: Data,
    F: 'static
        + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>, &Output<T6>) -> Out,
{
    /// Constructor.
    pub fn new(
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        t6: &T6,
        function: F,
    ) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let src4 = watch_stream(t4);
        let src5 = watch_stream(t5);
        let src6 = watch_stream(t6);
        let def = AllWith6Data { src1, src2, src3, src4, src5, src6, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.clone_ref().into());
        t4.register_target(weak.clone_ref().into());
        t5.register_target(weak.clone_ref().into());
        t6.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, T4, T5, T6, F, Out, T> stream::EventConsumer<T>
    for OwnedAllWith6<T1, T2, T3, T4, T5, T6, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    T6: EventOutput,
    Out: Data,
    F: 'static
        + Fn(&Output<T1>, &Output<T2>, &Output<T3>, &Output<T4>, &Output<T5>, &Output<T6>) -> Out,
{
    fn on_event(&self, stack: CallStack, _: &T) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let value4 = self.src4.value();
        let value5 = self.src5.value();
        let value6 = self.src6.value();
        let out = (self.function)(&value1, &value2, &value3, &value4, &value5, &value6);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, T3, T4, T5, T6, F> Debug for AllWith6Data<T1, T2, T3, T4, T5, T6, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AllWith6Data")
    }
}



// ================
// === AllWith8 ===
// ================

pub struct AllWith8Data<T1, T2, T3, T4, T5, T6, T7, T8, F> {
    src1:     watch::Ref<T1>,
    src2:     watch::Ref<T2>,
    src3:     watch::Ref<T3>,
    src4:     watch::Ref<T4>,
    src5:     watch::Ref<T5>,
    src6:     watch::Ref<T6>,
    src7:     watch::Ref<T7>,
    src8:     watch::Ref<T8>,
    function: F,
}
pub type OwnedAllWith8<T1, T2, T3, T4, T5, T6, T7, T8, F> =
    stream::Node<AllWith8Data<T1, T2, T3, T4, T5, T6, T7, T8, F>>;
pub type AllWith8<T1, T2, T3, T4, T5, T6, T7, T8, F> =
    stream::WeakNode<AllWith8Data<T1, T2, T3, T4, T5, T6, T7, T8, F>>;

impl<T1, T2, T3, T4, T5, T6, T7, T8, F, Out> HasOutput
    for AllWith8Data<T1, T2, T3, T4, T5, T6, T7, T8, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    T6: EventOutput,
    T7: EventOutput,
    T8: EventOutput,
    Out: Data,
    F: 'static
        + Fn(
            &Output<T1>,
            &Output<T2>,
            &Output<T3>,
            &Output<T4>,
            &Output<T5>,
            &Output<T6>,
            &Output<T7>,
            &Output<T8>,
        ) -> Out,
{
    type Output = Out;
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, F, Out> OwnedAllWith8<T1, T2, T3, T4, T5, T6, T7, T8, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    T6: EventOutput,
    T7: EventOutput,
    T8: EventOutput,
    Out: Data,
    F: 'static
        + Fn(
            &Output<T1>,
            &Output<T2>,
            &Output<T3>,
            &Output<T4>,
            &Output<T5>,
            &Output<T6>,
            &Output<T7>,
            &Output<T8>,
        ) -> Out,
{
    /// Constructor.
    pub fn new(
        label: Label,
        t1: &T1,
        t2: &T2,
        t3: &T3,
        t4: &T4,
        t5: &T5,
        t6: &T6,
        t7: &T7,
        t8: &T8,
        function: F,
    ) -> Self {
        let src1 = watch_stream(t1);
        let src2 = watch_stream(t2);
        let src3 = watch_stream(t3);
        let src4 = watch_stream(t4);
        let src5 = watch_stream(t5);
        let src6 = watch_stream(t6);
        let src7 = watch_stream(t7);
        let src8 = watch_stream(t8);
        let def = AllWith8Data { src1, src2, src3, src4, src5, src6, src7, src8, function };
        let this = Self::construct(label, def);
        let weak = this.downgrade();
        t1.register_target(weak.clone_ref().into());
        t2.register_target(weak.clone_ref().into());
        t3.register_target(weak.clone_ref().into());
        t4.register_target(weak.clone_ref().into());
        t5.register_target(weak.clone_ref().into());
        t6.register_target(weak.clone_ref().into());
        t7.register_target(weak.clone_ref().into());
        t8.register_target(weak.into());
        this
    }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, F, Out, T> stream::EventConsumer<T>
    for OwnedAllWith8<T1, T2, T3, T4, T5, T6, T7, T8, F>
where
    T1: EventOutput,
    T2: EventOutput,
    T3: EventOutput,
    T4: EventOutput,
    T5: EventOutput,
    T6: EventOutput,
    T7: EventOutput,
    T8: EventOutput,
    Out: Data,
    F: 'static
        + Fn(
            &Output<T1>,
            &Output<T2>,
            &Output<T3>,
            &Output<T4>,
            &Output<T5>,
            &Output<T6>,
            &Output<T7>,
            &Output<T8>,
        ) -> Out,
{
    fn on_event(&self, stack: CallStack, _: &T) {
        let value1 = self.src1.value();
        let value2 = self.src2.value();
        let value3 = self.src3.value();
        let value4 = self.src4.value();
        let value5 = self.src5.value();
        let value6 = self.src6.value();
        let value7 = self.src7.value();
        let value8 = self.src8.value();

        let out =
            (self.function)(&value1, &value2, &value3, &value4, &value5, &value6, &value7, &value8);
        self.emit_event(stack, &out);
    }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, F> Debug for AllWith8Data<T1, T2, T3, T4, T5, T6, T7, T8, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AllWith8Data")
    }
}



// ==============
// === Repeat ===
// ==============

#[derive(Debug)]
pub struct RepeatData<T> {
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the source struct
    /// stays alive at least as long as this struct.
    event: T,
    // behavior: watch::Ref<T2>,
}
pub type OwnedRepeat<T> = stream::Node<RepeatData<T>>;
pub type Repeat<T> = stream::WeakNode<RepeatData<T>>;

impl<T> HasOutput for RepeatData<T> {
    type Output = ();
}

impl<T> OwnedRepeat<T>
where T: EventOutput<Output = usize>
{
    /// Constructor.
    pub fn new(label: Label, src: &T) -> Self {
        let event = src.clone_ref();
        let definition = RepeatData { event };
        Self::construct_and_connect(label, src, definition)
    }
}

impl<T> stream::EventConsumer<usize> for OwnedRepeat<T>
where T: EventOutput<Output = usize>
{
    fn on_event(&self, stack: CallStack, event: &Output<T>) {
        for _ in 0..*event {
            self.emit_event(stack, &())
        }
    }
}
