//! This module defines inference helpers allowing for writing more general code in FRP node
//! generation macros.

use crate::data::*;



// =========================
// === Inference Helpers ===
// =========================

/// Data product type-level inference guidance. For a given input, it infers the product of FRP
/// data types. For example, for the input of `(BehaviorData<T1>,BehaviorData<T2>)`, the output
/// will be `BehaviorData<X>`. In case there is a single `EventData`, it will become the result.
/// For input `(BehaviorData<T1>,EventData<T2>)` it will resolve to `EventData<X>`.
pub trait InferProductType<T> {
    /// Inference results.
    type ProductType;
}

/// Accessor for inferred type.
pub type ProductType<T,X> = <X as InferProductType<T>>::ProductType;


// === Rules ===

/// Defines product type-level inference rules. To learn more, see the expanded version of the
/// usage below.
macro_rules! inference_rules {
    ($( $pat:tt => $result:ident )*) => {$(
        inference_rule! { $pat => $result }
    )*}
}

/// Internal utility for the `inference_rules` macro.
macro_rules! inference_rule {
    ( $t1:ident => $result:ident ) => {
        impl<X,T1> InferProductType <$t1<T1>> for X {
            type ProductType = $result<X>;
        }
    };

    ( ($t1:ident) => $result:ident ) => {
        impl<X,T1> InferProductType <$t1<T1>> for X {
            type ProductType = $result<X>;
        }
    };

    ( ($t1:ident, $t2:ident) => $result:ident ) => {
        impl<X,T1,T2> InferProductType <($t1<T1>,$t2<T2>)> for X {
            type ProductType = $result<X>;
        }
    };

    ( ($t1:ident, $t2:ident, $t3:ident) => $result:ident ) => {
        impl<X,T1,T2,T3> InferProductType <($t1<T1>,$t2<T2>,$t3<T3>)> for X {
            type ProductType = $result<X>;
        }
    };
}

inference_rules! {
    EventData    => EventData
    BehaviorData => BehaviorData

    (EventData    , EventData   ) => EventData
    (BehaviorData , EventData   ) => EventData
    (EventData    , BehaviorData) => EventData
    (BehaviorData , BehaviorData) => EventData
}



// =========================
// === ContainsEventData ===
// =========================

// === Definition ===

/// From the given set of FRP data selects the first occurrence of `EventData` if any. For example,
/// `SelectEventData<(BehaviorData<T1>,EventData<T2>,EventData<T3>)>` resolves to `EventData<T2>`.
pub type SelectEventData<T> = <T as ContainsEventData>::Result;

/// From the given set of FRP data selects the first occurrence of `EventData` if any.
pub trait ContainsEventData {
    /// The selected `EventData` type.
    type Result : Data;
}



// === Instances ===

/// Defines instances for `ContainsEventData`. See the expanded version of the usages below to
/// learn more.
macro_rules! define_contains_event_data_impls {
    ($( $pat:tt => $result:ident<$result_type:ident> )*) => {$(
        define_contains_event_data_impls_item! { $pat => $result<$result_type> }
    )*}
}

/// Internal utility for the `define_contains_event_data_impls` macro.
macro_rules! define_contains_event_data_impls_item {
    ($( ($($pat:tt<$pat_type:ident>),*) => $result:ident<$result_type:ident> )*) => {$(
        #[allow(unused_parens)]
        impl<$($pat_type),*> ContainsEventData for ($($pat<$pat_type>),*)
        where $result<$result_type> : Data {
            type Result = $result<$result_type>;
        }
    )*}
}

define_contains_event_data_impls! {
    ( EventData <T1> ) => EventData<T1>

    ( EventData    <T1> , EventData    <T2> ) => EventData <T1>
    ( EventData    <T1> , BehaviorData <T2> ) => EventData <T1>
    ( BehaviorData <T1> , EventData    <T2> ) => EventData <T2>

    ( EventData    <T1> , EventData    <T2> , EventData    <T3> ) => EventData <T1>
    ( BehaviorData <T1> , EventData    <T2> , EventData    <T3> ) => EventData <T2>
    ( EventData    <T1> , BehaviorData <T2> , EventData    <T3> ) => EventData <T1>
    ( EventData    <T1> , EventData    <T2> , BehaviorData <T3> ) => EventData <T1>
    ( BehaviorData <T1> , BehaviorData <T2> , EventData    <T3> ) => EventData <T3>
    ( BehaviorData <T1> , EventData    <T2> , BehaviorData <T3> ) => EventData <T2>
    ( EventData    <T1> , BehaviorData <T2> , BehaviorData <T3> ) => EventData <T1>
}
