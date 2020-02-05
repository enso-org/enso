//! This module defines a set of complex FRP building blocks. They allow you to run arbitrary Rust
//! code on the input data. You should rather not need to often use these.

use crate::prelude::*;

use crate::data::*;
use crate::node::*;
use crate::nodes::inference::*;



// ==============
// === Macros ===
// ==============

/// Similar to `deinfe_node` but specialized for lambda definitions.
///
/// Generates a ot of boilerplate for a new node definition. It generates the node struct, the
/// constructor, input / output relations, etc. In order to learn more, see the expanded version
/// of this macro used below.
macro_rules! define_lambda_node {
    (
        $(#$meta:tt)*
        pub struct $name:ident $shape_name:ident [$($poly_input:ident)*]
            { $( $field:ident : $field_type:ty ),* }
    ) => {
        $(#$meta)*
        #[allow(non_camel_case_types)]
        pub type $name<$($poly_input,)* Out> = NodeWrapper<$shape_name<$($poly_input,)* Out>>;

        $(#$meta)*
        #[derive(Debug)]
        #[allow(non_camel_case_types)]
        pub struct $shape_name<$($poly_input:Data,)* Out:Data> {
            $( $poly_input : Node<$poly_input> ),* ,
            $( $field      : $field_type ),*
        }

        #[allow(non_camel_case_types)]
        impl<$($poly_input:Data,)* Out:Data>
        KnownOutput for $shape_name<$($poly_input,)* Out> {
            type Output = Out;
        }

        #[allow(non_camel_case_types,unused_parens)]
        impl<$($poly_input:Data,)* Out:Data>
        KnownEventInput for $shape_name<$($poly_input,)* Out>
        where ($($poly_input),*) : ContainsEventData,
              SelectEventData<($($poly_input),*)> : Data {
            type EventInput = SelectEventData<($($poly_input),*)>;
        }

        paste::item! {
            /// A constructor trait. Used only in order to make Rust type checker happy.
            #[allow(non_camel_case_types)]
            pub trait [<$name New>]<$($poly_input,)* Func> {
                /// Constructor.
                fn new_named<Label:Into<CowString>>
                (label:Label, $($poly_input:$poly_input,)* f:Func) -> Self;
            }

            #[allow(non_camel_case_types,unused_parens)]
            impl<$($poly_input,)* OutVal, $([<T $poly_input>],)* Function>
            [<$name New>]<$([<T $poly_input>],)* Function>
            for $name<$($poly_input,)* ProductType<($($poly_input),*),OutVal>>
                where $($poly_input       : Data,)*
                      $([<T $poly_input>] : Into<Node<$poly_input>>,)*
                      $(Node<$poly_input> : AddTarget<Self>,)*
                      OutVal              : InferProductType<($($poly_input),*)>,
                      Function            : 'static + Fn($(&Content<$poly_input>),*) -> OutVal,
                      ProductType<($($poly_input),*),OutVal> : Data<Content=OutVal> {
                fn new_named<Label>
                (label:Label, $($poly_input:[<T $poly_input>],)* func:Function) -> Self
                where Label : Into<CowString> {
                    $(let $poly_input = $poly_input.into();)*
                    let func        = func.into();
                    let shape       = $shape_name{$($poly_input,)* func};
                    let this        = Self::construct(label,shape);
                    {
                        $(this.$poly_input.add_target(&this);)*
                    }
                    this
                }
            }
        }

        #[allow(non_camel_case_types)]
        impl<$($poly_input:Data,)* Out:Data> HasInputs for $shape_name<$($poly_input,)* Out> {
            fn inputs(&self) -> Vec<NodeWithAnyOutput> {
                vec![$((&self.$poly_input).into()),*]
            }
        }
    }
}


// ==============
// === Lambda ===
// ==============

define_lambda_node! {
    /// Transforms input data with the provided function. Lambda accepts a single input and outputs
    /// message of the same type as the input message.
    pub struct Lambda LambdaShape [source] {
        func : Lambda1Func<source,Out>
    }
}


// === LambdaFunc ===

/// Newtype wrapper for function stored in the `Lambda` node.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Lambda1Func<In1:Data,Out:Data> {
    #[derivative(Debug="ignore")]
    raw : Rc<dyn Fn(&Content<In1>) -> Out>
}

impl<In1,Out,Func> From<Func> for Lambda1Func<In1,Out>
    where In1  : Data,
          Out  : Data,
          Func : 'static + Fn(&Content<In1>) -> Content<Out> {
    fn from(func:Func) -> Self {
        let raw = Rc::new(move |a:&Content<In1>| { wrap(func(a)) });
        Self {raw}
    }
}

impl<In:Value,Out:Data> EventConsumer for Lambda<EventData<In>,Out> {
    fn on_event(&self, input:&Content<Self::EventInput>) {
        let output = (self.func.raw)(input);
        self.emit_event_raw(unwrap(&output));
    }
}



// ===============
// === Lambda2 ===
// ===============

define_lambda_node! {
    /// Transforms input data with the provided function. `Lambda2` accepts two inputs. If at least
    /// one of the inputs was event, the output message will be event as well. In case both inputs
    /// were behavior, a new behavior will be produced.
    pub struct Lambda2 Lambda2Shape [source1 source2] {
        func : Lambda2Func<source1,source2,Out>
    }
}


// === LambdaFunc ===

/// Newtype wrapper for function stored in the `Lambda2` node.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Lambda2Func<In1:Data,In2:Data,Out:Data> {
    #[derivative(Debug="ignore")]
    raw : Rc<dyn Fn(&Content<In1>,&Content<In2>) -> Out>
}

impl<In1,In2,Out,Func> From<Func> for Lambda2Func<In1,In2,Out>
    where In1:Data, In2:Data, Out:Data,
          Func : 'static + Fn(&Content<In1>,&Content<In2>) -> Content<Out> {
    fn from(func:Func) -> Self {
        let raw = Rc::new(move |a:&Content<In1>,b:&Content<In2>| { wrap(func(a,b)) });
        Self {raw}
    }
}

impl<In1,In2,Out> EventConsumer for Lambda2<EventData<In1>,BehaviorData<In2>,Out>
    where In1:Value, In2:Value, Out:Data {
    fn on_event(&self, event:&Content<Self::EventInput>) {
        let value2 = self.source2.current_value();
        let output = (self.func.raw)(event,&value2);
        self.emit_event_raw(unwrap(&output));
    }
}

impl<In1,In2,Out> EventConsumer for Lambda2<BehaviorData<In1>,EventData<In2>,Out>
    where In1:Value, In2:Value, Out:Data {
    fn on_event(&self, event:&Content<Self::EventInput>) {
        let value1 = self.source1.current_value();
        let output = (self.func.raw)(&value1,event);
        self.emit_event_raw(unwrap(&output));
    }
}



// =============
// === Utils ===
// =============

/// A debug trace utility. Prints every incoming event to the console.
pub fn trace<T,Label,Source>(label:Label, source:Source) -> Lambda<T,T>
    where T          : Data,
          Label      : Str,
          Source     : Into<Node<T>>,
          Content<T> : Value + InferProductType<T,ProductType=T>,
          Node<T>    : AddTarget<Lambda<T,T>> {
    let label = label.into();
    Lambda::new_named("trace",source, move |t| {
        println!("TRACE [{}]: {:?}", label, t);
        t.clone()
    })
}
