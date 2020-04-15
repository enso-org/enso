//! This module implements an Functional Reactive Programming system. It is an advanced event
//! handling framework which allows describing events and actions by creating declarative event
//! stream diagrams.
//!
//! Please read this document as the initial introduction to FRP concepts:
//! https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md
//!
//! The architecture of the FRP engine is pretty complex and it is hard to understand all the
//! dependencies by reading the code only. In order to make it easier, the following diagram was
//! prepared. It visualizes a simple FRP network of `source -> count -> map(|t| t.to_string())`:
//!
//! - StreamNode colors indicate to which network the nodes belong (nodes of that color will be dropped as
//!   soon as the network gets dropped). Nodes with double color will be dropped as soon as an event
//!   will be emitted to nodes from a dropped network.
//! - Black solid edges without arrows are just fields inside structure.
//! - Black solid edges with arrows are `Rc` pointers.
//! - Black dashed edges with arrows are `Weak` pointers.
//! - Red solid edges with arrows are trait object interfaces.
//!
//! ```dot
//! digraph G {
//!     layout=neato
//!     node [shape=box style=rounded]
//!     Network1[pos="-3,-1!",color=blue]
//!     Network2[pos="-3,-5.5!",color=crimson]
//!     Network1 -> node_1
//!     Network2 -> node_2
//!     Network2 -> node_3
//!
//!     node_1[label="Source<()>", pos="-0.5,-1!", color=blue]
//!     SourceData[pos="-0.5,0!",color=blue]
//!     node_1 -> SourceData
//!     node_data_1[label="StreamNodeData<()>",pos="1.5,-1!",color=blue]
//!     node_1 -> node_data_1
//!     stream_1[label="Stream<()>", pos="3.5,-1!"]
//!     stream_1 -> node_data_1 [style=dashed]
//!
//!     StreamInput_1[label="StreamInput<()>", pos="1.5,-2!",color=blue]
//!     StreamInput_1_[pos="1.5,-2!",label="",color=crimson, width=1.49, height=0.6]
//!     node_data_1 -> StreamInput_1 [arrowhead=none]
//!     WeakCount_[pos="1.5,-3!",label="",color=crimson, width=1.25, height=0.6]
//!     WeakCount[pos="1.5,-3!",color=blue]
//!     StreamInput_1 -> WeakCount [color=red]
//!     node_2[label="Count",pos="-0.5,-4!",color=crimson]
//!     CountData[pos="-0.5,-3!",color=crimson]
//!     node_2 -> CountData
//!     node_data_2[label="StreamNodeData<usize>",pos="1.5,-4!",color=crimson]
//!     node_2 -> node_data_2
//!     WeakCount -> node_data_2 [style=dashed]
//!     WeakCount -> CountData
//!     stream_2[label="Stream<usize>",pos="3.5,-4!"]
//!     stream_2 -> node_data_2 [style=dashed]
//!
//!     StreamInput_2[label="StreamInput<usize>", pos="1.5,-5!",color=crimson]
//!     node_data_2 -> StreamInput_2
//!     WeakMap[pos="1.5,-6!",color=crimson]
//!     StreamInput_2 -> WeakMap [color=red]
//!     node_3[label="Map<Count,[f]>",pos="-0.5,-7!",color=crimson]
//!     MapData[pos="-0.5,-6!",color=crimson]
//!     node_3 -> MapData
//!     node_data_3[label="StreamNodeData<String>",pos="1.5,-7!",color=crimson]
//!     node_3 -> node_data_3
//!     WeakMap -> node_data_3 [style=dashed] [weight=10]
//!     WeakMap -> MapData
//!     stream_3[label="Stream<String>", pos="3.5,-7!"]
//!     stream_3 -> node_data_3 [style=dashed]
//! }
//! ```

#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

#![feature(specialization)]
#![feature(trait_alias)]
#![feature(weak_into_raw)]
#![feature(associated_type_defaults)]

#![feature(unboxed_closures)]
#![feature(fn_traits)]

pub mod debug;
pub mod data;
pub mod io;
pub mod macros;
pub mod network;
pub mod node;
pub mod nodes;
pub mod stream;

pub use network::*;
pub use node::*;
pub use nodes::*;

// FIXME: remove:
pub use io::mouse::Position;

pub use enso_prelude as prelude;
pub use ensogl_system_web as web;


pub use stream::Stream;

#[cfg(test)]
mod tests {
    use crate as frp;
    use crate::*;

    // #[test]
    // fn counter() {
    //     frp::new_network! { network1
    //         def source = source();
    //     }
    //     frp::new_network! { network2
    //         def count = source.count();
    //     }
    //     assert_eq!(count.value(),0);
    //     source.ping();
    //     assert_eq!(count.value(),1);
    //     source.ping();
    //     assert_eq!(count.value(),2);
    //     mem::drop(network1);
    //     source.ping();
    //     assert_eq!(count.value(),2);
    //     mem::drop(network2);
    //     source.ping();
    //     assert_eq!(count.value(),0);
    // }
}
