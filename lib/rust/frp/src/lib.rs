//! # Introduction
//!
//! **READ THIS PAGE CAREFULLY.**
//!
//! This module implements a sophisticated event handling framework, known as Functional Reactive
//! Programming system. It allows creating data flow diagrams from predefined nodes. Over years many
//! different FRP implementations appeared, especially in the Haskell community. The implementations
//! differ both in concepts they use, as well as the provided functionality. The following
//! implementation focuses on providing high performance and maintainability (ability to reason
//! about correctness of complex data flow graphs).
//!
//! In order to grasp some of the main ideas we strongly encourage you to read the following FRP
//! introduction. Although this implementation differs in few details, understanding these concepts
//! would be helpful: https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md
//!
//!
//!
//! # Usage
//!
//! The FRP system exposed by this library can be used in two modes - network and dynamic. The
//! former was designed as a highly maintainable framework and it is what you should choose almost
//! always. The API was designed mainly with this use case in mind. In order to use the network
//! mode, just use the API exposed by `Network` and all node types without "Owned" prefix. In order
//! to use dynamic mode, do not use `Network` and use nodes with the "Owned" prefix. The modes can
//! be mixed, but unless you really know what you are doing, you are advised not to do it.
//!
//!
//! ## Network mode
//! In this mode you define immutable FRP graphs called `Network`s. You are not allowed to add or
//! remove nodes from `Network` after it was defined. You are, however, allowed to create, remove,
//! connect, and reconnect networks dynamically. The `Network` manages the lifetime of all of
//! its nodes, and you are always working on weak references to the nodes. You can also define a
//! `BridgeNetwork` between two or more `Network`s which will be responsible for converting or
//! tagging data. The lifetime of `BridgeNetwork` is always managed by lifetimes of the `Network`s
//! it is connected to. It is dropped as soon as any of the networks gets dropped.
//!
//! An example use case is a slider and scene definition. The slider component can contain its own
//! internal FRP network which manages its state and mouse interactions. The scene FRP network can
//! connect to slider network whenever a new slider is created. In order to tag the value change
//! events with information which slider they originate from, a `BridgeNetwork` should be used.
//!
//!
//! ## Dynamic mode.
//! Please note, that although there are valid use cases when this mode is useful, you should always
//! default to network mode instead. In this mode the `Network` is not used and you are using strong
//! references to nodes instead (types prefixed with "Owned" in the API, like `frp::OwnedSource`).
//! Nodes are dropped as soon as no references exist to them anymore.
//!
//!
//!
//! # FRP graph architecture
//!
//! The architecture of the FRP engine is pretty complex and it is hard to understand all the
//! dependencies by reading the code only. In order to make it easier, the following diagram was
//! prepared. It visualizes a simple FRP network of `source -> count -> map(|t| t.to_string())`.
//! You can view the diagram in any GraphViz tool, for example this one:
//! https://dreampuf.github.io/GraphvizOnline
//!
//! Meaning of the graph elements:
//! - On the left there are two `Network` nodes which will be missing if used in dynamic mode.
//! - Colors indicate to which network nodes belong (nodes of that color will be dropped as soon as
//!   the network gets dropped). Nodes with double color will be dropped as soon as an event will be
//!   emitted to nodes belonging to the dropped network.
//! - Black solid edges without arrows are just fields inside structure.
//! - Black solid edges with arrows are `Rc` pointers.
//! - Black dashed edges with arrows are `Weak` pointers.
//! - Red solid edges with arrows are trait object interfaces.
//! - The gray arrows are possible connections depending on which mode was used. In case of network
//!   mode, either an edge to `Stream` or a node without the "Owned" prefix will be created
//!   (depending on what will be provided to node constructor). In dynamic mode, as you will be
//!   providing strong node references to their constructors, the arrow to the node with "Owned"
//!   prefix will be created.
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
//!
//!
//!     node_1[label="OwnedSource", pos="-0.5,-1!", color=blue]
//!     SourceData[pos="-0.5,0!",color=blue]
//!     node_1 -> SourceData
//!     node_data_1[label="stream::NodeData",pos="1.8,-1!",color=blue]
//!     node_1 -> node_data_1
//!     stream_1[label="Stream<()>", pos="4.0,-1.3!"]
//!     owned_stream_1[label="OwnedStream<()>", pos="4.0,-0.7!"]
//!     stream_1 -> node_data_1 [style=dashed]
//!     owned_stream_1 -> node_data_1
//!     Source[pos="1.8,0!",color=blue]
//!     Source -> SourceData [style=dashed]
//!     Source -> node_data_1 [style=dashed]
//!
//!
//!     node_data_1 -> Count [color=red]
//!     Count_[pos="1.8,-3!",label="",color=crimson, width=0.85, height=0.6]
//!     Count[pos="1.8,-3!",color=blue]
//!     node_2[label="OwnedCount",pos="-0.5,-4!",color=crimson]
//!     CountData[pos="-0.5,-3!",color=crimson]
//!     CountData -> stream_1       [color="#AAAAAA"]
//!     CountData -> owned_stream_1 [color="#AAAAAA"]
//!     CountData -> node_1         [color="#AAAAAA"]
//!     CountData -> Source         [color="#AAAAAA"]
//!     node_2 -> CountData
//!     node_data_2[label="stream::NodeData",pos="1.8,-4!",color=crimson]
//!     node_2 -> node_data_2
//!     Count -> node_data_2 [style=dashed]
//!     Count -> CountData [style=dashed]
//!     owned_stream_2[label="OwnedStream<usize>",pos="4.0,-3.7!"]
//!     stream_2[label="Stream<usize>",pos="4.0,-4.3!"]
//!     owned_stream_2 -> node_data_2
//!     stream_2 -> node_data_2 [style=dashed]
//!
//!
//!
//!     node_data_2 -> Map [color=red]
//!     Map[pos="1.8,-6!",color=crimson]
//!     node_3[label="OwnedMap",pos="-0.5,-7!",color=crimson]
//!     MapData[pos="-0.5,-6!",color=crimson]
//!     MapData -> owned_stream_2 [color="#AAAAAA"]
//!     MapData -> stream_2       [color="#AAAAAA"]
//!     MapData -> node_2         [color="#AAAAAA"]
//!     MapData -> Count          [color="#AAAAAA"]
//!     node_3 -> MapData
//!     node_data_3[label="stream::NodeData",pos="1.8,-7!",color=crimson]
//!     node_3 -> node_data_3
//!     Map -> node_data_3 [style=dashed]
//!     Map -> MapData [style=dashed]
//!     owned_stream_3[label="OwnedStream<String>", pos="4.0,-6.7!"]
//!     stream_3[label="Stream<String>", pos="4.0,-7.3!"]
//!     owned_stream_3 -> node_data_3
//!     stream_3 -> node_data_3 [style=dashed]
//! }
//!
//! ## Design Notes
//!
//! Every node is initialized with the `Default` value with exception of `toggle_true`, which starts
//! with `true` and not `false` (`Default` for `bool`).
//! ```

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
#![allow(incomplete_features)] // To be removed, see: https://github.com/enso-org/ide/issues/1559
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![feature(associated_type_defaults)]
#![feature(auto_traits)]
#![feature(negative_impls)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(unboxed_closures)]
#![recursion_limit = "512"]

pub mod data;
pub mod debug;
pub mod future;
pub mod io;
pub mod macros;
pub mod network;
pub mod node;
pub mod nodes;
pub mod stream;

pub use network::*;
pub use node::*;
pub use nodes::*;

pub use enso_web as web;
pub use stream::Stream;

/// Set of often used types and functions.
pub mod prelude {
    pub use enso_logger::WarningLogger as Logger;
    pub use enso_logger::*;
    pub use enso_prelude::*;
    pub use enso_profiler as profiler;
    pub use enso_profiler::prelude::*;
}

#[cfg(test)]
mod network_mode_tests {
    use crate as frp;

    #[test]
    fn lifetime_management() {
        frp::new_network! { network1
            def source = source::<()>();
        }
        frp::new_network! { network2
            def count   = source.count();
            def sampler = count.sampler();
        }
        assert_eq!(sampler.value(), 0);
        source.emit(());
        assert_eq!(sampler.value(), 1);
        source.emit(());
        assert_eq!(sampler.value(), 2);
        drop(network1);
        source.emit(());
        assert_eq!(sampler.value(), 2);
        drop(network2);
        source.emit(());
        assert_eq!(sampler.value(), 2);
    }
}

#[cfg(test)]
mod dynamic_mode_tests {
    use crate as frp;
    use frp::prelude::*;

    #[test]
    fn weak_memory_management() {
        frp::new_dynamic_network! {
            def source = source::<()>();
        }
        let weak_source = source.downgrade();
        assert!(weak_source.upgrade().is_some());
        drop(source);
        assert!(weak_source.upgrade().is_none());
    }

    #[test]
    fn lifetime_management_1() {
        frp::new_dynamic_network! {
            def source = source::<()>();
            def count  = source.count();
        }
        let weak_source = source.downgrade();
        assert!(weak_source.upgrade().is_some());
        drop(source);
        assert!(weak_source.upgrade().is_some());
        drop(count);
        assert!(weak_source.upgrade().is_none());
    }

    #[test]
    fn lifetime_management_2() {
        frp::new_dynamic_network! {
            def source  = source::<()>();
            def count   = source.count();
            def sampler = count.sampler();
        }
        // Dropping `count`. It's lifetime should be managed by `sampler` now.
        drop(count);
        assert_eq!(sampler.value(), 0);
        source.emit(());
        assert_eq!(sampler.value(), 1);
        source.emit(());
        assert_eq!(sampler.value(), 2);
        let weak_source = source.downgrade();
        drop(source);
        assert!(weak_source.upgrade().is_some());
        drop(sampler);
        assert!(weak_source.upgrade().is_some());
    }

    #[test]
    #[ignore] // Issue #427
    fn test_toggle_true() {
        frp::new_network! { network
            def toggle_src = source::<()>();
            def map_src    = source::<()>();
            def toggle     = toggle_src.toggle_true();
            def _map       = map_src.map2(&toggle,|_,value| assert!(*value));
        }
        map_src.emit(());
    }

    #[test]
    fn test_toggle() {
        frp::new_network! { network
            def toggle_src = source::<()>();
            def map_src    = source::<()>();
            def toggle     = toggle_src.toggle();
            def _map       = map_src.map2(&toggle,|_,value| assert!(*value));
        }
        toggle_src.emit(());
        map_src.emit(());
    }

    #[test]
    fn test_gate() {
        let passed_events = Rc::new(Cell::new(0));
        frp::new_network! { network
            behavior   <- source::<bool>();
            some_event <- source::<()>();

            gated      <- some_event.gate(&behavior);
            eval_ gated (passed_events.set(passed_events.get() + 1));

        };

        let input = &[false, true, true];
        for val in input {
            behavior.emit(val);
            some_event.emit(());
        }
        let true_count = input.iter().filter(|&&val| val).count();
        assert_eq!(passed_events.get(), true_count);
    }

    #[test]
    fn test_filter() {
        let passed_events = Rc::new(Cell::new(0));
        frp::new_network! { network
            source   <- source::<bool>();
            filtered <- source.filter(|&value| value);
            eval filtered ([passed_events](&value) {
                assert!(value);
                passed_events.set(passed_events.get() + 1);
            });
        };

        let input = &[false, true, true, false, false];
        for val in input {
            source.emit(*val);
        }
        let true_count = input.iter().filter(|&&val| val).count();
        assert_eq!(passed_events.get(), true_count);
    }

    #[test]
    fn test_filter_map() {
        let passed_events = Rc::new(Cell::new(0));
        frp::new_network! { network
            source        <- source::<bool>();
            filter_mapped <- source.filter_map(|v| v.as_some(()));
            eval_ filter_mapped (passed_events.set(passed_events.get() + 1));
        };

        let input = &[false, true, true, false, false];
        for val in input {
            source.emit(*val);
        }
        let true_count = input.iter().filter(|&&val| val).count();
        assert_eq!(passed_events.get(), true_count);
    }
}
