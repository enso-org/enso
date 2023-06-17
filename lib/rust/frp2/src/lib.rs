//! # Introduction
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
//! # FRP Network and FRP nodes
//! FRP logic is encoded by a [`Network`] of connected FRP [`Node`]s. Each node has zero or more
//! inputs and a single output. You can think of nodes like functions that are processing incoming
//! data. FRP nodes are grouped into [`Network`]s and they are dropped when the network is dropped.
//!
//! The [`Network`] is parametrized with a model, a data structure that can be modified by some of
//! FPR nodes, such as `map`.
//!
//!
//!
//! # Events and Behaviors
//! Every FRP implementation has its own evaluation logic. Some FRP implementations provide the user
//! with an explicit distinction between "events" and "behaviors". "Events" are simple values passed
//! trough the network, while "behaviors" are values that can be accessed at any time (like the
//! current mouse position). Such a distinction requires the user to explicitly specify where a
//! behavior should be created, for example by creating a "hold" node, which on every incoming event
//! clones its value and stores it in case someone would like to sample it. Such a distinction has
//! two major drawbacks:
//!
//! 1. It increases the verbosity of the FRP network, as users need to explicitly specify where a
//!    behavior should be created.
//! 2. It does not allow for some network optimizations. For example, if a node that requires the
//!    output to be sampled is removed, the hold node is no longer needed and the value does not
//!    need to ble cloned anymore.
//!
//! That's why this implementation makes this distinction implicit. The FRP network passes events
//! and some output ports are considered "behaviors" if at least one "sample" input port is
//! connected to the output port. There are three types of input ports:
//!
//! - [`Listen`]: A node can have only one listen port. If a node has this port it can also have
//!   zero or more [`Sample`] ports (but no [`ListenAndSample`] ports). In case an event is emitted
//!   to this port, the node will sample all of its [`Sample`] ports, evaluate its expression, and
//!   emit the output value.
//!
//! - [`Sample`]: In contrast to listen ports, if an event is emitted to a sample port, the node
//!   will not evaluate its expression. Sample ports are used only to sample the last emitted value
//!   in case a listen port is triggered.
//!
//! - [`ListenAndSample`]: This port is a combination of [`Listen`] and [`Sample`] ports. Unlike the
//!   [`Listen`] port, a node can have multiple [`ListenAndSample`] ports. In case an event is
//!   emitted on this port, the node will sample all of its [`ListenAndSample`] and [`Sample`]
//!   ports, evaluate its expression, and emit the output value.
//!
//!
//! # Imperative FRP evaluation order
//! This library implements so called "imperative FRP evaluation order". This means that a node
//! evaluates as soon as it receives an event on one of its listen ports. After evaluating, the node
//! emits the output value which triggers evaluation of all nodes listening on its output port. For
//! example, given the following FRP network:
//!
//! ```text
//! ╭───────╮                 [L] - Listen port.
//! │ Node1 │
//! ╰───┬───╯
//!     ├──────────╮
//!     ▼ [L]      ▼ [L]
//! ╭───────╮  ╭───────╮
//! │ Node2 │  │ Node3 │
//! ╰───┬───╯  ╰───┬───╯
//! ```
//!
//! If an event is emitted to the listen port of `Node1`:
//! - The event from `Node1` is emitted to `Node2`.
//! - `Node2` evaluates and emits the event to all of its children nodes, which will evaluate and
//!   emit their events to subsequent nodes.
//! - The event from `Node1` is emitted to `Node3`.
//! - `Node3` evaluates and emits the event to all of its children nodes, which will evaluate and
//!   emit their events to subsequent nodes.
//!
//!
//! # The "diamond problem"
//! ...
//!
//!
//! # The "initialization problem"
//! ...
//!
//!
//! # The "unused sub-network performance problem"
//! ...
//!
//!
//! # Reactive FRP evaluation order
//! An alternative way to imperative FRP evaluation order is a reactive one. This library does NOT
//! implement such a mode, but it is important to understand this difference, as we plan to
//! introduce this mode in the future. In this mode, the evaluation of some nodes is triggered
//! automatically, when some required values are not up to date. This mode eliminates the above
//! described problems:
//!
//! - It eliminates the "diamond problem" as a node will evaluate only after all of its inputs will
//!   be computed.
//! - It eliminates the "initialization problem", as the network will propagate initial values
//!   automatically after creation.
//! - It eliminates the "unused sub-network performance problem" as it can leave some nodes not
//!   evaluated it the system discovers that the output values will not be used by subsequent nodes.
//!
//! However, this mode has its own drawbacks:
//!
//! - It is impossible for the user to determine the order of FRP nodes evaluation.
//! - The FRP runtime has significantly more work at runtime, which will cause more computations to
//!   be performed, making passing values between nodes slower. However, this can be probably
//!   mitigated by the benefits described above.
//!
//!
//! # Passing data between nodes as references and clones
//! By default, the data is passed between nodes by reference. In case a node output is connected to
//! at least one sample port, the data will be cloned before it is emitted, so the sample ports will
//! be able to sample it on demand. If no sample ports are connected to the node output, the data
//! will not be cloned.

// === Features ===
#![feature(allocator_api)]
#![feature(test)]
#![feature(let_chains)]
#![feature(trait_alias)]
#![feature(downcast_unchecked)]
#![feature(type_alias_impl_trait)]
#![feature(core_intrinsics)]
#![feature(auto_traits)]
#![feature(negative_impls)]
#![feature(cell_update)]
#![feature(fn_traits)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![allow(clippy::module_inception)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use prelude::*;

use crate::callstack::DefInfo;


// ==============
// === Export ===
// ==============

pub mod callstack;
pub mod data;
pub mod metrics;
pub mod network;
pub mod node;
pub mod nodes;
pub mod runtime;

pub use enso_prelude as prelude;



// FIXME: this file will be finished as part of https://github.com/enso-org/enso/issues/7043

// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use network::Network_;

    #[test]
    fn test() {
        let net = Network_::new();
        let src1 = net.source::<usize>();
        let src2 = net.source::<usize>();
        let m1 = src1.map2_(src2, |a, b| 10 * a + b);
        let (_, results) = net.debug_collect(m1);
        results.assert_eq(&[]);
        src1.emit(&1);
        results.assert_eq(&[10]);
        src2.emit(&2);
        results.assert_eq(&[10]);
        src1.emit(&3);
        results.assert_eq(&[10, 32]);
    }

    #[test]
    fn test_network_drop() {
        let net1 = Network_::new();
        let net1_src = net1.source::<usize>();
        let net1_tgt = net1_src.map_(|t| t + 1);
        let (_, net1_results) = net1.debug_collect(net1_tgt);

        let net2 = Network_::new();
        let net2_tgt = net1_src.map_(|t| t * 3);
        let (_, net2_results) = net2.debug_collect(net2_tgt);

        net1_results.assert_eq(&[]);
        net2_results.assert_eq(&[]);
        net1_src.emit(&1);
        net1_results.assert_eq(&[2]);
        net2_results.assert_eq(&[3]);

        drop(net2);
        net1_src.emit(&2);
        net1_results.assert_eq(&[2, 3]);
        net2_results.assert_eq(&[3]);
    }

    #[test]
    fn test2() {
        for _ in 0..10 {
            let net = Network_::new();
            let _src1 = net.source::<usize>();
            // let src2 = net.source::<usize>();
            // let m1 = net.map2(src1, src2, map_fn);
        }
    }
}



// ===============
// === Benches ===
// ===============

#[cfg(test)]
mod benches {
    use super::*;
    extern crate test;
    use enso_frp as frp_old;
    use network::Network_;
    use test::Bencher;

    const REPS: usize = 100_000;

    fn map_fn(a: &usize, b: &usize) -> usize {
        if *a > 100 {
            10 * a + b
        } else {
            5 * a + 2 * b
        }
    }

    #[bench]
    fn bench_emit_plain_fn(bencher: &mut Bencher) {
        let mut sum = 0;
        bencher.iter(move || {
            for i in 0..REPS {
                sum += map_fn(&i, &i);
            }
            assert_ne!(sum, 0);
        });
    }

    // 3739029
    // 1744908
    // 1661864
    // 1595170
    // 1604364
    // 1493188
    // 1454128
    #[bench]
    fn bench_emit_frp_pod(bencher: &mut Bencher) {
        let net = Network_::new();
        let src1 = net.source::<usize>();
        let src2 = net.source::<usize>();
        let _m1 = src1.map2_(src2, map_fn);
        src2.emit(&2);

        bencher.iter(move || {
            for i in 0..REPS {
                src1.emit(&i);
            }
        });
    }

    // 1036659
    #[bench]
    fn bench_emit_frp_pod_old(bencher: &mut Bencher) {
        let net = frp_old::Network::new("label");
        frp_old::extend! { net
            src1 <- source::<usize>();
            src2 <- source::<usize>();
            _m1 <- map2(&src1, &src2, map_fn);
        }
        src2.emit(2);

        bencher.iter(move || {
            let _keep = &net;
            for i in 0..REPS {
                src1.emit(i);
            }
        });
    }

    // 10:   5047258
    // 50:  47114087
    #[bench]
    fn bench_emit_frp_chain_pod(bencher: &mut Bencher) {
        let net = Network_::new();
        let n1 = net.source::<usize>();
        let n2 = n1.map_(|t| t + 1);
        let mut prev = n2;
        for _ in 0..8 {
            let next = prev.map_(|t| t + 1);
            prev = next;
        }

        bencher.iter(move || {
            for i in 0..REPS {
                n1.emit(&i);
            }
        });
    }

    // 5: 2843612
    // 10: 6370118
    // 50: 79284775
    #[bench]
    fn bench_emit_frp_chain_pod_old(bencher: &mut Bencher) {
        let net = frp_old::Network::new("label");
        frp_old::extend! { net
            n1 <- source::<usize>();
            n2 <- map(&n1, |t| t + 1);
        }
        let mut prev = n2;
        for _ in 0..8 {
            frp_old::extend! { net
                next <- map(&prev, |t| t + 1);
            }
            prev = next;
        }

        bencher.iter(move || {
            let _keep = &net;
            for i in 0..REPS {
                n1.emit(i);
            }
        });
    }

    // 1488912
    #[bench]
    fn bench_emit_non_pod_frp(bencher: &mut Bencher) {
        let net = Network_::new();
        let src1 = net.source::<String>();
        let src2 = net.source::<usize>();
        let _m1 = src1.map2_(src2, |s, i| s.len() + i);
        src2.emit(&2);

        bencher.iter(move || {
            let str = "test".to_owned();
            for _ in 0..REPS {
                src1.emit(&str);
            }
        });
    }

    // 1077895
    #[bench]
    fn bench_emit_frp_non_pod_old(bencher: &mut Bencher) {
        let net = frp_old::Network::new("label");
        frp_old::extend! { net
            src1 <- source::<Rc<String>>();
            src2 <- source::<usize>();
            _m1 <- map2(&src1, &src2, |s, i| s.len() + i);
        }
        src2.emit(2);

        bencher.iter(move || {
            let str = Rc::new("test".to_owned());
            let _keep = &net;
            for _ in 0..REPS {
                src1.emit(&str);
            }
        });
    }

    // 1450107 // Including removal of frp network taking 60% of time.
    #[bench]
    fn bench_create_frp(bencher: &mut Bencher) {
        bencher.iter(move || {
            // with_runtime(|rt| rt.unsafe_clear());
            let net = Network_::new();
            for _ in 0..100_000 {
                let _src1 = net.source::<usize>();
                // let src2 = net.source::<usize>();
                // let m1 = src1.map2_(src2, map_fn);
            }
        });
    }

    // 8784825
    #[bench]
    fn bench_create_frp_old(bencher: &mut Bencher) {
        bencher.iter(move || {
            let net = frp_old::Network::new("label");
            for _ in 0..100_000 {
                frp_old::extend! { net
                    _src1 <- source::<usize>();
                }
                // let src2 = net.source::<usize>();
                // let m1 = net.map2(src1, src2, map_fn);
            }
        });
    }

    // 42284
    #[bench]
    fn bench_push_to_vector(bencher: &mut Bencher) {
        bencher.iter(move || {
            let mut vec = Vec::with_capacity(100_000);
            for i in 0..100_000 {
                vec.push(i);
            }
            assert_eq!(vec.len(), 100_000);
        });
    }
}
