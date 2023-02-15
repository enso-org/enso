//! Tool that generates interval reports from profiling data.
//!
//! # Usage
//!
//! The tool reads a
//! [JSON-formatted event log](https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md#file-format)
//! from stdin, and writes a report to stdout.
//!
//! For example:
//!
//! ```console
//! ~/git/enso/data $ cargo run --bin intervals < profile.json | less
//! ```

// === Features ===
#![feature(test)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

use enso_prelude::*;

use enso_profiler::format::AnyMetadata;
use enso_profiler_data as data;
use std::collections;



// ============
// === main ===
// ============

fn main() {
    use std::io::Read;

    let mut log = String::new();
    std::io::stdin().read_to_string(&mut log).unwrap();
    let profile: data::Profile<AnyMetadata> = log.parse().unwrap();
    let mut aggregator = data::aggregate::Aggregator::default();
    aggregator.add_profile(&profile);
    let root = data::aggregate::Frame::from(aggregator);
    let funcs = FuncCollector::run(&root);
    let kv_to_func = |(label, timings)| Func { label, timings };
    let mut funcs: Vec<_> = funcs.into_iter().map(kv_to_func).collect();
    funcs.sort_unstable_by(|a, b| a.timings.self_duration.total_cmp(&b.timings.self_duration));
    println!("self_duration total_duration count profiler");
    for Func { label, timings } in funcs.iter().rev() {
        let FuncTimings { total_duration, self_duration, count } = timings;
        println!("{self_duration:>6.1} {total_duration:>6.1} {count} {label}");
    }
    let mut total_duration = 0.0;
    for Func { timings, .. } in funcs.iter() {
        total_duration += timings.self_duration;
    }
    println!("0.0 {total_duration:>6.1} 1 (total_self_duration)");
}



// =====================
// === FuncCollector ===
// =====================

/// Aggregates all intervals created by a particular profiler, abstracting away where in the stack
/// it occurs.
#[derive(Default)]
struct FuncCollector {
    funcs: HashMap<Label, FuncTimings>,
}

impl FuncCollector {
    /// Aggregate all intervals created by a particular profiler.
    fn run(root: &data::aggregate::Frame) -> collections::HashMap<Label, FuncTimings> {
        let mut collector = FuncCollector::default();
        for (label, frame) in &root.children {
            collector.visit(label, frame);
        }
        let FuncCollector { funcs, .. } = collector;
        funcs
    }
}

impl FuncCollector {
    /// Add time spent in an interval to the running sums; recurse into children.
    fn visit(&mut self, label: &Label, frame: &data::aggregate::Frame) {
        let func = self.funcs.entry(label.clone()).or_default();
        func.self_duration += frame.self_duration();
        func.total_duration += frame.total_duration();
        func.count += frame.interval_count();
        for (label, frame) in &frame.children {
            self.visit(label, frame);
        }
    }
}

type Label = ImString;



// ===================
// === FuncTimings ===
// ===================

/// Aggregate of all time spent in a particular profiler's intervals.
#[derive(Default)]
struct FuncTimings {
    total_duration: f64,
    self_duration:  f64,
    count:          usize,
}



// ============
// === Func ===
// ============

/// Identifies a profiler, and contains information about the time spent in its intervals.
struct Func {
    label:   Label,
    timings: FuncTimings,
}
