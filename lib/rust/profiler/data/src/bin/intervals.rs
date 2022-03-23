//! Tool that generates interval reports from profiling data.
//!
//! # Usage
//!
//! The tool reads a JSON-formatted event log from stdin, and writes a report to stdout.
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
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

use enso_profiler_data as data;
use std::collections;



// ============
// === main ===
// ============

fn main() {
    use std::io::Read;

    let mut log = String::new();
    std::io::stdin().read_to_string(&mut log).unwrap();
    let profile: data::Profile<()> = log.parse().unwrap();
    let mut aggregator = data::aggregate::Aggregator::default();
    aggregator.visit_profile(&profile);
    let root = data::aggregate::Frame::from(aggregator);
    let funcs = FuncCollector::run(&root);
    let mut funcs: Vec<_> = funcs.into_iter().collect();
    funcs.sort_by_key(|(_, func)| 0 - (func.self_duration * 10.0).round() as i64);
    println!("self_duration total_duration profiler");
    for (label, func) in &funcs {
        println!("{:>6.1} {:>6.1} {}", func.self_duration, func.total_duration, label);
    }
}



// =====================
// === FuncCollector ===
// =====================

/// Aggregates all intervals created by a particular profiler, abstracting away where in the stack
/// it occurs.
#[derive(Default)]
struct FuncCollector {
    funcs: collections::HashMap<Label, Func>,
}

impl FuncCollector {
    /// Aggregate all intervals created by a particular profiler.
    fn run(root: &data::aggregate::Frame) -> collections::HashMap<Label, Func> {
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
        for (label, frame) in &frame.children {
            self.visit(label, frame);
        }
    }
}

/// Aggregate of all time spent in a particular profiler's intervals.
#[derive(Default)]
struct Func {
    total_duration: f64,
    self_duration:  f64,
}

type Label = std::rc::Rc<String>;
