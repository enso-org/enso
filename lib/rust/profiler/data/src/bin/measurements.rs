//! Tool that generates measurement hierarchy reports from profiling data.
//!
//! # Usage
//!
//! The tool reads a JSON-formatted event log from stdin, and writes a report to stdout.
//!
//! For example:
//!
//! ```console
//! ~/git/enso/data $ cargo run --bin measurements < profile.json | less
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

use enso_profiler::format::AnyMetadata;
use enso_profiler_data as profiler_data;



// =========================
// === print_measurement ===
// =========================

/// Pretty-print a [`profiler_data::Measurement`], including all children, in a way that illustrates
/// the hierarchy of the data. Results will be written to stdout.
fn print_measurement<Metadata: std::fmt::Display>(
    profile: &profiler_data::Profile<Metadata>,
    measurement: profiler_data::MeasurementId,
    i: usize,
) {
    let measurement = &profile[measurement];
    let mut indent = String::new();
    for _ in 0..i {
        indent.push_str("  ");
    }
    println!("{}{}", indent, measurement.label);
    print!("{indent}");
    print!("  {:.1}", measurement.created.into_ms());
    for active in &measurement.intervals {
        let interval = &profile[*active];
        print!("  {}", fmt_interval(interval.interval));
    }
    println!();
    for active in &measurement.intervals {
        let interval = &profile[*active];
        for metadata in &interval.metadata {
            println!("{}  {}", indent, metadata.data);
        }
    }
    for child in &measurement.children {
        print_measurement(profile, *child, i + 1);
    }
}


// === formatting ===

/// Format a [`profiler_data::Interval`] in an easy-to-read way.
fn fmt_interval(interval: profiler_data::Interval) -> String {
    let start = interval.start.into_ms();
    let end = interval.end.map(|x| format!("{:.1}", x.into_ms())).unwrap_or_default();
    format!("{start:.1}-{end}")
}



// ============
// === main ===
// ============

fn main() {
    use std::io::Read;

    let mut log = String::new();
    std::io::stdin().read_to_string(&mut log).unwrap();
    let profile: profiler_data::Profile<AnyMetadata> = log.parse().unwrap();
    for root in &profile.root_measurement().children {
        print_measurement(&profile, *root, 0);
    }
}
