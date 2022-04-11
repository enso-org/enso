//! Tool that generates Chrome DevTools-compatible files from profiling interval data.
//!
//! The Chrome DevTools profile format has no official publicly available documentation.
//! Someone's description of it is available here:
//! https://docs.google.com/document/d/1lieZBBXZiEKOVk5vLCGmMT99_O-5lv9cGXoKnhqlY4g/preview
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
//! ~/git/enso/data $ cargo run --bin intervals < profile.json > devtools.json
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



/// Support for the Chrome DevTools profile format.
mod devtools {
    // =============
    // === Event ===
    // =============

    /// DevTools-profile interval.
    #[derive(serde::Serialize)]
    pub struct Event {
        pub name:         String,
        #[serde(rename = "cat")]
        pub category:     String,
        #[serde(rename = "ph")]
        pub event_type:   EventType,
        #[serde(rename = "ts")]
        pub timestamp_us: u64,
        #[serde(rename = "dur")]
        pub duration_us:  u64,
        #[serde(rename = "pid")]
        pub process_id:   u32,
        #[serde(rename = "tid")]
        pub thread_id:    u32,
        // Actually a type of map, but we don't need to write anything there.
        pub args:         Option<()>,
    }

    /// Information about type of event in DevTools profiling interval.
    #[derive(Clone, Copy, Eq, PartialEq, serde::Serialize)]
    pub enum EventType {
        #[serde(rename = "X")]
        Complete,
    }
}



// ============
// === main ===
// ============

fn main() {
    use std::io::Read;
    let mut log = String::new();
    std::io::stdin().read_to_string(&mut log).unwrap();
    let profile: data::Profile<()> = log.parse().unwrap();
    let events = IntervalTranslator::run(&profile);
    serde_json::to_writer(std::io::stdout(), &events).unwrap();
}



// ==========================
// === IntervalTranslator ===
// ==========================

/// Translates `profiler` data to the Chrome DevTools format.
struct IntervalTranslator<'p, Metadata> {
    profile: &'p data::Profile<Metadata>,
    events:  Vec<devtools::Event>,
}

impl<'p, Metadata> IntervalTranslator<'p, Metadata> {
    /// Translate `profiler` data to the Chrome DevTools format.
    fn run(profile: &'p data::Profile<Metadata>) -> Vec<devtools::Event> {
        let events = Default::default();
        let mut builder = Self { profile, events };
        // We skip the root node APP_LIFETIME, which is not a real measurement.
        for child in &profile.root_interval().children {
            builder.visit_interval(*child, 0);
        }
        let Self { events, .. } = builder;
        events
    }
}

impl<'p, Metadata> IntervalTranslator<'p, Metadata> {
    /// Translate an interval, and its children.
    fn visit_interval(&mut self, active: data::IntervalId, row: u32) {
        let active = &self.profile[active];
        let measurement = &self.profile[active.measurement];
        let start = active.interval.start.into_ms();
        // DevTools ignores open intervals.
        if let Some(duration_ms) = active.interval.duration_ms() {
            let duration_us = (duration_ms * 1000.0) as u64;
            let event = devtools::Event {
                name: measurement.label.to_string(),
                event_type: devtools::EventType::Complete,
                category: "interval".to_owned(),
                duration_us,
                timestamp_us: (start * 1000.0) as u64,
                process_id: 1,
                thread_id: 1,
                args: None,
            };
            self.events.push(event);
        }
        for child in &active.children {
            self.visit_interval(*child, row + 1);
        }
    }
}
