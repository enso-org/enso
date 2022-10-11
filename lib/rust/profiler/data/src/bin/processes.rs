//! Tool for comparing the latencies of different processes in reacting to an event.
//!
//! # Usage
//!
//! The tool reads a JSON-formatted event log from stdin, and writes CSV data to stdout.
//!
//! For example:
//!
//! ```console
//! ~/git/enso/data $ cargo run --bin processes compile_new_shaders,backend_execution < profile.json
//! ```

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

use enso_profiler_data as profiler_data;
use profiler_data::Class;
use profiler_data::MeasurementId;
use profiler_data::OpaqueMetadata;
use profiler_data::Profile;
use profiler_data::Timestamp;
use std::collections::HashMap;
use std::default::Default;
use std::path::Path;
use std::str::FromStr;



// ===============
// === Process ===
// ===============

/// Used to classify work into sets that are executed in parallel with each other.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Process(usize);


// === Processes ===

/// A profile's [`Process`]es.
#[derive(Debug)]
pub struct Processes {
    in_order: Vec<Process>,
    names:    HashMap<Process, String>,
    by_label: HashMap<String, Process>,
}

impl FromStr for Processes {
    type Err = ();
    fn from_str(labels: &str) -> Result<Self, Self::Err> {
        let process_labels = labels.split(',').map(|s| s.to_owned());
        let mut in_order = Vec::new();
        let mut names = HashMap::new();
        let mut by_label = HashMap::new();
        for (i, label) in process_labels.enumerate() {
            let p = Process(i);
            in_order.push(p);
            names.insert(p, label.clone());
            by_label.insert(label, p);
        }
        Ok(Self { in_order, names, by_label })
    }
}



// =================================
// === Categorizing measurements ===
// =================================

/// Categorize the given profile's measurements by process.
pub fn categorize_measurements(
    profile: &Profile<OpaqueMetadata>,
    process_by_label: &HashMap<String, Process>,
) -> HashMap<MeasurementId, Process> {
    let root = profile.root_measurement_id();
    let mut measurement_process = Default::default();
    let current = Default::default();
    categorize_subtree(&mut measurement_process, profile, process_by_label, root, current);
    measurement_process
}

fn categorize_subtree(
    measurement_process: &mut HashMap<MeasurementId, Process>,
    profile: &Profile<OpaqueMetadata>,
    process_by_label: &HashMap<String, Process>,
    measurement_id: MeasurementId,
    current: Option<Process>,
) {
    let measurement = &profile[measurement_id];
    let new = process_by_label.get(&measurement.label.name).cloned();
    if let Some(process) = new {
        measurement_process.insert(measurement_id, process);
    }
    let current = new.or(current);
    for &child in &measurement.children {
        categorize_subtree(measurement_process, profile, process_by_label, child, current);
    }
}



// =========================
// === Process end times ===
// =========================

/// Find the end of each process, i.e. when the last work attributed to it is completed.
pub fn process_ends(
    profile: &Profile<OpaqueMetadata>,
    measurement_process: &HashMap<MeasurementId, Process>,
    root: MeasurementId,
) -> Vec<(Process, f64)> {
    let mut ends = Default::default();
    for &child in &profile[root].children {
        gather_ends(&mut ends, profile, measurement_process, child);
    }
    let root_start = profile[root].created;
    ends.into_iter()
        .map(|(process, end)| {
            let end = end.into_ms() - root_start.into_ms();
            (process, end)
        })
        .collect()
}

fn gather_ends(
    ends: &mut HashMap<Process, Timestamp>,
    profile: &Profile<OpaqueMetadata>,
    measurement_process: &HashMap<MeasurementId, Process>,
    measurement_id: MeasurementId,
) {
    let measurement = &profile[measurement_id];
    if let Some(process) = measurement_process.get(&measurement_id) {
        let last_interval = measurement.intervals.last();
        let end = last_interval.and_then(|&i| profile[i].interval.end);
        if let Some(new_end) = end {
            let end = ends.entry(*process).or_default();
            if new_end > *end {
                *end = new_end;
            }
        }
    }
    for &child in &measurement.children {
        gather_ends(ends, profile, measurement_process, child);
    }
}



// ====================
// === Working time ===
// ====================

/// Sum the time any profiler not attributable to a foreign process is active during the given
/// interval.
pub fn working_time_in_interval(
    profile: &Profile<OpaqueMetadata>,
    measurement_process: &HashMap<MeasurementId, Process>,
    interval_start: Timestamp,
    interval_end: Timestamp,
) -> f64 {
    let mut total = 0.0;
    for &i in &profile.root_interval().children {
        let interval = &profile[i];
        if measurement_process.contains_key(&interval.measurement) {
            continue;
        }
        let interval = interval.interval;
        let start = interval.start;
        if let Some(end) = interval.end {
            let start = std::cmp::max(start, interval_start).into_ms();
            let end = std::cmp::min(end, interval_end).into_ms();
            let duration = end - start;
            if duration.is_sign_positive() {
                total += duration;
            }
        }
    }
    total
}



// ===========================
// === Highlighted regions ===
// ===========================

/// Get the region of interest in the profile, identified by a special profiler that must be present
/// in the data.
pub fn get_highlighted_region(
    profile: &Profile<OpaqueMetadata>,
) -> (MeasurementId, Timestamp, Timestamp) {
    let is_highlight = |&m: &MeasurementId| profile[m].classify() == Class::Highlight;
    let mut highlights: Vec<_> = profile.measurement_ids().filter(is_highlight).collect();
    let mut highlights = highlights.drain(..);
    let head = highlights.next();
    let rest = highlights.len();
    let m_id = match (head, rest) {
        (Some(first), 0) => first,
        _ => {
            let clause1 = "This tool currently only supports profiles of batch-mode workflows";
            let clause2 = "which should all have exactly one highlighted region";
            unimplemented!("{}, {}.", clause1, clause2);
        }
    };
    let measurement = &profile[m_id];
    let start = measurement.created;
    let non_empty_highlight_required = "Incomplete profile: Highlighted region contains no data.";
    let last_interval = measurement.intervals.last().expect(non_empty_highlight_required);
    let end = profile[*last_interval].interval.end;
    let complete_profile_required = "Incomplete profile: Highlighted region was not ended.";
    let end = end.expect(complete_profile_required);
    (m_id, start, end)
}



// ============
// === Main ===
// ============

fn main() {
    let mut args = std::env::args();
    let argv0 = args.next().unwrap();
    let labels = "foreign_process_label1,foreign_process_label2,...";
    let profiles = "profile1.json profile2.json ...";
    let usage = &format!("Usage: {} {} {}", argv0, labels, profiles);
    let processes = Processes::from_str(&args.next().expect(usage)).expect(usage);
    let mut cols = vec!["profile".into(), "main".into()];
    cols.extend(processes.in_order.iter().map(|p| processes.names[p].clone()));
    println!("{}", cols.join(","));
    for path in args {
        let path = Path::new(&path);
        let results = analyze_file(path, &processes);
        let results: Vec<_> = results.iter().map(|x| x.to_string()).collect();
        let file = path.file_stem().unwrap().to_str().unwrap();
        println!("{},{}", file, results.join(","));
    }
}

fn analyze_file(path: &Path, processes: &Processes) -> Vec<f64> {
    let log = std::fs::read_to_string(path).unwrap();
    let profile: Profile<OpaqueMetadata> = log.parse().unwrap();
    let measurement_process = categorize_measurements(&profile, &processes.by_label);
    let (root, root_start, root_end) = get_highlighted_region(&profile);
    let other_process_latencies: HashMap<_, _> = process_ends(&profile, &measurement_process, root)
        .into_iter()
        .map(|(p, end)| (p, end))
        .collect();
    let main_process_time =
        working_time_in_interval(&profile, &measurement_process, root_start, root_end);
    let process_latency = |p| other_process_latencies.get(p).cloned().unwrap_or_default();

    let mut results = vec![main_process_time];
    results.extend(processes.in_order.iter().map(process_latency));
    results
}
