//! The crate provides an executable for collecting the performance statistics by analyzing the
//! log files.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_prelude::*;

use clap::Parser;
use clap::ValueHint;
use lazy_static::lazy_static;
use regex::Regex;
use std::fmt;
use std::io::Result;
use std::path::PathBuf;
use std::process;
use time::format_description::well_known::Rfc3339;
use time::Duration;
use time::OffsetDateTime;
use tokio::fs::File;
use tokio::io::AsyncBufReadExt;
use tokio::io::BufReader;
use tokio_stream::wrappers::LinesStream;
use tokio_stream::StreamExt;



// =====================
// === CLI Arguments ===
// =====================

#[derive(Parser, Debug)]
#[clap(version, about)]
struct Args {
    /// Logfile to analyze.
    #[clap(value_name = "FILE", value_hint = ValueHint::FilePath)]
    log: PathBuf,

    /// Specification file.
    #[clap(long, value_name = "FILE", value_hint = ValueHint::FilePath)]
    spec: PathBuf,

    /// Wstest log file.
    #[clap(long, value_name = "FILE", value_hint = ValueHint::FilePath)]
    wstest_log: Option<PathBuf>,

    /// Number of iterations to skip.
    #[clap(long, default_value = "0")]
    skip_iterations: usize,

    /// Calculate median instead of mean.
    #[clap(long)]
    median: bool,
}



// =========================
// === Log Specification ===
// =========================

/// A specification containing lines to lookup in the log file.
#[derive(Debug)]
struct Spec {
    matches: Vec<String>,
}



// =================
// === Operation ===
// =================

/// Timed operation.
#[derive(Debug)]
struct Operation {
    duration:  Duration,
    timestamp: OffsetDateTime,
    line:      String,
}

impl Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let duration_millis = self.duration.whole_milliseconds();
        let timestamp = self.timestamp.format(&Rfc3339).unwrap();
        let truncated_line = self.line.chars().take(80).collect::<String>();

        write!(f, "{}ms [{}] {}...", duration_millis, timestamp, truncated_line)
    }
}



// =================
// === Iteration ===
// =================

/// Single iteration containing multiple sub-operations.
#[derive(Debug)]
struct Iteration {
    pub operations: Vec<Operation>,
}

impl Iteration {
    pub fn total_time(&self) -> Duration {
        let mut total: Duration = Duration::ZERO;
        for operation in &self.operations {
            total += operation.duration
        }
        total
    }
}



// ==================
// === Statistics ===
// ==================

/// Final statistics about benchmarked operation.
#[derive(Debug)]
struct Stats {
    min:  Duration,
    max:  Duration,
    avg:  Duration,
    line: String,
}

impl Display for Stats {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let avg_millis = self.avg.whole_milliseconds();
        let min_millis = self.min.whole_milliseconds();
        let max_millis = self.max.whole_milliseconds();
        let truncated_line = self.line.chars().take(80).collect::<String>();

        write!(f, "{avg_millis}ms [{min_millis}..{max_millis}] {truncated_line}")
    }
}



// =================
// === Constants ===
// =================

/// First operation of Wstest sequence of operations.
static WSTEST_FIRST_OPERATION: usize = 1;

/// Capture group containing the timestamp part of the log line.
static RE_LOGLINE_TIMESTAMP_CAPTURE_GROUP: usize = 2;

/// Capture group containing the message part of the log line
static RE_LOGLINE_MESSAGE_CAPTURE_GROUP: usize = 3;

lazy_static! {
    /// Regex for parsing the log line.
    static ref RE_LOGLINE: Regex = Regex::new(r"\[([\w]+)\] \[([\w\d:.-]+)\] (.*)").unwrap();

    /// Specification for the log file produced by the wstest tool.
    static ref WSTEST_SPEC: Spec = Spec {
        matches: vec![
            "wstest sent bench request".to_string(),
            "wstest handled response".to_string(),
        ],
    };
}



// =================
// === Utilities ===
// =================

/// Read the file with specification.
async fn read_specs(path: &PathBuf) -> Result<Spec> {
    let file = File::open(path).await?;
    let lines_reader = BufReader::new(file).lines();
    let matches = LinesStream::new(lines_reader).collect::<Result<Vec<_>>>().await?;

    Ok(Spec { matches })
}

/// Extract the iterations information from logfile according to the provided spec.
async fn read_logfile(path: &PathBuf, spec: &Spec) -> Result<Vec<Iteration>> {
    let file = File::open(path).await?;
    let mut lines = BufReader::new(file).lines();

    let mut iterations = vec![];
    let mut current_operations = vec![];
    let mut matches = spec.matches.iter();
    let mut current_match = matches.next().expect("Empty spec!");

    while let Some(line) = lines.next_line().await? {
        if let Some(cap) = RE_LOGLINE.captures(line.as_str()) {
            let groups = (
                cap.get(RE_LOGLINE_TIMESTAMP_CAPTURE_GROUP),
                cap.get(RE_LOGLINE_MESSAGE_CAPTURE_GROUP),
            );
            match groups {
                (Some(timestamp), Some(message)) =>
                    if message.as_str().contains(current_match) {
                        let duration = Duration::ZERO;
                        let timestamp =
                            OffsetDateTime::parse(timestamp.as_str(), &Rfc3339).unwrap();
                        let line = message.as_str().to_string();

                        current_operations.push(Operation { duration, timestamp, line });

                        if let Some(m) = matches.next() {
                            current_match = m
                        } else {
                            matches = spec.matches.iter();
                            current_match = matches.next().expect("Empty spec!");
                            iterations.push(Iteration { operations: current_operations });
                            current_operations = vec![];
                        }
                    },
                _ => {
                    eprintln!("[ERR] Invalid log line [{}]", line);
                }
            }
        }
    }

    Ok(iterations)
}

/// Calcualte median of values.
fn median<I>(durations_iter: I) -> Duration
where I: Iterator<Item = Duration> {
    let mut durations = durations_iter.collect::<Vec<_>>();
    durations.sort();

    let mid = durations.len() / 2;
    if durations.len() % 2 == 0 {
        (durations[mid - 1] + durations[mid]) / 2
    } else {
        durations[mid]
    }
}

/// Merge iterations from two log files.
///
/// Function insert operations from logfile after the first operation of wstest tool.
fn merge_iterations(ws_iterations: &mut [Iteration], log_iterations: Vec<Iteration>) {
    log_iterations.into_iter().zip(ws_iterations.iter_mut()).for_each(|(log_iter, ws_iter)| {
        ws_iter
            .operations
            .splice(WSTEST_FIRST_OPERATION..WSTEST_FIRST_OPERATION, log_iter.operations);
    });
}

/// Calculate operation durations for each benchmark iteration.
fn calculate_durations(iterations: &mut Vec<Iteration>) {
    for iteration in iterations {
        let mut timestamp = OffsetDateTime::UNIX_EPOCH;

        for mut op in iteration.operations.iter_mut() {
            if timestamp == OffsetDateTime::UNIX_EPOCH {
                timestamp = op.timestamp
            }
            op.duration = op.timestamp - timestamp;
            timestamp = op.timestamp;
        }
    }
}

/// Analyze benchmark results.
fn analyze_iterations(iterations: &[Iteration], use_median: bool) -> Vec<Stats> {
    let iterations_len = iterations.len();
    let operations_len = iterations.first().unwrap().operations.len();

    let mut stats = (0..operations_len)
        .map(|operation_index| {
            let current_line = &iterations[0].operations[operation_index].line;
            let durations = iterations.iter().map(|it| it.operations[operation_index].duration);

            let min = durations.clone().min().unwrap_or(Duration::ZERO);
            let max = durations.clone().max().unwrap_or(Duration::ZERO);
            let avg = if use_median {
                median(durations)
            } else {
                durations.sum::<Duration>() / iterations_len as u32
            };
            let line = current_line.to_string();

            Stats { min, max, avg, line }
        })
        .collect_vec();

    let overall_stats = iterations_average(iterations, &stats);
    stats.push(overall_stats);

    stats
}

/// Calculate the average of all iterations.
fn iterations_average(ops: &[Iteration], stats: &[Stats]) -> Stats {
    let min_opt = ops.iter().map(|o| o.total_time()).min();
    let max_opt = ops.iter().map(|o| o.total_time()).max();

    let min = min_opt.unwrap_or(Duration::ZERO);
    let max = max_opt.unwrap_or(Duration::ZERO);
    let avg = stats.iter().map(|s| s.avg).sum();
    let line = String::from("Total");

    Stats { min, max, avg, line }
}



// ============
// === Main ===
// ============

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let spec = read_specs(&args.spec).await?;

    let log_iterations = read_logfile(&args.log, &spec).await?;

    let mut iterations = if let Some(path_buf) = args.wstest_log {
        let mut ws_iterations = read_logfile(&path_buf, &WSTEST_SPEC).await?;

        // skip warmup iterations
        let start_time = &ws_iterations[0].operations[0].timestamp;
        let log_iterations_without_warmup = log_iterations
            .into_iter()
            .skip_while(|iteration| {
                let first_operation = &iteration.operations[0];
                &first_operation.timestamp < start_time
            })
            .collect_vec();

        if ws_iterations.len() != log_iterations_without_warmup.len() {
            eprintln!(
                "[ERR] Unequal number of benchmark iterations in log files! [{}] vs. [{}]",
                ws_iterations.len(),
                log_iterations_without_warmup.len()
            );
            process::exit(1);
        }

        merge_iterations(&mut ws_iterations, log_iterations_without_warmup);

        ws_iterations
    } else {
        log_iterations
    };

    // cleanup iterations info before analyzing
    iterations.drain(..args.skip_iterations);
    calculate_durations(&mut iterations);

    let stats = analyze_iterations(&iterations, args.median);

    println!("avg [min..max] (of {} records)", iterations.len());
    for s in &stats {
        println!("{}", s);
    }

    Ok(())
}
