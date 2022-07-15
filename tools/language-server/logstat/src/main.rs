use std::fmt;
use std::io::Result;
use std::path::PathBuf;
use std::process;

use clap::Parser;
use clap::ValueHint;
use lazy_static::lazy_static;
use regex::Regex;
use time::format_description::well_known::Rfc3339;
use time::Duration;
use time::OffsetDateTime;
use tokio::fs::File;
use tokio::io::AsyncBufReadExt;
use tokio::io::BufReader;

#[derive(Parser, Debug)]
#[clap(version, about)]
struct Args {
    /// Logfile to analyze
    #[clap(value_name = "FILE", value_hint = ValueHint::FilePath)]
    log: PathBuf,

    /// Specification file
    #[clap(long, value_name = "FILE", value_hint = ValueHint::FilePath)]
    spec: PathBuf,

    /// Wstest log file
    #[clap(long, value_name = "FILE", value_hint = ValueHint::FilePath)]
    wstest_log: Option<PathBuf>,

    /// Number of iterations to skip
    #[clap(long, default_value = "0")]
    skip_iterations: usize,

    /// Calculate median instead of mean
    #[clap(long)]
    median: bool,
}

/// Specification contains lines to lookup in the log file
#[derive(Debug)]
struct Spec {
    matches: Vec<String>,
}

/// Timed operation
#[derive(Debug)]
struct Operation {
    duration:  Duration,
    timestamp: OffsetDateTime,
    line:      String,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}ms [{}] {}...",
            self.duration.whole_milliseconds(),
            self.timestamp.format(&Rfc3339).unwrap(),
            self.line.chars().take(80).collect::<String>()
        )
    }
}

/// Single iteration containing multiple sub-operations
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

/// Final statistics about benchmarked operation
#[derive(Debug)]
struct Stats {
    min:  Duration,
    max:  Duration,
    avg:  Duration,
    line: String,
}

impl fmt::Display for Stats {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{avg}ms [{min}..{max}] {line}",
            avg = self.avg.whole_milliseconds(),
            min = self.min.whole_milliseconds(),
            max = self.max.whole_milliseconds(),
            line = self.line.chars().take(80).collect::<String>()
        )
    }
}

lazy_static! {
    static ref RE_LOG: Regex = Regex::new(r"\[([\w]+)\] \[([\w\d:.-]+)\] (.*)").unwrap();
    static ref WSTEST_SPEC: Spec = Spec {
        matches: vec![
            "wstest sent bench request".to_string(),
            "wstest handled response".to_string(),
        ],
    };
}

/// Read the file with specification
async fn read_specs(path: &PathBuf) -> Result<Spec> {
    let file = File::open(path).await?;
    let mut lines = BufReader::new(file).lines();
    let mut matches = Vec::new();

    while let Some(line) = lines.next_line().await? {
        matches.push(line);
    }

    Ok(Spec { matches })
}

/// Extract the iterations information from logfile according to the provided spec
async fn read_logfile(path: &PathBuf, spec: &Spec) -> Result<Vec<Iteration>> {
    let file = File::open(path).await?;
    let mut lines = BufReader::new(file).lines();

    let mut iterations = vec![];
    let mut current_operations = vec![];
    let mut matches = spec.matches.iter();

    let mut current_match = if let Some(m) = matches.next() {
        m
    } else {
        matches = spec.matches.iter();
        matches.next().expect("Empty spec!")
    };

    while let Some(line) = lines.next_line().await? {
        if let Some(cap) = RE_LOG.captures(line.as_str()) {
            let groups = (cap.get(2), cap.get(3));
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


/// Calcualte median of values
fn median(durations: &mut [Duration]) -> Duration {
    durations.sort();
    let mid = durations.len() / 2;
    if durations.len() % 2 == 0 {
        (durations[mid - 1] + durations[mid]) / 2
    } else {
        durations[mid]
    }
}

/// Cleanup iterations info before analyzing
fn cleanse_iterations(ops: &mut Vec<Iteration>, skip: usize) {
    ops.drain(..skip);
}

/// Merge iterations from two log files
fn merge_iterations(ws_iterations: &mut Vec<Iteration>, log_iterations: Vec<Iteration>) {
    for (log_iter, ws_iter) in log_iterations.into_iter().zip(ws_iterations.into_iter()) {
        ws_iter.operations.splice(1..1, log_iter.operations);
    }
}

/// Calculate operation durations for each benchmark iteration
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

/// Analyze benchmark results
fn analyze_iterations(iterations: &[Iteration], use_median: bool) -> Vec<Stats> {
    let mut stats = vec![];

    let ops_len = iterations.len();
    let splits_len = iterations.first().unwrap().operations.len();

    let mut i = 0;
    while i < splits_len {
        let mut min = Duration::MAX;
        let mut max = Duration::ZERO;
        let mut sum = Duration::ZERO;
        let mut durations = vec![];

        let current_line = &iterations[0].operations[i].line;

        for iteration in iterations {
            let timed = &iteration.operations[i];

            durations.push(timed.duration);
            min = timed.duration.min(min);
            max = timed.duration.max(max);
            sum += timed.duration;
        }

        let avg = if use_median { median(&mut durations) } else { sum / ops_len as u32 };
        let line = current_line.to_string();

        stats.push(Stats { min, max, avg, line });

        i += 1;
    }

    let overall_stats = iterations_average(iterations, &stats);
    stats.push(overall_stats);

    stats
}

/// Calculate the average of all iterations
fn iterations_average(ops: &[Iteration], stats: &[Stats]) -> Stats {
    let min_opt = ops.into_iter().map(|o| o.total_time()).min();
    let max_opt = ops.into_iter().map(|o| o.total_time()).max();

    let min = min_opt.unwrap_or(Duration::ZERO);
    let max = max_opt.unwrap_or(Duration::ZERO);
    let avg = stats.into_iter().map(|s| s.avg).sum();
    let line = String::from("Total");

    Stats { min, max, avg, line }
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let spec = read_specs(&args.spec).await?;

    let mut log_iterations = read_logfile(&args.log, &spec).await?;

    let mut iterations;
    let mut ws_iterations;

    if let Some(path_buf) = args.wstest_log {
        ws_iterations = read_logfile(&path_buf, &WSTEST_SPEC).await?;

        // skip warmup iterations
        let start_time = &ws_iterations[0].operations[0].timestamp;
        log_iterations = log_iterations
            .into_iter()
            .skip_while(|iteration| {
                let first_operation = &iteration.operations[0];
                &first_operation.timestamp < start_time
            })
            .collect();

        if ws_iterations.len() != log_iterations.len() {
            eprintln!(
                "[ERR] Unequal number of benchmark iterations in log files! [{}] vs. [{}]",
                ws_iterations.len(),
                log_iterations.len()
            );
            process::exit(1);
        }

        merge_iterations(&mut ws_iterations, log_iterations);

        iterations = ws_iterations;
    } else {
        iterations = log_iterations;
    }

    cleanse_iterations(&mut iterations, args.skip_iterations);
    calculate_durations(&mut iterations);

    let stats = analyze_iterations(&iterations, args.median);

    println!("avg [min..max] (of {} records)", iterations.len());
    for s in &stats {
        println!("{}", s);
    }

    Ok(())
}
