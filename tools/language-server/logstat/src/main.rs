use std::fmt;
use std::io::Result;
use std::path::PathBuf;

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
    /// Logfiles to analyze
    #[clap(value_name = "LOGFILE", value_hint = ValueHint::FilePath)]
    files: Vec<PathBuf>,

    /// Specification file
    #[clap(long, value_name = "FILE", value_hint = ValueHint::FilePath)]
    spec: PathBuf,

    /// Number of iterations to skip
    #[clap(long, default_value = "10")]
    warmup_iterations: usize,
}

/// Specification contains lines to lookup in the log file
#[derive(Debug)]
struct Spec {
    matches: Vec<String>,
}

/// Timed operation
struct Timed {
    timestamp: OffsetDateTime,
    duration:  Duration,
    line:      String,
}

impl fmt::Display for Timed {
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
struct Iteration {
    pub operations: Vec<Timed>,
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
    let mut ops = vec![];
    let mut matches = spec.matches.iter();

    let mut current_operations = vec![];
    let mut current_timestamp = OffsetDateTime::from_unix_timestamp(0).unwrap();
    let mut current_match = if let Some(m) = matches.next() {
        m.to_string()
    } else {
        matches = spec.matches.iter();
        matches.next().expect("Empty spec!").to_string()
    };

    while let Some(line) = lines.next_line().await? {
        if let Some(cap) = RE_LOG.captures(line.as_str()) {
            let groups = (cap.get(2), cap.get(3));
            match groups {
                (Some(timestamp), Some(message)) => {
                    if message.as_str().contains(current_match.as_str()) {
                        let datetime = OffsetDateTime::parse(timestamp.as_str(), &Rfc3339).unwrap();
                        if current_timestamp == OffsetDateTime::UNIX_EPOCH {
                            current_timestamp = datetime
                        }

                        let this_duration = datetime - current_timestamp;
                        current_timestamp = datetime;

                        let timed = Timed {
                            timestamp: datetime,
                            duration:  this_duration,
                            line:      message.as_str().to_string(),
                        };
                        current_operations.push(timed);

                        if let Some(m) = matches.next() {
                            current_match = m.to_string()
                        } else {
                            matches = spec.matches.iter();
                            current_match = matches.next().expect("Empty spec!").to_string();
                            current_timestamp = OffsetDateTime::UNIX_EPOCH;
                            let op = Iteration { operations: current_operations };
                            ops.push(op);
                            current_operations = vec![];
                        }
                    }
                }
                _ => {
                    eprintln!("[ERR] Invalid log line [{}]", line);
                }
            }
        }
    }

    Ok(ops)
}

/// Cleanup iterations info before analyzing
fn cleanse(ops: &[Iteration], skip: usize) -> &[Iteration] {
    &ops[skip..]
}

/// Analyze benchmark results
fn analyze_iterations(ops: &[Iteration]) -> Vec<Stats> {
    let mut stats = vec![];
    let ops_len = ops.len();
    let splits_len = ops.first().unwrap().operations.len();
    let mut i = 0;

    while i < splits_len {
        let mut min = Duration::MAX;
        let mut max = Duration::ZERO;
        let mut sum = Duration::ZERO;
        let current_line = &ops[0].operations[i].line;
        for op in ops {
            let timed = &op.operations[i];
            min = timed.duration.min(min);
            max = timed.duration.max(max);
            sum += timed.duration;
        }
        let op_stats =
            Stats { min, max, avg: sum / ops_len as u32, line: current_line.to_string() };
        stats.push(op_stats);
        i += 1;
    }

    let overall_stats = iterations_average(ops, &stats);
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

    let raw_iterations = read_logfile(&args.files[0], &spec).await?;
    let iterations = cleanse(&raw_iterations, args.warmup_iterations);
    let stats = analyze_iterations(iterations);

    println!("Stats (of {} records)", iterations.len());
    for s in &stats {
        println!("{}", s);
    }

    Ok(())
}
