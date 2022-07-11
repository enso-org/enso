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
    spec:  PathBuf,
}

#[derive(Debug)]
struct Spec {
    matches: Vec<String>,
}

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

struct Operation {
    pub splits: Vec<Timed>,
}
impl Operation {
    pub fn total_time(&self) -> Duration {
        let mut total: Duration = Duration::ZERO;
        for split in &self.splits {
            total += split.duration
        }
        total
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[")?;
        for split in &self.splits {
            writeln!(f, "{}", split)?;
        }
        writeln!(f, "total {}ms", self.total_time().whole_milliseconds())?;
        writeln!(f, "]")?;
        Ok(())
    }
}


lazy_static! {
    static ref RE_LOG: Regex = Regex::new(r"\[([\w]+)\] \[([\w\d:.-]+)\] (.*)").unwrap();
}


async fn read_specs(path: &PathBuf) -> Result<Spec> {
    let file = File::open(path).await?;
    let mut lines = BufReader::new(file).lines();
    let mut matches = Vec::new();

    while let Some(line) = lines.next_line().await? {
        matches.push(line);
    }

    Ok(Spec { matches })
}

async fn read_logfile(path: &PathBuf, spec: &Spec) -> Result<Vec<Operation>> {
    let file = File::open(path).await?;
    let mut lines = BufReader::new(file).lines();
    let mut ops = vec![];
    let mut matches = spec.matches.iter();

    let mut current_splits = vec![];
    let mut current_timestamp = OffsetDateTime::from_unix_timestamp(0).unwrap();
    let mut current_match = if let Some(m) = matches.next() {
        m.to_string()
    } else {
        matches = spec.matches.iter();
        matches.next().expect("Empty spec!").to_string()
    };

    while let Some(line) = lines.next_line().await? {
        match RE_LOG.captures(line.as_str()) {
            Some(cap) => {
                let groups = (cap.get(2), cap.get(3));
                match groups {
                    (Some(timestamp), Some(message)) => {
                        if message.as_str().contains(current_match.as_str()) {
                            let datetime =
                                OffsetDateTime::parse(timestamp.as_str(), &Rfc3339).unwrap();
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
                            current_splits.push(timed);

                            if let Some(m) = matches.next() {
                                current_match = m.to_string()
                            } else {
                                matches = spec.matches.iter();
                                current_match = matches.next().expect("Empty spec!").to_string();
                                current_timestamp = OffsetDateTime::UNIX_EPOCH;
                                let op = Operation { splits: current_splits };
                                ops.push(op);
                                current_splits = vec![];
                            }
                        }
                    }
                    _ => {
                        eprintln!("[ERR] Invalid log line [{}]", line);
                    }
                }
            }
            None => {}
        }
    }

    Ok(ops)
}

#[derive(Debug)]
struct OperationStats {
    min:  Duration,
    max:  Duration,
    avg:  Duration,
    line: String,
}

impl fmt::Display for OperationStats {
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

fn cleanse(ops: &[Operation]) -> &[Operation] {
    &ops[8..]
}

fn analyze_splits(ops: &[Operation]) -> Vec<OperationStats> {
    let mut stats = vec![];
    let ops_len = ops.len();
    let splits_len = ops.first().unwrap().splits.len();
    let mut i = 0;

    while i < splits_len {
        let mut min = Duration::MAX;
        let mut max = Duration::ZERO;
        let mut sum = Duration::ZERO;
        let current_line = &ops[0].splits[i].line;
        for op in ops {
            let timed = &op.splits[i];
            min = timed.duration.min(min);
            max = timed.duration.max(max);
            sum += timed.duration;
        }
        let op_stats =
            OperationStats { min, max, avg: sum / ops_len as u32, line: current_line.to_string() };
        stats.push(op_stats);
        i += 1;
    }

    let operation_stats = operation_average(ops, &stats);
    stats.push(operation_stats);

    stats
}

fn operation_average(ops: &[Operation], stats: &[OperationStats]) -> OperationStats {
    let min_opt = ops.into_iter().map(|o| o.total_time()).min();
    let max_opt = ops.into_iter().map(|o| o.total_time()).max();

    let min = min_opt.unwrap_or(Duration::ZERO);
    let max = max_opt.unwrap_or(Duration::ZERO);
    let avg = stats.into_iter().map(|s| s.avg).sum();
    let line = String::from("Total");

    OperationStats { min, max, avg, line }
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let spec = read_specs(&args.spec).await?;

    let ops = read_logfile(&args.files[0], &spec).await?;
    let cleansed_ops = cleanse(&ops);
    let stats = analyze_splits(cleansed_ops);

    println!("Stats");
    for s in &stats {
        println!("{}", s);
    }

    Ok(())
}
