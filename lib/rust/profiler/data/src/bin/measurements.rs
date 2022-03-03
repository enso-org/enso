use std::io::Read;
use enso_profiler_data as profiler_data;

fn fmt_interval(interval: profiler_data::Interval) -> String {
    let start = interval.start.into_ms();
    let end = interval.end.map(|x| format!("{:.1}", x.into_ms())).unwrap_or_default();
    format!("{:.1}-{}", start, end)
}

fn print_measurement(measurement: &enso_profiler_data::Measurement<()>, i: usize) {
    let mut indent = String::new();
    for _ in 0..i {
        indent.extend("  ".chars());
    }
    println!("{}{}", indent, measurement.label);
    match &measurement.lifetime {
        profiler_data::Lifetime::Async(profiler_data::AsyncLifetime { active, .. }) => {
            println!("{}  intervals:", indent);
            for active in active {
                println!("{}    interval: {}", indent, fmt_interval(*active));
            }
        }
        profiler_data::Lifetime::NonAsync { active } => {
            println!("{}  interval: {}", indent, fmt_interval(*active));
        }
    }
    println!("{}  children:", indent);
    for child in &measurement.children {
        print_measurement(child, i + 2);
    }
}

fn main() {
    let mut log = String::new();
    std::io::stdin().read_to_string(&mut log).unwrap();
    let app_lifetime: profiler_data::Measurement<()> = log.parse().unwrap();
    for root in app_lifetime.children {
        print_measurement(&root, 0);
    }
}
