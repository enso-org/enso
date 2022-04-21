//! Produce a diagram illustrating timings of messages between Enso processes.
//!
//! # Interface
//!
//! Reads from stdin a multi-process profile file containing information logged by the IDE and by
//! the language server. Writes to stdout an SVG representation of message timings.
//!
//! ```console
//! ~/git/enso/data $ cargo run --bin message_beanpoles < profile.json > out.svg
//! ```
//!
//! # Usage example
//!
//! First, run the application and collect both IDE and language-server profiling data.
//! To enable collecting language-server data, see:
//! https://github.com/enso-org/enso/pull/3392#issue-1201784793
//! To capture a profile, see:
//! https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md#collecting-the-data
//! Save the IDE profile to `~/profile.json', and the Language Server message log as
//! `~/messages.csv'.
//!
//! Then:
//! ```console
//! # Convert the language server messages to the enso_profiler format.
//! profiler/enso_data $ cargo run --bin api_events_to_profile < ~/messages.csv > ~/messages.json
//! # Merge the IDE profiler and the language server messages profile.
//! profiler/enso_data $ cat ~/profile.json ~/messages.json > ~/fullprofile.json
//! # Render an SVG diagram of the message timings.
//! profiler/enso_data $ cargo run --bin message_beanpoles < ~/fullprofile.json > ~/diagram.svg
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
use enso_profiler_enso_data as enso_data;
use enso_profiler_enso_data::beanpole;



// ============
// === Main ===
// ============

fn main() {
    use std::io::Read;

    let mut profile = String::new();
    std::io::stdin().read_to_string(&mut profile).unwrap();
    let profiles: Vec<Result<data::Profile<enso_data::Metadata>, data::Error<_>>> =
        data::parse_multiprocess_profile(&profile).collect();
    let mut profiles_ = Vec::new();
    for profile in profiles {
        match profile {
            Ok(profile) => profiles_.push(profile),
            Err(data::Error::RecoverableFormatError { with_missing_data, .. }) =>
                profiles_.push(with_missing_data.unwrap()),
            Err(e) => panic!("{}", e),
        }
    }
    let profiles = profiles_;
    assert_eq!(profiles.len(), 2);

    let mut metadata0 = vec![];
    let mut metadata1 = vec![];
    collect_metadata(&profiles[0], profiles[0].root_interval_id(), &mut metadata0);
    collect_metadata(&profiles[1], profiles[1].root_interval_id(), &mut metadata1);
    let mut dia = beanpole::Diagram::default();
    let frontend = dia.process("Ide");
    let ls = dia.process("LanguageServer");
    let engine = dia.process("Engine");
    // TODO[kw]: Add metadata to format and read these fields from the file.
    let mut offset0 = None;
    let mut offset1 = None;
    for meta in metadata0.into_iter() {
        if let enso_data::Metadata::RpcEvent(message) = meta.data {
            let abs_time = meta.mark.into_ms();
            let offset = offset0.get_or_insert(abs_time);
            let time = abs_time - *offset;
            dia.message(ls, frontend, time, message);
        }
    }
    for meta in metadata1.into_iter() {
        if let enso_data::Metadata::BackendMessage(message) = meta.data {
            let abs_time = meta.mark.into_ms();
            let offset = offset1.get_or_insert(abs_time);
            let time = abs_time - *offset;
            let (p0, p1) = match message.direction {
                enso_data::backend::Direction::Request => (ls, engine),
                enso_data::backend::Direction::Response => (engine, ls),
            };
            dia.message(p0, p1, time, message.endpoint);
        }
    }
    beanpole::svg::write_diagram(&dia, std::io::stdout()).unwrap();
}

fn collect_metadata<M: Clone>(
    profile: &data::Profile<M>,
    interval: data::IntervalId,
    metadata: &mut Vec<data::Metadata<M>>,
) {
    let interval = &profile[interval];
    metadata.extend(interval.metadata.iter().cloned());
    for &child in &interval.children {
        collect_metadata(profile, child, metadata);
    }
}
