//! Tool that translates a backend message log into the [`enso_profiler`] JSON format.
//!
//! # Usage
//!
//! The tool reads a CSV backend message log, and converts it to a
//! [JSON-formatted event log](https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md#file-format)
//! for use with [`enso_profiler`] tools.
//!
//! For example:
//!
//! ```console
//! ~/git/enso/enso_data $ cargo run --bin api_events_to_profile < messages.csv > messages.json
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

use enso_profiler::format;
use enso_profiler_enso_data::backend;



// ==================================
// === Backend message log format ===
// ==================================

mod api_events {
    use super::*;

    #[derive(Clone, Debug, serde::Deserialize)]
    pub struct Message {
        pub timestamp:  chrono::DateTime<chrono::offset::Utc>,
        pub direction:  backend::Direction,
        pub request_id: Option<String>,
        pub endpoint:   String,
    }

    pub fn parse(r: impl std::io::Read) -> Result<Vec<Message>, Box<dyn std::error::Error>> {
        let mut log = Vec::new();
        let mut reader = csv::ReaderBuilder::new().has_headers(false).from_reader(r);
        for message in reader.deserialize() {
            let message: Message = message?;
            log.push(message);
        }
        Ok(log)
    }
}



// ============
// === Main ===
// ============

fn main() {
    use std::io::Write;

    let must_be_csv = "Parse error (Is this a CSV logged by the language server?)";
    let backend_messages = api_events::parse(std::io::stdin()).expect(must_be_csv);
    let mut backend_profile = format::Builder::new();
    backend_profile.time_offset_ms(0.0);
    backend_profile.process("LanguageServer");
    for message in backend_messages {
        let api_events::Message { timestamp, direction, request_id, endpoint } = message;
        let data = backend::Message { direction, request_id, endpoint };
        let timestamp = timestamp.timestamp_nanos();
        let timestamp = format::Timestamp::from_ms(timestamp as f64 / 1_000_000.0);
        backend_profile.metadata(timestamp, "BackendMessage", data);
    }
    let backend_profile = backend_profile.build_string();
    std::io::stdout().write_all(backend_profile.as_bytes()).expect("IO Error writing results");
}
