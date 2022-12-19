//! Data associated with a syntax tree.
//!
//! This data is currently represented as two lines containing one JSON value each, placed at the
//! end of a file after a line containing exactly the text "#### METADATA ####".

use std::collections::BTreeMap;
use std::str::FromStr;
use uuid::Uuid;



const MARKER: &str = "#### METADATA ####\n";



// ================
// === Metadata ===
// ================

/// Attaches stable IDs to AST nodes, and associates properties with them.
#[derive(Debug)]
pub struct Metadata {
    id_map: BTreeMap<Location, Uuid>,
}

impl Metadata {
    /// Return the UUID associated with the node identified by offset/length, if any is found.
    pub fn get_uuid(&self, index: usize, size: usize) -> Option<Uuid> {
        let loc = Location { index: Number { value: index }, size: Number { value: size } };
        Some(*self.id_map.get(&loc)?)
    }
}


// === Parsing ===

/// Corresponds to the JSON structure used to store `Metadata`.
#[derive(Debug)]
struct MetadataFormat {
    id_map: Vec<(Location, Uuid)>,
}

impl From<MetadataFormat> for Metadata {
    fn from(metadata: MetadataFormat) -> Self {
        let id_map = metadata.id_map.into_iter().collect();
        Self { id_map }
    }
}

/// Given source code, if a metadata section is found: Attempt to parse it; return the result, and
/// the non-metadata portion of the input.
pub fn parse(input: &str) -> Option<(Result, &str)> {
    let (code, metadata) = input.rsplit_once(MARKER)?;
    Some((metadata.parse().map(|data: MetadataFormat| data.into()), code))
}

/// Result of parsing metadata.
pub type Result<T = Metadata> = std::result::Result<T, String>;

impl FromStr for MetadataFormat {
    type Err = String;
    fn from_str(s: &str) -> Result<MetadataFormat> {
        let mut lines = s.lines();
        let line0 = lines.next().ok_or("Expected a value.")?;
        let id_map = serde_json::from_str(line0).map_err(|e| e.to_string())?;
        Ok(MetadataFormat { id_map })
    }
}


// === Location ===

/// Identifies a span in the source code.
#[derive(Debug, Copy, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq, PartialOrd, Ord)]
struct Location {
    /// The beginning of the span, as a byte offset from the beginning of the file.
    index: Number,
    /// The length of the span, in bytes.
    size:  Number,
}

#[derive(Debug, Copy, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq, PartialOrd, Ord)]
struct Number {
    value: usize,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bad_metadata() {
        MetadataFormat::from_str(MARKER).expect_err("Empty metadata is error.");
        MetadataFormat::from_str("[ , ]").expect_err("Invalid JSON is error.");
    }

    #[test]
    fn empty_metadata() {
        MetadataFormat::from_str("[]").expect("Empty sequence is valid.");
    }
}
