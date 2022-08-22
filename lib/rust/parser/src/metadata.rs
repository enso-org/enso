//! Data associated with a syntax tree.

use std::collections::BTreeMap;
use std::str::FromStr;
use uuid::Uuid;



// ================
// === Metadata ===
// ================

/// Attaches stable IDs to AST nodes, and associates properties with them.
#[derive(Debug)]
pub struct Metadata {
    id_map: BTreeMap<(usize, usize), Uuid>,
}

impl Metadata {
    /// Return the UUID associated with the node identified by offset/length, if any is found.
    pub fn get_uuid(&self, code_offset: usize, code_length: usize) -> Option<Uuid> {
        Some(*self.id_map.get(&(code_offset, code_length))?)
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
        let unpack = |k: Location| (k.index.value, k.size.value);
        let id_map = metadata.id_map.into_iter().map(|(k, v)| (unpack(k), v)).collect();
        Self { id_map }
    }
}

/// Given source code, if a metadata section is found: Attempt to parse it; return the result, and
/// the non-metadata portion of the input.
pub fn parse(input: &str) -> Option<(Result, &str)> {
    let (code, metadata) = input.rsplit_once("#### METADATA ####\n")?;
    Some((metadata.parse().map(|data: MetadataFormat| data.into()), code))
}

/// Result of parsing metadata.
pub type Result<T = Metadata> = std::result::Result<T, String>;

impl FromStr for MetadataFormat {
    type Err = String;
    fn from_str(s: &str) -> Result<MetadataFormat> {
        let mut lines = s.lines();
        let id_map = serde_json::from_str(lines.next().unwrap()).unwrap();
        Ok(MetadataFormat { id_map })
    }
}


// === Location ===

#[derive(Debug, Copy, Clone, serde::Serialize, serde::Deserialize)]
struct Location {
    index: Number,
    size:  Number,
}

#[derive(Debug, Copy, Clone, serde::Serialize, serde::Deserialize)]
struct Number {
    value: usize,
}
