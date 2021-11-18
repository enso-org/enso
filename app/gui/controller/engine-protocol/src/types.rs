//! Common types of JSON-RPC-based Enso services used by both Project Manager and Language Server.

use crate::prelude::*;

use crate::binary;

use serde::Deserialize;
use serde::Serialize;


// ===================
// === UTCDateTime ===
// ===================

/// Time in UTC time zone.
pub type UTCDateTime = chrono::DateTime<chrono::FixedOffset>;



// ================
// === Sha3_224 ===
// ================

/// SHA3-224 hash digest.
#[derive(Hash, Debug, Display, Clone, PartialEq, Eq, Serialize, Deserialize, Shrinkwrap)]
pub struct Sha3_224(String);

impl Sha3_224 {
    /// Create new SHA3-224 digest of any arbitrary `data`.
    pub fn new(data: &[u8]) -> Self {
        use sha3::Digest;
        let mut hasher = sha3::Sha3_224::new();
        hasher.input(data);
        hasher.into()
    }
}

impl From<sha3::Sha3_224> for Sha3_224 {
    fn from(hasher: sha3::Sha3_224) -> Self {
        use sha3::Digest;
        let result = hasher.result();
        let digest = hex::encode(result[..].to_vec());
        Self(digest)
    }
}

impl From<binary::message::EnsoDigest> for Sha3_224 {
    fn from(checksum: binary::message::EnsoDigest) -> Self {
        let digest = hex::encode(checksum.bytes);
        Self(digest)
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sha3_224() {
        let digest = Sha3_224::new(b"abc");
        let expected = "e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf".to_string();
        assert_eq!(digest.to_string(), expected);
    }
}
