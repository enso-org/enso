//! Serialization/deserialization support.
//!
//! Deserialization is used only for testing, but it is used by dependent crates, so it cannot be
//! gated with `#[cfg(test)]`.

use crate::prelude::*;



// ============
// === Tree ===
// ============

/// Deserialize a `Tree` from its binary representation.
pub fn deserialize_tree(data: &[u8]) -> Result<crate::syntax::tree::Tree, bincode::Error> {
    use bincode::Options;
    let options = bincode::DefaultOptions::new().with_fixint_encoding();
    options.deserialize(data)
}



// ============
// === Code ===
// ============

/// Serialized representation of a source code `Cow`.
#[derive(Serialize, Reflect)]
pub(crate) struct Code {
    #[reflect(hide)]
    begin: u32,
    #[reflect(hide)]
    len:   u32,
}

/// Serde wrapper to serialize a `Cow` as the `Code` representation.
#[allow(clippy::ptr_arg)] // This is the signature required by serde.
pub(crate) fn serialize_cow<S>(cow: &Cow<'_, str>, ser: S) -> Result<S::Ok, S::Error>
where S: serde::Serializer {
    let s = match cow {
        Cow::Borrowed(s) => *s,
        Cow::Owned(_) => panic!(),
    };
    let begin = s.as_ptr() as u32;
    let len = s.len() as u32;
    let serializable = Code { begin, len };
    serializable.serialize(ser)
}

pub(crate) fn deserialize_cow<'c, 'de, D>(deserializer: D) -> Result<Cow<'c, str>, D::Error>
where D: serde::Deserializer<'de> {
    let _ = deserializer.deserialize_u64(DeserializeU64);
    Ok(Cow::Owned(String::new()))
}



// =============
// === Error ===
// =============

/// Deserialization type for `crate::syntax::tree::Error`.
#[derive(Deserialize, Debug, Clone)]
pub(crate) struct Error(String);

impl From<Error> for crate::syntax::tree::Error {
    fn from(_: Error) -> Self {
        crate::syntax::tree::Error { message: "" }
    }
}



// ================
// === Visitors ===
// ================

struct DeserializeU64;

impl<'de> serde::de::Visitor<'de> for DeserializeU64 {
    type Value = u64;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "An unsigned 64-bit integer.")
    }

    fn visit_u64<E>(self, i: u64) -> Result<Self::Value, E>
    where E: serde::de::Error {
        Ok(i)
    }
}
