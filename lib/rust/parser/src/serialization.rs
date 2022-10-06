//! Serialization/deserialization support.
//!
//! Deserialization is used only for testing, but it is used by dependent crates, so it cannot be
//! gated with `#[cfg(test)]`.

use crate::prelude::*;



// ============
// === Tree ===
// ============

/// Serialize a `Tree` to its binary representation.
pub fn serialize_tree(data: &crate::syntax::tree::Tree) -> Result<Vec<u8>, bincode::Error> {
    use bincode::Options;
    let options = bincode::DefaultOptions::new().with_fixint_encoding();
    options.serialize(data)
}

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
        Cow::Borrowed(s) => {
            let begin = str::as_ptr(s) as u32;
            let len = s.len() as u32;
            Code { begin, len }
        }
        Cow::Owned(s) if s.is_empty() => Code { begin: 0, len: 0 },
        Cow::Owned(_) => panic!(),
    };
    s.serialize(ser)
}

pub(crate) fn deserialize_cow<'c, 'de, D>(deserializer: D) -> Result<Cow<'c, str>, D::Error>
where D: serde::Deserializer<'de> {
    let _ = deserializer.deserialize_u64(DeserializeU64);
    Ok(Cow::Owned(String::new()))
}



// ==============
// === Tokens ===
// ==============

pub(crate) fn serialize_optional_char<S>(c: &Option<char>, s: S) -> Result<S::Ok, S::Error>
where S: serde::Serializer {
    let value = c.map(|c| c as u32).unwrap_or(0xFFFF_FFFF);
    s.serialize_u32(value)
}

pub(crate) fn deserialize_optional_char<'c, 'de, D>(
    deserializer: D,
) -> Result<Option<char>, D::Error>
where D: serde::Deserializer<'de> {
    let value = deserializer.deserialize_u32(DeserializeU32)?;
    Ok(match value {
        0xFFFF_FFFF => None,
        x => Some(char::try_from(x).unwrap()),
    })
}



// =============
// === Error ===
// =============

/// Deserialization type for `crate::syntax::tree::Error`.
#[derive(Deserialize, Debug, Clone)]
pub(crate) struct Error(String);

impl From<Error> for crate::syntax::tree::Error {
    fn from(error: Error) -> Self {
        let message = error.0.into();
        crate::syntax::tree::Error { message }
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

struct DeserializeU32;

impl<'de> serde::de::Visitor<'de> for DeserializeU32 {
    type Value = u32;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "An unsigned 32-bit integer.")
    }

    fn visit_u32<E>(self, i: u32) -> Result<Self::Value, E>
    where E: serde::de::Error {
        Ok(i)
    }
}



// ========================================
// === General purpose value transforms ===
// ========================================

pub(crate) fn serialize_optional_int<S>(x: &Option<u32>, s: S) -> Result<S::Ok, S::Error>
where S: serde::Serializer {
    s.serialize_u32(x.unwrap_or(0xFFFF_FFFF))
}

pub(crate) fn deserialize_optional_int<'c, 'de, D>(
    deserializer: D,
) -> Result<Option<u32>, D::Error>
where D: serde::Deserializer<'de> {
    let value = deserializer.deserialize_u32(DeserializeU32)?;
    Ok(match value {
        0xFFFF_FFFF => None,
        x => Some(x),
    })
}
