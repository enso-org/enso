//! Utilities related to UUID: extensions to the `uuid::Uuid`, the binary protocol's `EnsoUUID`
//! and conversions between them.

use crate::prelude::*;

use crate::generated::binary_protocol_generated::org::enso::languageserver::protocol::binary;

use binary::EnsoUUID;



impl EnsoUUID {
    /// Creates a new random EnsoUUID.
    pub fn new_v4() -> EnsoUUID {
        Uuid::new_v4().into()
    }
}

/// Utilities extending the Uuid class.
pub trait UuidExt {
    /// The most significant 64 bits of this UUID's 128 bit value.
    ///
    /// Compatible with `java.util.UUID.getMostSignificantBits()`.
    fn most_significant_bits(&self) -> i64;

    /// The least significant 64 bits of this UUID's 128 bit value.
    ///
    /// Compatible with `java.util.UUID.getLeastSignificantBits()`.
    fn least_significant_bits(&self) -> i64;

    /// Constructs a new UUID using the specified data.
    ///
    /// `most_significant` is used for the most significant 64 bits of the UUID and
    /// `least_significant` becomes the least significant 64 bits of the UUID.
    fn from_bytes_split(least_significant: [u8; 8], most_significant: [u8; 8]) -> Self;
}

impl UuidExt for Uuid {
    fn most_significant_bits(&self) -> i64 {
        i64::from_be_bytes(self.as_bytes()[..8].try_into().unwrap())
    }

    fn least_significant_bits(&self) -> i64 {
        i64::from_be_bytes(self.as_bytes()[8..].try_into().unwrap())
    }

    fn from_bytes_split(least_significant: [u8; 8], most_significant: [u8; 8]) -> Self {
        // let most_significant_bytes  = most_significant_bits.to_le_bytes();
        // let least_significant_bytes = least_significant_bits.to_le_bytes();
        let all_bytes = least_significant.iter().chain(most_significant.iter()).rev();

        let mut bytes: [u8; 16] = [default(); 16];
        for (dst, src) in bytes.iter_mut().zip(all_bytes) {
            *dst = *src;
        }

        Uuid::from_bytes(bytes)
    }
}

impls! { From + &From <Uuid> for EnsoUUID {
    |uuid|
        EnsoUUID::new(uuid.least_significant_bits() as u64, uuid.most_significant_bits() as u64)
}}

impls! { From + &From <EnsoUUID> for Uuid {
    |enso_uuid| {
        let least_significant = enso_uuid.leastSigBits().to_le_bytes();
        let most_significant  = enso_uuid.mostSigBits().to_le_bytes();
        Uuid::from_bytes_split(least_significant,most_significant)
    }
}}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn least_significant_bits() {
        let uuid = Uuid::parse_str("38400000-8cf0-11bd-b23e-10b96e4ef00d").unwrap();
        assert_eq!(uuid.least_significant_bits(), -5603022497796657139);
    }

    #[test]
    fn most_significant_bits() {
        let uuid = Uuid::parse_str("38400000-8cf0-11bd-b23e-10b96e4ef00d").unwrap();
        assert_eq!(uuid.most_significant_bits(), 4053239666997989821);
    }

    #[test]
    fn uuid_round_trips() {
        let uuid = Uuid::parse_str("6de39f7b-df3a-4a3c-84eb-5eaf96ddbac2").unwrap();
        let enso = EnsoUUID::from(uuid);
        let uuid2 = Uuid::from(enso);
        assert_eq!(uuid, uuid2);
    }
}
