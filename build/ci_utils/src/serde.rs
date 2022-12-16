//! Utilities for serialization and deserialization using `serde`.

use crate::prelude::*;

use serde::de::Error;
use serde::Deserializer;
use serde::Serializer;



#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Either<T, U> {
    Left(T),
    Right(U),
}

impl<T, U> Either<T, U>
where T: Into<U>
{
    pub fn into_right(self) -> U {
        match self {
            Either::Left(t) => t.into(),
            Either::Right(r) => r,
        }
    }
}

pub trait WithShorthand<'a, Shorthand: Sized + Deserialize<'a>>: Deserialize<'a> {
    fn resolve(short: Shorthand) -> Self;

    fn de<D>(de: D) -> std::result::Result<Self, D::Error>
    where D: Deserializer<'a> {
        Either::<Shorthand, Self>::deserialize(de).map(|e| match e {
            Either::Left(shorthand) => Self::resolve(shorthand),
            Either::Right(value) => value,
        })
    }
}

impl<'a, T: Deserialize<'a>> WithShorthand<'a, T> for Vec<T> {
    fn resolve(short: T) -> Self {
        vec![short]
    }
}


#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum SingleOrSequence<T> {
    Single(T),
    Sequence(Vec<T>),
}

impl<T> From<SingleOrSequence<T>> for Vec<T> {
    fn from(value: SingleOrSequence<T>) -> Self {
        match value {
            SingleOrSequence::Single(value) => vec![value],
            SingleOrSequence::Sequence(values) => values,
        }
    }
}

/// Function to be used as `#[serde(deserialize_with="single_or_sequence")]`.
///
/// It allows deserializing a single T value into Vec<T>, rather than requiring being provided with
/// a single element list for such case.
pub fn single_or_sequence<'de, D, T>(de: D) -> std::result::Result<Vec<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>, {
    WithShorthand::de(de)
}

/// Module to be used as `#[serde(with="regex_vec")]`
///
/// It supports serialization of `Vec<Regex>` through either a single `String` or `String` sequence.
pub mod regex_vec {
    use super::*;

    use regex::Regex;

    /// See [`regex_vec`].
    pub fn serialize<S>(value: &[Regex], ser: S) -> std::result::Result<S::Ok, S::Error>
    where S: Serializer {
        ser.collect_seq(value.iter().map(Regex::as_str))
    }

    /// See [`regex_vec`].
    pub fn deserialize<'de, D: Deserializer<'de>>(
        de: D,
    ) -> std::result::Result<Vec<Regex>, D::Error> {
        let regex_texts: Vec<String> = single_or_sequence(de)?;
        regex_texts
            .iter()
            .map(String::as_str)
            .map(Regex::new)
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(D::Error::custom)
    }
}

/// Module that serializes/deserialized types using [`Display`]/[`FromString`] traits.
pub mod via_string {
    use super::*;

    /// Serializer, that uses [`Display`] trait.
    pub fn serialize<S, T>(value: &T, ser: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Display, {
        ser.collect_str(value)
    }

    /// Deserializer, that uses [`FromString`] trait.
    pub fn deserialize<'de, D, T>(de: D) -> std::result::Result<T, D::Error>
    where
        D: Deserializer<'de>,
        T: FromString, {
        let text = String::deserialize(de)?;
        T::from_str(&text).map_err(D::Error::custom)
    }
}
