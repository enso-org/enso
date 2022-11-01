use crate::prelude::*;

use reqwest::header::HeaderValue;
use reqwest::header::InvalidHeaderValue;
use std::fmt::Formatter;
use std::ops::RangeInclusive;



#[derive(Clone, Debug)]
pub struct ContentRange {
    pub range: RangeInclusive<usize>,
    pub total: Option<usize>,
}

impl ContentRange {
    pub fn whole(len: usize) -> Self {
        Self { range: 0..=len.saturating_sub(1), total: Some(len) }
    }

    /// Range length in bytes.
    pub fn len(&self) -> usize {
        1 + self.range.end() - self.range.start()
    }

    /// Check if the range is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl TryFrom<ContentRange> for HeaderValue {
    type Error = InvalidHeaderValue;

    fn try_from(value: ContentRange) -> std::result::Result<Self, Self::Error> {
        value.to_string().try_into()
    }
}

impl TryFrom<&ContentRange> for HeaderValue {
    type Error = InvalidHeaderValue;

    fn try_from(value: &ContentRange) -> std::result::Result<Self, Self::Error> {
        value.to_string().try_into()
    }
}

impl Display for ContentRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "bytes {}-{}/{}",
            self.range.start(),
            self.range.end(),
            self.total.map_or(String::from("*"), |total| total.to_string())
        )
    }
}
