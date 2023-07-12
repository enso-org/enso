use super::score;
use std::cmp::Ordering;

macro_rules! define_mixed_binary_radix_bases_impl {
    ($width_subtotal:expr, $ty:ty, ) => {
        const _STATIC_CHECK_ALL_BITS_FIT_IN_WORD: $ty = 1 << $width_subtotal;
    };
    ($width_subtotal:expr, $ty:ty, $(#[$attr:meta])* $ident:ident: $width:expr; $($rest:tt)*) => {
        $(#[$attr])* const $ident: $ty = 1 << $width_subtotal;
        define_mixed_binary_radix_bases_impl!($width_subtotal + $width, $ty, $($rest)*);
    };
}

/// Define factors partitioning the given type arithmetically into bitfields of specified
/// widths, starting from the little end of the word.
///
/// This enables packing multiple small numbers into a machine word that can be used as a key for
/// efficient sorting.
macro_rules! define_mixed_binary_radix_bases_low_to_high {
    ($ty:ty, $($rest:tt)*) => {
        define_mixed_binary_radix_bases_impl!(0, $ty, $($rest)*);
    };
}

/// Value enabling comparison of quality of a match.
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Score(u32);

/// Per-match penalty. These are only applied when converting a root-submatch score to a match
/// score, so each will not occur more than once and overflow is not possible.
const MATCH_BITS: u32 = 1;
/// A per-submatch penalty can occur up to 15 times in a match before it overflows.
const SUBMATCH_BITS: u32 = 4;
/// A per-character penalty can occur up to 255 times in a match before it overflows.
const CHAR_BITS: u32 = 8;

define_mixed_binary_radix_bases_low_to_high!(u32,
    /// When part of the pattern is matched against a prefix of a word in the target, this
    /// penalty is applied for each unused character in the rest of the word.
    CHAR_IN_MATCHED_WORD_SKIPPED_PENALTY: CHAR_BITS;
    /// When a submatch includes a `.` character, this penalty is applied.
    SUBMATCH_INCLUDES_NAMESPACE_PENALTY: SUBMATCH_BITS;
    /// When the target is an *alias*, this penalty is applied to the match.
    MATCH_TARGET_IS_ALIAS_PENALTY: MATCH_BITS;
    /// When the first character in the pattern matches a character that is not the first
    /// character of the submatch, this penalty is applied.
    SUBMATCH_NOT_FROM_START_PENALTY: SUBMATCH_BITS;
    /// When consecutive characters in the pattern are treated as initials rather than a single
    /// prefix, this penalty is applied.
    SUBMATCH_BY_INITIALS_PENALTY: SUBMATCH_BITS;
    /// When the match skips characters in the target, this penalty is applied.
    ///
    /// Although this overlaps with other criteria, applying the highest penalty for this case
    /// ensures that a perfect match is always the first result, even if it has other penalties
    /// (such as the [`MATCH_TARGET_IS_ALIAS`] penalty).
    MATCH_IS_IMPERFECT_PENALTY: MATCH_BITS;
);

/// Information about a match target that is used to evaluate the quality of the match.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct TargetInfo {
    /// True if this is an alias for a method, rather than the canonical name.
    pub is_alias: bool,
}

/// Observes how characters are matched within a submatch.
#[derive(Clone, Default)]
pub struct ScoreBuilder {
    // === State maintained to determine how further characters affect penalty ===
    word_chars_matched: bool,
    word_chars_skipped: bool,
    word_chars_matched_since_last_delimiter: bool,
    // === Penalty accrued so far ===
    penalty: u32,
}

impl score::ScoreBuilder for ScoreBuilder {
    type SubmatchScore = ScoreInfo;

    fn skip_word_chars(&mut self, count: core::num::NonZeroU32) {
        // Penalty if the first time we skip word chars, we haven't matched word chars first.
        if !self.word_chars_matched && !self.word_chars_skipped {
            self.penalty += SUBMATCH_NOT_FROM_START_PENALTY;
        }
        // Penalty for skipped chars in matched words.
        if self.word_chars_matched_since_last_delimiter {
            self.penalty += count.get() * CHAR_IN_MATCHED_WORD_SKIPPED_PENALTY;
        }
        self.word_chars_skipped = true;
    }

    fn match_word_char(&mut self) {
        self.word_chars_matched = true;
        self.word_chars_matched_since_last_delimiter = true;
    }

    fn match_delimiter(&mut self, _pattern: char, value: char) {
        self.word_chars_matched_since_last_delimiter = false;
        // Penalty for every matched `.`.
        if value == '.' {
            self.penalty += SUBMATCH_INCLUDES_NAMESPACE_PENALTY;
        }
    }

    fn skip_delimiter(&mut self, _pattern: Option<char>, value: char) {
        // Penalty for every skipped `.` after the first matched word character.
        if value == '.' && self.word_chars_matched {
            self.penalty += SUBMATCH_INCLUDES_NAMESPACE_PENALTY;
        }
        self.word_chars_matched_since_last_delimiter = false;
    }

    fn finish(self) -> Self::SubmatchScore {
        let Self { penalty, word_chars_skipped, .. } = self;
        ScoreInfo { penalty, word_chars_skipped }
    }
}

/// Score information for a submatch. Includes the accumulated penalty, and information needed
/// when merging a tree of submatch scores to produce a match score.
#[derive(Copy, Clone, Default, PartialEq, Eq)]
pub struct ScoreInfo {
    penalty:            u32,
    word_chars_skipped: bool,
}

impl PartialOrd for ScoreInfo {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl Ord for ScoreInfo {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&self.penalty, &other.penalty).reverse()
    }
}

impl score::SubmatchScore for ScoreInfo {
    type TargetInfo = TargetInfo;
    type MatchScore = Score;
    const ANY_PREFIX_MATCH_BEATS_ANY_INITIALS_MATCH: bool = true;

    fn match_score(self, target_info: TargetInfo) -> Score {
        let Self { mut penalty, word_chars_skipped } = self;
        if target_info.is_alias {
            penalty += MATCH_TARGET_IS_ALIAS_PENALTY;
        }
        if word_chars_skipped {
            penalty += MATCH_IS_IMPERFECT_PENALTY;
        }
        Score(u32::MAX - penalty)
    }

    fn with_submatch_by_initials_penalty(self) -> Self {
        Self {
            penalty:            self.penalty + SUBMATCH_BY_INITIALS_PENALTY,
            word_chars_skipped: true,
        }
    }
}

impl core::ops::Add for ScoreInfo {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        ScoreInfo {
            penalty:            self.penalty + rhs.penalty,
            word_chars_skipped: self.word_chars_skipped | rhs.word_chars_skipped,
        }
    }
}
