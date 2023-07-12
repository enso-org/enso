


// =========================
// === Score Builder API ===
// =========================

/// Interface for accumulating a score for a match by observing character-level match and non-match
/// events.
pub trait ScoreBuilder: Default + Clone + Sized {
    /// The result of observing all matching events for a submatch.
    type SubmatchScore: SubmatchScore;
    /// Adjust the score information as appropriate for the specified number of word characters
    /// in the target being skipped without matching anything in the pattern.
    fn skip_word_chars(&mut self, count: core::num::NonZeroU32);
    /// Adjust the score information as appropriate for a word character matching exactly and being
    /// consumed.
    fn match_word_char(&mut self);
    /// Adjust the score information as appropriate for the given delimiter being consumed by the
    /// given pattern character.
    fn match_delimiter(&mut self, pattern: char, value: char);
    /// Adjust the score information as appropriate for the given delimiter in the target being
    /// skipped while seeking a match for the given pattern character. The pattern character will be
    /// `None` if the delimiter is being skipped because the full pattern completed matching earlier
    /// in the target.
    fn skip_delimiter(&mut self, pattern: Option<char>, value: char);
    /// Return the score information for this submatch.
    fn finish(self) -> Self::SubmatchScore;


    // === Helpers for API consumers ===

    /// Helper for skipping 0 or more characters. See [`skip_word_chars`].
    fn skip_word_chars_if_any(&mut self, count: u32) {
        if let Some(count) = core::num::NonZeroU32::new(count) {
            self.skip_word_chars(count);
        }
    }
}


// === Submatch Score ===

/// Score information for a submatch. Must enable comparing quality of submatches, computing
/// parent submatch scores by merging submatches, and calculating a match score from the root of a
/// submatch tree.
pub trait SubmatchScore: core::ops::Add<Self, Output = Self> + Sized + Ord {
    /// Information about a target that is used to adjust the score for a match. When there are
    /// external factors that should be stronger than some match factors but weaker than others,
    /// they can be incorporated into the score calculation.
    type TargetInfo;
    /// The type representing the overall quality of a match.
    type MatchScore;
    /// If this is [`true`], it enables an optimization in the [`Matcher`].
    const ANY_PREFIX_MATCH_BEATS_ANY_INITIALS_MATCH: bool;
    /// Use the specified [`TargetInfo`] to compute the match score for a match with this submatch
    /// as its root.
    fn match_score(self, target_info: Self::TargetInfo) -> Self::MatchScore;
    /// Adjust the score as appropriate for when the submatch is performed by initials (rather than
    /// as a prefix).
    fn with_submatch_by_initials_penalty(self) -> Self;
}
