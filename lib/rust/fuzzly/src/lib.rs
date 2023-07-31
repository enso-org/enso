//! Fuzzy-matching library.
//!
//! Usage:
//! - Define a scoring algorithm, implementing [`score::ScoreBuilder`].
//! - Instantiate a matcher for the chosen scorer.
//! - Call [`Matcher::search`] to get results about whether the pattern matched the target, and if
//!   so: how well, according to the given scorer; which characters from the target were used in the
//!   match.
//! - The matcher may be reused for multiple matches, as a performance optimization. It retains no
//!   logical state between searches, but reuses buffers, which is much more efficient when
//!   performing many matches.

#![recursion_limit = "256"]
// === Features ===
#![feature(assert_matches)]
#![feature(core_intrinsics)]
#![feature(exclusive_range_pattern)]
#![feature(generators)]
#![feature(iter_advance_by)]
#![feature(iter_from_generator)]
#![feature(let_chains)]
#![feature(test)]
#![feature(type_changing_struct_update)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]


// ==============
// === Export ===
// ==============

pub mod score;



mod bitstring;
use bitstring::Bitstring;

use derivative::Derivative;
use std::num::NonZeroU32;
use std::ops::Add;

use score::*;



// ====================================
// === Result of a successful match ===
// ====================================

/// Result of a successful match of a pattern against a string.
#[derive(Debug, Clone, Derivative)]
#[derivative(PartialEq, Eq, PartialOrd, Ord)]
pub struct Match<Score> {
    /// Value that can be used to compare match quality.
    pub score:         Score,
    /// Identifies which characters in the string were matched against characters from the pattern.
    #[derivative(PartialEq = "ignore", PartialOrd = "ignore", Ord = "ignore")]
    pub match_indexes: MatchIndexes,
}

impl<Score> Match<Score> {
    /// Apply the given function to the score.
    pub fn map_score<Score2>(self, f: impl FnOnce(Score) -> Score2) -> Match<Score2> {
        let score = f(self.score);
        Match { score, ..self }
    }

    /// Remove matched-index information.
    pub fn without_match_indexes(self) -> Self {
        let match_indexes = Default::default();
        Self { match_indexes, ..self }
    }
}



// =================================
// === Matched character indexes ===
// =================================

/// Identifies the characters within the target of a match that were matched against characters in
/// the pattern.
#[derive(Debug, Clone, Default)]
pub struct MatchIndexes {
    indexes: Bitstring,
}

impl MatchIndexes {
    /// Produce a sequence of byte ranges corresponding to the substrings of matched characters, in
    /// unspecified order.
    ///
    /// The target should start with the same `target` that was passed to [`Matcher::search`] to
    /// produce this result. Any extra suffix will be ignored.
    pub fn to_byte_ranges(
        self,
        target: &str,
    ) -> impl '_ + Iterator<Item = enso_text::Range<enso_text::Byte>> {
        let target_len = target.len();
        let end = target
            .char_indices()
            .nth(self.indexes.len() as usize)
            .map(|(i, _)| i)
            .unwrap_or(target_len);
        let target = &target[..std::cmp::min(end, target_len)];
        core::iter::from_generator(move || {
            let mut indexes = self.indexes;
            let mut previously_in_match = false;
            let mut match_start = 0;
            let mut match_end = target_len;
            for (i, _) in target.char_indices().rev() {
                let now_in_match = indexes.pop().unwrap_or_default();
                if !now_in_match {
                    if previously_in_match {
                        let start = enso_text::Byte(match_start);
                        let end = enso_text::Byte(match_end);
                        yield enso_text::Range::new(start, end);
                    }
                    match_end = i;
                }
                previously_in_match = now_in_match;
                match_start = i;
            }
            if previously_in_match {
                let start = enso_text::Byte(0);
                let end = enso_text::Byte(match_end);
                yield enso_text::Range::new(start, end);
            }
        })
    }

    /// Produce a sequence of byte ranges corresponding to the substrings of matched characters, in
    /// unspecified order.
    pub fn byte_ranges<'t>(
        &self,
        target: &'t str,
    ) -> impl 't + Iterator<Item = enso_text::Range<enso_text::Byte>> {
        self.clone().to_byte_ranges(target)
    }
}



// ===============
// === Matcher ===
// ===============

/// NFA that computes match scores. Note that this type semantically has no persistent state, but
/// reusing one object for many matches is much more efficient than performing each match
/// independently, because the buffers will be reused.
#[derive(Debug, Default)]
pub struct Matcher<SB: ScoreBuilder> {
    /// The normalized pattern for the current match. This is used during a single call to
    /// [`search`].
    ///
    /// Its value is constructed during [`match_initials`] or [`match_prefixes`] to perform either
    /// a by-initials match or by-prefix match.
    pattern:            String,
    /// The normalized target for the current match. This is only used within [`do_match`], and is
    /// cleared between uses. It is persisted to reuse the buffer.
    ///
    /// It is the target, case-normalized, with `_` prepended to treat the beginning of the string
    /// as a word boundary.
    target:             String,
    /// Empty `Vec` used to reuse storage.
    states_buffer:      Vec<State<'static, WithMatchIndexes<SB>>>,
    /// Empty `Vec` used to reuse storage.
    next_states_buffer: Vec<State<'static, WithMatchIndexes<SB>>>,
    best_score:         Vec<Candidate<SB::SubmatchScore>>,
}

impl<SB: ScoreBuilder> Matcher<SB> {
    fn match_initials(
        &mut self,
        pattern: &str,
        target: &str,
    ) -> Option<WithMatchIndexes<SB::SubmatchScore>> {
        self.reset_pattern();
        let mut chars = pattern.chars();
        if let Some(c) = chars.next() {
            self.pattern.push(c);
        }
        for c in chars {
            self.pattern.push(' ');
            self.pattern.push(c);
        }
        Some(self.do_match(target)?.with_submatch_by_initials_penalty())
    }

    fn match_prefixes(
        &mut self,
        pattern: &str,
        target: &str,
    ) -> Option<WithMatchIndexes<SB::SubmatchScore>> {
        use core::fmt::Write;
        self.reset_pattern();
        self.pattern.write_str(pattern).unwrap();
        self.do_match(target)
    }

    /// Reset the pattern buffer so that it is ready to append a pattern to.
    fn reset_pattern(&mut self) {
        self.pattern.clear();
        // All patterns start with the any-delimiter character so that the match can start at the
        // beginning of any word in the target.
        self.pattern.push(' ');
    }

    /// Searches for [`self.pattern`] in the target string. Before calling this, [`self.pattern`]
    /// should be constructed by: Starting with a [` `] character, so that the pattern will begin
    /// matching at any delimiter (this is done in [`reset_pattern`]; Appending the case-normalized
    /// pattern, which should consist of delimiter characters (`.`, `_`, and ` `) and word
    /// characters (anything which is not a delimiter).
    fn do_match(&mut self, target: &str) -> Option<WithMatchIndexes<SB::SubmatchScore>> {
        /// Supports reusing *the storage* of a buffer, despite it being used for objects of
        /// different lifetimes.
        fn cast_lifetime<'a, 'b, T>(mut buffer: Vec<State<'a, T>>) -> Vec<State<'b, T>> {
            #[allow(unsafe_code)] // See comments in block.
            unsafe {
                // Ensure the `Vec` is empty.
                buffer.clear();
                // An empty `Vec` owns 0 objects, but may have storage allocated.
                // - 0 objects of lifetime 'a are the same as 0 objects of lifetime 'b.
                // - Storage for <'a> is valid storage for <'b> because lifetimes don't affect
                //   memory layout.
                core::mem::transmute::<Vec<State<'a, T>>, Vec<State<'b, T>>>(buffer)
            }
        }
        self.target.clear();
        self.target.push('_'); // Treat the beginning of the string as a word boundary.
        self.target.extend(target.chars().map(|c| c.to_ascii_lowercase()));
        let initial_state = State::new(&self.target);
        let mut states = cast_lifetime(core::mem::take(&mut self.states_buffer));
        let mut next_states = cast_lifetime(core::mem::take(&mut self.next_states_buffer));
        states.push(initial_state);
        self.best_score.clear();
        self.best_score.resize(target.len() + 1, Default::default());
        for (i, c) in self.pattern.char_indices() {
            let generation = i + 1;
            match c {
                '_' | ' ' | '.' =>
                    for state in states.drain(..) {
                        for new_state in state.match_delimiter(c) {
                            let new_score = new_state.score.finish().without_match_indexes();
                            let target_pos = new_state.target_suffix_remaining.len() as usize;
                            let score_entry = &mut self.best_score[target_pos];
                            if score_entry.generation == generation {
                                if score_entry.score < new_score {
                                    next_states[score_entry.index] = new_state;
                                    score_entry.score = new_score;
                                }
                            } else {
                                score_entry.generation = generation;
                                score_entry.index = next_states.len();
                                score_entry.score = new_score;
                                next_states.push(new_state);
                            }
                        }
                    },
                c =>
                    for state in states.drain(..) {
                        next_states.extend(state.match_word_character(c));
                    },
            }
            core::mem::swap(&mut states, &mut next_states);
        }
        let mut best = states.drain(..).map(|state| state.into_score()).max();
        // Strip the initial delimiter we prepended to the pattern and target.
        if let Some(best) = best.as_mut() {
            best.match_indexes.drop_front();
        }
        next_states.clear();
        self.states_buffer = cast_lifetime(states);
        self.next_states_buffer = cast_lifetime(next_states);
        best
    }

    /// Search for a pattern that may contain `.` characters.
    fn subsearch_compound(
        &mut self,
        pattern: &str,
        target: &str,
    ) -> Option<WithMatchIndexes<SB::SubmatchScore>> {
        if let Some((r#type, name)) = pattern.rsplit_once('.') {
            // Try to match both parts, then compute a composite score.
            let (r#type2, name2) = target.rsplit_once('.')?;
            let r#type = self.subsearch_compound(r#type, r#type2)?;
            let name = self.subsearch_simple(name, name2)?;
            let dot = {
                let mut builder = WithMatchIndexes::<SB>::default();
                builder.match_delimiter('.', '.');
                builder.finish()
            };
            Some(r#type + dot + name)
        } else {
            self.subsearch_simple(pattern, target)
        }
    }

    /// Search for a pattern that does not contain `.` characters.
    fn subsearch_simple(
        &mut self,
        pattern: &str,
        target: &str,
    ) -> Option<WithMatchIndexes<SB::SubmatchScore>> {
        // Try to match either by prefix, or by initials; return the best score.
        let matched_prefix = self.match_prefixes(pattern, target);
        let try_initials_match = {
            let no_delimiters_in_pattern = !pattern.contains('_') && !pattern.contains(' ');
            // Optimization: The scoring criteria may guarantee that an initials-match would
            // never be preferred to a prefix match.
            let best_match_could_be_initials =
                !SB::SubmatchScore::ANY_PREFIX_MATCH_BEATS_ANY_INITIALS_MATCH
                    || matched_prefix.is_none();
            no_delimiters_in_pattern && best_match_could_be_initials
        };
        let matched_initials =
            try_initials_match.then_some(()).and_then(|()| self.match_initials(pattern, target));
        core::cmp::max(matched_prefix, matched_initials)
    }

    /// Determine whether the given pattern matches the given target. If so, uses the matching
    /// characteristics along with the given target information to produce a score representing the
    /// quality of the match; returns this score along with information about what characters of the
    /// target were matched by the pattern.
    pub fn search(&mut self, pattern: &str, target: &str) -> Option<Match<SB::SubmatchScore>> {
        let pattern: String = normalize_pattern(pattern).collect();
        let root_submatch = self.subsearch_compound(&pattern, target)?;
        let WithMatchIndexes { inner: score, match_indexes } = root_submatch;
        let match_indexes = MatchIndexes { indexes: match_indexes };
        Some(Match { score, match_indexes })
    }
}

/// Normalize the search pattern. [`Matcher::search`] applies this to its arguments implicitly, but
/// it is exposed for testing.
///
/// This is not a static method of [`Matcher`] because it is independent of [`Matcher`]'s generic
/// parameterization.
fn normalize_pattern(pattern: &str) -> impl Iterator<Item = char> + '_ {
    // Strip leading and trailing whitespace.
    let pattern = pattern.trim();
    // Collapse any sequence of delimiters to the strongest, in this ranking: `.` > `_` > ` `
    core::iter::from_generator(|| {
        let mut delimiter = None;
        for c in pattern.chars() {
            match c {
                '.' => {
                    delimiter = Some('.');
                }
                '_' =>
                    if delimiter != Some('.') {
                        delimiter = Some('_');
                    },
                ' ' =>
                    if delimiter.is_none() {
                        delimiter = Some(' ');
                    },
                _ => {
                    if let Some(delimiter) = delimiter.take() {
                        yield delimiter;
                    }
                    yield c.to_ascii_lowercase()
                }
            }
        }
        if let Some(delimiter) = delimiter.take() {
            yield delimiter;
        }
    })
}


// === State ===

#[derive(Debug, Clone)]
struct State<'s, SB> {
    target_suffix_remaining: WordIndexedChars<'s>,
    score:                   SB,
}

impl<'s, SB: ScoreBuilder + 's> State<'s, SB> {
    fn new(target: &'s str) -> Self {
        Self {
            target_suffix_remaining: WordIndexedChars::new(target),
            score:                   Default::default(),
        }
    }

    /// Produce all possible resulting states resulting from applying specified delimiter to the
    /// current state.
    fn match_delimiter(mut self, c: char) -> impl Iterator<Item = State<'s, SB>> {
        core::iter::from_generator(move || {
            while let Ok(skipped) = self.target_suffix_remaining.advance_until_at_word_boundary() {
                self.score.skip_word_chars_if_any(skipped);
                let Some(d) = self.target_suffix_remaining.next() else { break };
                if c == '_' && d == '.' {
                    break;
                }
                if c == ' ' || c == d {
                    let mut matched_state = self.clone();
                    matched_state.score.match_delimiter(c, d);
                    yield matched_state;
                }
                self.score.skip_delimiter(Some(c), d);
            }
        })
    }

    /// Try to apply the given non-delimiter character to the state.
    fn match_word_character(mut self, c: char) -> Option<Self> {
        self.score.match_word_char();
        (self.target_suffix_remaining.next() == Some(c)).then_some(self)
    }

    /// Skip any remaining characters, and return the resulting score.
    fn into_score(mut self) -> SB::SubmatchScore {
        // Skip up to and including each remaining delimiter.
        while let Ok(skipped) = self.target_suffix_remaining.advance_until_at_word_boundary() {
            self.score.skip_word_chars_if_any(skipped);
            let Some(d) = self.target_suffix_remaining.next() else { break };
            self.score.skip_delimiter(None, d);
        }
        // Skip after the last delimiter.
        let trailing_word_chars = self.target_suffix_remaining.count() as u32;
        self.score.skip_word_chars_if_any(trailing_word_chars);
        // Return the score.
        self.score.finish()
    }
}


// === Candidate ===

/// Information used to ensure that:
/// - At any position in the pattern, there is at most one [`State`] for each possible position in
///   the target.
/// - When there is more than one possible [`State`] for a position in the target, the one with the
///   best score is kept.
#[derive(Debug, Default, Clone)]
struct Candidate<S> {
    /// A unique value for each position in the pattern; used to entries that were inserted during
    /// previous positions in the pattern.
    generation: usize,
    /// Position in `next_states` vector of best value seen so far for this position in the target.
    index:      usize,
    /// Best value seen for this position in the target.
    score:      S,
}


// === Expression Indexing ===

#[derive(Debug, Clone)]
struct WordIndexedChars<'s> {
    chars:           core::str::Chars<'s>,
    word_boundaries: Bitstring,
}

impl<'s> WordIndexedChars<'s> {
    fn new(pattern: &'s str) -> Self {
        let mut word_boundaries = Bitstring::new();
        for c in pattern.chars().rev() {
            word_boundaries.push(matches!(c, '_' | '.' | ' ' | '(' | ')'));
        }
        let chars = pattern.chars();
        Self { word_boundaries, chars }
    }

    /// Succeeds if there is any word boundary; returns the number of characters positions advanced.
    fn advance_until_at_word_boundary(&mut self) -> Result<u32, ()> {
        let n = self.word_boundaries.pop_trailing_zeros();
        match self.word_boundaries.is_empty() {
            false => {
                self.chars.advance_by(n as usize).unwrap();
                Ok(n)
            }
            true => Err(()),
        }
    }

    /// Returns the number of characters remaining.
    fn len(&self) -> u32 {
        self.word_boundaries.len()
    }
}

impl<'s> Iterator for WordIndexedChars<'s> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.word_boundaries.pop();
        self.chars.next()
    }
}


// === Matched character indexes ===

#[derive(Debug, Default, Clone, Derivative)]
#[derivative(PartialEq, Eq, PartialOrd, Ord)]
struct WithMatchIndexes<T> {
    inner:         T,
    #[derivative(PartialEq = "ignore", PartialOrd = "ignore", Ord = "ignore")]
    match_indexes: Bitstring,
}

impl<T> WithMatchIndexes<T> {
    fn without_match_indexes(self) -> T {
        self.inner
    }
}

impl<SB: ScoreBuilder> ScoreBuilder for WithMatchIndexes<SB> {
    type SubmatchScore = WithMatchIndexes<SB::SubmatchScore>;

    fn skip_word_chars(&mut self, count: NonZeroU32) {
        self.inner.skip_word_chars(count);
        self.match_indexes.push_zeros(count);
    }

    fn match_word_char(&mut self) {
        self.inner.match_word_char();
        self.match_indexes.push(true);
    }

    fn match_delimiter(&mut self, pattern: char, value: char) {
        self.inner.match_delimiter(pattern, value);
        let is_literal_match = pattern == value;
        self.match_indexes.push(is_literal_match);
    }

    fn skip_delimiter(&mut self, pattern: Option<char>, value: char) {
        self.inner.skip_delimiter(pattern, value);
        self.match_indexes.push(false);
    }

    fn finish(&self) -> Self::SubmatchScore {
        WithMatchIndexes {
            inner:         self.inner.finish(),
            match_indexes: self.match_indexes.clone(),
        }
    }
}

impl<S: SubmatchScore> SubmatchScore for WithMatchIndexes<S> {
    const ANY_PREFIX_MATCH_BEATS_ANY_INITIALS_MATCH: bool =
        S::ANY_PREFIX_MATCH_BEATS_ANY_INITIALS_MATCH;

    fn with_submatch_by_initials_penalty(self) -> Self {
        Self { inner: self.inner.with_submatch_by_initials_penalty(), ..self }
    }
}

impl<T: Add<Output = T>> Add<Self> for WithMatchIndexes<T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            inner:         self.inner + rhs.inner,
            match_indexes: self.match_indexes + rhs.match_indexes,
        }
    }
}



// ====================
// === Test Support ===
// ====================

#[cfg(test)]
mod test_utils {
    use super::*;

    /// Search for a pattern in a string.
    pub(crate) fn search(pattern: &str, target: &str) -> Option<MatchIndexes> {
        thread_local! {
            static MATCHER: std::cell::RefCell<Matcher<ScoreForgetter>> = Default::default();
        }
        MATCHER.with(|matcher| {
            matcher.borrow_mut().search(pattern, target).map(|result| result.match_indexes)
        })
    }

    pub(crate) fn fmt_match(indexes: MatchIndexes, target: &str) -> String {
        let chars = target.chars();
        let mut bits = indexes.indexes;
        let chars: Vec<_> = chars
            .rev()
            .map(|c| match bits.pop().unwrap_or_default() {
                false => c,
                true => c.to_ascii_uppercase(),
            })
            .collect();
        chars.iter().rev().collect()
    }

    /// Simple score builder for testing match results without considering order.
    #[derive(Debug, Default, Clone)]
    struct ScoreForgetter;

    impl ScoreBuilder for ScoreForgetter {
        type SubmatchScore = Zero;
        fn skip_word_chars(&mut self, _count: NonZeroU32) {}
        fn match_word_char(&mut self) {}
        fn match_delimiter(&mut self, _pattern: char, _value: char) {}
        fn skip_delimiter(&mut self, _pattern: Option<char>, _value: char) {}
        fn finish(&self) -> Self::SubmatchScore {
            Zero
        }
    }

    #[derive(Debug, Default, Clone, PartialOrd, Ord, PartialEq, Eq)]
    struct Zero;

    impl SubmatchScore for Zero {
        const ANY_PREFIX_MATCH_BEATS_ANY_INITIALS_MATCH: bool = true;
        fn with_submatch_by_initials_penalty(self) -> Self {
            self
        }
    }

    impl Add for Zero {
        type Output = Self;
        fn add(self, _rhs: Self) -> Self::Output {
            self
        }
    }
}



// ===========================
// === Accept/Reject Tests ===
// ===========================

#[cfg(test)]
mod test_match {
    use super::test_utils::*;
    use super::*;

    /// Given a pattern, a target, and a bool:
    /// - Check that the bool indicates whether the pattern matches the target.
    /// - If the pattern matches the target, check that the matched characters in the target are the
    ///   capitalized characters. (The capitalization will be ignored during matching due to
    ///   normalization; it is only used to as a channel for this expected result information.)
    fn check_matches<'a, 'b>(cases: impl IntoIterator<Item = (&'a str, &'b str, bool)>) {
        let expected: Vec<_> = cases.into_iter().map(|(p, t, m)| (p, t.to_owned(), m)).collect();
        let computed: Vec<_> = expected
            .iter()
            .map(|(pattern, target, _)| {
                let result = search(pattern, target);
                let matched = result.is_some();
                let target = match result {
                    Some(match_indexes) => fmt_match(match_indexes, target.as_str()),
                    None => target.to_owned(),
                };
                (*pattern, target, matched)
            })
            .collect();
        assert_eq!(&computed[..], &expected[..]);
    }

    #[test]
    fn test_matches_initials() {
        check_matches([
            // === Sequences of initials of words ===
            ("drf", "Data.Read_File21", true),
            ("dr", "Data.Read_file21", true),
            ("df", "Data.read_File21", true),
            ("rf", "data.Read_File21", true),
            ("d", "Data.read_file21", true),
            ("r", "data.Read_file21", true),
            ("f", "data.read_File21", true),
            // === Character not present ===
            ("dxf", "data.read_file21", false),
            // === Characters not present in this order ===
            ("fr", "data.read_file21", false),
            // === Not all initials ===
            ("darf", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_spaces() {
        check_matches([
            // === Sequences of prefixes of words, separated by spaces ===
            ("data r f", "DATA.Read_File21", true),
            ("da re", "DAta.REad_file21", true),
            ("re fi", "data.REad_FIle21", true),
            ("da fi", "DAta.read_FIle21", true),
            // === Not prefixes ===
            ("ead file", "data.read_file21", false),
            ("da ile", "data.read_file21", false),
            ("da rf", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_underscores() {
        check_matches([
            // === Sequence of word-prefixes matching a sequence of underscore-joined words ===
            ("read_file", "data.READ_FILE21", true),
            ("re_fi", "data.REad_FIle21", true),
            ("a_c", "A_b_C", true),
            // === Underscore does not match `.` ===
            ("data_re", "data.read_file21", false),
            // === Underscored prefixes do not match across `.` ===
            ("da_re", "data.read_file21", false),
            ("da_fi", "data.read_file21", false),
            ("data_file", "data.read_file21", false),
            // === No initials-matching is performed when underscores are present ===
            ("a_bc", "aa_bb_cc", false),
        ]);
    }

    #[test]
    fn test_matches_with_dots() {
        check_matches([
            // === Dot occurs at end of word and matches exactly ===
            ("data.read", "DATA.READ_file21", true),
            ("data.re", "DATA.REad_file21", true),
            // === Dot at ends of input matches ===
            ("data.", "DATA.read_file21", true),
            (".rea", "data.REAd_file21", true),
            // === Match prefix on one side and initials on the other ===
            ("da.rf", "DAta.Read_File21", true),
            ("da.fi", "DAta.read_FIle21", true),
            ("fb.re", "Foo_Bar.REad_file21", true),
            ("fb.fi", "Foo_Bar.read_FIle21", true),
            // === Dot does not match underscore ===
            ("read.file", "data.read_file21", false),
            ("re.fi", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_mixed_delimiters() {
        check_matches([
            // === Test combining rules of `.`/`_`/` `/concatenation ===
            ("data.read_file", "DATA.READ_FILE21", true),
            ("d r_f", "Data.Read_File21", true),
            ("da re_fi", "DAta.REad_FIle21", true),
            ("data.r f", "DATA.Read_File21", true),
            ("data.rf", "DATA.Read_File21", true),
            ("data.file", "DATA.read_FILE21", true),
            ("da.read_file", "DAta.READ_FILE21", true),
            ("data.ead_ile", "data.read_file21", false),
            ("data.refi", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_numbers() {
        check_matches([
            // === Numbers match when exactly present ===
            ("data.read_file21", "DATA.READ_FILE21", true),
            // === Number prefixes match ===
            ("data.read_file2", "DATA.READ_FILE21", true),
            ("data file2", "DATA.read_FILE21", true),
            // === Prefix-match doesn't treat a number as a word ===
            ("data 2", "data.read_file21", false),
            // === Initials-match doesn't treat a number as a word ===
            ("drf2", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_spaces_and_parentheses() {
        check_matches([
            // === Match against word found in parentheses ===
            ("fbq", "Foo_Bar (Quux)", true),
            ("fo_ba qu", "FOo_BAr (QUux)", true),
            // === Match when pattern includes parentheses ===
            ("fo_ba (", "FOo_BAr (quux)", true),
            ("fo_ba (qu", "FOo_BAr (QUux)", true),
            ("fo_ba (quux)", "FOo_BAr (QUUX)", true),
        ])
    }

    fn check_normalization(cases: &[(&str, &str)]) {
        let normalized = cases.iter().map(|(_raw, normalized)| normalized);
        let raw = cases.iter().map(|(raw, _normalized)| raw);
        let computed: Vec<String> = raw.map(|raw| normalize_pattern(raw).collect()).collect();
        let expected: Vec<_> = normalized.map(|s| s.to_owned()).collect();
        assert_eq!(&computed[..], &expected[..]);
    }

    #[test]
    fn test_pattern_normalization() {
        check_normalization(&[
            // === Any sequence of delimiters collapses to the strongest ===
            ("data  .  read  _  file", "data.read_file"),
            ("da re _fi", "da re_fi"),
            ("da     refi", "da refi"),
            ("data. re fi", "data.re fi"),
            ("data .refi", "data.refi"),
            // === Leading and trailing underscores and dots are preserved ===
            ("_data.read_file_", "_data.read_file_"),
            (".data.read_file.", ".data.read_file."),
            // === Leading and trailing spaces are stripped ===
            (" data.read_file ", "data.read_file"),
        ]);
    }
}



// =========================
// === Match index tests ===
// =========================

#[cfg(test)]
mod test_match_indexes {
    use super::test_utils::*;

    /// Strips parentheses from `target` and matches `pattern` against it. Asserts that when
    /// `MatchIndexes::to_byte_ranges` is applied to the result, the byte ranges output correspond
    /// to the parenthesized parts of the target.
    fn check_match_ranges(pattern: &str, target: &str) {
        let mut target_stripped = String::new();
        let mut expected = vec![];
        let mut start = None;
        for c in target.chars() {
            match c {
                '(' => start = Some(target_stripped.len()),
                ')' => {
                    let start = enso_text::Byte(start.unwrap());
                    let end = enso_text::Byte(target_stripped.len());
                    expected.push(enso_text::Range::new(start, end));
                }
                c => target_stripped.push(c),
            }
        }
        let result = search(pattern, &target_stripped).unwrap();
        let mut byte_ranges: Vec<_> = result.to_byte_ranges(&target_stripped).collect();
        byte_ranges.sort_by_key(|range| range.start);
        assert_eq!(byte_ranges, expected);
    }

    #[test]
    fn test_byte_ranges_simple() {
        check_match_ranges("abc", "(a)_(b)_(c)");
        check_match_ranges("abc", "xyz_(abc)_xyz");
        check_match_ranges("abc", "xyz_(abc)_xyz");
    }

    #[test]
    fn test_byte_ranges_unicode() {
        check_match_ranges("a🦄b🦄c🦄", "(a)_(🦄)_(b)_(🦄)_(c)_(🦄)");
        check_match_ranges("abc🦄", "xyz_(abc🦄)_xyz");
        check_match_ranges("🦄abc", "xyz_(🦄abc)_xyz");
        check_match_ranges("a🦄c", "xyz_(a🦄c)_xyz");
    }

    #[test]
    fn test_byte_ranges_pattern_normalized() {
        check_match_ranges("a . b . c", "(a.b.c)");
    }

    #[test]
    fn test_byte_ranges_ignores_extra_suffix() {
        let result = search("ab", "abc").unwrap();
        let byte_ranges_for_original_input: Vec<_> = result.byte_ranges("abc").collect();
        let byte_ranges_for_suffixed_input: Vec<_> = result.byte_ranges("abcd").collect();
        assert_eq!(byte_ranges_for_suffixed_input, byte_ranges_for_original_input);
    }
}



// ==================
// === Benchmarks ===
// ==================

#[cfg(test)]
mod bench {
    extern crate test;

    use super::*;
    use test::Bencher;

    #[bench]
    fn bench_a_a_a_a_a_a(b: &mut Bencher) {
        // This input demonstrates the worst-case quadratic time complexity. For a pathological
        // input of this length, it runs at ~1ms/iter. (Memory usage is `O(pattern + target)`).
        let pattern = "a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a";
        let target = "a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a\
            a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a_a";
        b.iter(|| test_utils::search(pattern, target).unwrap());
    }
}
