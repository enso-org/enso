//! Fuzzy-matching library.

#![recursion_limit = "256"]
// === Features ===
#![feature(core_intrinsics)]
#![feature(generators)]
#![feature(iter_from_generator)]
#![feature(iter_advance_by)]
#![feature(let_chains)]
#![feature(type_changing_struct_update)]
#![feature(exclusive_range_pattern)]
#![feature(assert_matches)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


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
    pub fn to_byte_ranges(
        self,
        target: &str,
    ) -> impl '_ + Iterator<Item = enso_text::Range<enso_text::Byte>> {
        debug_assert_eq!(target.char_indices().count(), self.indexes.len() as usize);
        core::iter::from_generator(move || {
            let mut indexes = self.indexes;
            let mut previously_in_match = false;
            let mut match_start = 0;
            let mut match_end = target.len();
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
pub struct Matcher<SB> {
    /// The normalized pattern for the current match.
    pattern:            String,
    /// The normalized target for the current match.
    target:             String,
    /// Empty `Vec` used to reuse storage.
    states_buffer:      Vec<State<'static, WithMatchIndexes<SB>>>,
    /// Empty `Vec` used to reuse storage.
    next_states_buffer: Vec<State<'static, WithMatchIndexes<SB>>>,
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

    /// Shared match implementation supporting both by-prefix and by-initials matching.
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
        let mut next_states = cast_lifetime(core::mem::take(&mut self.states_buffer));
        states.push(initial_state);
        for c in self.pattern.chars() {
            match c {
                '_' | ' ' | '.' =>
                    for state in states.drain(..) {
                        next_states.extend(state.match_delimiter(c));
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

    fn subsearch(
        &mut self,
        pattern: &str,
        target: &str,
    ) -> Option<WithMatchIndexes<SB::SubmatchScore>> {
        if let Some((r#type, name)) = pattern.rsplit_once('.') {
            // Try to match both parts, then compute a composite score.
            let (r#type2, name2) = target.rsplit_once('.')?;
            let r#type = self.subsearch(r#type, r#type2)?;
            let name = self.subsearch(name, name2)?;
            let dot = {
                let mut builder = WithMatchIndexes::<SB>::default();
                builder.match_delimiter('.', '.');
                builder.finish()
            };
            Some(r#type + dot + name)
        } else {
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
            let matched_initials = try_initials_match
                .then_some(())
                .and_then(|()| self.match_initials(pattern, target));
            core::cmp::max(matched_prefix, matched_initials)
        }
    }

    /// Determine whether the given pattern matches the given target. If so, uses the matching
    /// characteristics along with the given target information to produce a score representing the
    /// quality of the match; returns this score along with information about what characters of the
    /// target were matched by the pattern.
    pub fn search(&mut self, pattern: &str, target: &str) -> Option<Match<SB::SubmatchScore>> {
        let pattern: String = normalize_pattern(pattern).collect();
        let root_submatch = self.subsearch(&pattern, target)?;
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
    chars: WordIndexedChars<'s>,
    score: SB,
}

impl<'s, SB: ScoreBuilder + 's> State<'s, SB> {
    fn new(target: &'s str) -> Self {
        Self { chars: WordIndexedChars::new(target), score: Default::default() }
    }

    /// Produce all possible resulting states resulting from applying specified delimiter to the
    /// current state.
    fn match_delimiter(mut self, c: char) -> impl Iterator<Item = State<'s, SB>> {
        core::iter::from_generator(move || {
            while let Ok(skipped) = self.chars.advance_until_at_word_boundary() {
                self.score.skip_word_chars_if_any(skipped);
                let Some(d) = self.chars.next() else { break };
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
        (self.chars.next() == Some(c)).then_some(self)
    }

    /// Skip any remaining characters, and return the resulting score.
    fn into_score(mut self) -> SB::SubmatchScore {
        // Skip up to and including each remaining delimiter.
        while let Ok(skipped) = self.chars.advance_until_at_word_boundary() {
            self.score.skip_word_chars_if_any(skipped);
            let Some(d) = self.chars.next() else { break };
            self.score.skip_delimiter(None, d);
        }
        // Skip after the last delimiter.
        let trailing_word_chars = self.chars.count() as u32;
        self.score.skip_word_chars_if_any(trailing_word_chars);
        // Return the score.
        self.score.finish()
    }
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
    fn map<U>(self, f: impl FnOnce(T) -> U) -> WithMatchIndexes<U> {
        WithMatchIndexes { inner: f(self.inner), ..self }
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

    fn finish(self) -> Self::SubmatchScore {
        WithMatchIndexes { inner: self.inner.finish(), ..self }
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

    /// Search for a pattern in a string using a temporary [`Matcher`]. When performing multiple
    /// searches it is more efficient to reuse a [`Matcher`], but this is convenient for testing.
    pub(crate) fn search(pattern: &str, target: &str) -> Option<MatchIndexes> {
        Some(Matcher::<ScoreForgetter>::default().search(pattern, target)?.match_indexes)
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
        fn finish(self) -> Self::SubmatchScore {
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
}
