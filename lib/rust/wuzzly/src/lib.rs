#![feature(core_intrinsics)]
#![feature(generators)]
#![feature(iter_from_generator)]
#![feature(iter_advance_by)]
#![feature(let_chains)]
#![feature(type_changing_struct_update)]
#![feature(exclusive_range_pattern)]
#![feature(assert_matches)]


pub mod score;

pub use score::Score;

use derivative::Derivative;
use std::num::NonZeroU32;
use std::ops::Add;

use score::*;

mod std_score {
    use super::score;
    use super::Score;
    use std::cmp::Ordering;

    macro_rules! define_mixed_binary_radix_bases_impl {
        ($width_subtotal:expr, $ty:ty, ) => {
            const _STATIC_CHECK_ALL_BITS_FIT_IN_WORD: $ty = 1 << $width_subtotal;
        };
        ($width_subtotal:expr, $ty:ty, $ident:ident: $width:expr; $($rest:tt)*) => {
            const $ident: $ty = 1 << $width_subtotal;
            define_mixed_binary_radix_bases_impl!($width_subtotal + $width, $ty, $($rest)*);
        };
    }

    /// Define factors partitioning the given type arithmetically into bitfields of specified
    /// widths, starting from the little end of the word.
    macro_rules! define_mixed_binary_radix_bases_low_to_high {
        ($ty:ty, $($rest:tt)*) => {
            define_mixed_binary_radix_bases_impl!(0, $ty, $($rest)*);
        };
    }

    /// Per-match penalty. These are only applied when converting a root-submatch score to a match
    /// score, so each will not occur more than once and overflow is not possible.
    const MATCH_BITS: u32 = 1;
    /// A per-submatch penalty can occur up to 15 times in a match before it overflows.
    const SUBMATCH_BITS: u32 = 4;
    /// A per-character penalty can occur up to 255 times in a match before it overflows.
    const CHAR_BITS: u32 = 8;

    define_mixed_binary_radix_bases_low_to_high!(u32,
        CHAR_IN_MATCHED_WORD_SKIPPED_PENALTY: CHAR_BITS;
        SUBMATCH_INCLUDES_NAMESPACE_PENALTY: SUBMATCH_BITS;
        MATCH_TARGET_IS_ALIAS_PENALTY: MATCH_BITS;
        SUBMATCH_NOT_FROM_START_PENALTY: SUBMATCH_BITS;
        SUBMATCH_BY_INITIALS_PENALTY: SUBMATCH_BITS;
        MATCH_IS_IMPERFECT_PENALTY: MATCH_BITS;
    );

    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
    pub struct TargetInfo {
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
}



// ===============
// === Matcher ===
// ===============

#[derive(Default)]
struct Matcher<SB> {
    pattern:            String,
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

    fn do_match(&mut self, target: &str) -> Option<WithMatchIndexes<SB::SubmatchScore>> {
        /// Supports reusing *the storage* of a buffer, despite it being used for objects of
        /// different lifetimes.
        fn cast_lifetime<'a, 'b, T>(mut buffer: Vec<State<'a, T>>) -> Vec<State<'b, T>> {
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

    pub fn search(
        &mut self,
        pattern: &str,
        target: &str,
        target_info: <SB::SubmatchScore as SubmatchScore>::TargetInfo,
    ) -> Option<WithMatchIndexes<Score>> {
        let pattern: String = normalize_pattern(pattern.chars()).into_iter().collect();
        self.subsearch(&pattern, target)
            .map(|info| info.map(|score_info| score_info.match_score(target_info)))
    }
}

/// Normalize the search pattern. [`Matcher::search`] applies this to its arguments implicitly, but
/// it is exposed for testing.
fn normalize_pattern(pattern: impl Iterator<Item = char>) -> impl Iterator<Item = char> {
    // Collapse any sequence of delimiters to the strongest, in this ranking: `.` > `_` > ` `
    core::iter::from_generator(|| {
        let mut delimiter = None;
        for c in pattern {
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

/// Search for a pattern in a string using a temporary [`Matcher`]. When performing multiple
/// searches it is more efficient to reuse a [`Matcher`], but this is convenient for testing.
fn search(
    pattern: &str,
    target: &str,
    target_info: std_score::TargetInfo,
) -> Option<WithMatchIndexes<Score>> {
    Matcher::<std_score::ScoreBuilder>::default().search(pattern, target, target_info)
}

fn fmt_match(s: WithMatchIndexes<&str>) -> String {
    let chars = s.inner.chars();
    let mut bits = s.match_indexes;
    let chars: Vec<_> = chars
        .rev()
        .map(|c| match bits.pop().unwrap_or_default() {
            false => c,
            true => c.to_ascii_uppercase(),
        })
        .collect();
    chars.iter().rev().collect()
}


// === State ===

#[derive(Clone)]
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
        (self.chars.next() == Some(c)).then(|| self)
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
            word_boundaries.push(matches!(c, '_' | '.'));
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

#[derive(Default, Clone, Derivative)]
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
        self.match_indexes.push(true);
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
    type TargetInfo = S::TargetInfo;
    const ANY_PREFIX_MATCH_BEATS_ANY_INITIALS_MATCH: bool =
        S::ANY_PREFIX_MATCH_BEATS_ANY_INITIALS_MATCH;

    fn match_score(self, target_info: Self::TargetInfo) -> Score {
        self.inner.match_score(target_info)
    }

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

mod bitstring;
use bitstring::Bitstring;



// ===========================
// === Accept/Reject Tests ===
// ===========================

#[cfg(test)]
mod test_match {
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
                let result = search(pattern, &target, Default::default());
                let matched = result.is_some();
                let target = match result {
                    Some(result) => fmt_match(result.map(|_| target.as_str())),
                    None => target.to_owned(),
                };
                (*pattern, target, matched)
            })
            .collect();
        assert_eq!(&computed[..], &expected[..]);
    }

    fn check_normalization(cases: &[(&str, &str)]) {
        let normalized = cases.iter().map(|(_raw, normalized)| normalized);
        let raw = cases.iter().map(|(raw, _normalized)| raw);
        let computed: Vec<String> =
            raw.map(|raw| normalize_pattern(raw.chars()).collect()).collect();
        let expected: Vec<_> = normalized.map(|s| s.to_owned()).collect();
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
    fn test_normalization() {
        check_normalization(&[
            // === Any sequence of delimiters collapses to the strongest ===
            ("data  .  read  _  file", "data.read_file"),
            ("da re _fi", "da re_fi"),
            ("da     refi", "da refi"),
            ("data. re fi", "data.re fi"),
            ("data .refi", "data.refi"),
            // === Leading and trailing underscores and dots are preserved ===
            //("_data.read_file_", "_data.read_file_"),
            //(".data.read_file.", ".data.read_file."),
            // === Leading and trailing spaces are stripped ===
            //(" data.read_file ", "data.read_file"),
        ]);
    }
}



// =====================
// === Ranking Tests ===
// =====================

#[cfg(test)]
mod test_score {
    use super::*;

    /// Given a pattern and a set of targets, assert that the targets match and are in order by
    /// score from best match to worst.
    fn check_order(pattern: &str, inputs: &[(&str, std_score::TargetInfo)]) {
        let expected = &inputs[..];
        let mut computed: Vec<_> = inputs.iter().cloned().collect();
        computed
            .sort_by_key(|(target, target_info)| search(pattern, target, *target_info).unwrap());
        computed.reverse();
        assert_eq!(computed, expected);
    }

    #[test]
    fn test_order() {
        check_order("ab", &[
            // Exact match: Name
            ("ab", std_score::TargetInfo { is_alias: false }),
            // Exact match: Alias
            ("ab", std_score::TargetInfo { is_alias: true }),
            // Prefix match, first-word: Name
            ("abx", std_score::TargetInfo { is_alias: false }),
            // Prefix match, first-word: Name (Lower % of word used)
            ("abxx", std_score::TargetInfo { is_alias: false }),
            // Prefix match, first-word: Type (Exact match)
            ("ab.x", std_score::TargetInfo { is_alias: false }),
            // Prefix match, first-word: Type
            ("abx.x", std_score::TargetInfo { is_alias: false }),
            // Prefix match, first-word: Type (Lower % of word used)
            ("abxx.x", std_score::TargetInfo { is_alias: false }),
            // Prefix match, first-word: Alias
            ("abx", std_score::TargetInfo { is_alias: true }),
            // Prefix match, first-word: Alias (Lower % of word used)
            ("abxx", std_score::TargetInfo { is_alias: true }),
            // Prefix match, later-word: Name
            ("x_ab", std_score::TargetInfo { is_alias: false }),
            // Prefix match, later-word: Name (Lower % of word used)
            ("x_abx", std_score::TargetInfo { is_alias: false }),
            // Prefix match, later-word: Type
            ("x_ab.x", std_score::TargetInfo { is_alias: false }),
            // Prefix match, later-word: Type (Lower % of word used)
            ("x_abx.x", std_score::TargetInfo { is_alias: false }),
            // Prefix match, later-word: Alias
            ("x_ab", std_score::TargetInfo { is_alias: true }),
            // Prefix match, later-word: Alias (Lower % of word used)
            ("x_abx", std_score::TargetInfo { is_alias: true }),
            // Initials match: Name
            ("ax_bx", std_score::TargetInfo { is_alias: false }),
            // Initials match: Type
            ("ax.bx", std_score::TargetInfo { is_alias: false }),
            // Initials match: Alias
            ("ax_bx", std_score::TargetInfo { is_alias: true }),
        ])
    }
}
