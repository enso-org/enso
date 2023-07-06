#![feature(generators)]
#![feature(iter_from_generator)]
#![feature(iter_advance_by)]
#![feature(let_chains)]
#![feature(drain_filter)]


#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Score(pub u32);

pub trait ScoreBuilder: Default + Clone + Sized {
    type SubmatchScore: SubmatchScore;
    fn skip_word_chars(&mut self, count: u32);
    fn match_word_char(&mut self);
    fn match_delimiter(&mut self, pattern: char, value: char);
    fn skip_delimiter(&mut self, pattern: char, value: char);
    fn finish(self) -> Self::SubmatchScore;
}

pub trait SubmatchScore: core::ops::Add<Self, Output = Self> + Sized + Ord {
    type TargetInfo;
    fn with_submatch_by_initials_penalty(self) -> Self;
    fn any_prefix_match_beats_any_initials_match() -> bool;
    fn target_score(self, target_info: Self::TargetInfo) -> Score;
}

mod std_score {
    use std::cmp::Ordering;
    use super::Score;

    const MAX_CHARS_LN2: u32 = 8;
    const MAX_SUBMATCHES_LN2: u32 = 4;

    const SUBMATCH_BY_INITIALS_PENALTY: u32 =
        1 << (MAX_CHARS_LN2 + MAX_SUBMATCHES_LN2 + 1 + MAX_SUBMATCHES_LN2);
    const SUBMATCH_NOT_FROM_START_PENALTY: u32 = 1 << (MAX_CHARS_LN2 + MAX_SUBMATCHES_LN2 + 1);
    const TARGET_IS_ALIAS_PENALTY: u32 = 1 << (MAX_CHARS_LN2 + MAX_SUBMATCHES_LN2);
    const SUBMATCH_INCLUDES_NAMESPACE_PENALTY: u32 = 1 << MAX_CHARS_LN2;
    const CHAR_IN_MATCHED_WORD_SKIPPED_PENALTY: u32 = 1;

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

    impl super::ScoreBuilder for ScoreBuilder {
        type SubmatchScore = ScoreInfo;

        fn skip_word_chars(&mut self, count: u32) {
            // Penalty if the first time we skip word chars, we haven't matched word chars first.
            if !self.word_chars_matched && !self.word_chars_skipped {
                self.penalty += SUBMATCH_NOT_FROM_START_PENALTY;
            }
            // Penalty for skipped chars in matched words.
            if self.word_chars_matched_since_last_delimiter {
                self.penalty += count * CHAR_IN_MATCHED_WORD_SKIPPED_PENALTY;
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

        fn skip_delimiter(&mut self, _pattern: char, value: char) {
            self.word_chars_matched_since_last_delimiter = false;
            // Penalty for every skipped `.` after the first matched word character.
            if value == '.' && self.word_chars_matched {
                self.penalty += SUBMATCH_INCLUDES_NAMESPACE_PENALTY;
            }
        }

        fn finish(self) -> Self::SubmatchScore {
            let Self { penalty, .. } = self;
            ScoreInfo { penalty }
        }
    }

    #[derive(Copy, Clone, Default, PartialEq, Eq)]
    pub struct ScoreInfo {
        penalty: u32,
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

    impl super::SubmatchScore for ScoreInfo {
        type TargetInfo = TargetInfo;

        fn with_submatch_by_initials_penalty(self) -> Self {
            Self { penalty: self.penalty + SUBMATCH_BY_INITIALS_PENALTY }
        }

        fn any_prefix_match_beats_any_initials_match() -> bool {
            true
        }

        fn target_score(self, target_info: TargetInfo) -> Score {
            let mut penalty = self.penalty;
            if target_info.is_alias {
                penalty += TARGET_IS_ALIAS_PENALTY;
            }
            Score(u32::MAX - penalty)
        }
    }

    impl core::ops::Add for ScoreInfo {
        type Output = Self;
        fn add(self, rhs: Self) -> Self::Output {
            ScoreInfo { penalty: self.penalty + rhs.penalty }
        }
    }
}



// ===============
// === Matcher ===
// ===============

#[derive(Default)]
struct Matcher<SB> {
    needle:             String,
    haystack:           String,
    /// Empty `Vec` used to reuse storage.
    states_buffer:      Vec<State<'static, SB>>,
    /// Empty `Vec` used to reuse storage.
    next_states_buffer: Vec<State<'static, SB>>,
}

impl<SB: ScoreBuilder> Matcher<SB>
{
    fn match_initials(&mut self, needle: &str, haystack: &str) -> Option<SB::SubmatchScore> {
        self.reset_needle();
        let mut chars = needle.chars();
        if let Some(c) = chars.next() {
            self.needle.push(c);
        }
        for c in chars {
            self.needle.push(' ');
            self.needle.push(c);
        }
        Some(self.do_match(haystack)?.with_submatch_by_initials_penalty())
    }

    fn match_prefixes(&mut self, needle: &str, haystack: &str) -> Option<SB::SubmatchScore> {
        use core::fmt::Write;
        self.reset_needle();
        self.needle.write_str(needle).unwrap();
        self.do_match(haystack)
    }

    fn reset_needle(&mut self) {
        self.needle.clear();
        self.needle.push(' ');
    }

    fn do_match(&mut self, haystack: &str) -> Option<SB::SubmatchScore> {
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
        use core::fmt::Write;
        self.haystack.clear();
        self.haystack.push('_');
        self.haystack.write_str(haystack).unwrap();
        let initial_state =
            State { chars: WordIndexedChars::new(&self.haystack), score: Default::default() };
        let mut states = cast_lifetime(core::mem::take(&mut self.states_buffer));
        let mut next_states = cast_lifetime(core::mem::take(&mut self.states_buffer));
        states.push(initial_state);
        for c in self.needle.chars() {
            match c {
                '_' | ' ' | '.' => {
                    for mut state in states.drain(..) {
                        while let Ok(skipped) = state.chars.advance_until_at_word_boundary() {
                            if skipped != 0 {
                                state.score.skip_word_chars(skipped);
                            }
                            let Some(d) = state.chars.next() else { break };
                            if c == '_' && d == '.' {
                                break;
                            }
                            if c == ' ' || c == d {
                                let mut matched_state = state.clone();
                                matched_state.score.match_delimiter(c, d);
                                next_states.push(matched_state);
                            }
                            state.score.skip_delimiter(c, d);
                        }
                    }
                    core::mem::swap(&mut states, &mut next_states);
                }
                c => {
                    states.drain_filter(|state| {
                        state.score.match_word_char();
                        state.chars.next() != Some(c)
                    });
                }
            }
        }
        let best = states
            .drain(..)
            .map(|mut state| {
                while let Ok(skipped) = state.chars.advance_until_at_word_boundary() {
                    if skipped != 0 {
                        state.score.skip_word_chars(skipped);
                    }
                    let Some(d) = state.chars.next() else { break };
                    state.score.skip_delimiter('$', d);
                }
                let trailing_word_chars = state.chars.count() as u32;
                if trailing_word_chars != 0 {
                    state.score.skip_word_chars(trailing_word_chars);
                }
                state.score.finish()
            })
            .max();
        next_states.clear();
        self.states_buffer = cast_lifetime(states);
        self.next_states_buffer = cast_lifetime(next_states);
        best
    }

    fn subsearch(&mut self, needle: &str, haystack: &str) -> Option<SB::SubmatchScore> {
        if let Some((r#type, name)) = needle.rsplit_once('.') {
            // Try to match both parts, then compute a composite score.
            let (r#type2, name2) = haystack.rsplit_once('.')?;
            let r#type = self.subsearch(r#type, r#type2)?;
            let name = self.subsearch(name, name2)?;
            let dot = {
                let mut builder = SB::default();
                builder.match_delimiter('.', '.');
                builder.finish()
            };
            Some(r#type + dot + name)
        } else {
            // Try to match either by prefix, or by initials; return the best score.
            let matched_prefix = self.match_prefixes(needle, haystack);
            let try_initials_match = {
                let no_delimiters_in_needle = !needle.contains('_') && !needle.contains(' ');
                // Optimization: The scoring criteria may guarantee that an initials-match would
                // never be preferred to a prefix match.
                let best_match_could_be_initials =
                    !SB::SubmatchScore::any_prefix_match_beats_any_initials_match()
                        || matched_prefix.is_none();
                no_delimiters_in_needle && best_match_could_be_initials
            };
            let matched_initials = try_initials_match
                .then_some(())
                .and_then(|()| self.match_initials(needle, haystack));
            core::cmp::max(matched_prefix, matched_initials)
        }
    }

    pub fn search(&mut self, needle: &str, haystack: &str, target_info: <SB::SubmatchScore as SubmatchScore>::TargetInfo) -> Option<Score> {
        let needle: String = normalize_needle(needle.chars()).into_iter().collect();
        self.subsearch(&needle, haystack).map(|info| info.target_score(target_info))
    }
}

/// Normalize the search pattern. [`Matcher::search`] applies this to its arguments implicitly, but
/// it is exposed for testing.
fn normalize_needle(needle: impl Iterator<Item = char>) -> impl Iterator<Item = char> {
    // Collapse any sequence of delimiters to the strongest, in this ranking: `.` > `_` > ` `
    core::iter::from_generator(|| {
        let mut delimiter = None;
        for c in needle {
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
                    yield c
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
fn search(needle: &str, haystack: &str, target_info: std_score::TargetInfo) -> Option<Score> {
    Matcher::<std_score::ScoreBuilder>::default().search(needle, haystack, target_info)
}


// === State ===

#[derive(Clone)]
struct State<'s, SB> {
    chars: WordIndexedChars<'s>,
    score: SB,
}


// === Expression Indexing ===

#[derive(Debug, Clone)]
struct WordIndexedChars<'s> {
    chars:           core::str::Chars<'s>,
    word_boundaries: u64,
}

impl<'s> WordIndexedChars<'s> {
    fn new(pattern: &'s str) -> Self {
        assert!(pattern.len() <= 64); // FIXME
        let mut word_boundaries = 0;
        for c in pattern.chars().rev() {
            word_boundaries <<= 1;
            word_boundaries |= matches!(c, '_' | '.') as u64;
        }
        let chars = pattern.chars();
        Self { word_boundaries, chars }
    }

    /// Succeeds if there is any word boundary; returns the number of characters positions advanced.
    fn advance_until_at_word_boundary(&mut self) -> Result<u32, ()> {
        let n = self.word_boundaries.trailing_zeros();
        match n {
            64 => Err(()),
            valid_shift => self.advance_by(valid_shift as usize).map(|()| n).map_err(|_| ()),
        }
    }
}

impl<'s> Iterator for WordIndexedChars<'s> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.word_boundaries >>= 1;
        self.chars.next()
    }

    fn advance_by(&mut self, n: usize) -> Result<(), usize> {
        self.word_boundaries >>= n;
        self.chars.advance_by(n)
    }
}



// ===========================
// === Accept/Reject Tests ===
// ===========================

#[cfg(test)]
mod test_match {
    use super::*;

    fn check_matches<'a, 'b>(cases: impl IntoIterator<Item = (&'a str, &'b str, bool)>) {
        let expected: Vec<_> = cases.into_iter().collect();
        let computed: Vec<_> = expected
            .iter()
            .map(|&(needle, haystack, _)| (needle, haystack, search(needle, haystack, Default::default()).is_some()))
            .collect();
        assert_eq!(&computed[..], &expected[..]);
    }

    fn check_normalization(cases: &[(&str, &str)]) {
        let normalized = cases.iter().map(|(_raw, normalized)| normalized);
        let raw = cases.iter().map(|(raw, _normalized)| raw);
        let computed: Vec<String> =
            raw.map(|raw| normalize_needle(raw.chars()).collect()).collect();
        let expected: Vec<_> = normalized.map(|s| s.to_owned()).collect();
        assert_eq!(&computed[..], &expected[..]);
    }

    #[test]
    fn test_matches_initials() {
        check_matches([
            // === Sequences of initials of words ===
            ("drf", "data.read_file21", true),
            ("dr", "data.read_file21", true),
            ("df", "data.read_file21", true),
            ("rf", "data.read_file21", true),
            ("d", "data.read_file21", true),
            ("r", "data.read_file21", true),
            ("f", "data.read_file21", true),
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
            ("data r f", "data.read_file21", true),
            ("da re", "data.read_file21", true),
            ("re fi", "data.read_file21", true),
            ("da fi", "data.read_file21", true),
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
            ("read_file", "data.read_file21", true),
            ("re_fi", "data.read_file21", true),
            ("a_c", "a_b_c", true),
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
            ("data.read", "data.read_file21", true),
            ("data.re", "data.read_file21", true),
            // === Dot at ends of input matches ===
            ("data.", "data.read_file21", true),
            (".rea", "data.read_file21", true),
            // === Match prefix on one side and initials on the other ===
            ("da.re", "data.read_file21", true),
            ("da.fi", "data.read_file21", true),
            ("fb.re", "foo_bar.read_file21", true),
            ("fb.fi", "foo_bar.read_file21", true),
            // === Dot does not match underscore ===
            ("read.file", "data.read_file21", false),
            ("re.fi", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_mixed_delimiters() {
        check_matches([
            // === Test combining rules of `.`/`_`/` `/concatenation ===
            ("data.read_file", "data.read_file21", true),
            ("d r_f", "data.read_file21", true),
            ("da re_fi", "data.read_file21", true),
            ("data.r f", "data.read_file21", true),
            ("data.rf", "data.read_file21", true),
            ("data.file", "data.read_file21", true),
            ("da.read_file", "data.read_file21", true),
            ("data.ead_ile", "data.read_file21", false),
            ("data.refi", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_numbers() {
        check_matches([
            // === Numbers match when exactly present ===
            ("data.read_file21", "data.read_file21", true),
            // === Number prefixes match ===
            ("data.read_file2", "data.read_file21", true),
            ("data file2", "data.read_file21", true),
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
    fn check_order(needle: &str, inputs: &[(&str, std_score::TargetInfo)]) {
        let expected = &inputs[..];
        let mut computed: Vec<_> = inputs.iter().cloned().collect();
        computed.sort_by_key(|(haystack, target)| search(needle, haystack, *target).unwrap());
        computed.reverse();
        assert_eq!(computed, expected);
    }

    #[test]
    fn test_order() {
        check_order("ab", &[
            // Prefix match, first-word: Name (Exact match)
            ("ab", std_score::TargetInfo { is_alias: false }),
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

            // Prefix match, first-word: Alias (Exact match)
            ("ab", std_score::TargetInfo { is_alias: true }),
            // Prefix match, first-word: Alias
            ("abc", std_score::TargetInfo { is_alias: true }),
            // Prefix match, first-word: Alias (Lower % of word used)
            ("abcc", std_score::TargetInfo { is_alias: true }),

            // Prefix match, later-word: Name
            ("z_ab", std_score::TargetInfo { is_alias: false }),
            // Prefix match, later-word: Name (Lower % of word used)
            ("z_abc", std_score::TargetInfo { is_alias: false }),

            // Prefix match, later-word: Type
            ("z_ab.x", std_score::TargetInfo { is_alias: false }),
            // Prefix match, later-word: Type (Lower % of word used)
            ("z_abc.x", std_score::TargetInfo { is_alias: false }),

            // Prefix match, later-word: Alias
            ("z_ab", std_score::TargetInfo { is_alias: true }),
            // Prefix match, later-word: Alias (Lower % of word used)
            ("z_abc", std_score::TargetInfo { is_alias: true }),

            // Initials match: Name
            ("ax_bx", std_score::TargetInfo { is_alias: false }),
            // Initials match: Type
            ("ax.bx", std_score::TargetInfo { is_alias: false }),
            // Initials match: Alias
            ("ax_bx", std_score::TargetInfo { is_alias: true }),
        ])
    }
}
