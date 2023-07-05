#![feature(generators)]
#![feature(iter_from_generator)]
#![feature(iter_advance_by)]
#![feature(let_chains)]
#![feature(drain_filter)]


#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Score(pub u32);

mod std_score {
    use super::Score;

    const MAX_CHARS_LN2: u32 = 8;
    const MAX_SUBMATCHES_LN2: u32 = 4;

    const SUBMATCH_BY_INITIALS_PENALTY: u32 =
        1 << MAX_CHARS_LN2 << MAX_SUBMATCHES_LN2 << 1 << MAX_SUBMATCHES_LN2;
    const SUBMATCH_NOT_FROM_START_PENALTY: u32 = 1 << MAX_CHARS_LN2 << MAX_SUBMATCHES_LN2 << 1;
    const TARGET_IS_ALIAS_PENALTY: u32 = 1 << MAX_CHARS_LN2 << MAX_SUBMATCHES_LN2;
    const SUBMATCH_INCLUDES_NAMESPACE_PENALTY: u32 = 1 << MAX_CHARS_LN2;
    const CHAR_IN_MATCHED_WORD_SKIPPED_PENALTY: u32 = 1;

    pub struct TargetInfo {
        is_alias: bool,
    }

    /// Observes how characters are matched within a submatch.
    #[derive(Clone, Default)]
    pub struct ScoreBuilder {
        // State maintained to determine how further characters affect penalty.
        word_chars_matched: bool,
        word_chars_matched_since_last_delimiter: bool,
        // The penalty accrued so far.
        penalty: u32,
    }

    impl ScoreBuilder {
        pub fn skip_word_chars(&mut self, count: u32) {
            if !self.word_chars_matched {
                self.penalty += SUBMATCH_NOT_FROM_START_PENALTY;
            }
            if self.word_chars_matched_since_last_delimiter {
                self.penalty += count * CHAR_IN_MATCHED_WORD_SKIPPED_PENALTY;
            }
        }

        pub fn match_word_char(&mut self) {
            self.word_chars_matched = true;
            self.word_chars_matched_since_last_delimiter = true;
        }

        pub fn match_delimiter(&mut self, _pattern: char, value: char) {
            self.word_chars_matched_since_last_delimiter = false;
            if value == '.' {
                self.penalty += SUBMATCH_INCLUDES_NAMESPACE_PENALTY;
            }
        }

        pub fn skip_delimiter(&mut self, _pattern: char, _value: char) {
            // It is not necessary to update `word_chars_matched_since_last_delimiter` here because
            // when a delimiter is skipped, word chars will not be matched until a delimiter is
            // matched.
        }

        pub fn finish(self) -> ScoreInfo {
            let Self { penalty, .. } = self;
            ScoreInfo { penalty }
        }
    }

    #[derive(Copy, Clone, Default)]
    pub struct ScoreInfo {
        penalty: u32,
    }

    impl ScoreInfo {
        pub fn with_submatch_by_initials_penalty(self) -> Self {
            Self { penalty: self.penalty + SUBMATCH_BY_INITIALS_PENALTY }
        }

        pub fn any_prefix_match_beats_any_initials_match() -> bool {
            true
        }
    }

    impl core::ops::Add for ScoreInfo {
        type Output = Self;
        fn add(self, rhs: Self) -> Self::Output {
            ScoreInfo { penalty: self.penalty + rhs.penalty }
        }
    }

    impl From<ScoreInfo> for Score {
        fn from(value: ScoreInfo) -> Self {
            Score(u32::MAX - value.penalty)
        }
    }

    impl From<&ScoreInfo> for Score {
        fn from(value: &ScoreInfo) -> Self {
            Score::from(*value)
        }
    }
}
use std_score::*;

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

fn match_initials(needle: &str, haystack: &str) -> Option<ScoreInfo> {
    let mut exploded = String::with_capacity(haystack.len() * 2 - 1);
    let mut needle = needle.chars();
    if let Some(c) = needle.next() {
        exploded.push(c);
    }
    for c in needle {
        exploded.push(' ');
        exploded.push(c);
    }
    Some(match_prefixes(&exploded, haystack)?.with_submatch_by_initials_penalty())
}

#[derive(Clone)]
struct State<'s> {
    chars: WordIndexedChars<'s>,
    score: ScoreBuilder,
}

fn match_prefixes(needle: &str, haystack: &str) -> Option<ScoreInfo> {
    let needle = format!(" {needle}");
    let haystack = format!("_{haystack}");
    let initial_state =
        State { chars: WordIndexedChars::new(&haystack), score: Default::default() };
    let mut states = vec![initial_state];
    let mut next_states = vec![];
    for c in needle.chars() {
        match c {
            '_' | ' ' | '.' => {
                for mut state in states.drain(..) {
                    while let Ok(skipped) = state.chars.advance_until_at_word_boundary() {
                        state.score.skip_word_chars(skipped);
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
    states.into_iter().map(|state| state.score.finish()).max_by_key(|info| Score::from(info))
}

fn search(needle: &str, haystack: &str) -> Option<Score> {
    search_(needle, haystack).map(|info| Score::from(info))
}

fn search_(needle: &str, haystack: &str) -> Option<ScoreInfo> {
    if let Some((r#type, name)) = needle.rsplit_once('.') {
        // Try to match both parts, then compute a composite score.
        let (r#type2, name2) = haystack.rsplit_once('.')?;
        let r#type = search_(r#type, r#type2)?;
        let name = search_(name, name2)?;
        let dot = {
            let mut builder = ScoreBuilder::default();
            builder.match_delimiter('.', '.');
            builder.finish()
        };
        Some(r#type + dot + name)
    } else {
        // Try to match either by prefix, or by initials; return the best score.
        let matched_prefix = match_prefixes(needle, haystack);
        let try_initials_match = {
            let no_delimiters_in_needle = !needle.contains('_') && !needle.contains(' ');
            // Optimization: The scoring criteria may guarantee that an initials-match would never
            // be preferred to a prefix match.
            let best_match_could_be_initials =
                !ScoreInfo::any_prefix_match_beats_any_initials_match() || matched_prefix.is_none();
            no_delimiters_in_needle && best_match_could_be_initials
        };
        let matched_initials =
            try_initials_match.then_some(()).and_then(|()| match_initials(needle, haystack));
        core::cmp::max_by_key(matched_prefix, matched_initials, |info| info.map(Score::from))
    }
}

#[cfg(test)]
mod test_match {
    use super::*;

    fn check_matches<'a, 'b>(cases: impl IntoIterator<Item = (&'a str, &'b str, bool)>) {
        let expected: Vec<_> = cases.into_iter().collect();
        let computed: Vec<_> = expected
            .iter()
            .map(|&(needle, haystack, _)| {
                let normalized_needle: String = normalize_needle(needle.chars()).collect();
                (needle, haystack, search(&normalized_needle, haystack).is_some())
            })
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
        ]);
    }

    #[test]
    fn test_matches_with_dots() {
        check_matches([
            // === Dot occurs at end of word and matches exactly ===
            ("data.read", "data.read_file21", true),
            ("data.re", "data.read_file21", true),
            // === Dot follows prefix ===
            ("da.re", "data.read_file21", true),
            ("da.fi", "data.read_file21", true),
            // === Dot follows initials ===
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
            ("da rf", "data.read_file21", false),
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

#[cfg(test)]
mod test_score {
    use super::*;

    fn check_order(needle: &str, haystacks: &[&str]) {
        let expected = haystacks;
        let mut computed: Vec<_> = haystacks.iter().cloned().collect();
        computed.sort_by_key(|haystack| search(needle, haystack).unwrap());
        computed.reverse();
        assert_eq!(computed, expected);
    }

    #[test]
    fn test_order() {
        check_order("sc", &["scaolubracorucb", "select_columns"])
    }
}
