#![feature(generators)]
#![feature(iter_from_generator)]
#![feature(iter_advance_by)]
#![feature(let_chains)]
#![feature(drain_filter)]


/*
#[derive(Clone)]
struct ScoreBuilder {}

impl ScoreBuilder {
    fn new(haystack: &str) -> Self {
        Self {}
    }

    fn advance(&mut self, c: char, c1: char, consume: bool) {}

    fn finish(self) -> Score {
        Score(0)
    }
}
 */

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Score(usize);

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

    #[must_use]
    fn advance_until_at_word_boundary(&mut self) -> bool {
        let n = self.word_boundaries.trailing_zeros();
        match n {
            64 => false,
            valid_shift => self.advance_by(valid_shift as usize).is_ok(),
        }
    }

    #[must_use]
    fn advance_to_next_word_start(&mut self) -> bool {
        self.advance_until_at_word_boundary() && self.next().is_some()
    }

    #[must_use]
    fn next_word_start(&self) -> Option<Self> {
        let mut this = self.clone();
        this.advance_to_next_word_start().then_some(this)
    }

    #[must_use]
    fn advance_while_at_word_boundary(&mut self) -> bool {
        let n = self.word_boundaries.trailing_ones();
        match n {
            64 => false,
            valid_shift => self.advance_by(valid_shift as usize).is_ok(),
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

fn match_initials(needle: &str, haystack: &str) -> bool {
    let mut exploded = String::with_capacity(haystack.len() * 2 - 1);
    let mut needle = needle.chars();
    if let Some(c) = needle.next() {
        exploded.push(c);
    }
    for c in needle {
        exploded.push(' ');
        exploded.push(c);
    }
    match_prefixes(&exploded, haystack)
}

// a_a_a ~ a_a_b_a_a_a
// (a_a_a) ~ a_a_b_a_a_a
// (a_a_a,_a_a) ~ a_a_b_a_a_a

fn match_prefixes(needle: &str, haystack: &str) -> bool {
    let needle = format!(" {needle}");
    let haystack = format!("_{haystack}");
    let mut states = vec![WordIndexedChars::new(&haystack)];
    let mut next_states = vec![];
    for c in needle.chars() {
        let states_dbg: Vec<_> = states.iter().map(|state| format!("{state:?}")).collect();
        match c {
            '_' | ' ' | '.' => {
                for mut state in states.drain(..) {
                    while state.advance_until_at_word_boundary() {
                        let Some(d) = state.next() else { break };
                        if c == '_' && d == '.' {
                            break
                        }
                        if c == ' ' || c == d {
                            next_states.push(state.clone());
                        }
                    }
                }
                core::mem::swap(&mut states, &mut next_states);
            }
            c => {
                states.drain_filter(|state| state.next() != Some(c));
            }
        }
    }
    !states.is_empty()
}

fn search(needle: &str, haystack: &str) -> Option<Score> {
    if let Some((r#type, name)) = needle.rsplit_once('.') {
        if let Some((r#type2, name2)) = haystack.rsplit_once('.') {
            (search(r#type, r#type2).is_some() && search(name, name2).is_some()).then_some(Score(0))
        } else {
            None
        }
    } else {
        let matched_prefix = match_prefixes(needle, haystack);
        let matched_initials =
            !needle.contains('_') && !needle.contains(' ') && match_initials(needle, haystack);
        (matched_prefix || matched_initials).then_some(Score(0))
    }
}

#[cfg(test)]
mod test {
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
