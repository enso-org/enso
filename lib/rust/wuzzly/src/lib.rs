#![feature(generators)]
#![feature(iter_from_generator)]
#![feature(iter_advance_by)]


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

fn normalize_haystack(haystack: impl Iterator<Item = char>) -> impl Iterator<Item = char> {
    // Insert [`INVISISPACE`] before any sequence of digits.
    core::iter::from_generator(|| {
        let mut subsequent_digit_breaks_word = false;
        for c in haystack {
            if subsequent_digit_breaks_word && matches!(c, '0'..='9') {
                yield INVISISPACE;
            }
            subsequent_digit_breaks_word = !matches!(c, '0'..='9' | '_' | '.');
            yield c;
        }
    })
}

fn step(c: char, c1: char) -> bool {
    todo!()
}

#[derive(Clone)]
struct HaystackIter<'s> {
    chars:           core::str::Chars<'s>,
    word_boundaries: u64,
}

const INVISISPACE: char = '\u{200B}';

impl<'s> HaystackIter<'s> {
    fn new(pattern: &'s str) -> Self {
        assert!(pattern.len() <= 64); // FIXME
        let mut word_boundaries = 0;
        for c in pattern.chars().rev() {
            word_boundaries <<= 1;
            word_boundaries |= matches!(c, '\u{200B}' | '_' | '.') as u64;
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
    fn advance_while_at_word_boundary(&mut self) -> bool {
        let n = self.word_boundaries.trailing_ones();
        match n {
            64 => false,
            valid_shift => self.advance_by(valid_shift as usize).is_ok(),
        }
    }
}

impl<'s> Iterator for HaystackIter<'s> {
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

fn search(needle: &str, haystack: &str) -> Option<Score> {
    let haystack: String = normalize_haystack(haystack.chars()).collect();
    let haystack = HaystackIter::new(&haystack);
    let mut best_match = None;
    let mut partial_matches = vec![(needle.chars(), haystack)];
    while let Some((mut needle, mut haystack)) = partial_matches.pop() {
        match needle.next() {
            // Advance to each character following a word boundary.
            Some(' ') =>
                while haystack.advance_until_at_word_boundary() {
                    haystack.next().unwrap();
                    partial_matches.push((needle.clone(), haystack.clone()));
                },
            // Advance to the position after each following `_` without an intervening `.`.
            Some('_') => {
                while haystack.advance_until_at_word_boundary() {
                    match haystack.next().unwrap() {
                        // Stronger delimiter: End search.
                        '.' => break,
                        '_' => {
                            partial_matches.push((needle.clone(), haystack.clone()));
                        }
                        // Weaker delimiter: Not a match, but doesn't end search.
                        '\u{200B}' => (),
                        c1 => unreachable!("should be a word boundary: {c1}"),
                    }
                }
            }
            // Advance to position after `.` at current position.
            Some('.') => if haystack.next() == Some('.') {
                partial_matches.push((needle.clone(), haystack.clone()));
            },
            // Match c at current position; Also advance to each c following a word boundary.
            Some(c) =>
                while haystack.advance_while_at_word_boundary() {
                    match haystack.next() {
                        Some(c1) =>
                            if c1 == c {
                                partial_matches.push((needle.clone(), haystack.clone()));
                            },
                        None => break,
                    }
                    if !haystack.advance_until_at_word_boundary() {
                        break;
                    }
                },
            None => {
                // Every character in the needle was located in the haystack!
                best_match = Some(Score(0));
            }
        }
    }
    best_match
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
    fn test_matches_undelimited() {
        check_matches([
            // === Sequences of prefixes of words, concatenated ===
            ("darefi", "data.read_file21", true),
            ("datare", "data.read_file21", true),
            ("dare", "data.read_file21", true),
            ("refi", "data.read_file21", true),
            ("dafi", "data.read_file21", true),
            // === Character not present ===
            ("daxfi", "data.read_file21", false),
            // === Characters not present in this order ===
            ("lrd", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_spaces() {
        check_matches([
            // === Sequences of prefixes of words, separated by spaces ===
            ("data re", "data.read_file21", true),
            ("da re", "data.read_file21", true),
            ("re fi", "data.read_file21", true),
            ("da fi", "data.read_file21", true),
            ("da fi ", "data.read_file21", true),
            // === Sequences of prefixes of words: some concatenated, some spaced ===
            ("da refi", "data.read_file21", true),
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
            // === Dot must not follow prefix ===
            ("da.re", "data.read_file21", false),
            ("da.fi", "data.read_file21", false),
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
            ("da re_fi", "data.read_file21", true),
            ("da refi", "data.read_file21", true),
            ("data.re fi", "data.read_file21", true),
            ("data.refi", "data.read_file21", true),
            ("data.file", "data.read_file21", true),
            ("da.read_file", "data.read_file21", false),
            ("data.ead_ile", "data.read_file21", false),
        ]);
    }

    #[test]
    fn test_matches_with_numbers() {
        check_matches([
            // === Numbers match when exactly present ===
            ("data.read_file21", "data.read_file21", true),
            // === The first digit in a number is a word boundary ===
            ("darefi21", "data.read_file21", true),
            ("fi 21", "data.read_file21", true),
            // === Number prefixes match ===
            ("data.read_file2", "data.read_file21", true),
            ("darefi2", "data.read_file21", true),
            // === Digits in input may be prefixes of different numbers ===
            ("a12", "a19.b29", true),
            // === Digits that aren't first in a number aren't word boundaries ===
            ("data.read_file1", "data.read_file21", false),
            // === Non-matches with following digits are non-matches ===
            ("darexfi21", "data.read_file21", false),
            ("daredfi21", "data.read_file21", false),
            // === Explicit word boundary characters don't match implicit word boundary ===
            ("fi.21", "data.read_file21", false),
            ("fi_21", "data.read_file21", false),
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
        ]);
    }
}
