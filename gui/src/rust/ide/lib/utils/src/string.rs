//! This module provides utils for strings.

use enso_prelude::*;



// ===============================
// === Common Pre- and Postfix ===
// ===============================

/// Return the length of the longest common prefix of the two strings. If they are completely
/// different this will be zero.
///
/// Example:
/// ```
/// # use utils::string::common_prefix_length;
/// let a = "🐁hospital";
/// let b = "🐁host";
/// let c = "🐇bunny🐇";
///
/// assert_eq!(common_prefix_length(a, b), 4);
/// assert_eq!(common_prefix_length(a, c), 0);
/// assert_eq!(common_prefix_length(a, a), 9);
/// ```
pub fn common_prefix_length(source_a: &str, source_b: &str) -> usize {
    let shortest = source_a.chars().count().min(source_b.chars().count());
    let chars_a = source_a.chars();
    let chars_b = source_b.chars();
    let mut zipped = chars_a.zip(chars_b);
    let mismatch = zipped.find_position(|(a, b)| *a != *b);
    mismatch.map(|(ix, _)| ix).unwrap_or(shortest)
}

/// Return the length of the longest common postfix of the two strings. If they are completely
/// different this will be zero.
///
/// Example:
/// ```
/// # use utils::string::common_postfix_length;
/// let a = "sunny🐇yard";
/// let b = "🐇yard";
/// let c = "🐇";
///
/// assert_eq!(common_postfix_length(a, b), 5);
/// assert_eq!(common_postfix_length(a, c), 0);
/// assert_eq!(common_postfix_length(a, a), 10);
/// ```
pub fn common_postfix_length(source_a: &str, source_b: &str) -> usize {
    let shortest = source_a.chars().count().min(source_b.chars().count());
    let chars_a = source_a.chars().rev();
    let chars_b = source_b.chars().rev();
    let mut zipped = chars_a.zip(chars_b);
    let mismatch = zipped.find_position(|(a, b)| *a != *b);
    mismatch.map(|(ix, _)| ix).unwrap_or(shortest)
}
