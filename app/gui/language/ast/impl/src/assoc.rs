//! Rules for describing operator associativity.
//!
//! NOTE: They should be kept in sync with enso's implementation at:
//! `enso/Syntax/definition/src/main/scala/org/enso/syntax/text/ast/opr/Assoc.scala`


use crate::prelude::*;

use lazy_static::lazy_static;
use regex::Regex;



/// Operator associativity.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Assoc {
    Left,
    Right,
}

/// Checks if given operator identifier can be considered "applicative".
/// Examples are: `<*>`, `<*`, `<$>`.
pub fn is_applicative(operator: &str) -> bool {
    // We want to cache Regex, as library authors recommend, because compiling it is expensive.
    lazy_static! {
        // Unwrap is safe, as the input is fixed and covered by tests.
        static ref PATTERN:Regex = Regex::new("^<?[+*$]>?$").unwrap();
    }
    PATTERN.is_match(operator)
}

/// Character's "weight" when calculating associativity. Negative value means
/// weighing towards right-associativity, positive - towards left-associativity.
pub fn char_assoc(c: char) -> i32 {
    match c {
        '=' => -1,
        ',' => -1,
        '>' => -1,
        '<' => 1,
        _ => 0,
    }
}

impl Assoc {
    fn operator_weight(operator: &str) -> i32 {
        operator.chars().map(char_assoc).sum::<i32>()
    }

    /// Obtains associativity of given operator identifier.
    pub fn of(operator: &str) -> Assoc {
        if is_applicative(operator) || Self::operator_weight(operator) >= 0 {
            Assoc::Left
        } else {
            Assoc::Right
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assoc() {
        use Assoc::Left;
        use Assoc::Right;

        assert_eq!(Assoc::of("+"), Left);
        assert_eq!(Assoc::of("*"), Left);
        assert_eq!(Assoc::of(","), Right);
        assert_eq!(Assoc::of("*>"), Left);
    }

    #[test]
    fn test_applicative() {
        assert!(is_applicative("<$>"));
        assert!(is_applicative("<*>"));
        assert!(is_applicative("<*"));
        assert!(is_applicative("*>"));
        assert!(!is_applicative("="));
        assert!(!is_applicative("++"));
    }
}
