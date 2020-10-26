//! This file contains a macro to simplify writing the lexer rules.



// ==================
// === Rule Macro ===
// ==================

/// Define a rule for the lexer.
///
/// `state_name` must be the in-scope name of the state the rule is being defined for, `pattern` is
/// the (non-reference) pattern that the rule is being defined to match, and `code` is the code that
/// will be executed when the rule matches.
#[macro_export]
macro_rules! rule {
    ($state_name:ident($pattern:expr) => $code:expr) => {
        $state_name.create_rule(&$pattern,stringify!($code))
    }
}
