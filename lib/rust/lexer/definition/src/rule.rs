//! This file contains a macro to simplify writing the lexer rules.



// ===================
// === Rules Macro ===
// ===================

/// Define a group of rules for the lexer.
///
/// All of the rules must be defined for the same `state_name`, which must be the in-scope name of
/// the state for which the rules are being defined. Each `pattern` is a non-reference pattern that
/// the rule is being defined to match, and `code` is the code that will be executed when the rule
/// matches, omitting the (first) `reader` argument).
///
/// Branches are matched _in order_, from top-to-bottom, much like a standard `match` statement.
///
/// Please see `lexer.rs` for myriad examples of this macro's use.
#[macro_export]
macro_rules! rules {
    ($state_name:ident with $($pattern:expr => $path_root:ident $(.$path:ident)* ($($arg:tt)*)),+ $(,)?) => {
        $($state_name.create_rule(&$pattern,stringify!{
            $path_root $(.$path)* (reader,$($arg)*)
        });)*
    };
}
