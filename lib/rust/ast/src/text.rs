//! This module exports ast shapes that represent text (strings).



// ============
// === Text ===
// ============

/// The ast node for a string of text.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Text { pub text: String }
