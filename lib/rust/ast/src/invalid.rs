//! This module exports invalid ast shapes.



// ===============
// === Invalid ===
// ===============


// === Unrecognized ===

/// Unrecognized token.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Unrecognized { pub str: String }
