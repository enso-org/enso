// === Unrecognized ===

/// Unrecognized token.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Unrecognized { pub str: String }

impl From<Unrecognized> for Shape { fn from(val:Unrecognized) -> Self { Self::Unrecognized(val) } }
