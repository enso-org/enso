use crate::prelude::*;



// ==================
// === Data Types ===
// ==================

im_string_newtype!{
    /// Name of the Enso library.
    LibraryName,

    /// The source code of Enso program.
    EnsoCode,

    /// The Enso type representation. Can be a complex type, like `String|Int`.
    EnsoType,
}

impl EnsoType {
    /// The `Any` Enso type. It is the type which matches all other types.
    pub fn any() -> Self {
        "Any".into()
    }
}

/// Builtin library name. For internal usage only.
pub fn builtin_library() -> LibraryName {
    "builtin".into()
}
