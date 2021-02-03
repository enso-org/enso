
// ==================
// === Data Types ===
// ==================

/// The DataTypes specific for the Enso language.
pub mod enso {
    use crate::prelude::*;

    im_string_newtype!{
        /// Name of the Enso library.
        LibraryName,

        /// The source code of Enso program.
        Code,

        /// The Enso type representation. Can be a complex type, like `String|Int`.
        Type,
    }

    impl Type {
        /// The `Any` Enso type. It is the type which matches all other types.
        pub fn any() -> Self {
            "Any".into()
        }
    }

    /// Builtin library name. For internal usage only.
    pub fn builtin_library() -> LibraryName {
        "builtin".into()
    }
}
