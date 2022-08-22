//! FIXME[everyone] Modules should be documented.



// ==================
// === Data Types ===
// ==================

/// The DataTypes specific for the Enso language.
pub mod enso {
    use crate::prelude::*;

    im_string_newtype! {
        /// Name of the Enso library.
        LibraryName,

        /// The source code of Enso program.
        Code,

        /// The Enso type representation. Can be a complex type, like `String|Int`.
        Type,

        /// The Enso method name, like `main` or `my_cool_method`.
        Method,

        /// The Enso module represented as qualified path, like `Project.Data.Vector`.
        Module,
    }

    impl Type {
        /// The `Any` Enso type. It is the type which matches all other types.
        pub fn any() -> Self {
            "Any".into()
        }

        #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
        pub fn alternatives(&self) -> impl Iterator<Item = Type> + '_ {
            self.content.split('|').map(str::trim).map(Type::new)
        }
    }
}
