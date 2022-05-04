//! Names of fonts embedded in the app.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

/// Full name of the regular sans-serif font from the DejaVu font family.
pub const DEJA_VU_SANS: &str = "DejaVuSans";

/// Full name of the bold sans-serif font from the DejaVu font family.
pub const DEJA_VU_SANS_BOLD: &str = "DejaVuSans-Bold";

/// Full name of the regular monospaced sans-serif font from the DejaVu font family.
pub const DEJA_VU_SANS_MONO: &str = "DejaVuSansMono";

/// Full name of the bold monospaced sans-serif font from the DejaVu font family.
pub const DEJA_VU_SANS_MONO_BOLD: &str = "DejaVuSansMono-Bold";
