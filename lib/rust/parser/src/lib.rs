//! This library contains the implementation of the Enso parser.

pub mod data;
pub mod macros;
pub mod operators;

/// The prelude for the parser.
pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_logger::AnyLogger;

    /// The Enso logging library.
    pub mod logger {
        pub use enso_logger::*;
        pub use enso_logger::disabled::Logger as Disabled;
        pub use enso_logger::enabled::Logger as Enabled;
    }
}
