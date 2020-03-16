//! Root module for FRP node definitions.

pub mod dynamic;
pub mod inference;
pub mod lambda;
pub mod prim;

pub use dynamic::*;
pub use inference::*;
pub use lambda::*;
pub use prim::*;
