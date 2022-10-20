//! Browser timer handlers wrapped in FRP API.

mod delayed_repeats;
mod interval;
mod timeout;

pub use delayed_repeats::*;
pub use interval::*;
pub use timeout::*;
