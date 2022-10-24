//! Browser timer handlers wrapped in FRP API.

mod delayed_interval;
mod interval;
mod timeout;

pub use delayed_interval::*;
pub use interval::*;
pub use timeout::*;
