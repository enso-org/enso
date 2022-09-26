//use crate::prelude::*;

use crate::env::StrLikeVariable;

pub const CI: StrLikeVariable = StrLikeVariable::new("CI");

// /// An environment variable set commonly by most of popular CI systems.
// pub struct Ci;
//
// impl Variable for Ci {
//     const NAME: &'static str = "CI";
// }

/// Check if the environment suggests that we are being run in a CI.
pub fn run_in_ci() -> bool {
    std::env::var("CI").is_ok()
}
