use crate::prelude::*;



pub mod env {
    /// The Rust toolchain version which was selected by Rustup.
    ///
    /// If set, any cargo invocation will follow this version. Otherwise, Rustup will deduce
    /// toolchain to be used and set up this variable for the spawned process.
    ///
    /// Example value: `"nightly-2022-01-20-x86_64-pc-windows-msvc"`.
    #[derive(Clone, Copy, Debug)]
    pub struct Toolchain;

    impl crate::env::Variable for Toolchain {
        const NAME: &'static str = "RUSTUP_TOOLCHAIN";
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Rustup;

impl Program for Rustup {
    fn executable_name(&self) -> &'static str {
        "rustup"
    }
}
