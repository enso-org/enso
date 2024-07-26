use crate::prelude::*;

use crate::program::command::Manipulator;
use crate::program::version::IsVersion;



#[derive(Clone, Copy, Debug, strum::Display, strum::EnumString)]
pub enum OptimizationLevel {
    /// execute default optimization passes (equivalent to -Os)
    O,
    ///  execute no optimization passes
    O0,
    /// execute -O1 optimization passes (quick&useful opts, useful for iteration builds)
    O1,
    /// execute -O2 optimization passes (most opts, generally gets most perf)
    O2,
    /// execute -O3 optimization passes (spends potentially a lot of time optimizing)
    O3,
    /// execute -O4 optimization passes (also flatten the IR, which can take a lot more time and
    /// memory, but is useful on more nested / complex / less-optimized input)
    O4,
    /// execute default optimization passes, focusing on code size
    Os,
    /// execute default optimization passes, super-focusing on code size
    Oz,
}

impl Manipulator for OptimizationLevel {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        let flag = format!("-{self}");
        command.arg(flag);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Output<'a>(pub &'a Path);

impl Manipulator for Output<'_> {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg("-o").arg(self.0);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WasmOpt;

impl Program for WasmOpt {
    type Version = Version;
    fn executable_name(&self) -> &str {
        "wasm-opt"
    }
}

// wasm-opt (like the whole binaryen) uses a single number as a version.
#[derive(Clone, Copy, Debug, Display, PartialEq, PartialOrd, Deref, Eq)]
pub struct Version(pub u32);

impl std::str::FromStr for Version {
    type Err = <u32 as std::str::FromStr>::Err;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::str::FromStr::from_str(s).map(Self)
    }
}

impl IsVersion for Version {
    fn find_in_text_internal(text: &str) -> Result<Self> {
        let number_regex = regex::Regex::new(r#"\d+"#)?;
        let number_match = number_regex.find(text).context("No number in the given text.")?;
        let number_text = number_match.as_str();
        number_text.parse2()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn version_parsing() -> Result {
        let sample_version_string = "wasm-opt version 108 (version_108)";
        assert_eq!(WasmOpt.parse_version(sample_version_string)?, Version(108));
        Ok(())
    }
}
