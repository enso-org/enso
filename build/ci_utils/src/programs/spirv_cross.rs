//! Wrapper for [spirv-cross](https://github.com/KhronosGroup/SPIRV-Cross).

use crate::prelude::Program;



/// SPIRV-Cross is a practical tool and library for performing reflection on SPIR-V and
/// disassembling SPIR-V back to high level languages.
#[derive(Clone, Copy, Debug, Default)]
pub struct SpirvCross;

impl Program for SpirvCross {
    fn executable_name(&self) -> &'static str {
        "spirv-cross"
    }
}
