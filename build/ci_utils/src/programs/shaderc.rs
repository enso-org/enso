//! A collection of tools, libraries, and tests for Vulkan shader compilation.
//!
//! See the [GitHub repository](https://github.com/google/shaderc) for more information.

use crate::prelude::*;



// =============
// === glslc ===
// =============

/// A command-line GLSL/HLSL to SPIR-V compiler with Clang-compatible arguments.
#[derive(Clone, Copy, Debug, Default)]
pub struct Glslc;

impl Program for Glslc {
    fn executable_name(&self) -> &'static str {
        "glslc"
    }
}


// =================
// === spirv-opt ===
// =================

/// SPIR-V Optimizer.
#[derive(Clone, Copy, Debug, Default)]
pub struct SpirvOpt;

impl Program for SpirvOpt {
    fn executable_name(&self) -> &'static str {
        "spirv-opt"
    }
}
