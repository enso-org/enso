use crate::prelude::*;

#[derive(Clone, Copy, Debug, Default)]
pub struct Glslc;
impl Program for Glslc {
    fn executable_name(&self) -> &'static str {
        "glslc"
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct SpirvOpt;
impl Program for SpirvOpt {
    fn executable_name(&self) -> &'static str {
        "spirv-opt"
    }
}
