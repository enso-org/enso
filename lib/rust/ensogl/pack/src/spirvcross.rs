use crate::prelude::*;

pub mod program {
    use super::*;

    #[derive(Clone, Copy, Debug, Default)]
    pub struct SpirvCross;
    impl Program for SpirvCross {
        fn executable_name(&self) -> &'static str {
            "spirv-cross"
        }
    }
}

// pub mod goodie {
//     use super::*;
//
//     #[derive(Clone, Copy, Debug, Default)]
//     pub struct SpirvCross;
//     impl Goodie for SpirvCross {
//     }
// }
