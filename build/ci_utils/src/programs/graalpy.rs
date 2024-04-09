use crate::prelude::*;



#[derive(Clone, Copy, Debug)]
pub struct GraalPy;

impl Program for GraalPy {
    fn executable_name(&self) -> &'static str {
        "graalpy"
    }
}
