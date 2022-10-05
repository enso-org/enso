use crate::prelude::*;



#[derive(Clone, Copy, Debug, Default)]
pub struct Npx;

impl Program for Npx {
    fn executable_name(&self) -> &'static str {
        "npx"
    }
}
