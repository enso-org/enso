use crate::prelude::*;



#[derive(Clone, Copy, Debug)]
pub struct Conda;

impl Program for Conda {
    fn executable_name(&self) -> &'static str {
        "conda"
    }
    fn default_locations(&self) -> Vec<PathBuf> {
        if let Some(path) = std::env::var_os("CONDA") {
            vec![PathBuf::from(path)]
        } else {
            default()
        }
    }
}
