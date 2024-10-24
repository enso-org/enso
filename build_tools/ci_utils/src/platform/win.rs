use crate::prelude::*;



pub fn program_files_x86() -> Result<PathBuf> {
    // TODO consider if should support 32-bit win targets?
    crate::env::expect_var_os("ProgramFiles(x86)").map(PathBuf::from)
}
