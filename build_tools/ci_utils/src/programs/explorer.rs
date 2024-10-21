//! Windows File Explorer-related utilities.
//!
//! While this module builds cross-platform, it is only useful on Windows.

use crate::prelude::*;



#[derive(Debug, Clone, Copy)]
pub struct Explorer;

impl Program for Explorer {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &str {
        "explorer"
    }
}


/// Open the parent folder of the given path in Windows File Explorer and select the given path.
// Windows only, due to platform-specific `raw_arg` usage.
#[cfg(windows)]
pub fn show_selected(path: impl AsRef<Path>) -> Result {
    let argument = format!(r#"/select,"{}""#, path.as_ref().display());
    // We use `raw_arg` to avoid escaping the path. The usual quoting rules don't work here.
    // We ignore the child, as we don't need to wait for it to finish.
    let _child = Explorer.cmd()?.raw_arg(argument).spawn();
    Ok(())
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[cfg(windows)]
    #[test]
    #[ignore]
    fn show_my_path() {
        setup_logging().ok();
        let path = std::env::current_exe().unwrap();
        show_selected(path).unwrap();
    }
}
