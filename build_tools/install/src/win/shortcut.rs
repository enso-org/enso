//! Utilities for managing start menu and desktop shortcuts.

use crate::prelude::*;

use ide_ci::env::consts::SHORTCUT_SUFFIX;



/// Location of the shortcut.
#[derive(Copy, Clone, Debug, Display)]
pub enum Location {
    /// Local user's Start Menu shortcut (in the Programs folder).
    Menu,
    /// Local user's Desktop shortcut.
    Desktop,
}

impl Location {
    /// Get the directory where the shortcut should be created.
    #[context("Failed to get {self} shortcut location.")]
    pub fn directory(&self) -> Result<PathBuf> {
        match self {
            Self::Menu => Ok(crate::win::start_menu_programs()?),
            Self::Desktop => Ok(crate::win::desktop()?),
        }
    }

    /// Get the shortcut file path.
    #[context("Failed to get {self} shortcut '{name}' path.")]
    pub fn shortcut_path(&self, name: &str) -> Result<PathBuf> {
        Ok(self.directory()?.join(name).with_extension(SHORTCUT_SUFFIX))
    }

    /// Create shortcut that links to target.
    #[context("Failed to create {self} shortcut `{}`.", name)]
    pub fn create_shortcut(&self, name: &str, target: &Path) -> Result {
        let shortcut_path = self.shortcut_path(name)?;
        create_shortcut_customized(&shortcut_path, target, |link| {
            link.set_name(Some(name.into()));
        })
    }

    /// Remove the shortcut.
    #[context("Failed to remove {self} shortcut `{}`.", name)]
    pub fn remove_shortcut(&self, name: &str) -> Result {
        let shortcut_path = self.shortcut_path(name)?;
        ide_ci::fs::remove_file_if_exists(shortcut_path)
    }
}

/// Create a Windows shortcut (`.lnk` file).
pub fn create_shortcut_customized(
    shortcut_path: &Path,
    target: &Path,
    f: impl FnOnce(&mut mslnk::ShellLink),
) -> Result {
    // Paths with verbatim prefix (i.e. `\\?\`) are not supported by the Windows Shell API.
    let target = target.without_verbatim_prefix();
    info!("Creating shortcut {} -> {}", shortcut_path.display(), target.display());
    ide_ci::fs::create_parent_dir_if_missing(shortcut_path)?;
    let mut link = mslnk::ShellLink::new(target)?;
    f(&mut link);
    link.create_lnk(shortcut_path)?;
    Ok(())
}
