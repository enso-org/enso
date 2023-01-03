//! Support for dealing with Microsoft Visual C++ Redistributables.
//!
//! See <https://learn.microsoft.com/en-us/cpp/windows/redistributing-visual-cpp-files> for the
//! official documentation.

use crate::prelude::*;

use crate::programs::vs;
use crate::programs::vs::VCToolsRedistDir;



/// Get the directory with MSVC redistributable files for a given platform.
///
/// This requires [`VCToolsRedistDir`] environment variable to be set, e.g. by having invoked
/// [`apply_dev_environment`].
pub fn locate_dir(platform: vs::Platforms) -> Result<PathBuf> {
    let redist_path = VCToolsRedistDir.get()?;
    let platform_segment = platform.to_string();
    Ok(redist_path.join(platform_segment))
}

/// Obtain a list of all Visual C++ runtime (CRT) redistributable DLLs for a given target.
///
/// This requires [`VCToolsRedistDir`] environment variable to be set, e.g. by having invoked
/// [`apply_dev_environment`].
///
/// Note that this does not include the C runtime (UCRT) which is now a part of Windows.
#[context("Failed to locate MSVC CRT redistributables for {platform}.")]
pub async fn list_crt_dlls(platform: vs::Platforms) -> Result<Vec<PathBuf>> {
    let redist_dir = locate_dir(platform)?;
    // The first component is "*.CRT" because we only want to redistribute CRT DLLs. If we matched
    // all folders, we would also get other libraries, like MFC.
    let redist_binaries_glob = redist_dir.join_iter(["*.crt", "**", "*.dll"]);
    // Be careful about case! Windows is case-insensitive.
    let glob_options = glob::MatchOptions { case_sensitive: false, ..default() };
    glob::glob_with(redist_binaries_glob.as_str(), glob_options)?.try_collect().with_context(|| {
        format!("Failed to list CRT redistributables in {}.", redist_binaries_glob.display())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::programs::vs::apply_dev_environment;

    #[tokio::test]
    #[ignore]
    async fn test_listing_dlls() -> Result {
        setup_logging()?;
        apply_dev_environment().await?;
        let dlls = list_crt_dlls(vs::Platforms::local()?).await?;
        dbg!(&dlls);
        Ok(())
    }
}
