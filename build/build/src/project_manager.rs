use crate::prelude::*;

use crate::paths::TargetTriple;

use ide_ci::github::release::ARCHIVE_EXTENSION;



pub fn url(target: &TargetTriple) -> Result<Url> {
    #[allow(clippy::format_in_format_args)] // [mwu] I find this more readable.
    let url_text = format!(
        "https://github.com/enso-org/{repo}/releases/download/{tag}/{asset}.{ext}",
        repo = "ci-build",
        tag = target.versions.tag(),
        asset = format!("project-manager-bundle-{target}"),
        ext = ARCHIVE_EXTENSION,
    );
    Url::parse(&url_text).anyhow_err()
}

pub async fn ensure_present(dist_path: impl AsRef<Path>, target: &TargetTriple) -> Result {
    // Check if already done
    let build_info_file = dist_path.as_ref().join("installed-enso-version");
    let old_info = dbg!(build_info_file.read_to_json::<TargetTriple>());
    if old_info.contains(target) {
        debug!(
            "Project Manager in version {target} is already installed, according to {info}.",
            info = build_info_file.display()
        );
    } else {
        // We remove the build info file to avoid misinformation if the build is interrupted during
        // the call to `download_project_manager`.
        ide_ci::fs::remove_if_exists(&build_info_file)?;

        let url = url(target)?;
        ide_ci::io::download_and_extract(url, &dist_path).await?;
        ide_ci::fs::allow_owner_execute(crate::paths::project_manager(&dist_path))?;
        build_info_file.write_as_json(&target)?;
    }
    Ok(())
}
