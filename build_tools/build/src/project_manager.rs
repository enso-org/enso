use crate::prelude::*;

use crate::paths::TargetTriple;



pub fn url(target: &TargetTriple) -> Result<Url> {
    #[allow(clippy::format_in_format_args)] // [mwu] I find this more readable.
    let url_text = format!(
        "https://github.com/enso-org/{repo}/releases/download/{tag}/{asset}.{ext}",
        repo = "ci-build",
        tag = target.versions.tag(),
        asset = format!("project-manager-bundle-{target}"),
        ext = ide_ci::github::release::archive_extension(),
    );
    Ok(Url::parse(&url_text)?)
}
