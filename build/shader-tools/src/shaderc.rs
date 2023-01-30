//! Downloading and repackaging the `shaderc` collection of tools.

use crate::prelude::*;

use ide_ci::programs::shaderc::Glslc;
use ide_ci::programs::shaderc::SpirvOpt;
use ide_ci::programs::Strip;



/// The binaries from the `shaderc` collection that we actually use.
pub fn binaries_to_package() -> [&'static str; 2] {
    [Glslc.executable_name(), SpirvOpt.executable_name()]
}

/// Regex that matches URLs.
/// ```
/// # use enso_build_shader_tools::shaderc::url_regex;
/// let url_text =
///     "https://storage.googleapis.com/shaderc/badges/build_link_windows_vs2017_release.html";
/// assert!(url_regex().is_match(url_text));
/// ```
pub fn url_regex() -> regex::Regex {
    // As per https://uibakery.io/regex-library/url
    regex::Regex::new(
        r#"https?://(?:www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b(?:[-a-zA-Z0-9()@:%_\+.~#?&/=]*)"#,
    ).unwrap()
}

/// Download URL for shaderc package as [advertised on their page](https://github.com/google/shaderc/blob/main/downloads.md).
/// Note that this URL actually contains a redirection to the recent CI build. The redirection can
/// be resolved using [`get_redirection_target`]. It is `<meta>`-based redirection, not the 3xx one.
pub fn download_url() -> Result<Url> {
    match TARGET_OS {
        OS::Linux =>
            "https://storage.googleapis.com/shaderc/badges/build_link_linux_gcc_release.html",
        OS::MacOS =>
            "https://storage.googleapis.com/shaderc/badges/build_link_macos_clang_release.html",
        OS::Windows =>
            "https://storage.googleapis.com/shaderc/badges/build_link_windows_vs2017_release.html",
        _ => bail!("Unsupported OS: {}.", TARGET_OS),
    }
    .parse2()
}

/// Get the URL that is the target of the redirection used by `shaderc` download page.
///
/// ```
/// # use enso_build_shader_tools::shaderc::get_redirection_target;
/// let html = r#"<meta http-equiv="refresh" content="0; url=https://storage.googleapis.com/shaderc/artifacts/prod/graphics_shader_compiler/shaderc/windows/continuous_release_2017/406/20230118-144628/install.zip" />"#;
/// let url = get_redirection_target(html).unwrap();
/// assert_eq!(url.as_str(),"https://storage.googleapis.com/shaderc/artifacts/prod/graphics_shader_compiler/shaderc/windows/continuous_release_2017/406/20230118-144628/install.zip");
/// ```
#[context("Failed to get the redirection target from the HTML text: {html}")]
pub fn get_redirection_target(html: &str) -> Result<Url> {
    let regex = url_regex();
    let dom = html_parser::Dom::parse(html)?;
    debug!("{:#?}", &dom);
    let [html_parser::Node::Element(element)] = dom.children.as_slice() else {
        bail!("Expected one child node.");
    };
    ensure!(element.name == "meta", "Expected meta tag.");
    let Some(Some(content)) = element.attributes.get("content") else {
        bail!("Expected content attribute.");
    };
    let Some(url_match) = regex.captures(content).and_then(|captures| captures.get(0)) else {
        bail!("Expected URL.");
    };
    url_match.as_str().parse2()
}

/// Download the `shaderc` package.
#[context("Failed to download the shaderc package.")]
pub async fn download_package(output_dir: &Path) -> Result {
    let url = download_url()?;
    let body = ide_ci::io::download_all(url).await?;
    let text = std::str::from_utf8(&body)?;
    let url = get_redirection_target(text)?;

    let temp = tempfile::tempdir()?;
    let downloaded_archive = ide_ci::io::download_to_dir(url, &temp).await?;
    info!("Download to {} complete.", downloaded_archive.display());
    ide_ci::archive::extract_to(&downloaded_archive, &output_dir).await
}

/// Strip down the `shaderc` package to the bare minimum that we require.
#[context("Failed to strip down the shaderc package.")]
pub async fn strip_package(package_path: &Path, output_package: &Path) -> Result {
    let extracted_content_dir = package_path.join("install");
    for binary in binaries_to_package() {
        let file = Path::new("bin").join(binary).with_executable_extension();
        let path =
            ide_ci::fs::tokio::copy_between(&extracted_content_dir, &output_package, &file).await?;
        // Only linux packages contain the debug symbols, which make them really heavy.
        if TARGET_OS == OS::Linux {
            Strip.cmd()?.arg(&path).run_ok().await?;
        }
    }
    Ok(())
}

/// Download and strip down the `shaderc` package.
#[context("Failed to generate stripped down shaderc package.")]
pub async fn generate_stripped_package(output_dir: &Path) -> Result {
    let extracted_archive = tempfile::tempdir()?;
    let extracted_archive = extracted_archive.path();
    download_package(extracted_archive).await?;
    strip_package(extracted_archive, output_dir).await
}
