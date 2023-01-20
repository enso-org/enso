use crate::prelude::*;

/// Program that discards symbols and other data from object files.
#[derive(Debug, Clone, Copy)]
pub struct Strip;

impl Program for Strip {
    fn executable_name(&self) -> &str {
        "strip"
    }
}

/// Regex that matches URLs.
pub fn url_regex() -> regex::Regex {
    // As per https://uibakery.io/regex-library/url
    regex::Regex::new(
        r#"https?://(?:www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b(?:[-a-zA-Z0-9()@:%_\+.~#?&/=]*)"#,
    ).unwrap()
}

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

pub async fn strip_shaderc_package(output_dir: &Path) -> Result {
    let url = download_url()?;
    println!("{}", url);
    let body = ide_ci::io::download_all(url).await?;
    let text = std::str::from_utf8(&body)?;
    println!("{}", text);

    let url = get_redirection_target(text)?;


    let temp = tempfile::tempdir()?;

    let downloaded_archive = ide_ci::io::download_to_dir(url, &temp).await?;
    info!("Download to {} complete.", downloaded_archive.display());

    let extracted_archive = temp.as_ref().join("extracted");
    ide_ci::archive::extract_to(&downloaded_archive, &extracted_archive).await?;
    let binaries_to_package = ["glslc", "spirv-opt"];

    let extracted_content_dir = extracted_archive.join("install");
    let files_to_package = binaries_to_package
        .into_iter()
        .map(|binary| Path::new("bin").join(binary).with_executable_extension());

    for file in files_to_package {
        let path =
            ide_ci::fs::tokio::copy_between(&extracted_content_dir, &output_dir, &file).await?;
        if TARGET_OS == OS::Linux {
            Strip.cmd()?.arg(&path).run_ok().await?;
        }
    }
    Ok(())
}
