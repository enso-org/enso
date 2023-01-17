use ide_ci::define_env_var;
pub use ide_ci::prelude;
use ide_ci::program::command::Manipulator;

use prelude::*;

pub const SPIRV_TOOLS_URL: &str = "https://github.com/KhronosGroup/SPIRV-Cross";

pub mod cmake;

define_env_var! {
    ENSO_RELEASE_ID, octocrab::models::ReleaseId;
}


pub mod shaderc {
    use crate::prelude::*;

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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cmake::CMake;
    use crate::cmake::SetVariable;
    use ide_ci::io::web::download_file;
    use ide_ci::programs::vs::apply_dev_environment;
    use ide_ci::programs::Git;

    #[tokio::test]
    async fn strip_shaderc_package() -> Result {
        setup_logging()?;
        // As per https://uibakery.io/regex-library/url
        let regex = regex::Regex::new(
            r#"https?://(?:www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b(?:[-a-zA-Z0-9()@:%_\+.~#?&/=]*)"#,
        )?;

        let url = shaderc::download_url()?;
        let body = ide_ci::io::download_all(url).await?;
        let text = std::str::from_utf8(&body)?;
        println!("{}", text);
        let dom = html_parser::Dom::parse(text)?;
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

        let url: Url = url_match.as_str().parse2()?;
        println!("{}", url);


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

        let stripped_down_archive = temp.as_ref().join("stripped");
        for file in files_to_package {
            ide_ci::fs::tokio::copy_between(&extracted_content_dir, &stripped_down_archive, &file)
                .await?;
        }

        let output_path = Path::new("shaderc-stripped.tar.gz");
        ide_ci::fs::tokio::remove_file_if_exists(&output_path).await?;
        ide_ci::archive::compress_directory(&output_path, &stripped_down_archive).await?;

        Ok(())
    }

    #[tokio::test]
    async fn compile_spirv_cross() -> Result {
        setup_logging()?;
        if TARGET_OS == OS::Windows {
            apply_dev_environment().await?;
        }
        // let path = Path::new(r"C:\temp\spirv-cross");
        let path = tempfile::tempdir()?;
        let path = path.as_ref();
        let build_dir = path.join("_build");
        let install_dir = path.join("_install");
        ide_ci::fs::tokio::reset_dir(&path).await?;

        let git = Git.clone(&path, &(SPIRV_TOOLS_URL.try_into()?)).await?;

        ide_ci::fs::tokio::reset_dir(&build_dir).await?;
        CMake
            .cmd()?
            .arg(&path)
            .apply(&SetVariable::option("SPIRV_CROSS_ENABLE_TESTS", false))
            .current_dir(&build_dir)
            .run_ok()
            .await?;
        CMake
            .cmd()?
            .arg("--build")
            .arg(".")
            .arg("-j")
            .args(["--config", "Release"])
            .current_dir(&build_dir)
            .run_ok()
            .await?;

        ide_ci::fs::tokio::reset_dir(&install_dir).await?;
        CMake
            .cmd()?
            .arg("--install")
            .arg(".")
            .args(["--prefix", install_dir.as_str()])
            .current_dir(&build_dir)
            .run_ok()
            .await?;

        let archive_name = format!("spirv-cross-{}.tar.gz", TARGET_OS);
        let package = ide_ci::archive::create(&archive_name, [install_dir.join("bin")]).await?;


        Ok(())
    }
}
