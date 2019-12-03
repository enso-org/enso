#![feature(option_unwrap_none)]

use std::{fs, path};
use std::io::ErrorKind;

/// Download the release package from github
///
/// The project_url should be a project's main page on github.
pub fn github_download(
    project_url     : &str,
    version         : &str,
    filename        : &str,
    destination_dir : &path::Path
) {
    let url = format!(
        "{project}/releases/download/{version}/{filename}",
        project  = project_url,
        version  = version,
        filename = filename
    );

    let destination_file = path::Path::new(destination_dir)
        .join(filename);

    let rm_error = fs::remove_file(&destination_file).err();
    let fatal_rm_error =
        rm_error.filter(|err| err.kind() != ErrorKind::NotFound);
    fatal_rm_error.unwrap_none();

    download_lp::download(
        url.as_str(),
        destination_dir.to_str().unwrap()
    ).unwrap();
}
