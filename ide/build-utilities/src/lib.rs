#![feature(option_unwrap_none)]

use std::{fs, path};
use std::io::ErrorKind;

/// A structure describing a concrete release package on github.
pub struct GithubRelease<Str:AsRef<str>> {
    pub project_url : Str,
    pub version     : Str,
    pub filename    : Str,
}

impl<Str:AsRef<str>> GithubRelease<Str> {
    /// Download the release package from github
    ///
    /// The project_url should be a project's main page on github.
    pub fn download(&self, destination_dir:&path::Path) {
        let url = format!(
            "{project}/releases/download/{version}/{filename}",
            project  = self.project_url.as_ref(),
            version  = self.version.as_ref(),
            filename = self.filename.as_ref());
        let destination_dir_str = destination_dir.to_str().unwrap();
        let destination_file    = destination_dir.join(self.filename.as_ref());

        Self::remove_old_file(&destination_file);
        download_lp::download(url.as_str(),destination_dir_str).unwrap();
    }

    fn remove_old_file(file:&path::Path) {
        let result      = fs::remove_file(&file);
        let error       = result.err();
        let fatal_error = error.filter(|err| err.kind() != ErrorKind::NotFound);
        fatal_error.unwrap_none();
    }
}
