use crate::prelude::*;

use crate::archive::Format;

use std::vec::IntoIter;



pub mod bsd {
    use super::*;

    /// Options specific for `bsdtar`.
    #[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
    pub enum Switch {
        /// Symbolic links named on the command line will be followed; the target of the link will
        /// be archived, not the link itself.
        FollowSymlinksInCommand,
    }

    impl AsRef<OsStr> for Switch {
        fn as_ref(&self) -> &OsStr {
            match self {
                Switch::FollowSymlinksInCommand => "-H",
            }
            .as_ref()
        }
    }
}

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Compression {
    Bzip2,
    Gzip,
    Lzma,
    Xz,
}

impl Compression {
    pub fn deduce_from_extension(extension: impl AsRef<Path>) -> Result<Compression> {
        let extension = extension.as_ref().to_str().unwrap();
        if extension == "bz2" {
            Ok(Compression::Bzip2)
        } else if extension == "gz" {
            Ok(Compression::Gzip)
        } else if extension == "lzma" {
            Ok(Compression::Lzma)
        } else if extension == "xz" {
            Ok(Compression::Xz)
        } else {
            bail!("The extension `{}` does not denote a supported compression algorithm for TAR archives.", extension)
        }
    }
}

impl Display for Compression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Compression::*;
        write!(f, "{}", match self {
            Bzip2 => "bzip2",
            Gzip => "gzip",
            Lzma => "lzma",
            Xz => "xz",
        })
    }
}

impl AsRef<str> for Compression {
    fn as_ref(&self) -> &str {
        match self {
            Compression::Bzip2 => "-j",
            Compression::Gzip => "-z",
            Compression::Lzma => "--lzma",
            Compression::Xz => "-J",
        }
    }
}

impl AsRef<OsStr> for Compression {
    fn as_ref(&self) -> &OsStr {
        let str: &str = self.as_ref();
        str.as_ref()
    }
}

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Switch<'a> {
    TargetFile(&'a Path),
    Verbose,
    UseFormat(Compression),
    WorkingDir(&'a Path),
}

impl<'a> IntoIterator for &'a Switch<'a> {
    type Item = &'a OsStr;
    type IntoIter = IntoIter<&'a OsStr>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Switch::TargetFile(tgt) => vec!["-f".as_ref(), tgt.as_ref()],
            Switch::Verbose => vec!["--verbose".as_ref()],
            Switch::UseFormat(compression) => vec![compression.as_ref()],
            Switch::WorkingDir(dir) => vec!["--directory".as_ref(), dir.as_ref()],
        }
        .into_iter()
    }
}

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Flavor {
    Gnu,
    Bsd,
}

impl Flavor {
    pub fn from_version_text(text: &str) -> Result<Self> {
        if text.contains("bsdtar") {
            Ok(Flavor::Bsd)
        } else if text.contains("GNU tar") {
            Ok(Flavor::Gnu)
        } else {
            bail!("The output of `tar --version` does not contain a recognizable flavor. The version text was: {text}")
        }
    }
}

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Command {
    Append,
    Create,
    Extract,
    List,
}

impl AsRef<str> for Command {
    fn as_ref(&self) -> &str {
        match self {
            Command::Append => "-r",
            Command::Create => "-c",
            Command::Extract => "-x",
            Command::List => "-t",
        }
    }
}

impl AsRef<OsStr> for Command {
    fn as_ref(&self) -> &OsStr {
        let str: &str = self.as_ref();
        str.as_ref()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Tar;

impl Program for Tar {
    fn executable_name(&self) -> &'static str {
        "tar"
    }
}

impl Tar {
    pub async fn flavor(&self) -> Result<Flavor> {
        let text = self.version_string().await?;
        Flavor::from_version_text(&text)
    }

    #[context("Failed to crate an archive {}.", output_archive.as_ref().display())]
    pub fn pack_cmd<P: AsRef<Path>>(
        &self,
        output_archive: impl AsRef<Path>,
        paths_to_pack: impl IntoIterator<Item = P>,
    ) -> Result<crate::prelude::Command> {
        let mut cmd = self.cmd()?;
        cmd.arg(Command::Create);

        if let Ok(Format::Tar(Some(compression))) = Format::from_filename(&output_archive) {
            cmd.args(&Switch::UseFormat(compression));
        }

        cmd.args(&Switch::TargetFile(output_archive.as_ref()));

        let paths: Vec<PathBuf> =
            paths_to_pack.into_iter().map(|path| path.as_ref().to_owned()).collect();

        match paths.as_slice() {
            [item] =>
                if let Some(parent) = crate::fs::canonicalize(item)?.parent() {
                    cmd.args(&Switch::WorkingDir(parent));
                    cmd.arg(item.file_name().unwrap()); // None can happen only when path ends with
                                                        // ".." - that's why we canonicalize
                },
            // [dir] if dir.is_dir() => {
            //     cmd.args(&Switch::WorkingDir(dir.to_owned()));
            //     cmd.arg(".");
            // }
            _ => {
                todo!("")
            } /* paths => {
               *     if let Some(parent) = output_archive.as_ref().parent() {
               *         cmd.arg(Switch::WorkingDir(parent.to_owned()).format_arguments());
               *         for path_to_pack in paths {
               *             if path_to_pack.is_absolute() {
               *                 pathdiff::diff_paths(parent, path_to_pack).ok_or_else(||
               * anyhow!("failed to relativize paths {} {}", parent, path_to_pack))
               *             }
               *             cmd.arg(&path_to_pack);
               *         },
               *     }
               * } */
        }


        Ok(cmd)
        // cmd_from_args![Command::Create, val [switches], output_archive.as_ref(), ref
        // [paths_to_pack]]
    }

    pub async fn pack<P: AsRef<Path>>(
        self,
        output_archive: impl AsRef<Path>,
        paths_to_pack: impl IntoIterator<Item = P>,
    ) -> Result {
        self.pack_cmd(output_archive, paths_to_pack)?.run_ok().await
    }

    pub async fn pack_directory_contents(
        self,
        compression: Option<Compression>,
        output_archive: impl AsRef<Path>,
        root_directory: impl AsRef<Path>,
    ) -> Result {
        // See: https://stackoverflow.com/a/3035446
        let mut cmd = self.cmd()?;
        cmd.arg(Command::Create)
            .args(compression)
            .args(&Switch::TargetFile(output_archive.as_ref()))
            .args(&Switch::WorkingDir(root_directory.as_ref()));
        if TARGET_OS == OS::Windows && Tar.flavor().await.contains(&Flavor::Bsd) {
            // Used only when `tar` is `bsdtar`. This is the default
            // but e.g. Git can come with its own non-bsd tar. GNU tar does not support this option.
            //
            // This flag is to tell `tar` to resolve symlinks that appear on the command line.
            // On Windows when "." is a symlink, only the symlink is archived otherwise.
            cmd.arg(bsd::Switch::FollowSymlinksInCommand);
        }

        cmd.arg(".").run_ok().await
    }

    pub async fn unpack(
        &self,
        archive: impl AsRef<Path>,
        output_directory: impl AsRef<Path>,
    ) -> Result {
        crate::fs::tokio::create_dir_if_missing(&output_directory).await?;
        self.cmd()?
            .arg(Command::Extract)
            .args(&Switch::TargetFile(archive.as_ref()))
            .args(&Switch::WorkingDir(output_directory.as_ref()))
            .run_ok()
            .await
    }
}


#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::archive::extract_to;
    use crate::archive::pack_directory_contents;
    use crate::log::setup_logging;

    #[test]
    fn deduce_format_from_extension() {
        let expect_ok = |str: &str, expected: Compression| {
            assert_eq!(Compression::deduce_from_extension(OsStr::new(str)).unwrap(), expected);
        };

        expect_ok("bz2", Compression::Bzip2);
        expect_ok("gz", Compression::Gzip);
        expect_ok("lzma", Compression::Lzma);
        expect_ok("xz", Compression::Xz);
    }

    #[tokio::test]
    async fn test_directory_packing() -> Result {
        setup_logging()?;
        let archive_temp = tempfile::tempdir()?;
        let archive_path = archive_temp.path().join("archive.tar.gz");


        let temp = tempfile::tempdir()?;
        let filename = "bar.txt";
        crate::fs::tokio::write(temp.path().join(filename), "bar contents").await?;

        let linked_temp = archive_temp.path().join("linked");
        symlink::symlink_dir(temp.path(), &linked_temp)?;

        pack_directory_contents(&archive_path, &linked_temp).await?;
        assert!(archive_path.exists());
        assert!(archive_path.metadata()?.len() > 0);

        let temp2 = tempfile::tempdir()?;
        extract_to(&archive_path, temp2.path()).await?;
        assert!(temp2.path().join(filename).exists());
        assert_eq!(
            crate::fs::tokio::read(temp2.path().join(filename)).await?,
            "bar contents".as_bytes()
        );


        Ok(())
    }

    #[test]
    #[ignore]
    fn pack_command_test() {
        let cmd = Tar.pack_cmd("output.tar.gz", ["target.bmp"]).unwrap();
        debug!("{:?}", cmd);
        dbg!(cmd);
    }
}
