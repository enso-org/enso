use crate::prelude::*;

use crate::project::gui::ide_desktop_from_context;
use crate::project::Context;

use ide_ci::actions::artifacts::upload_compressed_directory;
use ide_ci::actions::artifacts::upload_single_file;
use ide_ci::actions::workflow::is_in_env;



#[derive(Clone, Debug)]
pub struct Artifact {
    /// Directory with unpacked client distribution.
    pub unpacked:            PathBuf,
    /// Entry point within an unpacked client distribution.
    pub unpacked_executable: PathBuf,
    /// File with the compressed client image (like installer or AppImage).
    pub image:               PathBuf,
    /// File with the checksum of the image.
    pub image_checksum:      PathBuf,
}

impl Artifact {
    fn new(
        target_os: OS,
        target_arch: Arch,
        version: &Version,
        dist_dir: impl AsRef<Path>,
    ) -> Self {
        let unpacked = dist_dir.as_ref().join(match target_os {
            OS::Linux => "linux-unpacked",
            OS::MacOS if target_arch == Arch::AArch64 => "mac-arm64",
            OS::MacOS if target_arch == Arch::X86_64 => "mac",
            OS::Windows => "win-unpacked",
            _ => todo!("{target_os}-{target_arch} combination is not supported"),
        });
        let unpacked_executable = match target_os {
            OS::Linux => "enso",
            OS::MacOS => "Enso.app",
            OS::Windows => "Enso.exe",
            _ => todo!("{target_os}-{target_arch} combination is not supported"),
        }
        .into();
        let image = dist_dir.as_ref().join(match target_os {
            OS::Linux => format!("enso-linux-{version}.AppImage"),
            OS::MacOS => format!("enso-mac-{version}.dmg"),
            OS::Windows => format!("enso-win-{version}.exe"),
            _ => todo!("{target_os}-{target_arch} combination is not supported"),
        });

        Self {
            image_checksum: image.with_extension("sha256"),
            image,
            unpacked,
            unpacked_executable,
        }
    }

    pub async fn upload_as_ci_artifact(&self) -> Result {
        if is_in_env() {
            upload_compressed_directory(&self.unpacked, format!("ide-unpacked-{TARGET_OS}"))
                .await?;
            upload_single_file(&self.image, format!("ide-{TARGET_OS}")).await?;
            upload_single_file(&self.image_checksum, format!("ide-{TARGET_OS}")).await?;
        } else {
            info!("Not in the CI environment, will not upload the artifacts.")
        }
        Ok(())
    }

    pub fn start_unpacked(
        &self,
        extra_ide_options: impl IntoIterator<Item: AsRef<OsStr>>,
    ) -> Command {
        let application_path = self.unpacked.join(&self.unpacked_executable);
        let mut command = if TARGET_OS == OS::MacOS {
            let mut ret = Command::new("open");
            ret.arg(application_path);
            ret
        } else {
            Command::new(application_path)
        };
        command.args(extra_ide_options);
        command
    }
}

#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct BuildInput {
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub version:         Version,
    #[derivative(Debug = "ignore")]
    pub project_manager: BoxFuture<'static, Result<crate::project::backend::Artifact>>,
    #[derivative(Debug = "ignore")]
    pub gui:             BoxFuture<'static, Result<crate::project::gui::Artifact>>,
    pub electron_target: Option<String>,
}

#[derive(Clone, Debug)]
pub enum OutputPath {
    /// The job must place the artifact under given path.
    Required(PathBuf),
    /// THe job may place the artifact anywhere, though it should use the suggested path if it has
    /// no "better idea" (like reusing existing cache).
    Suggested(PathBuf),
    /// The job is responsible for finding a place for artifacts.
    Whatever,
}


#[derive(Clone, Copy, Debug)]
pub struct Ide {
    pub target_os:   OS,
    pub target_arch: Arch,
}

impl Ide {
    pub fn build(
        &self,
        context: &Context,
        input: BuildInput,
        output_path: impl AsRef<Path> + Send + Sync + 'static,
    ) -> BoxFuture<'static, Result<Artifact>> {
        let BuildInput { version, project_manager, gui, electron_target } = input;
        let ide_desktop = ide_desktop_from_context(context);
        let target_os = self.target_os;
        let target_arch = self.target_arch;
        async move {
            let (gui, project_manager) = try_join!(gui, project_manager)?;
            ide_desktop
                .dist(&gui, &project_manager, &output_path, target_os, electron_target)
                .await?;
            Ok(Artifact::new(target_os, target_arch, &version, output_path))
        }
        .boxed()
    }
}

// impl IsTarget for Ide {
//     type BuildInput = BuildInput;
//     type Output = Artifact;
//
//     fn artifact_name(&self) -> &str {
//         // Version is not part of the name intentionally. We want to refer to PM bundles as
//         // artifacts without knowing their version.
//         static NAME: LazyLock<String> = LazyLock::new(|| format!("gui-{}", TARGET_OS));
//         &*NAME
//     }
//
//     fn build(
//         &self,
//         input: Self::BuildInput,
//         output_path: impl AsRef<Path> + Send + Sync + 'static,
//     ) -> BoxFuture<'static, Result<Self::Output>> {
//         let ide_desktop = crate::ide::web::IdeDesktop::new(&input.repo_root.app.ide_desktop);
//         async move {
//             let (gui, project_manager) = try_join(input.gui, input.project_manager).await?;
//             ide_desktop.dist(&gui, &project_manager, &output_path).await?;
//             Ok(Artifact::new(&input.version, output_path.as_ref()))
//         }
//         .boxed()
//     }
// }
