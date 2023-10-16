use crate::prelude::*;

use crate::project::gui::ide_desktop_from_context;
use crate::project::gui::GuiBuildWithWatchedWasm;
use crate::project::Context;
use crate::project::Gui;
use crate::project::IsArtifact;
use crate::source::WatchTargetJob;

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
    pub fn new(
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

    pub async fn upload_as_ci_artifact(&self, prefix: impl AsRef<str>) -> Result {
        if is_in_env() {
            let prefix = prefix.as_ref();
            upload_compressed_directory(&self.unpacked, format!("{prefix}-unpacked-{TARGET_OS}"))
                .await?;
            let packed_artifact_name = format!("{prefix}-{TARGET_OS}");
            upload_single_file(&self.image, &packed_artifact_name).await?;
            upload_single_file(&self.image_checksum, &packed_artifact_name).await?;
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
pub struct BuildInput<GuiArtifact> {
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub version:         Version,
    #[derivative(Debug = "ignore")]
    pub project_manager: BoxFuture<'static, Result<crate::project::backend::Artifact>>,
    #[derivative(Debug = "ignore")]
    pub gui:             BoxFuture<'static, Result<GuiArtifact>>,
    pub electron_target: Option<String>,
    /// The name base used to generate CI run artifact names.
    pub artifact_name:   String,
}

#[derive(Clone, Copy, Debug)]
pub struct Ide {
    pub target_os:   OS,
    pub target_arch: Arch,
}

impl Default for Ide {
    fn default() -> Self {
        Self { target_os: TARGET_OS, target_arch: TARGET_ARCH }
    }
}

impl Ide {
    pub fn build(
        &self,
        context: &Context,
        input: BuildInput<impl IsArtifact>,
        output_path: impl AsRef<Path> + Send + Sync + 'static,
    ) -> BoxFuture<'static, Result<Artifact>> {
        let BuildInput { version, project_manager, gui, electron_target, artifact_name: _ } = input;
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

    /// Setup watch for IDE.
    ///
    /// This includes both WASM watcher and watcher for the web parts.
    pub fn watch(
        &self,
        context: &Context,
        gui_watch_job: WatchTargetJob<Gui>,
        get_project_manager: BoxFuture<'static, Result<crate::project::backend::Artifact>>,
        ide_options: Vec<String>,
    ) -> BoxFuture<'static, Result> {
        let ide_desktop = ide_desktop_from_context(context);
        let GuiBuildWithWatchedWasm { perhaps_watched_wasm, build_info, destination: _ } =
            Gui.perhaps_setup_wasm_watcher(context.clone(), gui_watch_job);
        async move {
            ide_desktop
                .watch(perhaps_watched_wasm, build_info, get_project_manager, ide_options)
                .await
        }
        .boxed()
    }
}
