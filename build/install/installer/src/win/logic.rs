//! Code that performs the installation.

use crate::prelude::*;

use crate::win::config::Config;
use crate::Payload;

use enso_install_config::UNINSTALLER_NAME;
use flate2::read::GzDecoder;



/// Register file extensions and their associations in the Windows registry.
pub fn register_file_associations(
    file_associations: &[(
        enso_install::win::prog_id::FileType,
        enso_install::win::prog_id::FileExtension,
    )],
) -> Result {
    for (file_type, file_extension) in file_associations {
        info!("Registering file extension '{}'.", file_extension.extension);
        file_extension.register()?;

        info!("Registering file type '{}'.", file_type.prog_id);
        file_type.register()?;
    }

    info!("Refreshing file associations in the shell.");
    enso_install::win::refresh_file_associations();

    Ok(())
}

/// Set application as the default handler for the given URL protocol, e.g. `enso://`.
///
/// This is necessary for the deep linking to work.
pub fn register_url_protocol(executable_path: &Path, protocol: &str) -> Result {
    let info = enso_install::win::prog_id::ProtocolInfo::new(protocol, executable_path);
    info.register()
}

/// Register the uninstaller in the Windows registry.
pub fn register_uninstaller(
    config: &Config,
    install_directory: &Path,
    uninstaller_path: &Path,
    installation_size_bytes: u64,
) -> Result {
    let mut uninstall_info = enso_install::win::uninstall::UninstallInfo::new(
        &config.pretty_name,
        uninstaller_path.as_str(),
    );
    uninstall_info.install_location = Some(install_directory.display().to_string());
    uninstall_info.install_date = Some(chrono::Local::now().to_string());
    uninstall_info.publisher = Some(config.publisher.clone());
    uninstall_info.display_icon = Some(uninstaller_path.display().to_string());
    uninstall_info.display_version = Some(config.version.to_string());
    uninstall_info.estimated_size_kib = Some((installation_size_bytes / 1024) as u32);
    uninstall_info.write_to_registry(&config.uninstall_key)?;
    Ok(())
}

/// Install Enso.
///
/// The archive payload is binary data of the tar.gz archive that contains the Enso app.
pub fn install_with_updates(
    install_location: &Path,
    payload: Payload,
    config: &Config,
    sender: &std::sync::mpsc::Sender<crate::InstallerUpdate>,
) -> Result {
    let send = |update| {
        info!("Sending update: {update:?}");
        let _ = sender.send(update);
    };
    let report_progress = |progress| {
        send(crate::InstallerUpdate::Progress(progress));
    };
    macro_rules! stage_at {
        ($progress:tt, $($arg:tt)*) => {
            send(crate::InstallerUpdate::Stage(format!($($arg)*)));
            send(crate::InstallerUpdate::Progress($progress));
        };
    }
    macro_rules! bail {
        ($($arg:tt)*) => {{
            let msg = format!($($arg)*);
            let err = anyhow::Error::msg(msg.clone());
            send(crate::InstallerUpdate::Finished(Err(err)));
            anyhow::bail!("{msg}");
        }};
    }

    // Only one installer / uninstaller can run at a time.
    let _guard = enso_install::locked_installation_lock()?;

    let enso_install_config::payload::Metadata { total_files, total_bytes } = *payload.metadata;
    stage_at!(0.00, "Checking disk space.");
    // TODO? A potential improvement would be to take account for previous installation size when
    //       performing the in-place update. Then the needed space would be the difference between
    //       the new and the old installation size.
    let per_file_overhead = 4096; // The default allocation unit size on NTFS.
    let space_required = total_bytes + (total_files * per_file_overhead);
    match check_disk_space(install_location, space_required) {
        Ok(Some(msg)) => bail!("{msg}"),
        Ok(None) => {} // Ok, enough space.
        Err(err) => {
            // We don't know, so let's just log the warning and try to carry on.
            warn!("Failed to check disk space: {err:?}");
        }
    }

    stage_at!(0.01, "Checking for running processes.");
    match enso_install::is_already_running(install_location, &[]) {
        Ok(Some(msg)) => bail!("{msg}"),
        Ok(None) => {} // Ok, no colliding processes.
        Err(err) => {
            // We don't know, so let's just log the warning and try to carry on.
            warn!("Failed to check for running processes: {err:?}");
        }
    }

    stage_at!(0.03, "Removing old installation files (if present).");
    ide_ci::fs::reset_dir(install_location)?;

    let executable_location = install_location.join(&config.executable_filename);

    // Extract the files.
    let decoder = GzDecoder::new(payload.data);
    let archive = tar::Archive::new(decoder);

    let extraction_progress_start = 0.06;
    let extraction_progress_step = 0.82;
    let mut files_extracted = 0;
    let mut bytes_extracted = 0;

    stage_at!(extraction_progress_start, "Extracting files.");
    let mut bytes_being_extracted = 0;
    let to_our_path = |entry: &tar::Entry<GzDecoder<&[u8]>>| -> Option<PathBuf> {
        // If we receive a new file, update the counters.
        files_extracted += 1;
        bytes_extracted += bytes_being_extracted;
        bytes_being_extracted = entry.header().size().unwrap_or(0);

        let files_ratio = (files_extracted as f64 / total_files as f64).min(1.0);
        let bytes_ratio = (bytes_extracted as f64 / total_bytes as f64).min(1.0);
        let extraction_progresss = (files_ratio + bytes_ratio) / 2.0;
        let progress = extraction_progress_start + extraction_progress_step * extraction_progresss;
        trace!("files_extracted: {files_extracted}/{total_files}, bytes_extracted: {bytes_extracted}/{total_bytes}, extraction_progresss: {extraction_progresss}, progress: {progress}");
        report_progress(progress);
        Some(install_location.join(entry.path().ok()?))
    };

    ide_ci::archive::tar::extract_files_sync(archive, to_our_path)?;
    // As we've been incrementing this values when extracting the next file, we need to cover the
    // last file.
    bytes_extracted += bytes_being_extracted;


    let post_extraction_progress = extraction_progress_start + extraction_progress_step;

    stage_at!(post_extraction_progress, "Registering file types.");
    register_file_associations(&config.file_associations)?;

    for protocol in &config.url_protocols {
        stage_at!(0.90, "Registering URL protocol '{protocol}'.");
        register_url_protocol(&executable_location, protocol)?;
    }

    stage_at!(0.92, "Registering the application path.");
    let app_paths_info = enso_install::win::app_paths::AppPathInfo::new(&executable_location);
    app_paths_info.write_to_registry()?;

    stage_at!(0.94, "Registering the uninstaller.");
    register_uninstaller(
        config,
        install_location,
        &install_location.join(UNINSTALLER_NAME.with_executable_extension()),
        bytes_extracted,
    )?;

    stage_at!(0.96, "Creating Start Menu entry.");
    enso_install::win::shortcut::Location::Menu
        .create_shortcut(&config.shortcut_name, &executable_location)?;

    stage_at!(0.98, "Creating Desktop shortcut.");
    enso_install::win::shortcut::Location::Desktop
        .create_shortcut(&config.shortcut_name, &executable_location)?;


    stage_at!(1.0, "Installation complete.");
    send(crate::InstallerUpdate::Finished(Ok(())));
    Ok(())
}

/// Check if there is enough disk space to install the application.
///
/// If the space is insufficient, returns an error message. If the space is sufficient, returns
/// `None`. If the necessary information cannot be obtained, returns an error.
///
/// Note that usually it is better to ignore the error than to fail the installation process. Not
/// knowing that the disk space is sufficient is not meaning that it is insufficient.
/// For example, we might be targetting a network path for which we cannot obtain the disk space.
pub fn check_disk_space(
    installation_directory: &Path,
    bytes_required: u64,
) -> Result<Option<String>> {
    use sysinfo::Disks;
    let disks = Disks::new_with_refreshed_list();
    // This should yield an absolute path, prefixed with the drive label.
    let path = installation_directory
        .canonicalize()
        // We use absolutize as a fallback, because canonicalize fails for non-existent paths. We
        // attempt to use canonicalize first, because it resolves symlinks.
        .or_else(|_| installation_directory.absolutize().map(PathBuf::from))?;

    // We need to remove the verbatim prefix (that canonicalize likes to add) in order to match the
    // disk list mount points format.
    let path = path.without_verbatim_prefix();
    let disk = disks
        .into_iter()
        .find(|disk| path.starts_with(disk.mount_point()))
        .context("No disk information found for the installation directory.")?;

    let required_space = bytesize::ByteSize(bytes_required);
    let free_space = bytesize::ByteSize(disk.available_space());

    if free_space < required_space {
        let msg = format!(
            "Not enough disk space on {} to install. Required: {:.2}, available: {:.2}.",
            disk.mount_point().display(),
            required_space,
            free_space
        );
        return Ok(Some(msg));
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    /// Test to manually check the generated disk space message.
    fn check_disk_space_test() -> Result {
        let my_path = ide_ci::env::current_dir()?;
        let r = check_disk_space(&my_path, 10_000_000_000_000);
        let _ = dbg!(r);
        Ok(())
    }

    #[test]
    #[ignore]
    /// Test to manually check the running processes.
    fn is_already_running_test() -> Result {
        setup_logging().ok();
        let install_path = crate::win::get_install_dir("Enso")?;
        let r = enso_install::is_already_running(&install_path, &[])?;
        dbg!(r);
        Ok(())
    }
}
