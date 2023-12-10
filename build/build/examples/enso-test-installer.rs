use enso_build::prelude::*;

const INSTALLER_PATH: &str = r"H:\NBO\enso2\dist\ide\enso-win-2023.2.1-dev.exe";

/// Path to the NSIS-generated uninstaller executable.
const UNINSTALLER_PATH: &str = r"C:\Users\mwurb\AppData\Local\Programs\Enso\Uninstall Enso.exe";

#[instrument]
async fn update_installer() -> Result {
    use ide_ci::programs::Cmd;
    Cmd.run_command()?
        .current_dir(r"H:\NBO\electron-builder")
        .args(&["pnpm", "run", "compile"])
        .run_ok()
        .await?;

    Cmd.run_command()?
        .current_dir(r"H:\NBO\electron-builder")
        .env("DEBUG", "electron-builder*")
        .args(&[
            "npx",
            "electron-builder",
            "--config",
            r"H:\NBO\enso2\dist\ide\builder-effective-config-nsis.yaml",
            "--projectDir",
            r"H:\NBO\enso2\app\ide-desktop\lib\client",
            "--publish",
            "never",
        ])
        .run_ok()
        .await
}

#[instrument]
async fn uninstall() -> Result {
    let tempdir = tempfile::tempdir()?;

    let uninstaller_copy = ide_ci::fs::copy_to(UNINSTALLER_PATH, tempdir.path())?;

    let mut cmd = Command::new(uninstaller_copy);
    cmd.arg("/S"); // Silent mode (no UI)
    cmd.arg(format!("/D={}", UNINSTALLER_PATH.try_parent()?.display()));
    cmd.run_ok().await
}

#[instrument]
async fn install(silent: bool) -> Result {
    let mut cmd = Command::new(INSTALLER_PATH);
    if silent {
        // Silent mode (no UI)
        cmd.arg("/S");
    }
    cmd.run_ok().await
}

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;

    update_installer().await?;
    if ide_ci::fs::metadata(r"C:\Users\mwurb\AppData\Local\Programs\Enso").is_ok() {
        uninstall().await?;
    } else {
        info!("Enso is not installed, skipping uninstallation.");
    }
    install(true).await?;
    install(false).await?;

    Ok(())
}
