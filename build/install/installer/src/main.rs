// #![windows_subsystem = "windows"]
/*!
A very simple application that shows your name in a message box.
Unlike `basic_d`, this example uses layout to position the controls in the window
 */


extern crate native_windows_derive as nwd;
extern crate native_windows_gui as nwg;


use anyhow::Context;
use enso_install_config::ENSO_ICON_ID;
use enso_installer::InstallerUpdate;
use nwd::NwgUi;
use nwg::NativeUi;
use std::ops::Deref;
use tracing::info;

#[derive(Default, NwgUi)]
#[allow(missing_debug_implementations)]
pub struct BasicApp {
    #[nwg_control(position: (300, 300), title: "Basic example",
    flags: "WINDOW|VISIBLE", icon: Some(&data.enso_icon))]
    // #[nwg_events( OnWindowClose: [BasicApp::say_goodbye] )]
    window: nwg::Window,

    #[nwg_layout(parent: window, spacing: 1)]
    grid: nwg::GridLayout,

    #[nwg_control(icon: Some(&data.enso_icon), size: (64,64))]
    #[nwg_layout_item(layout: grid, row: 0, col: 0)]
    image: nwg::ImageFrame,

    #[nwg_control(text: "Preparing the installer...")]
    #[nwg_layout_item(layout: grid, row: 1, col: 0)]
    label: nwg::Label,

    #[nwg_control(step: 1, range: 0..100)]
    #[nwg_layout_item(layout: grid, row: 2, col: 0)]
    progress_bar: nwg::ProgressBar,

    #[nwg_resource]
    embed: nwg::EmbedResource,

    #[nwg_resource(source_embed: Some(&data.embed), source_embed_str: Some(ENSO_ICON_ID), strict: false, size: Some((256, 256)))]
    enso_icon: nwg::Icon,

    #[nwg_control(parent: window, interval: std::time::Duration::from_millis(100), active: true)]
    #[nwg_events(OnTimerTick: [BasicApp::tick])]
    timer: nwg::AnimationTimer,

    installer_state: std::cell::RefCell<Option<std::sync::mpsc::Receiver<InstallerUpdate>>>,
}

impl BasicApp {
    fn tick(&self) {
        let installer_state = self.installer_state.borrow();
        if let Some(receiver) = installer_state.deref() {
            if let Ok(update) = receiver.try_recv() {
                info!("Update: {:?}", update);
                match update {
                    InstallerUpdate::Progress(progress) => {
                        self.progress_bar.set_pos((progress * 100.0) as u32);
                    }
                    InstallerUpdate::Stage(stage) => {
                        self.label.set_text(&stage);
                    }
                    InstallerUpdate::Finished(result) => {
                        // self.label.set_text(&format!("Done: {result:?}"));

                        if let Err(err) = result {
                            self.label.set_text("Installation failed.");
                            self.progress_bar.set_state(nwg::ProgressBarState::Error);
                            self.progress_bar.set_pos(100);
                            nwg::modal_error_message(
                                self.window.handle,
                                "Installation error",
                                &format!("The installation has failed: {err}"),
                            );
                        } else {
                            self.label.set_text("Installation complete.");
                        }

                        // Close window and stop the program.
                        nwg::stop_thread_dispatch();
                        self.window.close();
                    }
                }
            }
        }
    }
}

pub fn setup_logging() -> ide_ci::prelude::Result<std::path::PathBuf> {
    use ide_ci::log::*;
    use tracing_subscriber::prelude::*;
    // Generate filename based on the current time.
    let crate_name = env!("CARGO_PKG_NAME");
    let timestamp = chrono::Local::now().format("%Y-%m-%d-%H-%M-%S");
    let filename = format!("{crate_name}-{timestamp}.log");
    let temp_location = std::env::temp_dir();
    let log_file = temp_location.join(filename);
    let file = ide_ci::fs::create(&log_file)?;
    let registry = tracing_subscriber::Registry::default()
        .with(GlobalFilteringLayer)
        .with(stderr_log_layer())
        .with(file_log_layer(file));

    tracing::subscriber::set_global_default(registry)
        .context("Failed to set global default subscriber.")?;
    Ok(log_file)
}

fn main() -> enso_build_base::prelude::Result {
    use enso_install::prelude::*;

    let logfile = crate::setup_logging()?;
    info!("Logging to: {}", logfile.display());

    // let config = enso_install_config::electron_builder_config_from_env()?;
    // let config = enso_install::sanitized_electron_builder_config();
    let config = enso_installer::win::fill_config()?;
    let archive = enso_install::get_package_payload()?;

    nwg::init().expect("Failed to init Native Windows GUI");
    let mut font = nwg::Font::default();
    nwg::FontBuilder::new()
        .family("Segoe UI")
        .size(46)
        .build(&mut font)
        .expect("Failed to create default font");
    nwg::Font::set_global_default(Some(font));
    let app = BasicApp::default();

    let app = BasicApp::build_ui(app).expect("Failed to build UI");

    app.window.set_text(&format!("{} installer", &config.pretty_name));

    let install_dir = enso_install::win::user_program_files()?.join(&config.pretty_name);
    let (handle, receiver) =
        enso_installer::win::spawn_installer_thread(&install_dir, archive, config.clone());
    *app.installer_state.borrow_mut() = Some(receiver);

    let installed_app = install_dir.join(&config.executable_filename);


    debug!("Starting event loop");
    nwg::dispatch_thread_events();
    debug!("Event loop finished");
    let result = handle.join().expect("Failed to join the installer thread");
    if let Err(err) = &result {
        nwg::modal_error_message(
            app.window.handle,
            "Installation error",
            &format!("The installation has failed: {err}"),
        );
        ide_ci::programs::explorer::show_selected(&logfile)?;
        result
    } else {
        info!("Starting installed application: {}", installed_app.display());
        Command::new(installed_app).spawn().expect("Failed to start the installed application");
        Ok(())
    }
}


// // === Features ===
// // #![feature(core_intrinsics)]
// // === Standard Linter Configuration ===
// #![deny(non_ascii_idents)]
// #![warn(unsafe_code)]
// #![allow(clippy::bool_to_int_with_if)]
// #![allow(clippy::let_and_return)]
//
// use ide_ci::prelude::*;
//
// use enso_install_config::INSTALLER_PAYLOAD_ID;
//
//
// // ==============
// // === Export ===
// // ==============
//
// #[cfg(windows)]
// pub mod win;
//
//
//
// #[tokio::main]
// async fn main() -> Result {
//     setup_logging()?;
//     let lock = enso_install::lock()?;
//     let _guard = lock.lock()?;
//     #[cfg(windows)]
//     {
//         let config = win::fill_config()?;
//         let archive = enso_install::win::resource::get_binary(INSTALLER_PAYLOAD_ID)?;
//         let install_dir = enso_install::win::user_program_files()?.join(&config.pretty_name);
//         win::install(install_dir, archive, &config)?;
//     }
//     #[cfg(not(windows))]
//     {
//         bail!("Unsupported platform.");
//     }
//     Ok(())
// }
//
//
// #[cfg(test)]
// mod tests {
//     #[test]
//     fn uninstaller_name_matches() {
//         assert_eq!(enso_install_config::INSTALLER_NAME, env!("CARGO_PKG_NAME"));
//     }
// }
