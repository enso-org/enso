use crate::prelude::*;

use native_windows_gui::NativeUi;
use std::sync::mpsc::Receiver;



extern crate native_windows_gui as nwg;
use crate::win::access_payload;
use crate::win::config;
use crate::win::get_install_dir;
use crate::win::spawn_installer_thread;
use crate::InstallerUpdate;


pub mod ui;

/// The number of ticks the progress is divided into.
pub const PROGRESS_BAR_TICKS: u32 = 1000;

/// The installer's UI application.
///
/// This struct holds all the UI elements and the logic to drive the installation process.
/// The design follows the pattern recommended by the `native-windows-gui` library.
// Note: we do not use the derive API, as:
// - it does not support the `FlexboxLayout`,
// - is generally more trouble than it's worth.
#[derive(Default)]
#[allow(missing_debug_implementations)]
pub struct InstallerApp {
    /// Path to the log file. We use it to point the user to the log file in case of an error.
    pub logfile:          PathBuf,
    /// Title used by the main window and some of the dialogs.
    pub window_title:     String,
    /// The main window of the installer.
    pub window:           nwg::Window,
    /// The main window's layout. Column with the top layout and the progress bar.
    pub layout:           nwg::FlexboxLayout,
    /// Row with the Enso icon and the label.
    pub top_layout:       nwg::FlexboxLayout,
    /// The Enso icon (displayed left to the label).
    pub image:            nwg::ImageFrame,
    /// The label that shows the current stage of the installation.
    pub label:            nwg::Label,
    /// The progress bar that shows the overall installation progress.
    pub progress_bar:     nwg::ProgressBar,
    /// Handle to the embedded resources, such as the [`InstallerApp::enso_icon`].
    pub embed:            nwg::EmbedResource,
    /// The icon handle that is displayed by the [`InstallerApp::image`].
    pub enso_icon:        nwg::Icon,
    /// The timer we use to drive the [`InstallerApp::tick`] method.
    ///
    /// Note that despite the name, the `AnimationTimer` is recommended to be used as a total
    /// replacement for the `Timer`.
    pub timer:            nwg::AnimationTimer,
    /// Facilitates communication from the "installer backend" thread to the UI thread.
    pub backend_receiver: std::cell::RefCell<Option<Receiver<InstallerUpdate>>>,
    /// Handle to the thread that runs the installation logic.
    pub backend_thread:   std::cell::RefCell<Option<std::thread::JoinHandle<Result>>>,
    /// Result of the installation process.
    ///
    /// This should be filled by the UI thread before breaking the event loop.
    pub result:           std::cell::RefCell<Option<Result>>,
    /// Path to the installed application executable.
    ///
    /// After a successful installation, this is used to launch the application.
    pub installed_app:    std::cell::RefCell<PathBuf>,
}

impl InstallerApp {
    /// Create a new instance of the installer application.
    ///
    /// This includes setting up logging.
    #[allow(clippy::new_ret_no_self)] // This follow the pattern advertised by the NWG crate.
    pub fn new() -> Result<ui::Ui> {
        let window_title =
            format!("{} installer", enso_install::sanitized_electron_builder_config().product_name);
        let dialog_title = window_title.clone();
        let logfile =
            enso_install::win::ui::setup_logging_or_fatal(env!("CARGO_PKG_NAME"), &window_title);
        let logfile_copy = logfile.clone();

        // We intercept all the errors that can occur during the initialization of the UI.
        let result = move || -> Result<ui::Ui> {
            ui::init_global()?;
            let app = InstallerApp { logfile, window_title, ..Default::default() };
            InstallerApp::build_ui(app).context("Failed to build UI")
        }();
        if let Err(err) = &result {
            // We use "error" rather than "fatal", because "fatal" panics as soon as the error
            // dialog is closed. And we still want to open the logs.
            ui::error_message(&dialog_title, &format!("Installer failed to start: {err:?}"));
            let _ = ide_ci::programs::explorer::show_selected(logfile_copy);
        }
        result
    }

    /// Runs the installation.
    ///
    /// # Panics
    /// In case of failure, shows an error message and opens the log file - then panics.
    pub fn run(&self) {
        let result = || -> Result {
            let config = config::fill_config()?;
            let install_dir = get_install_dir(&config.pretty_name)?;
            let payload = access_payload()?;
            let (handle, receiver) = spawn_installer_thread(&install_dir, payload, config.clone())?;
            self.backend_receiver.borrow_mut().replace(receiver);
            self.backend_thread.borrow_mut().replace(handle);
            *self.installed_app.borrow_mut() = install_dir.join(&config.executable_filename);
            Ok(())
        }()
        .context("Failed to start installation.");
        if let Err(err) = result {
            self.fail_installation(err);
        } else {
            debug!("Starting event loop");
            nwg::dispatch_thread_events();
            debug!("Event loop finished");
        }
    }

    /// Handle a single event dispatched by the event loop.
    pub fn handle_ui_event(
        &self,
        event: nwg::Event,
        evt_data: nwg::EventData,
        handle: nwg::ControlHandle,
    ) {
        match event {
            nwg::Event::OnTimerTick =>
                if handle == self.timer {
                    self.tick();
                },
            nwg::Event::OnWindowClose => {
                // Prevent manual closing of the window. Installation should not be interrupted.
                if let nwg::EventData::OnWindowClose(close_data) = evt_data {
                    close_data.close(false);
                }
            }
            _ => {}
        }
    }


    /// Stop the event loop due to an installation error.
    ///
    /// Shows the dialog with the error message and points the user to the log file.
    pub fn fail_installation(&self, error: anyhow::Error) {
        let msg = format!("{error:?}");
        let _ = self.result.borrow_mut().replace(Result::Err(error));
        self.label.set_text("Installation failed.");
        self.progress_bar.set_state(nwg::ProgressBarState::Error);
        self.progress_bar.set_pos(100);

        error!(msg);
        info!("Showing modal error message.");
        nwg::modal_error_message(&self.window, &self.window_title, &msg);
        info!("Stopping the event loop due to an error.");
        nwg::stop_thread_dispatch();
        info!("Showing the log file in the file explorer.");
        let _ = ide_ci::programs::explorer::show_selected(&self.logfile);
    }

    /// Mark installation as successful and stop the event loop.
    pub fn succeed_installation(&self) {
        info!("Installation completed successfully.");
        let _ = self.result.borrow_mut().replace(Ok(()));
        self.label.set_text("Installation completed successfully.");
        nwg::stop_thread_dispatch();
        self.window.close();

        let installed_app = &self.installed_app.borrow();
        info!("Starting the installed application: {}", installed_app.display());
        let _ = Command::new(&**installed_app).spawn().inspect_err(|err| {
            // We won't stop the whole world if we can't start the installed application.
            // Still, we should leave some trace of what happened.
            error!("Failed to start the installed application: {err:?}");
        });
    }

    /// Method called by the timer to update the UI.
    ///
    /// It pulls updates from the installer backend and updates the UI accordingly.
    pub fn tick(&self) {
        let Ok(installer_state) = self.backend_receiver.try_borrow_mut() else {
            // If the receiver is already borrowed, it means that we have re-entered this method
            // from the failure dialog event loop. In such case we do nothing.
            return;
        };

        if let Some(receiver) = installer_state.deref() {
            loop {
                if self.result.borrow().is_some() {
                    // If the installation has already finished, we don't need to do anything.
                    return;
                }

                match receiver.try_recv() {
                    Ok(update) => {
                        info!("Update: {:?}", update);
                        match update {
                            InstallerUpdate::Progress(progress) => {
                                let new_ticks = (progress * PROGRESS_BAR_TICKS as f64) as u32;
                                self.progress_bar.set_pos(new_ticks);
                            }
                            InstallerUpdate::Stage(stage) => {
                                self.label.set_text(&stage);
                            }
                            InstallerUpdate::Finished(result) => {
                                if let Err(err) = result {
                                    self.fail_installation(err);
                                } else {
                                    self.succeed_installation();
                                }
                                break;
                            }
                        }
                    }
                    Err(std::sync::mpsc::TryRecvError::Empty) => break,
                    Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                        // We expect to receive `InstallerUpdate::Finished` before the channel is
                        // closed. Otherwise, it means that the backend thread has crashed.
                        let err =
                            anyhow!("The installer backend thread has unexpectedly disconnected.");
                        self.fail_installation(err);
                        break;
                    }
                }
            }
        }
    }
}
