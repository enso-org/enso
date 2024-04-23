#![windows_subsystem = "windows"]
/*!ĀĀ
A very simple application that shows your name in a message box.
Unlike `basic_d`, this example uses layout to position the controls in the window
 */


extern crate native_windows_gui as nwg;

use enso_installer::prelude::*;

use anyhow::Context;
use enso_install_config::ENSO_ICON_ID;
use enso_installer::InstallerUpdate;
use nwg::stretch::geometry::Size;
use nwg::stretch::style::Dimension;
use nwg::stretch::style::FlexDirection;

use enso_install::win::local_app_data;
use ide_ci::log::file_log_layer;
use ide_ci::log::stderr_log_layer;
use ide_ci::log::GlobalFilteringLayer;
use nwg::NativeUi;

/// Size for the Enso icon displayed in the window next to the text label.
pub const ICON_SIZE: u32 = 32;

/// Default font used in the application.
///
/// Segoe is the default font in Windows since Vista and we can reasonably expect it to be
/// available.
pub const DEFAULT_FONT: &str = "Segoe UI";

#[derive(Default)]
#[allow(missing_debug_implementations)]
pub struct InstallerApp {
    pub window:          nwg::Window,
    pub layout:          nwg::FlexboxLayout,
    pub top_layout:      nwg::FlexboxLayout,
    pub image:           nwg::ImageFrame,
    pub label:           nwg::Label,
    pub progress_bar:    nwg::ProgressBar,
    /// Handle to the embedded resources, such as the [`InstallerApp::enso_icon`].
    pub embed:           nwg::EmbedResource,
    /// The icon handle that is displayed by the [`InstallerApp::image`].
    pub enso_icon:       nwg::Icon,
    /// The timer we use to drive the [`InstallerApp::tick`] method.
    ///
    /// Note that despite the name, the `AnimationTimer` is recommended to be used as a total
    /// replacement for the `Timer`.
    pub timer:           nwg::AnimationTimer,
    pub installer_state: std::cell::RefCell<Option<std::sync::mpsc::Receiver<InstallerUpdate>>>,
}

impl InstallerApp {
    fn tick(&self) {
        let installer_state = self.installer_state.borrow();
        if let Some(receiver) = installer_state.deref() {
            while let Ok(update) = receiver.try_recv() {
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


mod ui {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    pub struct Ui {
        inner:           Rc<InstallerApp>,
        default_handler: RefCell<Option<nwg::EventHandler>>,
    }

    impl Debug for Ui {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Ui").finish()
        }
    }

    impl NativeUi<Ui> for InstallerApp {
        fn build_ui(mut data: InstallerApp) -> std::result::Result<Ui, nwg::NwgError> {
            nwg::EmbedResource::builder().build(&mut data.embed)?;
            nwg::Icon::builder()
                .source_embed(Some(&data.embed))
                .source_embed_str(Some(ENSO_ICON_ID))
                .strict(false)
                .size(Some((ICON_SIZE, ICON_SIZE)))
                .build(&mut data.enso_icon)?;
            nwg::Window::builder()
                .position((300, 300))
                .title("Installer")
                .flags(nwg::WindowFlags::WINDOW | nwg::WindowFlags::VISIBLE)
                .icon(Some(&data.enso_icon))
                .size((400, 100))
                .build(&mut data.window)?;
            nwg::ImageFrame::builder()
                .icon(Some(&data.enso_icon))
                .size((ICON_SIZE as i32, ICON_SIZE as i32))
                .parent(&data.window)
                .build(&mut data.image)?;
            nwg::Label::builder()
                .text("Preparing the installer...")
                .parent(&data.window)
                .build(&mut data.label)?;
            nwg::ProgressBar::builder()
                .step(1)
                .range(0..100)
                .parent(&data.window)
                .build(&mut data.progress_bar)?;
            nwg::AnimationTimer::builder()
                .parent(&data.window)
                .interval(std::time::Duration::from_millis(100))
                .active(true)
                .build(&mut data.timer)?;
            let inner = Rc::new(data);
            let ui = Ui { inner: inner.clone(), default_handler: Default::default() };

            let evt_ui = Rc::downgrade(&inner);
            let handle_events = move |_evt, _evt_data, _handle| {
                if let Some(evt_ui) = evt_ui.upgrade() {
                    match _evt {
                        nwg::Event::OnTimerTick =>
                            if &_handle == &evt_ui.timer {
                                InstallerApp::tick(&evt_ui)
                            },
                        _ => {}
                    }
                }
            };

            let event_handler = nwg::full_bind_event_handler(&ui.window.handle, handle_events);
            *ui.default_handler.borrow_mut() = Some(event_handler);

            nwg::FlexboxLayout::builder()
                .parent(&ui.window)
                .flex_direction(FlexDirection::Row)
                .child(&ui.image)
                .child_size(Size {
                    width:  Dimension::Points(ICON_SIZE as f32),
                    height: Dimension::Points(ICON_SIZE as f32),
                })
                .child(&ui.label)
                .child_flex_grow(1.0)
                .child_size(Size { width: Dimension::Auto, height: Dimension::Auto })
                .build_partial(&ui.top_layout)?;
            nwg::FlexboxLayout::builder()
                .parent(&ui.window)
                .flex_direction(FlexDirection::Column)
                .child_layout(&ui.top_layout)
                .child_size(Size { width: Dimension::Percent(1.0), height: Dimension::Auto })
                .child(&ui.progress_bar)
                .child_size(Size {
                    width:  Dimension::Percent(0.9),
                    height: Dimension::Points(16.0),
                })
                .build(&ui.layout)?;
            Ok(ui)
        }
    }

    impl Drop for Ui {
        /// To make sure that everything is freed without issues, the default handler must be
        /// unbound.
        fn drop(&mut self) {
            let handler = self.default_handler.borrow();
            if let Some(handler) = handler.as_ref() {
                nwg::unbind_event_handler(handler);
            }
        }
    }

    impl Deref for Ui {
        type Target = InstallerApp;
        fn deref(&self) -> &InstallerApp {
            &self.inner
        }
    }
}

/// Setup loggin gfor the installer. It logs both to stderr and to a file in the temp directory.
pub fn setup_logging() -> Result<PathBuf> {
    use tracing_subscriber::prelude::*;
    // Generate filename based on the current time.
    let crate_name = env!("CARGO_PKG_NAME");
    let timestamp = chrono::Local::now().format("%Y-%m-%d %H-%M-%S");
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

/// Get the default installation directory.
pub fn get_install_dir(pretty_name: &str) -> Result<PathBuf> {
    let programs_dir = enso_install::win::user_program_files()
        .or_else(|e| {
            warn!("Failed to get the user's program files directory: {e:?}");
            // The Windows might refuse to provide the user's program files directory in some cases,
            // like brand-new user accounts that don't have the directory created yet.
            // Thus, we fall back to the default location, as documented in:
            // https://learn.microsoft.com/en-us/windows/win32/shell/knownfolderid
            Result::Ok(local_app_data()?.join("Programs"))
        })
        .context("Failed to get the user's program files directory")?;
    Ok(programs_dir.join(pretty_name))
}

fn main() -> Result {
    let logfile = setup_logging()?;
    info!("Logging to: {}", logfile.display());

    let config = enso_installer::win::fill_config()?;
    let payload = enso_installer::Payload {
        data:     enso_install::get_package_payload()?,
        metadata: enso_installer::access_payload_metadata(),
    };

    nwg::init().expect("Failed to init Native Windows GUI");
    let mut font = nwg::Font::default();
    nwg::FontBuilder::new()
        .family(DEFAULT_FONT)
        .size(16)
        .build(&mut font)
        .expect("Failed to create default font");
    nwg::Font::set_global_default(Some(font));
    let app = InstallerApp::default();

    let app = InstallerApp::build_ui(app).expect("Failed to build UI");

    app.window.set_text(&format!("{} installer", &config.pretty_name));

    let install_dir = get_install_dir(&config.pretty_name)?;
    let (handle, receiver) =
        enso_installer::win::spawn_installer_thread(&install_dir, payload, config.clone());
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

#[cfg(test)]
mod tests {
    #[test]
    fn installer_name_matches() {
        assert_eq!(enso_install_config::INSTALLER_NAME, env!("CARGO_PKG_NAME"));
    }
}
