// #![windows_subsystem = "windows"]
/*!
A very simple application that shows your name in a message box.
Unlike `basic_d`, this example uses layout to position the controls in the window
 */


extern crate native_windows_derive as nwd;
extern crate native_windows_gui as nwg;


use enso_install_config::ENSO_ICON_ID;
use enso_install_config::INSTALLER_PAYLOAD_ID;
use enso_installer::InstallerUpdate;
use nwd::NwgUi;
use nwg::NativeUi;
use std::ops::Deref;
use std::sync::Arc;
use std::sync::Mutex;
use tracing::info;
use windows::Win32::Foundation::HWND;

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
    name_edit: nwg::ProgressBar,

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
                        self.name_edit.set_pos((progress * 100.0) as u32);
                    }
                    InstallerUpdate::Stage(stage) => {
                        self.label.set_text(&stage);
                    }
                    InstallerUpdate::Finished(result) => {
                        self.label.set_text(&format!("Done: {result:?}"));
                        // Close window and stop the program.
                        nwg::stop_thread_dispatch();
                        self.window.close();
                    }
                }
            }
            // let lock = self.installer_state.lock().unwrap();
            // let text = &lock.text;
            // if self.label.text() != *text {
            //     info!("Tick: {}", text);
            //     self.label.set_text(&text);
            // }
            // let progress: u32 = (lock.progress * 100.0).floor() as u32;
            // if self.name_edit.pos() != progress {
            //     self.name_edit.set_pos(progress);
            // }
            //
            // // Send custom app-specific event into the event loop.
            // unsafe {
            //     winapi::um::winuser::PostMessageW(self.window.handle.hwnd().unwrap(), 0x8000, 0,
            // 0);
        }
    }
}
//
// #[derive(Clone, Debug, Default)]
// pub struct InstallerState {
//     pub text:     String,
//     /// Overall progress of the installation process (0.0 - 1.0).
//     pub progress: f32,
//
//     pub error: String,
// }
//
// impl InstallerState {
//     pub fn new() -> Arc<Mutex<Self>> {
//         let ret = Self::default();
//         Arc::new(Mutex::new(ret))
//     }
//
//     pub fn spawn(state: &Arc<Mutex<Self>>) {
//         let state = state.clone();
//         std::thread::spawn(move || {
//             let mut i = 0;
//             loop {
//                 std::thread::sleep(std::time::Duration::from_secs(1));
//                 let mut state = state.lock().unwrap();
//                 state.text = format!("Progress: {}", i);
//                 state.progress = i as f32 / 100.0;
//                 i += 1;
//             }
//         });
//     }
// }

fn main() -> enso_build_base::prelude::Result {
    use enso_install::prelude::*;
    setup_logging()?;

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
    //
    // app.label.set_text("Foo b\r\nBar\n✅");
    // info!("Label size: {:?}", app.label.size());

    // InstallerState::spawn(&app.installer_state);

    let install_dir = enso_install::win::user_program_files()?.join(&config.pretty_name);
    let (handle, receiver) =
        enso_installer::win::spawn_installer_thread(&install_dir, archive, config.clone());
    *app.installer_state.borrow_mut() = Some(receiver);

    let installed_app = install_dir.join(&config.executable_filename);

    // app.label.set_size(64, 64);

    // let handle_events = move |evt, evt_data, handle| {
    //     trace!("Event: {:?}", evt);
    // };
    // nwg::full_bind_event_handler(&app.window.handle, handle_events);


    // Post message using windows crate
    // let hwnd = app.window.handle.hwnd().unwrap();
    // second_thread(hwnd);
    // unsafe {
    //     winapi::um::winuser::PostMessageW(hwnd, 0x8001, 0, 0);
    // }
    // let hwnd = app.window.handle.hwnd();
    // unsafe {
    //     windows::Win32::UI::WindowsAndMessaging::PostMessageW(hwnd, 0x8000, 0, 0);
    // };


    debug!("Starting event loop");
    nwg::dispatch_thread_events();
    debug!("Event loop finished");

    info!("Starting installed application: {}", installed_app.display());
    Command::new(installed_app).spawn().expect("Failed to start the installed application");

    Ok(())
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
