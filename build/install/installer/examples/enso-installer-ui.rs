// #![windows_subsystem = "windows"]
/*!
A very simple application that shows your name in a message box.
Unlike `basic_d`, this example uses layout to position the controls in the window
 */


extern crate native_windows_derive as nwd;
extern crate native_windows_gui as nwg;


use enso_install_config::ENSO_ICON_ID;
use enso_install_config::INSTALLER_PAYLOAD_ID;
use nwd::NwgUi;
use nwg::NativeUi;
use std::sync::Arc;
use std::sync::Mutex;
use tracing::info;
use windows::Win32::Foundation::HWND;

const ICON_SIZE: (u32, u32) = (128, 128);

#[derive(Default, NwgUi)]
#[allow(missing_debug_implementations)]
pub struct BasicApp {
    #[nwg_control(size: (640, 480), position: (300, 300), title: "Basic example", 
        flags: "WINDOW|VISIBLE", icon: Some(&data.enso_icon))]
    // #[nwg_events( OnWindowClose: [BasicApp::say_goodbye] )]
    window: nwg::Window,

    #[nwg_layout(parent: window, spacing: 1)]
    grid: nwg::GridLayout,

    #[nwg_control(icon: Some(&data.enso_icon), size: (128, 128))]
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

    #[nwg_resource(source_embed: Some(&data.embed), source_embed_str: Some(ENSO_ICON_ID), strict: false, size: Some(ICON_SIZE))]
    enso_icon: nwg::Icon,

    #[nwg_control(parent: window, interval: std::time::Duration::from_millis(1), active: true)]
    #[nwg_events(OnTimerTick: [BasicApp::tick])]
    timer: nwg::AnimationTimer,

    installer_state: Arc<InstallerState>,
}

impl BasicApp {
    fn tick(&self) {
        let text = self.installer_state.text.lock().unwrap();
        if self.label.text() != *text {
            info!("Tick: {}", text);
            self.label.set_text(&text);
        }
    }
}

#[derive(Debug, Default)]
pub struct InstallerState {
    pub text: Arc<Mutex<String>>,
}

impl InstallerState {
    pub fn new() -> Arc<Self> {
        let ret = Self { text: Arc::new(Mutex::new("".to_string())) };
        Arc::new(ret)
    }

    pub fn spawn(self: &Arc<Self>) {
        let state = self.clone();
        std::thread::spawn(move || {
            let mut i = 0;
            loop {
                std::thread::sleep(std::time::Duration::from_secs(1));
                let mut text = state.text.lock().unwrap();
                *text = format!("Progress: {}", i);
                i += 1;
            }
        });
    }
}

fn main() -> enso_build_base::prelude::Result {
    use enso_install::prelude::*;
    setup_logging()?;

    // let config = enso_install_config::electron_builder_config_from_env()?;
    let config = enso_install::sanitized_electron_builder_config();
    let archive = enso_install::get_package_payload()?;

    nwg::init().expect("Failed to init Native Windows GUI");
    let mut font = nwg::Font::default();
    nwg::FontBuilder::new()
        .family("Segoe UI")
        .size(46)
        .build(&mut font)
        .expect("Failed to create default font");
    nwg::Font::set_global_default(Some(font));
    let app = BasicApp { installer_state: InstallerState::new(), ..Default::default() };

    let app = BasicApp::build_ui(app).expect("Failed to build UI");

    app.window.set_text(&config.product_name);

    app.label.set_text("Foo b\r\nBar\n✅");
    info!("Label size: {:?}", app.label.size());

    app.installer_state.spawn();

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
    Ok(())
}
