// #![windows_subsystem = "windows"]
/*!
A very simple application that shows your name in a message box.
Unlike `basic_d`, this example uses layout to position the controls in the window
 */


extern crate native_windows_derive as nwd;
extern crate native_windows_gui as nwg;

use enso_install_config::ENSO_ICON_ID;
use nwd::NwgUi;
use nwg::NativeUi;


#[derive(Default, NwgUi)]
pub struct BasicApp {
    #[nwg_control(size: (300, 115), position: (300, 300), title: "Basic example", flags: "WINDOW|VISIBLE")]
    #[nwg_events( OnWindowClose: [BasicApp::say_goodbye] )]
    window: nwg::Window,

    #[nwg_layout(parent: window, spacing: 1)]
    grid: nwg::GridLayout,

    #[nwg_control(text: "Say my name")]
    #[nwg_layout_item(layout: grid, row: 0, col: 0)]
    label: nwg::Label,

    #[nwg_control(text: "Heisenberg", focus: true)]
    #[nwg_layout_item(layout: grid, row: 1, col: 0)]
    name_edit: nwg::TextInput,

    #[nwg_control(text: "Say my name")]
    #[nwg_layout_item(layout: grid, col: 0, row: 2, row_span: 2)]
    #[nwg_events( OnButtonClick: [BasicApp::say_hello] )]
    hello_button: nwg::Button,
}

impl BasicApp {
    fn say_hello(&self) {
        nwg::modal_info_message(&self.window, "Hello", &format!("Hello {}", self.name_edit.text()));
    }

    fn say_goodbye(&self) {
        nwg::modal_info_message(
            &self.window,
            "Goodbye",
            &format!("Goodbye {}", self.name_edit.text()),
        );
        nwg::stop_thread_dispatch();
    }
}

fn main() -> enso_build_base::prelude::Result {
    use enso_install::prelude::*;
    setup_logging()?;

    nwg::init().expect("Failed to init Native Windows GUI");
    nwg::Font::set_global_family("Segoe UI").expect("Failed to set default font");
    let _app = BasicApp::build_ui(Default::default()).expect("Failed to build UI");
    //let embed = nwg::EmbedResource::default();
    let aaa = nwg::EmbedResource::load(None)?;
    let icon = aaa.icon_str(ENSO_ICON_ID, None);
    debug!("icon: {:?}", icon.is_some());


    nwg::dispatch_thread_events();
    Ok(())
}
