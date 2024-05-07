//! The UI setup for the installer application.
//!
//! This is roughly what `native-windows-derive` would generate.

use crate::prelude::*;

use std::cell::RefCell;
use std::rc::Rc;



extern crate native_windows_gui as nwg;

use crate::win::app::InstallerApp;
use crate::win::app::PROGRESS_BAR_TICKS;
use enso_install_config::ENSO_ICON_ID;
use nwg::stretch::geometry::Size;
use nwg::stretch::style::Dimension;
use nwg::stretch::style::FlexDirection;
use nwg::NativeUi;


/// Size for the Enso icon displayed in the window next to the text label.
pub const ICON_SIZE: u32 = 32;

/// Default font used in the application.
///
/// Segoe UI is the default font in Windows since Vista and we can reasonably expect it to be
/// available.
pub const DEFAULT_FONT: &str = "Segoe UI";

pub struct Ui {
    /// Inner application data, that is shared with the event callbacks.
    inner:           Rc<InstallerApp>,
    /// Main events handler handle - so we can unbind it when the UI is dropped.
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
            .title(&data.window_title)
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
            .range(0..PROGRESS_BAR_TICKS)
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
        let handle_events = move |evt, evt_data, handle| {
            if let Some(evt_ui) = evt_ui.upgrade() {
                evt_ui.handle_ui_event(evt, evt_data, handle);
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
            .child_size(Size { width: Dimension::Percent(0.9), height: Dimension::Points(16.0) })
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

/// Display an error message to the user and log it.
pub fn error_message(title: &str, message: &str) {
    error!("{message}");
    nwg::error_message(title, message);
}

/// Init the global state of the `native-windows-gui` library.
pub fn init_global() -> Result {
    nwg::init().context("Failed to init Native Windows GUI")?;
    let mut font = nwg::Font::default();
    nwg::FontBuilder::new()
        .family(DEFAULT_FONT)
        .size(16)
        .build(&mut font)
        .context("Failed to create default font")?;
    nwg::Font::set_global_default(Some(font));
    Ok(())
}
