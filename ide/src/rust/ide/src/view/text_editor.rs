//! This module contains TextEditor, an UiComponent to edit Enso Modules or Text Files.

use crate::prelude::*;

use crate::view::temporary_panel::TemporaryPadding;
use crate::view::temporary_panel::TemporaryPanel;

use data::text::TextChange;
use enso_frp::io::keyboard_old::KeyMask;
use enso_frp::io::keyboard_old;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::Scene;
use ensogl::display::shape::primitive::StyleWatch;
use ensogl::display::shape::text::glyph::font;
use ensogl::display::shape::text::text_field::{TextField, FocusManager};
use ensogl::display::shape::text::text_field::TextFieldProperties;
use ensogl::system::web::platform;
use ensogl_theme;
use nalgebra::Vector2;
use nalgebra::zero;
use utils::channel::process_stream_with_handle;



// ==================
// === TextEditor ===
// ==================

shared! { TextEditor

/// TextEditor allows us to edit text files or Enso Modules. Extensible code highlighting is
/// planned to be implemented for it.
#[derive(Debug)]
pub struct TextEditorData {
    text_field : TextField,
    padding    : TemporaryPadding,
    position   : Vector2<f32>,
    size       : Vector2<f32>,
    controller : controller::Text,
    logger     : Logger
}

impl {
    /// Saves text editor's content to file.
    pub fn save(&self) {
        let controller = self.controller.clone();
        let text       = self.text_field.get_content();
        let logger     = self.logger.clone();
        executor::global::spawn(async move {
            if controller.store_content(text).await.is_err() {
                let file_path  = controller.file_path();
                let message:&str = &format!("Failed to save file: {}", file_path);
                logger.error(message);
            } else {
                logger.info("File saved");
            }
        });
    }

    /// Get the editor's display object.
    pub fn display_object(&self) -> display::object::Instance {
        self.text_field.display_object()
    }
}}

impl TextEditor {
    /// Creates a new TextEditor.
    pub fn new<'t,S:Into<&'t Scene>>
    ( logger           : impl AnyLogger
    , scene            : S
    , controller       : controller::Text
    , keyboard_actions : &mut keyboard_old::Actions
    , fonts            : &mut font::Registry
    , focus_manager    : &FocusManager
    ) -> Self {
        let scene      = scene.into();
        let logger     = Logger::sub(logger,"TextEditor");
        let camera     = scene.camera();
        let screen     = camera.screen();
        let font       = fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap();
        let padding    = default();
        let position   = zero();
        let size       = Vector2::new(screen.width, screen.height / 2.0);
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
        let styles     = StyleWatch::new(&scene.style_sheet);
        let base_color = styles.get_color(ensogl_theme::vars::text_editor::text::color);
        let base_color = color::Rgba::from(base_color);
        let text_size  = 16.0;
        let properties = TextFieldProperties {font,text_size,base_color,size};
        let text_field = TextField::new(scene,properties,focus_manager);
        // world.add_child(&text_field); // FIXME !!!

        let data = TextEditorData {controller,text_field,padding,position,size,logger};
        Self::new_from_data(data).initialize(keyboard_actions)
    }

    fn get_save_keys_mask() -> KeyMask {
        if let platform::MacOS = platform::current() {
            KeyMask::meta_plus('s')
        } else {
            KeyMask::control_plus('s')
        }
    }

    fn initialize(self, keyboard_actions:&mut keyboard_old::Actions) -> Self {
        let save_keys   = Self::get_save_keys_mask();
        let text_editor = Rc::downgrade(&self.rc);
        keyboard_actions.add_action_for_key_mask(save_keys,enclose!((text_editor) move || {
            if let Some(text_editor) = text_editor.upgrade() {
                text_editor.borrow().save();
            }
        })).forget(); // FIXME remove forget

        self.setup_notifications();
        executor::global::spawn(self.reload_content());
        self.update();
        self
    }

    /// Modify the underlying TextEditorData.
    pub fn modify_data<F:FnMut(&mut TextEditorData)>(&mut self, mut f:F) {
        f(&mut self.rc.borrow_mut());
        self.update();
    }

    /// Setup handlers for notification going from text field and from controller.
    fn setup_notifications(&self) {
        let weak = self.downgrade();
        let text_field = self.with_borrowed(|data| data.text_field.clone_ref());
        text_field.set_text_edit_callback(enclose!((weak) move |change| {
            if let Some(this) = weak.upgrade() {
                this.handle_text_field_notification(change);
            }
        }));

        let notifications_sub = self.with_borrowed(|data| data.controller.subscribe());
        executor::global::spawn(process_stream_with_handle(notifications_sub,weak,|notif,this| {
            this.handle_model_notification(notif)
        }));
    }

    fn handle_model_notification(&self, notification:controller::text::Notification)
    -> impl Future<Output=()> {
        match notification {
            controller::text::Notification::Invalidate => self.reload_content()
        }
    }

    fn handle_text_field_notification(&self, change:TextChange) {
        let (logger,controller) = self.with_borrowed(|data|
            (data.logger.clone_ref(),data.controller.clone_ref()));
        let result = controller.apply_text_change(change);
        if result.is_err() {
            logger.error(|| "Error while notifying controllers about text change");
            logger.error(|| format!("{:?}", result));
        }
    }

    /// Reload the TextEditor content with data obtained from controller
    fn reload_content(&self) -> impl Future<Output=()> {
        let (logger,controller) = self.with_borrowed(|data|
            (data.logger.clone(),data.controller.clone_ref()));
        let weak  = self.downgrade();
        async move {
            if let Ok(content) = controller.read_content().await {
                if let Some(this) = weak.upgrade() {
                    this.with_borrowed(|data| data.text_field.set_content(&content));
                    logger.info("File loaded");
                }
            }
        }
    }

    /// Updates the underlying display object, should be called after setting size or position.
    fn update(&self) {
        let data     = self.rc.borrow_mut();
        let z_origin = 0.0;
        let padding  = data.padding;
        let position = data.position;
        let position = Vector3::new(position.x + padding.left,position.y + padding.bottom,z_origin);
        data.text_field.set_position(position);
        let padding  = Vector2::new(padding.left + padding.right, padding.top + padding.bottom);
        data.text_field.set_size(data.size - padding);
    }
}

impl TemporaryPanel for TextEditor {
    fn set_padding(&mut self, padding: TemporaryPadding) {
        self.modify_data(|data| data.padding = padding);
    }

    fn padding(&self) -> TemporaryPadding {
        self.rc.borrow().padding
    }

    fn set_size(&mut self, size:Vector2<f32>) {
        self.modify_data(|data| data.size = size);
    }

    fn size(&self) -> Vector2<f32> {
        self.rc.borrow_mut().text_field.size()
    }

    fn set_position(&mut self, position:Vector2<f32>) {
        self.modify_data(|data| data.position = position);
    }

    fn position(&self) -> Vector2<f32> {
        let position = self.rc.borrow().text_field.position();
        Vector2::new(position.x, position.y)
    }
}
