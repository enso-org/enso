//! This module contains TextEditor, an UiComponent to edit Enso Modules or Text Files.

use crate::prelude::*;

use crate::notification;
use crate::view::temporary_panel::TemporaryPadding;
use crate::view::temporary_panel::TemporaryPanel;

use ensogl::display;
use ensogl::display::shape::text::glyph::font::FontRegistry;
use ensogl::display::shape::text::text_field::TextField;
use ensogl::display::shape::text::text_field::TextFieldProperties;
use ensogl::display::world::*;
use data::text::TextChange;
use enso_frp::io::KeyboardActions;
use enso_frp::io::KeyMask;
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
        let file_path  = controller.file_path();
        let text       = self.text_field.get_content();
        let logger     = self.logger.clone();
        executor::global::spawn(async move {
            if controller.store_content(text).await.is_err() {
                let message:&str = &format!("Failed to save file: {}", file_path);
                logger.error(message);
            } else {
                logger.info("File saved");
            }
        });
    }

    /// Get the editor's display object.
    pub fn display_object(&self) -> display::object::Node {
        self.text_field.display_object()
    }
}}

impl TextEditor {
    /// Creates a new TextEditor.
    pub fn new
    ( logger           : &Logger
    , world            : &World
    , controller       : controller::Text
    , keyboard_actions : &mut KeyboardActions
    , fonts            : &mut FontRegistry
    ) -> Self {
        let logger     = logger.sub("TextEditor");
        let scene      = world.scene();
        let camera     = scene.camera();
        let screen     = camera.screen();
        let font       = fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap();
        let padding    = default();
        let position   = zero();
        let size       = Vector2::new(screen.width, screen.height);
        let black      = Vector4::new(0.0,0.0,0.0,1.0);
        let base_color = black;
        let text_size  = 16.0;
        let properties = TextFieldProperties {font,text_size,base_color,size};
        let text_field = TextField::new(&world,properties);
        // world.add_child(&text_field); // FIXME !!!

        let data = TextEditorData {controller,text_field,padding,position,size,logger};
        Self::new_from_data(data).initialize(keyboard_actions)
    }

    fn initialize(self, keyboard_actions:&mut KeyboardActions) -> Self {
        let save_keys   = KeyMask::new_control_character('s');
        let text_editor = Rc::downgrade(&self.rc);
        keyboard_actions.set_action(save_keys,enclose!((text_editor) move |_| {
            if let Some(text_editor) = text_editor.upgrade() {
                text_editor.borrow().save();
            }
        }));

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
            this.handle_controller_notification(notif)
        }));
    }

    fn handle_controller_notification(&self, notification:notification::Text)
    -> impl Future<Output=()> {
        match notification {
            notification::Text::Invalidate => self.reload_content()
        }
    }

    fn handle_text_field_notification(&self, change:&TextChange) {
        let (logger,controller) = self.with_borrowed(|data|
            (data.logger.clone(),data.controller.clone_ref()));
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
        // TODO: Set text field size once the size change gets supported.
        // https://app.zenhub.com/workspaces/enso-5b57093c92e09f0d21193695/issues/luna/ide/217
        // let padding  = Vector2::new(padding.left + padding.right, padding.top + padding.bottom);
        // self.text_field.set_size(self.dimensions - padding);
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
