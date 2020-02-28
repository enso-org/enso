//! This module contains TextEditor, an UiComponent to edit Enso Modules or Text Files.

use crate::prelude::*;

use crate::view::temporary_panel::TemporaryPadding;
use crate::view::temporary_panel::TemporaryPanel;

use basegl::display::object::DisplayObjectOps;
use basegl::display::shape::text::glyph::font::FontRegistry;
use basegl::display::shape::text::text_field::TextField;
use basegl::display::shape::text::text_field::TextFieldProperties;
use basegl::display::world::*;
use enso_frp::io::KeyboardActions;
use enso_frp::io::KeyMask;
use nalgebra::Vector2;
use nalgebra::zero;



// ==================
// === TextEditor ===
// ==================

shared! { TextEditor

/// TextEditor allows us to edit text files or Enso Modules. Extensible code highlighting is
/// planned to be implemented for it.
#[derive(Debug)]
pub struct TextEditorData {
    text_field           : TextField,
    padding              : TemporaryPadding,
    position             : Vector2<f32>,
    size                 : Vector2<f32>,
    controller           : controller::text::Handle,
    logger               : Logger
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
}}

impl TextEditor {
    /// Creates a new TextEditor.
    pub fn new
    ( logger           : &Logger
    , world            : &World
    , controller       : controller::text::Handle
    , keyboard_actions : &mut KeyboardActions) -> Self {
        let logger     = logger.sub("TextEditor");
        let scene      = world.scene();
        let camera     = scene.camera();
        let screen     = camera.screen();
        let mut fonts  = FontRegistry::new();
        let font       = fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap();
        let padding    = default();
        let position   = zero();
        let size       = Vector2::new(screen.width, screen.height);
        let black      = Vector4::new(0.0,0.0,0.0,1.0);
        let base_color = black;
        let text_size  = 16.0;
        let properties = TextFieldProperties {font,text_size,base_color,size};
        let text_field = TextField::new(&world,properties);

        let text_field_weak   = text_field.downgrade();
        let controller_clone = controller.clone_ref();
        let logger_ref        = logger.clone();
        executor::global::spawn(async move {
            if let Ok(content) = controller_clone.read_content().await {
                if let Some(text_field) = text_field_weak.upgrade() {
                    text_field.set_content(&content);
                    logger_ref.info("File loaded");
                }
            }
        });

        world.add_child(&text_field);

        let data = TextEditorData {controller,text_field,padding,position,size,logger};
        Self::new_from_data(data).initialize(keyboard_actions)
    }

    fn initialize(self, keyboard_actions:&mut KeyboardActions) -> Self {
        let save_keys   = KeyMask::new_control_character('s');
        let text_editor = Rc::downgrade(&self.rc);
        keyboard_actions.set_action(save_keys,move |_| {
            if let Some(text_editor) = text_editor.upgrade() {
                text_editor.borrow().save();
            }
        });
        self.with_borrowed(move |data| {
            let logger           = data.logger.clone();
            let controller_clone = data.controller.clone_ref();
            data.text_field.set_text_edit_callback(move |change| {
                let result = controller_clone.apply_text_change(change);
                if result.is_err() {
                    logger.error(|| "Error while notifying controllers about text change");
                    logger.error(|| format!("{:?}", result));
                }
            });
        });
        self.update();
        self
    }

    /// Modify the underlying TextEditorData.
    pub fn modify_data<F:FnMut(&mut TextEditorData)>(&mut self, mut f:F) {
        f(&mut self.rc.borrow_mut());
        self.update();
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
        data.text_field.update();
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
