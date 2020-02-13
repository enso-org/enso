//! A module defining TextField. TextField is a basegl component displaying editable block of text.

pub mod content;
pub mod cursor;
pub mod keyboard;
pub mod location;
pub mod render;

use crate::prelude::*;

use crate::display::object::DisplayObjectData;
use crate::display::shape::text::text_field::content::TextFieldContent;
use crate::display::shape::text::text_field::content::TextChange;
use crate::display::shape::text::text_field::cursor::Cursors;
use crate::display::shape::text::text_field::cursor::Cursor;
use crate::display::shape::text::text_field::cursor::Step;
use crate::display::shape::text::text_field::cursor::CursorNavigation;
use crate::display::shape::text::text_field::location::TextLocation;
use crate::display::shape::text::text_field::location::TextLocationChange;
use crate::display::shape::text::text_field::keyboard::TextFieldFrp;
use crate::display::shape::text::glyph::font::FontHandle;
use crate::display::shape::text::glyph::font::FontRegistry;
use crate::display::shape::text::text_field::render::TextFieldSprites;
use crate::display::shape::text::text_field::render::assignment::GlyphLinesAssignmentUpdate;
use crate::display::world::World;
use crate::system::web::text_input::KeyboardBinding;

use nalgebra::Vector2;
use nalgebra::Vector3;
use nalgebra::Vector4;



// =====================
// === TextComponent ===
// =====================

/// A display properties of TextField.
#[derive(Debug)]
pub struct TextFieldProperties {
    /// FontHandle used for rendering text.
    pub font: FontHandle,
    /// Text size being a line height in pixels.
    pub text_size: f32,
    /// Base color of displayed text.
    pub base_color: Vector4<f32>,
    /// Size of this component.
    pub size: Vector2<f32>,
}

impl TextFieldProperties {
    const DEFAULT_FONT_FACE:&'static str = "DejaVuSansMono";

    fn default(fonts:&mut FontRegistry) -> Self {
        TextFieldProperties {
            font      : fonts.get_or_load_embedded_font(Self::DEFAULT_FONT_FACE).unwrap(),
            text_size : 16.0,
            base_color: Vector4::new(1.0, 1.0, 1.0, 1.0),
            size      : Vector2::new(100.0,100.0),
        }
    }
}

shared! { TextField

    /// Component rendering text
    ///
    /// This component is under heavy construction, so the api may easily changed in few future
    /// commits.
    #[derive(Debug)]
    pub struct TextFieldData {
        properties       : TextFieldProperties,
        content          : TextFieldContent,
        cursors          : Cursors,
        rendered         : TextFieldSprites,
        display_object   : DisplayObjectData,
        frp              : Option<TextFieldFrp>,
        keyboard_binding : Option<KeyboardBinding>,
    }

    impl {
        /// Set position of this TextField.
        pub fn set_position(&mut self, position:Vector3<f32>) {
            self.display_object.set_position(position);
        }

        /// Scroll text by given offset in pixels.
        pub fn scroll(&mut self, offset:Vector2<f32>) {
            let position_change = -Vector3::new(offset.x,offset.y,0.0);
            self.rendered.display_object.mod_position(|pos| *pos += position_change );
            let mut update = self.assignment_update();
            if offset.x != 0.0 {
                update.update_after_x_scroll(offset.x);
            }
            if offset.y != 0.0 {
                update.update_line_assignment();
            }
            self.rendered.update_glyphs(&mut self.content);
        }

        /// Get current scroll position.
        pub fn scroll_position(&self) -> Vector2<f32> {
            self.rendered.display_object.position().xy()
        }

        /// Add cursor.
        pub fn add_cursor(&mut self, position:TextLocation) {
            self.cursors.add_cursor(position);
            self.rendered.update_cursor_sprites(&self.cursors, &mut self.content);
        }

        /// Move all cursors by given step.
        pub fn navigate_cursors(&mut self, step:Step, selecting:bool) {
            let content        = &mut self.content;
            let mut navigation = CursorNavigation {content,selecting};
            self.cursors.navigate_all_cursors(&mut navigation,step);
            self.rendered.update_cursor_sprites(&self.cursors, &mut self.content);
        }

        /// Jump cursor to point on the screen.
        pub fn jump_cursor(&mut self, point:Vector2<f32>, selecting:bool) {
            let content        = &mut self.content;
            let mut navigation = CursorNavigation {content,selecting};
            self.cursors.remove_additional_cursors();
            self.cursors.jump_cursor(&mut navigation,point);
            self.rendered.update_cursor_sprites(&self.cursors, &mut self.content);
        }

        /// Make change in text content.
        ///
        /// As an opposite to `edit` function, here we don't care about cursors, just do the change
        /// described in `TextChange` structure.
        pub fn apply_change(&mut self, change:TextChange) {
            self.content.apply_change(change);
            self.assignment_update().update_after_text_edit();
            self.rendered.update_glyphs(&mut self.content);
        }

        /// Get the selected text.
        pub fn get_selected_text(&self) -> String {
            let cursor_select  = |c:&Cursor| self.content.copy_fragment(c.selection_range());
            let mut selections = self.cursors.cursors.iter().map(cursor_select);
            selections.join("\n")
        }

        /// Edit text.
        ///
        /// All the currently selected text will be removed, and the given string will be inserted
        /// by each cursor.
        pub fn write(&mut self, text:&str) {
            let trimmed                 = text.trim_end_matches('\n');
            let is_line_per_cursor_edit = trimmed.contains('\n') && self.cursors.cursors.len() > 1;
            let cursor_ids              = self.cursors.sorted_cursor_indices();

            if is_line_per_cursor_edit {
                let cursor_with_line = cursor_ids.iter().cloned().zip(trimmed.split('\n'));
                self.write_per_cursor(cursor_with_line);
            } else {
                let cursor_with_line = cursor_ids.iter().map(|cursor_id| (*cursor_id,text));
                self.write_per_cursor(cursor_with_line);
            };
            self.assignment_update().update_after_text_edit();
            self.rendered.update_glyphs(&mut self.content);
            self.rendered.update_cursor_sprites(&self.cursors, &mut self.content);
        }

        /// Remove all text selected by all cursors.
        pub fn remove_selection(&mut self) {
            self.write("");
        }

        /// Do delete operation on text.
        ///
        /// For cursors with selection it will just remove the selected text. For the rest, it will
        /// remove all content covered by `step`.
        pub fn do_delete_operation(&mut self, step:Step) {
            let content           = &mut self.content;
            let selecting         = true;
            let mut navigation    = CursorNavigation {content,selecting};
            let without_selection = |c:&Cursor| !c.has_selection();
            self.cursors.navigate_cursors(&mut navigation,step,without_selection);
            self.remove_selection();
        }

        /// Update underlying Display Object.
        pub fn update(&self) {
            self.display_object.update()
        }
    }
}


// === Constructor ===

impl TextField {
    /// Create new empty TextField
    pub fn new(world:&World, properties:TextFieldProperties) -> Self {
        Self::new_with_content(world,"",properties)
    }

    /// Create new TextField with predefined content.
    pub fn new_with_content(world:&World, initial_content:&str, properties:TextFieldProperties)
    -> Self {
        let data = TextFieldData::new(world,initial_content,properties);
        let rc   = Rc::new(RefCell::new(data));
        let frp  = TextFieldFrp::new(Rc::downgrade(&rc));
        with(rc.borrow_mut(), move |mut data| {
            data.keyboard_binding = Some(frp.bind_frp_to_js_text_input_actions());
            data.frp              = Some(frp);
        });
        Self{rc}
    }
}


// === Private ===

impl TextFieldData {
    fn new(world:&World, initial_content:&str, properties:TextFieldProperties) -> Self {
        let logger           = Logger::new("TextField");
        let display_object   = DisplayObjectData::new(logger);
        let content          = TextFieldContent::new(initial_content,&properties);
        let cursors          = Cursors::default();
        let rendered         = TextFieldSprites::new(world,&properties);
        let frp              = None;
        let keyboard_binding = None;
        display_object.add_child(rendered.display_object.clone_ref());

        Self {properties,content,cursors,rendered,display_object,frp,keyboard_binding}.initialize()
    }

    fn initialize(mut self) -> Self{
        self.assignment_update().update_line_assignment();
        self.rendered.update_glyphs(&mut self.content);
        self.rendered.update_cursor_sprites(&self.cursors, &mut self.content);
        self
    }

    fn assignment_update(&mut self) -> GlyphLinesAssignmentUpdate {
        GlyphLinesAssignmentUpdate {
            content       : &mut self.content,
            assignment    : &mut self.rendered.assignment,
            scroll_offset : -self.rendered.display_object.position().xy(),
            view_size     : self.properties.size,
        }
    }

    fn write_per_cursor<'a,It>(&mut self, cursor_id_with_text_to_insert:It)
    where It : Iterator<Item=(usize,&'a str)> {
        let mut location_change = TextLocationChange::default();
        for (cursor_id,to_insert) in cursor_id_with_text_to_insert {
            let cursor   = &mut self.cursors.cursors[cursor_id];
            let replaced = location_change.apply_to_range(cursor.selection_range());
            let change   = TextChange::replace(replaced,to_insert);
            location_change.add_change(&change);
            *cursor = Cursor::new(change.inserted_text_range().end);
            self.content.apply_change(change);
        }
    }
}


// === Display Object ===

impl From<&TextField> for DisplayObjectData {
    fn from(text_fields: &TextField) -> Self {
        text_fields.rc.borrow().display_object.clone_ref()
    }
}
