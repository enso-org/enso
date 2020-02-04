//! A module defining TextField. TextField is a basegl component displaying editable block of text.

pub mod content;
pub mod cursor;
pub mod render;

use crate::prelude::*;

use crate::display::object::DisplayObjectData;
use crate::display::shape::text::text_field::content::TextFieldContent;
use crate::display::shape::text::text_field::content::TextChange;
use crate::display::shape::text::text_field::cursor::Cursors;
use crate::display::shape::text::text_field::cursor::Step;
use crate::display::shape::text::text_field::cursor::CursorNavigation;
use crate::display::shape::text::glyph::font::FontId;
use crate::display::shape::text::glyph::font::FontRegistry;
use crate::display::shape::text::text_field::render::TextFieldSprites;
use crate::display::shape::text::text_field::render::assignment::GlyphLinesAssignmentUpdate;
use crate::display::world::World;

use nalgebra::Vector2;
use nalgebra::Vector3;
use nalgebra::Vector4;



// =====================
// === TextComponent ===
// =====================

/// A display properties of TextField.
#[derive(Clone,Copy,Debug)]
pub struct TextFieldProperties {
    /// FontId used for rendering text.
    pub font_id: FontId,
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
            font_id   : fonts.load_embedded_font(Self::DEFAULT_FONT_FACE).unwrap(),
            text_size : 16.0,
            base_color: Vector4::new(1.0, 1.0, 1.0, 1.0),
            size      : Vector2::new(100.0,100.0),
        }
    }
}


/// Component rendering text
///
/// This component is under heavy construction, so the api may easily changed in few future
/// commits.
#[derive(Debug)]
pub struct TextField {
    properties     : TextFieldProperties,
    content        : TextFieldContent,
    cursors        : Cursors,
    rendered       : TextFieldSprites,
    display_object : DisplayObjectData,
}

impl TextField {
    /// Create new TextField.
    pub fn new
    ( world           : &World
    , initial_content : &str
    , properties      : TextFieldProperties
    , fonts           : &mut FontRegistry)
    -> Self {
        let logger         = Logger::new("TextField");
        let display_object = DisplayObjectData::new(logger);
        let content        = TextFieldContent::new(initial_content,&properties);
        let cursors        = Cursors::default();
        let rendered       = TextFieldSprites::new(world, &properties, fonts);
        display_object.add_child(rendered.display_object.clone_ref());

        let mut text_field = TextField {properties,content,cursors,rendered,display_object};
        text_field.initialize(fonts);
        text_field
    }

    fn initialize(&mut self, fonts:&mut FontRegistry) {
        self.assignment_update(fonts).update_line_assignment();
        self.rendered.update_glyphs(&mut self.content,fonts);
        self.rendered.update_cursor_sprites(&self.cursors, &mut self.content.full_info(fonts));
    }

    /// Set position of this TextField.
    pub fn set_position(&mut self, position:Vector3<f32>) {
        self.display_object.set_position(position);
    }

    /// Scroll text by given offset in pixels.
    pub fn scroll(&mut self, offset:Vector2<f32>, fonts:&mut FontRegistry) {
        self.rendered.display_object.mod_position(|pos| *pos -= Vector3::new(offset.x,offset.y,0.0));
        let mut update = self.assignment_update(fonts);
        if offset.x != 0.0 {
            update.update_after_x_scroll(offset.x);
        }
        if offset.y != 0.0 {
            update.update_line_assignment();
        }
        self.rendered.update_glyphs(&mut self.content,fonts);
    }

    /// Get current scroll position.
    pub fn scroll_position(&self) -> Vector2<f32> {
        self.rendered.display_object.position().xy()
    }

    /// Move all cursors by given step.
    pub fn navigate_cursors(&mut self, step:Step, selecting:bool, fonts:&mut FontRegistry) {
        let content        = self.content.full_info(fonts);
        let mut navigation = CursorNavigation {content,selecting};
        self.cursors.navigate_all_cursors(&mut navigation,step);
        self.rendered.update_cursor_sprites(&self.cursors, &mut self.content.full_info(fonts));
    }

    /// Jump cursor to point on the screen.
    pub fn jump_cursor(&mut self, point:Vector2<f32>, selecting:bool, fonts:&mut FontRegistry) {
        let content        = self.content.full_info(fonts);
        let mut navigation = CursorNavigation {content,selecting};
        self.cursors.remove_additional_cursors();
        self.cursors.jump_cursor(&mut navigation,point);
        self.rendered.update_cursor_sprites(&self.cursors, &mut self.content.full_info(fonts));
    }

    /// Make change in text content.
    pub fn make_change(&mut self, change:TextChange, fonts:&mut FontRegistry) {
        self.content.make_change(change);
        self.assignment_update(fonts).update_after_text_edit();
        self.rendered.update_glyphs(&mut self.content,fonts);
    }

    /// Update underlying Display Object.
    pub fn update(&self) {
        self.display_object.update()
    }

    fn assignment_update<'a,'b>(&'a mut self, fonts:&'b mut FontRegistry)
    -> GlyphLinesAssignmentUpdate<'a,'a,'b> {
        GlyphLinesAssignmentUpdate {
            content       : self.content.full_info(fonts),
            assignment    : &mut self.rendered.assignment,
            scroll_offset : -self.rendered.display_object.position().xy(),
            view_size     : self.properties.size,
        }
    }
}

// === Getters ===

impl TextField {
    /// Content of this TextField.
    pub fn content(&self) -> &TextFieldContent {
        &self.content
    }
    /// Description of all cursors in text field.
    pub fn cursors(&self) -> &Cursors {
        &self.cursors
    }
}

impl From<&TextField> for DisplayObjectData {
    fn from(text_fields: &TextField) -> Self {
        text_fields.display_object.clone_ref()
    }
}
