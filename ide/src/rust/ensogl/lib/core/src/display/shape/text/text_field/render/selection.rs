//! Drawing selection utilities.

use crate::display::shape::primitive::system::ShapeSystem;
use crate::display::shape::text::text_field::content::TextFieldContent;
use crate::display::shape::text::text_field::cursor::Cursor;
use crate::display::symbol::geometry::compound::sprite::Sprite;
use crate::display::object::traits::*;

use enso_data::text::TextLocation;
use nalgebra::Vector2;
use nalgebra::Vector3;
use std::ops::Range;



// =================================
// === SelectionSpritesGenerator ===
// =================================

/// A helper structure for generating selection sprites.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct SelectionSpritesGenerator<'a,'b> {
    pub line_height : f32,
    pub system      : &'a ShapeSystem,
    pub content     : &'b mut TextFieldContent,
}

impl<'a,'b> SelectionSpritesGenerator<'a,'b> {
    /// Generate sprites for given selection.
    pub fn generate(&mut self, selection : &Range<TextLocation>) -> Vec<Sprite> {
        let mut return_value = Vec::new();
        if selection.start.line < selection.end.line {
            return_value.push(self.top_line_of_multiline_selection(&selection));
            return_value.push(self.bottom_line_of_multiline_selection(&selection));
            if selection.end.line - selection.start.line > 1 {
                let lines = (selection.start.line+1)..selection.end.line;
                return_value.push(self.whole_line_selection_block(&lines));
            }
        } else if selection.start.column < selection.end.column {
            return_value.push(self.single_line_selection(&selection))
        }
        return_value
    }

    const FULL_LINE_WIDTH:f32 = 1e6;

    fn single_line_selection(&mut self, selection:&Range<TextLocation>) -> Sprite {
        let start  = Cursor::render_position(&selection.start,self.content);
        let end    = Cursor::render_position(&selection.end,self.content);
        let width  = end.x - start.x;
        let x      = start.x + width/2.0;
        let y      = start.y;
        let size   = Vector2::new(width,self.line_height);
        let sprite = self.system.new_instance();
        sprite.set_position(Vector3::new(x,y,-1.0));
        sprite.size.set(size);
        sprite
    }

    fn top_line_of_multiline_selection(&mut self, selection : &Range<TextLocation>) -> Sprite {
        let start  = Cursor::render_position(&selection.start,self.content);
        let width  = Self::FULL_LINE_WIDTH;
        let x      = start.x + width/2.0;
        let y      = start.y;
        let size   = Vector2::new(width,self.line_height);
        let sprite = self.system.new_instance();
        sprite.set_position(Vector3::new(x,y,-1.0));
        sprite.size.set(size);
        sprite
    }

    fn bottom_line_of_multiline_selection(&mut self, selection : &Range<TextLocation>) -> Sprite {
        let end    = Cursor::render_position(&selection.end,self.content);
        let width  = end.x;
        let x      = width/2.0;
        let y      = end.y;
        let size   = Vector2::new(width,self.line_height);
        let sprite = self.system.new_instance();
        sprite.set_position(Vector3::new(x,y,-1.0));
        sprite.size.set(size);
        sprite
    }

    fn whole_line_selection_block(&mut self, lines:&Range<usize>) -> Sprite {
        let lines_count = lines.end - lines.start;
        let width  = Self::FULL_LINE_WIDTH;
        let height = (lines_count as f32) * self.line_height;
        let x      = width/2.0;
        let bottom = TextLocation::at_line_begin(lines.end - 1);
        let min_y  = Cursor::render_position(&bottom,self.content).y - self.line_height / 2.0;
        let y      = min_y + height/2.0;
        let size   = Vector2::new(width,height);
        let sprite = self.system.new_instance();
        sprite.set_position(Vector3::new(x,y,-1.0));
        sprite.size.set(size);
        sprite
    }
}
