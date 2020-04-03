//! Rendering TextField.

pub mod assignment;
pub mod selection;

use crate::prelude::*;

use crate::display;
use crate::display::object::traits::*;
use crate::display::shape::text::glyph::font::FontHandle;
use crate::display::shape::text::glyph::system::GlyphSystem;
use crate::display::shape::text::text_field::content::TextFieldContent;
use crate::display::shape::text::text_field::cursor::Cursor;
use crate::display::shape::text::text_field::cursor::Cursors;
use crate::display::shape::text::text_field::render::assignment::GlyphLinesAssignment;
use crate::display::shape::text::text_field::render::assignment::LineFragment;
use crate::display::shape::text::text_field::render::selection::SelectionSpritesGenerator;
use crate::display::shape::text::text_field::TextFieldProperties;
use crate::display::shape::*;
use crate::display::shape::primitive::system::ShapeSystem;
use crate::display::symbol::geometry::compound::sprite::Sprite;
use crate::display::world::World;

use nalgebra::{Vector2, zero};
use nalgebra::Vector3;
use crate::math::topology::unit::PixelDistance;
use crate::display::Glsl;



// =======================
// === RenderedContent ===
// =======================

/// Alias for line of glyph sprites. This is for distinct `glyph::system::Line` type from glyph
/// system.from `text_field::content::line::Line` being a whole line of text in Text Field content.
type GlyphLine = crate::display::shape::text::glyph::system::Line;

/// Structure containing sprites bound to one cursor with its selection.
#[derive(Debug)]
pub struct CursorSprites {
    /// Cursor sprite.
    pub cursor: Sprite,
    /// Selection sprites.
    pub selection: Vec<Sprite>,
}

/// Structure with all data and sprites required for rendering specific TextField.
#[derive(Debug)]
pub struct TextFieldSprites {
    /// System used for rendering glyphs.
    pub glyph_system: GlyphSystem,
    /// System used for rendering cursors.
    pub cursor_system: ShapeSystem,
    /// System used for rendering selections.
    pub selection_system: ShapeSystem,
    /// All drawn glyph lines.
    pub glyph_lines: Vec<GlyphLine>,
    /// All drawn cursors..
    pub cursors: Vec<CursorSprites>,
    /// Current assignment of glyph lines to actual lines of text.
    pub assignment: GlyphLinesAssignment,
    /// line height in pixels.
    pub line_height: f32,
    /// Display object of the whole rendered content.
    pub display_object: display::object::Node,
}


// === Construction ===

impl TextFieldSprites {

    /// Create RenderedContent structure.
    pub fn new(world:&World, properties:&TextFieldProperties) -> Self {
        let font              = properties.font.clone_ref();
        let line_height       = properties.text_size;
        let window_size       = properties.size;
        let color             = properties.base_color;
        let selection_system  = Self::create_selection_system(world);
        let cursor_system     = Self::create_cursor_system(world,line_height,&color);
        let cursors           = Vec::new();
        let mut glyph_system  = GlyphSystem::new(world,font.clone_ref());
        let display_object    = display::object::Node::new(Logger::new("RenderedContent"));
        display_object.add_child(&selection_system);
        display_object.add_child(&glyph_system);
        display_object.add_child(&cursor_system);

        let assignment        = Self::create_assignment_structure(window_size,line_height,font);
        let glyph_lines_count = assignment.glyph_lines_count();
        let length            = assignment.max_glyphs_in_line;
        let bsl_start         = Vector2::new(0.0, 0.0);

        let indexes     = 0..glyph_lines_count;
        let glyph_lines = indexes.map(|_| {
            glyph_system.new_empty_line(bsl_start,line_height,length,color)
        }).collect();
        TextFieldSprites {glyph_system,cursor_system,selection_system,glyph_lines,cursors,
            line_height,display_object,assignment}
    }

    fn create_cursor_system(world:&World,line_height:f32,color:&Vector4<f32>) -> ShapeSystem {
        const WIDTH:f32         = 2.0;
        const COLOR_HIDDEN:&str = "vec4(0.0,0.0,0.0,0.0)";
        let color_glsl:Glsl     = color.into();
        let color_function      = format!("fract(input_time / 1000.0) < 0.5 ? {} : {}",
            color_glsl,COLOR_HIDDEN);
        let cursor_definition     = Rect(Vector2::new(WIDTH.px(),line_height.px()));
        let cursor_definition     = cursor_definition.fill(color_function);
        ShapeSystem::new(world,&cursor_definition)
    }

    fn create_selection_system(world:&World) -> ShapeSystem {
        const ROUNDING:f32       = 3.0;
        let width                = "input_size.x";
        let height               = "input_size.y";
        let selection_definition = Rect((width,height));
        ShapeSystem::new(world,&selection_definition)
    }

    fn create_assignment_structure
    ( window_size : Vector2<f32>
    , line_height : f32
    , font        : FontHandle
    ) -> GlyphLinesAssignment {
        // Display_size.(x/y).floor() makes space for all lines/glyph that fit in space in
        // their full size. But we have 2 more lines/glyph: one clipped from top or left, and one
        // from bottom or right.
        const ADDITIONAL:usize = 2;
        let displayed_lines    = (window_size.y / line_height).floor() as usize + ADDITIONAL;
        let space_width        = font.get_glyph_info(' ').advance * line_height;
        let displayed_chars    = (window_size.x / space_width).floor();
        // This margin is to ensure, that after x scrolling we won't need to refresh all the lines
        // at once.
        let x_margin           = (displayed_lines as f32) * line_height / space_width;
        let max_glyphs_in_line = (displayed_chars + 2.0 * x_margin).floor() as usize + ADDITIONAL;
        GlyphLinesAssignment::new(displayed_lines,max_glyphs_in_line,x_margin)
    }
}


// === Update ===

impl TextFieldSprites {
    /// Update all displayed glyphs.
    pub fn update_glyphs(&mut self, content:&mut TextFieldContent) {
        let glyph_lines       = self.glyph_lines.iter_mut().enumerate();
        let lines_assignment  = glyph_lines.zip(self.assignment.glyph_lines_fragments.iter());
        let dirty_lines       = std::mem::take(&mut content.dirty_lines);
        let dirty_glyph_lines = std::mem::take(&mut self.assignment.dirty_glyph_lines);

        for ((index,glyph_line),assignment) in lines_assignment {
            let is_glyph_line_dirty = dirty_glyph_lines.contains(&index);
            let assigned_line       = assignment.as_ref().map(|fragment| fragment.line_index);
            let is_line_dirty       = assigned_line.map_or(false, |l| dirty_lines.is_dirty(l));
            if is_glyph_line_dirty || is_line_dirty {
                match assignment {
                    Some(fragment) => Self::update_glyph_line(glyph_line,fragment,content),
                    None           => glyph_line.replace_text("".chars()),
                }
            }
        }
    }

    /// Update all displayed cursors with their selections.
    pub fn update_cursor_sprites
    (&mut self, cursors:&Cursors, content:&mut TextFieldContent, focused:bool) {
        let cursor_system = &self.cursor_system;
        self.cursors.resize_with(cursors.cursors.len(),|| Self::new_cursor_sprites(cursor_system));
        for (sprites,cursor) in self.cursors.iter_mut().zip(cursors.cursors.iter()) {
            let position = Cursor::render_position(&cursor.position,content);
            sprites.cursor.set_position(Vector3::new(position.x,position.y,0.0));
            let size = if focused { Vector2::new(2.0,self.line_height) } else { zero() };
            sprites.cursor.size().set(size);

            let selection = cursor.selection_range();
            let line_height   = self.line_height;
            let system        = &self.selection_system;
            let mut generator = SelectionSpritesGenerator {content,line_height,system};
            sprites.selection.clear();
            sprites.selection = generator.generate(&selection);
        }
    }

    fn update_glyph_line
    (glyph_line:&mut GlyphLine, fragment:&LineFragment, content:&mut TextFieldContent) {
        let bsl_start     = Self::baseline_start_for_fragment(fragment,content);
        let line          = &content.lines()[fragment.line_index];
        let chars         = &line.chars()[fragment.chars_range.clone()];
        glyph_line.set_baseline_start(bsl_start);
        glyph_line.replace_text(chars.iter().cloned());
    }

    /// The baseline start for given line's fragment.
    ///
    /// Because we're not rendering the whole lines, but only visible fragment of it (with some
    /// margin), the baseline used for placing glyph don't start on the line begin, but at the
    /// position of first char of fragment.
    fn baseline_start_for_fragment(fragment:&LineFragment, content:&mut TextFieldContent)
    -> Vector2<f32> {
        let mut line = content.line(fragment.line_index);
        if fragment.chars_range.start >= line.chars().len() {
            line.baseline_start()
        } else {
            let x = line.get_char_x_position(fragment.chars_range.start);
            let y = line.baseline_start().y;
            Vector2::new(x,y)
        }
    }

    fn new_cursor_sprites(cursor_system:&ShapeSystem) -> CursorSprites {
        CursorSprites {
            cursor    : cursor_system.new_instance(),
            selection : Vec::new(),
        }
    }
}
