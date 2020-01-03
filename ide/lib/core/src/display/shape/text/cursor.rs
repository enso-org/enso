use crate::prelude::*;

use crate::display::render::webgl::Context;
use crate::display::render::webgl::set_buffer_data;
use crate::display::shape::text::content::CharPosition;
use crate::display::shape::text::content::TextComponentContent;
use crate::display::shape::text::content::line::LineRef;
use crate::display::shape::text::buffer::glyph_square::point_to_iterable;
use crate::display::shape::text::font::Fonts;

use nalgebra::Point2;
use nalgebra::Translation2;
use std::collections::HashSet;
use std::cmp::Ordering;
use std::iter::once;
use std::ops::Range;
use web_sys::WebGlBuffer;


// ==============
// === Cursor ===
// ==============

/// Cursor in TextComponent with its selection
#[derive(Debug)]
pub struct Cursor {
    pub position    : CharPosition,
    pub selected_to : CharPosition,
}

impl Cursor {
    /// Create a new cursor at given position and without any selection.
    pub fn new(position:CharPosition) -> Self {
        Cursor {position,
            selected_to : position
        }
    }

    /// Get range of selected text by this cursor.
    pub fn selection_range(&self) -> Range<CharPosition> {
        match self.position.cmp(&self.selected_to) {
            Ordering::Equal   => self.position..self.position,
            Ordering::Greater => self.selected_to..self.position,
            Ordering::Less    => self.position..self.selected_to
        }
    }

    /// Check if char at given position is selected.
    pub fn is_char_selected(&self, position:CharPosition) -> bool {
        self.selection_range().contains(&position)
    }

    /// Get `LineRef` object of this cursor's line.
    pub fn current_line<'a>(&self, content:&'a mut TextComponentContent) -> LineRef<'a> {
        content.line(self.position.line)
    }

    /// Get the position where the cursor should be rendered. The returned point is on the
    /// _baseline_ of cursor's line, on the right side of character from the left side of the cursor
    /// (where usually the cursor is displayed by text editors).
    ///
    /// _Baseline_ is a font specific term, for details see [freetype documentation]
    //  (https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1).
    pub fn render_position(&self, content:&mut TextComponentContent, fonts:&mut Fonts)
    -> Point2<f64>{
        let font     = fonts.get_render_info(content.font);
        let mut line = self.current_line(content);
        if self.position.column > 0 {
            let char_index = self.position.column - 1;
            let x          = line.get_char_x_range(char_index,font).end;
            let y          = line.start_point().y;
            Point2::new(x.into(),y)
        } else {
            line.start_point()
        }
    }
}


// ===============
// === Cursors ===
// ===============

/// The number of vertices of single cursor.
const CURSOR_BASE_LAYOUT_SIZE : usize = 2;

lazy_static! {
    /// The base vertices position of single cursor. This position is then translated to the
    /// actual cursor position.
    pub static ref CURSOR_VERTICES_BASE_LAYOUT : [Point2<f32>;CURSOR_BASE_LAYOUT_SIZE] =
        [ Point2::new(0.0, -0.2)
        , Point2::new(0.0,  0.8)
        ];
}

/// Structure handling many cursors.
///
/// Usually there is only one cursor, but we have possibility of having many cursors in one text
/// component enabling editing in multiple lines/places at once. This structure also owns
/// a WebGL buffer with vertex positions of all cursors.
#[derive(Debug)]
pub struct Cursors {
    pub cursors       : Vec<Cursor>,
    pub dirty_cursors : HashSet<usize>,
    pub buffer        : WebGlBuffer,
}

impl Cursors {

    /// Create empty `Cursors` structure.
    pub fn new(gl_context:&Context) -> Self {
        Cursors {
            cursors       : Vec::new(),
            dirty_cursors : HashSet::new(),
            buffer        : gl_context.create_buffer().unwrap()
        }
    }

    /// Removes all current cursors and replace them with single cursor without any selection.
    pub fn set_cursor(&mut self, position:CharPosition) {
        self.cursors       = vec![Cursor::new(position)];
        self.dirty_cursors = once(0).collect();
    }

    /// Add new cursor without selection.
    pub fn add_cursor(&mut self, position:CharPosition) {
        let new_index = self.cursors.len();
        self.cursors.push(Cursor::new(position));
        self.dirty_cursors.insert(new_index);
    }

    /// Update the cursors' buffer data.
    pub fn update_buffer_data
    (&mut self, gl_context:&Context, content:&mut TextComponentContent, fonts:&mut Fonts) {
        let cursors          = self.cursors.iter();
        let cursors_vertices = cursors.map(|cursor| Self::cursor_vertices(cursor,content,fonts));
        let buffer_data      = cursors_vertices.flatten().collect_vec();
        set_buffer_data(gl_context,&self.buffer,buffer_data.as_slice());
        self.dirty_cursors.clear();
    }

    fn cursor_vertices(cursor:&Cursor, content:&mut TextComponentContent, fonts:&mut Fonts)
    -> SmallVec<[f32;12]> {
        let position    = cursor.render_position(content,fonts);
        let to_position = Translation2::new(position.x as f32,position.y as f32);
        let base        = CURSOR_VERTICES_BASE_LAYOUT.iter();
        let on_position = base.map(|p| to_position * p);
        on_position.map(point_to_iterable).flatten().collect()
    }

    /// Number of vertices in cursors' buffer.
    pub fn vertices_count(&self) -> usize {
        self.cursors.len() * CURSOR_BASE_LAYOUT_SIZE
    }
}
