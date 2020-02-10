//! Module with all structures describing the content of the TextField.
pub mod line;

use crate::prelude::*;

use crate::display::shape::text::glyph::font::FontId;
use crate::display::shape::text::glyph::font::FontRegistry;
use crate::display::shape::text::glyph::font::FontRenderInfo;
use crate::display::shape::text::text_field::content::line::Line;
use crate::display::shape::text::text_field::content::line::LineFullInfo;
use crate::display::shape::text::text_field::TextFieldProperties;

use nalgebra::Vector2;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeInclusive;



// ==================
// === DirtyLines ===
// ==================

/// Set of dirty lines' indices
#[derive(Debug)]
pub struct DirtyLines {
    /// A set of single edited lines.
    pub single_lines: HashSet<usize>,
    /// An open range of dirty lines. When the line is added or removed, all the lines below should
    /// be marked as dirty. In that case we set this field instead of putting each line id to
    /// HashSet.
    pub range: Option<RangeFrom<usize>>
}

impl Default for DirtyLines {
    /// Default `DirtyLines` where no line is dirty.
    fn default() -> Self {
        Self {
            single_lines : HashSet::new(),
            range        : None,
        }
    }
}

impl DirtyLines {
    /// Mark single line as dirty.
    pub fn add_single_line(&mut self, index:usize) {
        self.single_lines.insert(index);
    }

    /// Mark an open range of lines as dirty.
    pub fn add_lines_range_from(&mut self, range:RangeFrom<usize>) {
        let current_is_wider = self.range.as_ref().map_or(false, |cr| cr.start <= range.start);
        if !current_is_wider {
            self.range = Some(range);
        }
    }

    /// Mark an open range of lines as dirty.
    pub fn add_lines_range(&mut self, range:RangeInclusive<usize>) {
        for i in range {
            self.add_single_line(i);
        }
    }

    /// Check if line is marked as dirty.
    pub fn is_dirty(&self, index:usize) -> bool {
        let range_contains = self.range.as_ref().map_or(false, |r| r.contains(&index));
        range_contains || self.single_lines.contains(&index)
    }

    /// Check if there is any dirty line
    pub fn any_dirty(&self) -> bool {
        self.range.is_some() || !self.single_lines.is_empty()
    }
}



// ==============
// === Change ===
// ==============

/// A change type
#[derive(Copy,Clone,Debug)]
pub enum ChangeType {
    /// A change where we replace fragment of one line with text without new lines.
    SingleLine,
    /// A multi-line change is a change which is not a single line change (see docs for SingleLine).
    MultiLine
}

/// A structure describing a text operation in one place.
#[derive(Debug)]
pub struct TextChange {
    replaced : Range<TextLocation>,
    lines    : Vec<Vec<char>>,
}

impl TextChange {
    /// Creates operation which inserts text at given position.
    pub fn insert(at:TextLocation, text:&str) -> Self {
        TextChange {
            replaced : at..at,
            lines    : Self::mk_lines_as_char_vector(text)
        }
    }

    /// Creates operation which deletes text at given range.
    pub fn delete(range:Range<TextLocation>) -> Self {
        TextChange {
            replaced : range,
            lines    : vec![vec![]],
        }
    }

    /// Creates operation which replaces text at given range with given string.
    pub fn replace(replaced:Range<TextLocation>, text:&str) -> Self {
        TextChange {replaced,
            lines : Self::mk_lines_as_char_vector(text)
        }
    }

    /// A type of this change. See `ChangeType` doc for details.
    pub fn change_type(&self) -> ChangeType {
        if self.lines.is_empty() {
            panic!("Invalid change");
        }
        let is_one_line_modified = self.replaced.start.line == self.replaced.end.line;
        let is_one_line_inserted = self.lines.len() == 1;
        if is_one_line_modified && is_one_line_inserted {
            ChangeType::SingleLine
        } else {
            ChangeType::MultiLine
        }
    }

    fn mk_lines_as_char_vector(text:&str) -> Vec<Vec<char>> {
        TextFieldContent::split_to_lines(text).map(|s| s.chars().collect_vec()).collect()
    }
}



// ====================
// === TextLocation ===
// ====================

/// A position of character in a multiline text.
#[derive(Copy,Clone,Debug,PartialEq,Eq,PartialOrd,Ord)]
pub struct TextLocation {
    /// Line index.
    pub line: usize,
    /// Column is a index of char in given line.
    pub column: usize,
}

impl TextLocation {
    /// Create location at begin of given line.
    pub fn at_line_begin(line_index:usize) -> TextLocation {
        TextLocation {
            line   : line_index,
            column : 0,
        }
    }

    /// Create location at begin of the whole document.
    pub fn at_document_begin() -> TextLocation {
        TextLocation {
            line   : 0,
            column : 0,
        }
    }
}



// ============================
// === TextFieldContent ===
// ============================

/// The content of text component - namely lines of text.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct TextFieldContent {
    pub lines       : Vec<Line>,
    pub dirty_lines : DirtyLines,
    pub font        : FontId,
    pub line_height : f32,
}

/// The wrapper for TextFieldContent reference with font. That allows to get specific information
/// about lines and chars position in rendered text.
#[derive(Debug,Shrinkwrap)]
#[shrinkwrap(mutable)]
#[allow(missing_docs)]
pub struct TextFieldContentFullInfo<'a,'b> {
    #[shrinkwrap(main_field)]
    pub content : &'a mut TextFieldContent,
    pub font    : &'b mut FontRenderInfo
}

impl TextFieldContent {
    /// Create a text component containing `text`
    ///
    /// The text will be split to lines by `'\n'` characters.
    pub fn new(text:&str, properties:&TextFieldProperties) -> Self {
        TextFieldContent {
            line_height : properties.text_size,
            lines       : Self::split_to_lines(text).map(Line::new).collect(),
            dirty_lines : DirtyLines::default(),
            font        : properties.font_id,
        }
    }

    fn split_to_lines(text:&str) -> impl Iterator<Item=String> + '_ {
        text.split('\n').map(Self::cut_cr_at_end_of_line).map(|s| s.to_string())
    }

    /// Returns slice without carriage return (also known as CR or `'\r'`) at line's end
    fn cut_cr_at_end_of_line(from:&str) -> &str {
        if from.ends_with('\r') {
            &from[..from.len()-1]
        } else {
            from
        }
    }

    /// Get the full-info wrapper for this content.
    pub fn full_info<'a,'b>(&'a mut self, fonts:&'b mut FontRegistry)
    -> TextFieldContentFullInfo<'a,'b> {
        let font_id = self.font;
        TextFieldContentFullInfo {
            content : self,
            font    : fonts.get_render_info(font_id),
        }
    }

    /// Copy the fragment of text and return as String.
    pub fn copy_fragment(&self, fragment:Range<TextLocation>) -> String {
        let mut output = String::new();
        self.copy_fragment_to(fragment,&mut output);
        output
    }

    /// Extend the `output` with the specified fragment including `'\n'` between lines.
    pub fn copy_fragment_to<Output>(&self, fragment:Range<TextLocation>, output:&mut Output)
    where Output : Extend<char> {
        if fragment.start.line == fragment.end.line {
            let line_chars     = &self.lines[fragment.start.line].chars();
            let chars_fragment = &line_chars[fragment.start.column..fragment.end.column];
            output.extend(chars_fragment.iter().cloned())
        } else {
            let first_line = &self.lines[fragment.start.line];
            output.extend(first_line.chars()[fragment.start.column..].iter().cloned());
            output.extend(std::iter::once('\n'));
            let whole_lines = (fragment.start.line+1)..fragment.end.line;
            for line in &self.lines[whole_lines] {
                output.extend(line.chars().iter().cloned());
                output.extend(std::iter::once('\n'));
            }
            let last_line = &self.lines[fragment.end.line];
            output.extend(last_line.chars()[..fragment.end.column].iter().cloned());
        }

    }
}

// === Implementing Changes ===

impl TextFieldContent {
    /// Apply change to content.
    pub fn apply_change(&mut self, change:TextChange) {
        match change.change_type() {
            ChangeType::SingleLine => self.make_simple_change(change),
            ChangeType::MultiLine => self.make_multiline_change(change),
        }
    }

    /// Apply many changes to content.
    pub fn apply_changes<Changes:IntoIterator<Item=TextChange>>(&mut self, changes:Changes) {
        let change_key  = |chg:&TextChange | chg.replaced.start;
        let changes_vec = changes.into_iter().sorted_by_key(change_key);
        changes_vec.rev().for_each(|change| self.apply_change(change));
    }

    fn make_simple_change(&mut self, change:TextChange) {
        let line_index  = change.replaced.start.line;
        let new_content = change.lines.first().unwrap();
        let range       = change.replaced.start.column..change.replaced.end.column;
        self.lines[line_index].modify().splice(range,new_content.iter().cloned());
        self.dirty_lines.add_single_line(line_index);
    }

    fn make_multiline_change(&mut self, mut change:TextChange) {
        self.mix_content_into_change(&mut change);
        let start_line           = change.replaced.start.line;
        let end_line             = change.replaced.end.line;
        let replaced_lines_count = end_line - start_line + 1;
        let inserted_lines_count = change.lines.len();
        let inserted_lines       = change.lines.drain(0..change.lines.len()).map(Line::new_raw);
        self.lines.splice(start_line..=end_line,inserted_lines);
        if replaced_lines_count != inserted_lines_count {
            self.dirty_lines.add_lines_range_from(start_line..);
        } else {
            self.dirty_lines.add_lines_range(start_line..=end_line);
        }
    }

    /// Mix the unchanged parts of modified lines into change.
    ///
    /// This is for convenience of making multiline content changes. After mixing existing content
    /// into change we can just operate on whole lines (replace the whole lines of current content
    /// with the whole lines-to-insert in change description).
    fn mix_content_into_change(&mut self, change:&mut TextChange) {
        self.mix_first_edited_line_into_change(change);
        self.mix_last_edited_line_into_change(change);
    }

    fn mix_first_edited_line_into_change(&self, change:&mut TextChange) {
        let first_line   = change.replaced.start.line;
        let replace_from = change.replaced.start.column;
        let first_edited = &self.lines[first_line].chars();
        let prefix       = &first_edited[..replace_from];
        change.lines.first_mut().unwrap().splice(0..0,prefix.iter().cloned());
    }

    fn mix_last_edited_line_into_change(&mut self, change:&mut TextChange) {
        let last_line    = change.replaced.end.line;
        let replace_to   = change.replaced.end.column;
        let last_edited  = &self.lines[last_line].chars();
        let suffix       = &last_edited[replace_to..];
        change.lines.last_mut().unwrap().extend_from_slice(suffix);
    }
}

impl<'a,'b> TextFieldContentFullInfo<'a,'b> {
    /// Get a handy wrapper for line under index.
    pub fn line(&mut self, index:usize) -> LineFullInfo {
        LineFullInfo {
            height  : self.content.line_height,
            line    : &mut self.content.lines[index],
            line_id : index,
            font    : self.font,
        }
    }

    /// Get the nearest text location from the point on the screen.
    pub fn location_at_point(&mut self, point:Vector2<f32>) -> TextLocation {
        let line_opt = self.line_at_y_position(point.y);
        let mut line = match line_opt {
            Some(line)             => line,
            None if point.y >= 0.0 => self.line(0),
            None                   => self.line(self.lines.len()-1),
        };
        let column_opt = line.find_char_at_x_position(point.x);
        let column = match column_opt {
            Some(column)           => column,
            None if point.x <= 0.0 => 0,
            None                   => line.len(),
        };
        TextLocation{line:line.line_id, column}
    }

    /// Get the index of line which is displayed at given y screen coordinate.
    pub fn line_at_y_position(&mut self, y:f32) -> Option<LineFullInfo> {
        let index    = -(y / self.line_height).ceil();
        let is_valid = index >= 0.0 && index < self.lines.len() as f32;
        let index    = is_valid.and_option_from(|| Some(index as usize));
        index.map(move |i| self.line(i))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use nalgebra::Vector2;
    use nalgebra::Vector4;

    #[test]
    fn mark_single_line_as_dirty() {
        let mut dirty_lines = DirtyLines::default();
        dirty_lines.add_single_line(3);
        dirty_lines.add_single_line(5);
        assert!( dirty_lines.is_dirty(3));
        assert!(!dirty_lines.is_dirty(4));
        assert!( dirty_lines.is_dirty(5));
    }

    #[test]
    fn mark_line_range_as_dirty() {
        let mut dirty_lines = DirtyLines::default();
        dirty_lines.add_lines_range(3..=5);
        assert!(!dirty_lines.is_dirty(2));
        assert!( dirty_lines.is_dirty(3));
        assert!( dirty_lines.is_dirty(4));
        assert!( dirty_lines.is_dirty(5));
        assert!(!dirty_lines.is_dirty(6));
    }

    #[test]
    fn mark_line_range_from_as_dirty() {
        let mut dirty_lines = DirtyLines::default();
        dirty_lines.add_lines_range_from(3..);
        dirty_lines.add_lines_range_from(5..);
        assert!(!dirty_lines.is_dirty(2));
        assert!( dirty_lines.is_dirty(3));
        assert!( dirty_lines.is_dirty(4));
        assert!( dirty_lines.is_dirty(70000));
    }

    #[test]
    fn create_content() {
        let single_line    = "Single line";
        let mutliple_lines = "Multiple\r\nlines\n";

        let single_line_content = TextFieldContent::new(single_line   ,&mock_properties());
        let multiline_content   = TextFieldContent::new(mutliple_lines,&mock_properties());
        assert_eq!(1, single_line_content.lines.len());
        assert_eq!(3, multiline_content  .lines.len());
        assert_eq!(single_line, single_line_content.lines[0].chars().iter().collect::<String>());
        assert_eq!("Multiple" , multiline_content  .lines[0].chars().iter().collect::<String>());
        assert_eq!("lines"    , multiline_content  .lines[1].chars().iter().collect::<String>());
        assert_eq!(""         , multiline_content  .lines[2].chars().iter().collect::<String>());
    }

    #[test]
    fn edit_single_line() {
        let text                   = "Line a\nLine b\nLine c";
        let delete_from            = TextLocation {line:1, column:0};
        let delete_to              = TextLocation {line:1, column:4};
        let deleted_range          = delete_from..delete_to;
        let insert                 = TextChange::insert(TextLocation {line:1, column:1}, "ab");
        let delete                 = TextChange::delete(deleted_range.clone());
        let replace                = TextChange::replace(deleted_range, "text");

        let mut content            = TextFieldContent::new(text,&mock_properties());

        content.apply_change(insert);
        let expected              = vec!["Line a", "Labine b", "Line c"];
        assert_eq!(expected, get_lines_as_strings(&content));

        content.apply_change(delete);
        let expected = vec!["Line a", "ne b", "Line c"];
        assert_eq!(expected, get_lines_as_strings(&content));

        content.apply_change(replace);
        let expected = vec!["Line a", "text", "Line c"];
        assert_eq!(expected, get_lines_as_strings(&content));

        assert!(!content.dirty_lines.is_dirty(0));
        assert!( content.dirty_lines.is_dirty(1));
        assert!(!content.dirty_lines.is_dirty(2));
    }

    #[test]
    fn insert_multiple_lines() {
        let text             = "Line a\nLine b\nLine c";
        let inserted         = "Ins a\nIns b";
        let begin_loc        = TextLocation {line:0, column:0};
        let middle_loc       = TextLocation {line:1, column:2};
        let end_loc          = TextLocation {line:2, column:6};
        let insert_at_begin  = TextChange::insert(begin_loc ,inserted);
        let insert_in_middle = TextChange::insert(middle_loc,inserted);
        let insert_at_end    = TextChange::insert(end_loc   ,inserted);

        let mut content      = TextFieldContent::new(text,&mock_properties());

        content.apply_change(insert_at_end);
        let expected = vec!["Line a", "Line b", "Line cIns a", "Ins b"];
        assert_eq!(expected, get_lines_as_strings(&content));
        assert!(!content.dirty_lines.is_dirty(0));
        assert!(!content.dirty_lines.is_dirty(1));
        assert!( content.dirty_lines.is_dirty(2));
        content.dirty_lines = default();

        content.apply_change(insert_in_middle);
        let expected = vec!["Line a", "LiIns a", "Ins bne b", "Line cIns a", "Ins b"];
        assert_eq!(expected, get_lines_as_strings(&content));
        assert!(!content.dirty_lines.is_dirty(0));
        assert!( content.dirty_lines.is_dirty(1));
        assert!( content.dirty_lines.is_dirty(2));
        content.dirty_lines = default();

        content.apply_change(insert_at_begin);
        let expected = vec!["Ins a", "Ins bLine a", "LiIns a", "Ins bne b", "Line cIns a", "Ins b"];
        assert_eq!(expected, get_lines_as_strings(&content));
        assert!( content.dirty_lines.is_dirty(0));
        assert!( content.dirty_lines.is_dirty(1));
        assert!( content.dirty_lines.is_dirty(2));
    }

    #[test]
    fn delete_multiple_lines() {
        let text          = "Line a\nLine b\nLine c";
        let delete_from   = TextLocation {line:0, column:2};
        let delete_to     = TextLocation {line:2, column:3};
        let deleted_range = delete_from..delete_to;
        let delete        = TextChange::delete(deleted_range);

        let mut content   = TextFieldContent::new(text,&mock_properties());
        content.apply_change(delete);

        let expected = vec!["Lie c"];
        assert_eq!(expected, get_lines_as_strings(&content));
    }

    #[test]
    fn get_line_fragment() {
        let text = "Line a\nLine b\nLine c";
        let single_line   = TextLocation {line:1, column:1} .. TextLocation {line:1, column:4};
        let line_with_eol = TextLocation {line:1, column:1} .. TextLocation {line:2, column:0};
        let eol_with_line = TextLocation {line:0, column:6} .. TextLocation {line:1, column:4};
        let multi_line    = TextLocation {line:0, column:4} .. TextLocation {line:1, column:4};
        let whole_content = TextLocation {line:0, column:0} .. TextLocation {line:2, column:6};

        let content = TextFieldContent::new(text,&mock_properties());
        assert_eq!("ine"     , content.copy_fragment(single_line));
        assert_eq!("ine b\n" , content.copy_fragment(line_with_eol));
        assert_eq!("\nLine"  , content.copy_fragment(eol_with_line));
        assert_eq!(" a\nLine", content.copy_fragment(multi_line));
        assert_eq!(text      , content.copy_fragment(whole_content));
    }

    fn get_lines_as_strings(content:&TextFieldContent) -> Vec<String> {
        content.lines.iter().map(|l| l.chars().iter().collect()).collect()
    }

    fn mock_properties()->  TextFieldProperties {
        TextFieldProperties {
            font_id    : 0,
            text_size  : 0.0,
            base_color : Vector4::new(1.0, 1.0, 1.0, 1.0),
            size       : Vector2::new(1.0, 1.0)
        }
    }
}
