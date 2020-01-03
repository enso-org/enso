pub mod line;

use crate::prelude::*;

use crate::display::shape::text::font::FontId;
use crate::display::shape::text::font::FontRenderInfo;
use crate::display::shape::text::font::Fonts;
use crate::display::shape::text::content::line::Line;
use crate::display::shape::text::content::line::LineRef;

use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeInclusive;


// ==================
// === DirtyLines ===
// ==================

/// Set of dirty lines' indices
#[derive(Debug)]
pub struct DirtyLines {
    pub single_lines : HashSet<usize>,
    pub range        : Option<RangeFrom<usize>>
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
///
/// A change is simple if it's replace a fragment of one line with text without new lines. Otherwise
/// its a multiline change.
pub enum ChangeType {
    Simple, Multiline
}

/// A structure describing a text operation in one place.
pub struct TextChange {
    replaced : Range<CharPosition>,
    lines    : Vec<Vec<char>>,
}

impl TextChange {
    /// Creates operation which inserts text at given position.
    pub fn insert(position:CharPosition, text:&str) -> Self {
        TextChange {
            replaced : position..position,
            lines    : TextComponentContent::split_to_lines(text).map(|s| s.chars().collect_vec()).collect()
        }
    }

    /// Creates operation which deletes text at given range.
    pub fn delete(range:Range<CharPosition>) -> Self {
        TextChange {
            replaced : range,
            lines    : vec![vec![]],
        }
    }

    /// Creates operation which replaces text at given range with given string.
    pub fn replace(replaced:Range<CharPosition>, text:&str) -> Self {
        TextChange {replaced,
            lines : TextComponentContent::split_to_lines(text).map(|s| s.chars().collect_vec()).collect()
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
            ChangeType::Simple
        } else {
            ChangeType::Multiline
        }
    }
}

// ============================
// === TextComponentContent ===
// ============================

/// The content of text component - namely lines of text.
#[derive(Debug)]
pub struct TextComponentContent {
    pub lines       : Vec<Line>,
    pub dirty_lines : DirtyLines,
    pub font        : FontId,
}

/// A position of character in multiline text.
#[derive(Copy,Clone,Debug,PartialEq,Eq,PartialOrd,Ord)]
pub struct CharPosition {
    pub line   : usize,
    pub column : usize,
}

/// References to all needed stuff for generating buffer's data.
pub struct RefreshInfo<'a, 'b> {
    pub lines            : &'a mut [Line],
    pub dirty_lines      : DirtyLines,
    pub font             : &'b mut FontRenderInfo,
}

impl TextComponentContent {
    /// Create a text component containing `text`
    ///
    /// The text will be split to lines by `'\n'` characters.
    pub fn new(font_id:FontId, text:&str) -> Self {
        TextComponentContent {
            lines       : Self::split_to_lines(text).map(Line::new).collect(),
            dirty_lines : DirtyLines::default(),
            font        : font_id,
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

    /// LineRef structure for line at given index.
    pub fn line(& mut self, index:usize) -> LineRef {
        LineRef {
            line    : &mut self.lines[index],
            line_id : index,
        }
    }

    /// Get RefreshInfo for this content.
    ///
    /// The dirty flags for lines are moved to returned content, so the `self` dirty flags will be
    /// cleared after this call.
    pub fn refresh_info<'a,'b>(&'a mut self, fonts:&'b mut Fonts) -> RefreshInfo<'a,'b> {
        RefreshInfo {
            lines       : &mut self.lines,
            dirty_lines : std::mem::take(&mut self.dirty_lines),
            font        : fonts.get_render_info(self.font),
        }
    }

    /// Apply change to content.
    pub fn make_change(&mut self, change:TextChange) {
        match change.change_type() {
            ChangeType::Simple    => self.make_simple_change(change),
            ChangeType::Multiline => self.make_multiline_change(change),
        }
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


#[cfg(test)]
mod test {
    use super::*;

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
        let font_id        = 0;
        let single_line    = "Single line";
        let mutliple_lines = "Multiple\r\nlines\n";

        let single_line_content = TextComponentContent::new(font_id,single_line);
        let multiline_content   = TextComponentContent::new(font_id,mutliple_lines);
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
        let delete_from            = CharPosition{line:1, column:0};
        let delete_to              = CharPosition{line:1, column:4};
        let deleted_range          = delete_from..delete_to;
        let insert                 = TextChange::insert(CharPosition{line:1, column:1}, "ab");
        let delete                 = TextChange::delete(deleted_range.clone());
        let replace                = TextChange::replace(deleted_range, "text");

        let mut content            = TextComponentContent::new(0, text);

        content.make_change(insert);
        let expected              = vec!["Line a", "Labine b", "Line c"];
        assert_eq!(expected, get_lines_as_strings(&content));

        content.make_change(delete);
        let expected = vec!["Line a", "ne b", "Line c"];
        assert_eq!(expected, get_lines_as_strings(&content));

        content.make_change(replace);
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
        let begin_position   = CharPosition{line:0, column:0};
        let middle_position  = CharPosition{line:1, column:2};
        let end_position     = CharPosition{line:2, column:6};
        let insert_at_begin  = TextChange::insert(begin_position , inserted);
        let insert_in_middle = TextChange::insert(middle_position, inserted);
        let insert_at_end    = TextChange::insert(end_position   , inserted);

        let mut content      = TextComponentContent::new(0,text);

        content.make_change(insert_at_end);
        let expected = vec!["Line a", "Line b", "Line cIns a", "Ins b"];
        assert_eq!(expected, get_lines_as_strings(&content));
        assert!(!content.dirty_lines.is_dirty(0));
        assert!(!content.dirty_lines.is_dirty(1));
        assert!( content.dirty_lines.is_dirty(2));
        content.dirty_lines = default();

        content.make_change(insert_in_middle);
        let expected = vec!["Line a", "LiIns a", "Ins bne b", "Line cIns a", "Ins b"];
        assert_eq!(expected, get_lines_as_strings(&content));
        assert!(!content.dirty_lines.is_dirty(0));
        assert!( content.dirty_lines.is_dirty(1));
        assert!( content.dirty_lines.is_dirty(2));
        content.dirty_lines = default();

        content.make_change(insert_at_begin);
        let expected = vec!["Ins a", "Ins bLine a", "LiIns a", "Ins bne b", "Line cIns a", "Ins b"];
        assert_eq!(expected, get_lines_as_strings(&content));
        assert!( content.dirty_lines.is_dirty(0));
        assert!( content.dirty_lines.is_dirty(1));
        assert!( content.dirty_lines.is_dirty(2));
    }

    #[test]
    fn delete_multiple_lines() {
        let text          = "Line a\nLine b\nLine c";
        let delete_from   = CharPosition{line:0, column:2};
        let delete_to     = CharPosition{line:2, column:3};
        let deleted_range = delete_from..delete_to;
        let delete        = TextChange::delete(deleted_range);

        let mut content   = TextComponentContent::new(0,text);
        content.make_change(delete);

        let expected = vec!["Lie c"];
        assert_eq!(expected, get_lines_as_strings(&content));
    }

    fn get_lines_as_strings(content:&TextComponentContent) -> Vec<String> {
        content.lines.iter().map(|l| l.chars().iter().collect()).collect()
    }
}
