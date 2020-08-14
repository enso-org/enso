//! Module with all structures describing the content of the TextField.
pub mod line;
pub mod location;

use crate::prelude::*;

use crate::display::shape::text::glyph::font;
use crate::display::shape::text::text_field::content::line::Line;
use crate::display::shape::text::text_field::content::line::LineFullInfo;
use crate::display::shape::text::text_field::TextFieldProperties;

use data::text;
use data::text::TextChangeTemplate;
use data::text::TextLocation;
use data::text::split_to_lines;
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

/// A type representing change applied on TextFieldContent.
#[derive(Debug,Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Change(pub TextChangeTemplate<TextLocation,Vec<Vec<char>>>);

impl Change {
    /// Creates operation which inserts text at given position.
    pub fn insert(at:TextLocation, text:&str) -> Self {
        Self(TextChangeTemplate::insert(at,Self::mk_lines_as_char_vector(text)))
    }

    /// Creates operation which deletes text at given range.
    pub fn delete(range:Range<TextLocation>) -> Self {
        Self(TextChangeTemplate::delete(range))
    }

    /// Creates operation which replaces text at given range with given string.
    pub fn replace(replaced:Range<TextLocation>, text:&str) -> Self {
        Self(TextChangeTemplate::replace(replaced,Self::mk_lines_as_char_vector(text)))
    }

    /// Converts change representation to String.
    pub fn inserted_string(&self) -> String {
        self.inserted.iter().map(|line| line.iter().collect::<String>()).join("\n")
    }

    /// A type of this change. See `ChangeType` doc for details.
    pub fn change_type(&self) -> ChangeType {
        let is_one_line_modified        = self.replaced.start.line == self.replaced.end.line;
        let is_mostly_one_line_inserted = self.inserted.len() <= 1;
        if is_one_line_modified && is_mostly_one_line_inserted {
            ChangeType::SingleLine
        } else {
            ChangeType::MultiLine
        }
    }

    /// Returns text location range where the inserted text will appear after making this change.
    pub fn inserted_text_range(&self) -> Range<TextLocation> {
        let start         = self.replaced.start;
        let end_line      = start.line + self.inserted.len().saturating_sub(1);
        let last_line_len = self.inserted.last().map_or(0, |l| l.len());
        let end_column = if start.line == end_line {
            start.column + last_line_len
        } else {
            last_line_len
        };
        start..TextLocation{line:end_line, column:end_column}
    }

    fn mk_lines_as_char_vector(text:&str) -> Vec<Vec<char>> {
        split_to_lines(text).map(|s| s.chars().collect_vec()).collect()
    }
}



// ========================
// === TextFieldContent ===
// ========================

/// The content of text component - namely lines of text.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct TextFieldContent {
    /// A struct which describe which lines are dirty (were modified after last rendering).
    pub dirty_lines: DirtyLines,
    /// Font handle, used to specify character positions.
    #[derivative(Debug="ignore")]
    pub font: font::Handle,
    /// Line height in pixels, being a distance between baselines of consecutive lines.
    pub line_height: f32,
    /// Lines being the actual content.
    lines: Vec<Line>,
    /// This field caches the position of beginning of each line.
    line_offsets: Vec<usize>,
}

impl TextFieldContent {
    /// Create a text component containing `text`.
    ///
    /// The text will be split to lines on the `'\n'` character.
    pub fn new(text:&str, properties:&TextFieldProperties) -> Self {
        TextFieldContent {
            line_height  : properties.text_size,
            lines        : split_to_lines(text).map(Line::new).collect(),
            dirty_lines  : DirtyLines::default(),
            font         : properties.font.clone_ref(),
            line_offsets : Vec::new(),
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

    /// A `lines` accessor.
    ///
    /// It's a little bit quicker than getting specific line by `line` method, because it don't
    /// need to copy any additional data.
    pub fn lines(&self) -> &[Line] {
        self.lines.as_slice()
    }

    /// A mutablie `lines` accessor.
    ///
    /// Please be noted, that this invalidate some cached data.
    pub fn lines_mut(&mut self) -> &mut Vec<Line> {
        self.line_offsets.clear();
        &mut self.lines
    }

    /// Get a handy wrapper for line under index.
    pub fn line(&mut self, index:usize) -> LineFullInfo {
        LineFullInfo {
            height  : self.line_height,
            line    : &mut self.lines[index],
            line_id : index,
            font    : self.font.clone_ref(),
        }
    }

    /// Replaces content with a new text. This marks all lines as dirty.
    pub fn set_content(&mut self, text:&str) {
        self.lines = split_to_lines(text).map(Line::new).collect();
        self.line_offsets.clear();
        self.dirty_lines.add_lines_range_from(0..);
    }

    /// Get the nearest line from the point on the screen.
    pub fn line_location_at_point(&mut self, point:Vector2<f32>) -> usize {
        let line_opt = self.line_at_y_position(point.y);
        let line     = match line_opt {
            Some(line)             => line,
            None if point.y >= 0.0 => self.line(0),
            None                   => self.line(self.lines.len()-1),
        };
        line.line_id
    }

    /// Get the nearest column from te point on the screen.
    pub fn column_location_at_point(&mut self, line:usize, point:Vector2<f32>) -> usize {
        let mut line   = self.line(line);
        let column_opt = line.find_char_at_x_position(point.x);
        match column_opt {
            Some(column)           => column,
            None if point.x <= 0.0 => 0,
            None                   => line.len()
        }
    }

    /// Get the nearest text location from the point on the screen.
    pub fn location_at_point(&mut self, point:Vector2<f32>) -> TextLocation {
        let line   = self.line_location_at_point(point);
        let column = self.column_location_at_point(line,point);
        TextLocation{line,column}
    }

    /// Get the index of line which is displayed at given y screen coordinate.
    pub fn line_at_y_position(&mut self, y:f32) -> Option<LineFullInfo> {
        let index    = -(y / self.line_height).ceil();
        let is_valid = index >= 0.0 && index < self.lines.len() as f32;
        let index    = is_valid.and_option_from(|| Some(index as usize));
        index.map(move |i| self.line(i))
    }

    /// Converts location in this text represented by `row:column` pair to absolute char's position
    /// from document begin. Panics if location is invalid for current content.
    pub fn convert_location_to_char_index(&mut self, location:TextLocation) -> text::Index {
        if self.line_offsets.is_empty() {
            self.line_offsets.push(0);
        }
        if self.line_offsets.len() <= location.line {
            let required_len           = location.line + 1;
            let mut current_char_index = *self.line_offsets.last().unwrap();
            for line in self.lines.iter().take(required_len).skip(self.line_offsets.len()-1) {
                current_char_index += line.len() + 1;
                self.line_offsets.push(current_char_index);
            }
        }
        text::Index::new(self.line_offsets[location.line] + location.column)
    }

    /// Converts range of locations in this text represented by `row:column` pair to absolute
    /// char's position from document begin.
    pub fn convert_location_range_to_char_index(&mut self, range:&Range<TextLocation>)
    -> Range<text::Index> {
        let start = self.convert_location_to_char_index(range.start);
        let end   = self.convert_location_to_char_index(range.end);
        start..end
    }
}


// === Implementing Changes ===

impl TextFieldContent {
    /// Apply change to content.
    pub fn apply_change(&mut self, change:Change) {
        let first_modified_line = change.replaced.start.line;
        match change.change_type() {
            ChangeType::SingleLine => self.make_simple_change(change),
            ChangeType::MultiLine  => self.make_multiline_change(change),
        }
        // Invalidate lines offsets
        let first_offset_invalidated = first_modified_line + 1;
        self.line_offsets.resize(first_offset_invalidated,default());
    }

    /// Apply many changes to content.
    pub fn apply_changes<Changes:IntoIterator<Item=Change>>(&mut self, changes:Changes) {
        let change_key  = |chg:&Change | chg.replaced.start;
        let changes_vec = changes.into_iter().sorted_by_key(change_key);
        changes_vec.rev().for_each(|change| self.apply_change(change));
    }

    fn make_simple_change(&mut self, change:Change) {
        let line_index  = change.replaced.start.line;
        let empty_line  = default();
        let new_content = change.inserted.first().unwrap_or(&empty_line);
        let range       = change.replaced.start.column..change.replaced.end.column;
        self.lines[line_index].modify().splice(range,new_content.iter().cloned());
        self.dirty_lines.add_single_line(line_index);
    }

    fn make_multiline_change(&mut self, mut change:Change) {
        self.mix_content_into_change(&mut change);
        let start_line           = change.replaced.start.line;
        let end_line             = change.replaced.end.line;
        let replaced_lines_count = end_line - start_line + 1;
        let inserted_lines_count = change.inserted.len();
        let inserted_lines       = change.inserted.drain(0..inserted_lines_count).map(Line::new_raw);
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
    fn mix_content_into_change(&mut self, change:&mut Change) {
        if change.inserted.is_empty() {
            change.inserted.push(default())
        }
        self.mix_first_edited_line_into_change(change);
        self.mix_last_edited_line_into_change(change);
    }

    fn mix_first_edited_line_into_change(&self, change:&mut Change) {
        let first_line   = change.replaced.start.line;
        let replace_from = change.replaced.start.column;
        let first_edited = &self.lines[first_line].chars();
        let prefix       = &first_edited[..replace_from];
        change.inserted.first_mut().unwrap().splice(0..0, prefix.iter().cloned());
    }

    fn mix_last_edited_line_into_change(&mut self, change:&mut Change) {
        let last_line    = change.replaced.end.line;
        let replace_to   = change.replaced.end.column;
        let last_edited  = &self.lines[last_line].chars();
        let suffix       = &last_edited[replace_to..];
        change.inserted.last_mut().unwrap().extend_from_slice(suffix);
    }
}



#[cfg(test)]
pub(crate) mod test {
    use super::*;

    use crate::data::color;
    use crate::display::shape::text::glyph::font;

    use ensogl_text_msdf_sys as msdf_sys;
    use nalgebra::Vector2;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    async fn mark_single_line_as_dirty(){
        msdf_sys::initialized().await;
        let mut dirty_lines = DirtyLines::default();
        dirty_lines.add_single_line(3);
        dirty_lines.add_single_line(5);
        assert!( dirty_lines.is_dirty(3));
        assert!(!dirty_lines.is_dirty(4));
        assert!( dirty_lines.is_dirty(5));
    }

    #[wasm_bindgen_test(async)]
    async fn mark_line_range_as_dirty() {
        msdf_sys::initialized().await;
        let mut dirty_lines = DirtyLines::default();
        dirty_lines.add_lines_range(3..=5);
        assert!(!dirty_lines.is_dirty(2));
        assert!( dirty_lines.is_dirty(3));
        assert!( dirty_lines.is_dirty(4));
        assert!( dirty_lines.is_dirty(5));
        assert!(!dirty_lines.is_dirty(6));
    }

    #[wasm_bindgen_test(async)]
    async fn mark_line_range_from_as_dirty() {
        msdf_sys::initialized().await;
        let mut dirty_lines = DirtyLines::default();
        dirty_lines.add_lines_range_from(3..);
        dirty_lines.add_lines_range_from(5..);
        assert!(!dirty_lines.is_dirty(2));
        assert!( dirty_lines.is_dirty(3));
        assert!( dirty_lines.is_dirty(4));
        assert!( dirty_lines.is_dirty(70000));
    }

    #[wasm_bindgen_test(async)]
    async fn create_content() {
        msdf_sys::initialized().await;
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

    #[wasm_bindgen_test(async)]
    async fn edit_single_line() {
        msdf_sys::initialized().await;
        let text                   = "Line a\nLine b\nLine c";
        let delete_from            = TextLocation {line:1, column:0};
        let delete_to              = TextLocation {line:1, column:4};
        let deleted_range          = delete_from..delete_to;
        let insert                 = Change::insert(TextLocation {line:1, column:1}, "ab");
        let delete                 = Change::delete(deleted_range.clone());
        let replace                = Change::replace(deleted_range, "text");

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

    #[wasm_bindgen_test(async)]
    async fn insert_multiple_lines() {
        msdf_sys::initialized().await;
        let text             = "Line a\nLine b\nLine c";
        let inserted         = "Ins a\nIns b";
        let begin_loc        = TextLocation {line:0, column:0};
        let middle_loc       = TextLocation {line:1, column:2};
        let end_loc          = TextLocation {line:2, column:6};
        let insert_at_begin  = Change::insert(begin_loc, inserted);
        let insert_in_middle = Change::insert(middle_loc, inserted);
        let insert_at_end    = Change::insert(end_loc, inserted);

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

    #[wasm_bindgen_test(async)]
    async fn set_content_doesnt_break_location_conversion() {
        // Regression test for the issues:
        // https://github.com/luna/ide/issues/513
        // https://github.com/luna/ide/issues/516
        msdf_sys::initialized().await;
        let text          = "Line a\nLine b\nLine c";
        let second_text   = "Line new first\nLine b\nLine c";
        let text_location = TextLocation{line:1, column:1};
        let mut content   = TextFieldContent::new(text,&mock_properties());
        assert_eq!(content.convert_location_to_char_index(text_location).value,8);
        content.set_content(second_text);
        assert_eq!(content.convert_location_to_char_index(text_location).value,16);
    }

    #[wasm_bindgen_test(async)]
    async fn delete_multiple_lines() {
        msdf_sys::initialized().await;
        let text          = "Line a\nLine b\nLine c";
        let delete_from   = TextLocation {line:0, column:2};
        let delete_to     = TextLocation {line:2, column:3};
        let deleted_range = delete_from..delete_to;
        let delete        = Change::delete(deleted_range);

        let mut content   = TextFieldContent::new(text,&mock_properties());
        content.apply_change(delete);

        let expected = vec!["Lie c"];
        assert_eq!(expected, get_lines_as_strings(&content));
    }

    #[wasm_bindgen_test(async)]
    async fn get_line_fragment() {
        msdf_sys::initialized().await;
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

    #[wasm_bindgen_test(async)]
    async fn get_inserted_text_location_of_change() {
        msdf_sys::initialized().await;
        let one_line         = "One line";
        let two_lines        = "Two\nlines";
        let replaced_range   = TextLocation{line:1,column:2}..TextLocation{line:2,column:2};

        let one_line_change  = Change::replace(replaced_range.clone(), one_line);
        let two_lines_change = Change::replace(replaced_range.clone(), two_lines);

        let one_line_expected  = replaced_range.start..TextLocation{line:1, column:10};
        let two_lines_expected = replaced_range.start..TextLocation{line:2, column:5 };
        assert_eq!(one_line_expected , one_line_change .inserted_text_range());
        assert_eq!(two_lines_expected, two_lines_change.inserted_text_range());
    }

    #[wasm_bindgen_test(async)]
    async fn converting_location_to_offset() {
        msdf_sys::initialized().await;
        let text             = "First\nSecond\nThird";
        let mut content      = TextFieldContent::new(text,&mock_properties());

        assert_eq!(0 ,content.convert_location_to_char_index(TextLocation{line:0, column:0}).value);
        assert_eq!(2 ,content.convert_location_to_char_index(TextLocation{line:0, column:2}).value);
        assert_eq!(5 ,content.convert_location_to_char_index(TextLocation{line:0, column:5}).value);
        assert_eq!(6 ,content.convert_location_to_char_index(TextLocation{line:1, column:0}).value);
        assert_eq!(12,content.convert_location_to_char_index(TextLocation{line:1, column:6}).value);
        assert_eq!(13,content.convert_location_to_char_index(TextLocation{line:2, column:0}).value);
        assert_eq!(18,content.convert_location_to_char_index(TextLocation{line:2, column:5}).value);

        let removed_range = TextLocation{line:1, column:1}..TextLocation{line:1, column:3};
        let change        = Change::delete(removed_range);
        content.apply_change(change);

        assert_eq!(10,content.convert_location_to_char_index(TextLocation{line:1, column:4}).value);
        assert_eq!(16,content.convert_location_to_char_index(TextLocation{line:2, column:5}).value);
    }

    fn get_lines_as_strings(content:&TextFieldContent) -> Vec<String> {
        content.lines.iter().map(|l| l.chars().iter().collect()).collect()
    }

    pub(crate) fn mock_properties()->  TextFieldProperties {
        TextFieldProperties {
            font       : font::Handle::new(font::RenderInfo::mock_font("Test font".to_string())),
            text_size  : 0.0,
            base_color : color::Rgba::new(1.0, 1.0, 1.0, 1.0),
            size       : Vector2::new(1.0, 1.0)
        }
    }
}
