#![allow(missing_docs)] // TODO: Fixme

//! A module defining TextField. TextField is a ensogl component displaying editable block of text.

pub mod content;
pub mod cursor;
pub mod frp;
pub mod render;
pub mod word_occurrence;

use crate::prelude::*;

use crate::data::color;
use crate::display;
use crate::display::object::traits::*;
use crate::display::Scene;
use crate::display::shape::text::glyph::font;
use crate::display::shape::text::text_field::content::location::TextLocationChange;
use crate::display::shape::text::text_field::content::TextFieldContent;
use crate::display::shape::text::text_field::cursor::Cursor;
use crate::display::shape::text::text_field::cursor::CursorId;
use crate::display::shape::text::text_field::cursor::CursorNavigation;
use crate::display::shape::text::text_field::cursor::Cursors;
use crate::display::shape::text::text_field::cursor::Step;
use crate::display::shape::text::text_field::frp::TextFieldFrp;
use crate::display::shape::text::text_field::render::assignment::GlyphLinesAssignmentUpdate;
use crate::display::shape::text::text_field::render::TextFieldSprites;
use crate::display::shape::text::text_field::word_occurrence::WordOccurrences;
use crate::system::web::text_input::KeyboardBinding;

use data::text::TextChange;
use data::text::TextLocation;
use nalgebra::Vector2;
use nalgebra::Vector3;



// =====================
// === Focus Manager ===
// =====================

#[derive(Clone,CloneRef,Debug)]
pub struct FocusManager {
    binding    : Rc<RefCell<KeyboardBinding>>,
    focused_on : Rc<CloneCell<Option<WeakTextField>>>,
}

impl FocusManager {
    pub fn new_with_js_handlers() -> Self {
        FocusManager {
            binding    : Rc::new(RefCell::new(KeyboardBinding::create())),
            focused_on : default()
        }
    }

    pub fn set_focus_on(&self, text_field:&TextField) {
        let current = self.focused_on.get().and_then(|ptr| ptr.upgrade());
        let already_focused = current.as_ref().map_or(false, |ptr| (ptr.identity_equals(text_field)));
        if !already_focused {
            current.for_each(|current| current.on_defocus());
            let tf_ref = text_field.rc.borrow();
            let frp    = &tf_ref.frp.as_ref().unwrap().keyboard;
            frp.bind_frp_to_js_text_input_actions(&mut self.binding.borrow_mut());
            self.focused_on.set(Some(text_field.downgrade()))
        }

    }
}



// =====================
// === TextComponent ===
// =====================

/// A display properties of TextField.
#[derive(Debug)]
pub struct TextFieldProperties {
    /// Font handle used for rendering text.
    pub font: font::Handle,
    /// Text size being a line height in pixels.
    pub text_size: f32,
    /// Base color of displayed text.
    pub base_color: color::Rgba,
    /// Size of this component.
    pub size: Vector2<f32>,
}

impl TextFieldProperties {
    const DEFAULT_FONT_FACE:&'static str = "DejaVuSansMono";

    /// A default set of properties.
    pub fn default(fonts:&mut font::Registry) -> Self {
        TextFieldProperties {
            font      : fonts.get_or_load_embedded_font(Self::DEFAULT_FONT_FACE).unwrap(),
            text_size : 16.0,
            base_color: color::Rgba::new(1.0,1.0,1.0,0.0),
            size      : Vector2::new(100.0,100.0),
        }
    }
}

// TODO: All measurements in text field should use the `math/topology/unit` units.

shared! { TextField

    /// Component rendering text
    ///
    /// This component is under heavy construction, so the api may easily changed in few future
    /// commits.
    #[derive(Derivative)]
    #[derivative(Debug)]
    pub struct TextFieldData {
        properties       : TextFieldProperties,
        content          : TextFieldContent,
        cursors          : Cursors,
        rendered         : TextFieldSprites,
        display_object   : display::object::Instance,
        frp              : Option<TextFieldFrp>,
        word_occurrences : Option<WordOccurrences>,
        #[derivative(Debug="ignore")]
        text_change_callback : Option<Box<dyn FnMut(TextChange)>>,
        focus_manager        : FocusManager,
        // TODO[ao] this should be infered from focus_manager, but it requires much refactoring.
        focused              : bool,
    }

    impl {
        /// Get TextField's line height.
        pub fn line_height(&self) -> f32 {
            self.content.line_height
        }

        /// Display object getter.
        pub fn display_object(&self) -> display::object::Instance {
            self.display_object.clone()
        }

        /// Set position of this TextField.
        pub fn set_position(&mut self, position:Vector3<f32>) {
            self.display_object.set_position(position);
        }

        /// Get position of this TextField.
        pub fn position(&self) -> Vector3<f32> {
            self.display_object.position()
        }

        /// Get size.
        pub fn size(&self) -> Vector2<f32> {
            self.properties.size
        }

        /// Set size.
        pub fn set_size(&mut self, size:Vector2<f32>) {
            self.properties.size = size;
            self.rendered.update_lines(&self.properties);
        }

        /// Set color.
        pub fn set_base_color(&mut self, base_color:color::Rgba) {
            self.properties.base_color = base_color;
            self.rendered.update_lines(&self.properties);
        }

        /// Scroll text by given offset in pixels.
        pub fn scroll(&mut self, offset:Vector2<f32>) {
            let scroll_position = self.scroll_position();
            let padding_lines   = 2;
            let lines           = self.content.lines().len() + padding_lines;
            let text_height     = self.content.line_height * lines as f32;
            let view_height     = self.size().y;
            let height          = (text_height - view_height).max(0.0);
            let offset_y        = offset.y.min(scroll_position.y).max(scroll_position.y - height);
            let offset          = Vector2::new(offset.x, offset_y);
            if offset.x != 0.0 || offset.y != 0.0 {
                let position_change = -Vector3::new(offset.x,offset.y,0.0);
                self.rendered.display_object.mod_position(|pos| *pos += position_change);
                let mut update = self.assignment_update();
                if offset.x != 0.0 {
                    update.update_after_x_scroll(offset.x);
                }
                if offset.y != 0.0 {
                    update.update_line_assignment();
                }
                self.rendered.update_glyphs(&mut self.content);
            }
        }

        /// Get current scroll position.
        pub fn scroll_position(&self) -> Vector2<f32> {
            self.rendered.display_object.position().xy()
        }

        /// Clear word occurrences.
        pub fn clear_word_occurrences(&mut self) {
            self.word_occurrences = None;
        }

        /// Finish multicursor mode, removing any additional cursors.
        pub fn finish_multicursor_mode(&mut self) {
            self.cursors.finish_multicursor_mode();
            self.rendered.update_cursor_sprites(&self.cursors,&mut self.content,self.focused);
            self.clear_word_occurrences();
        }

        /// Removes all cursors except one which is set and given point.
        pub fn set_cursor(&mut self, point:Vector2<f32>) {
            self.clear_word_occurrences();
            self.cursors.finish_multicursor_mode();
            self.jump_cursor(point,false);
        }

        /// Add cursor at point on the screen.
        pub fn add_cursor(&mut self, point:Vector2<f32>) {
            self.cursors.add_cursor(TextLocation::at_document_begin());
            self.jump_cursor(point,false);
        }

        /// Jump active cursor to point on the screen.
        pub fn jump_cursor(&mut self, point:Vector2<f32>, selecting:bool) {
            let point_on_text   = self.relative_position(point);
            let text_field_size = self.size();
            let content         = &mut self.content;
            let mut navigation      = CursorNavigation{selecting,content,text_field_size};
            self.cursors.jump_cursor(&mut navigation,point_on_text);
            self.rendered.update_cursor_sprites(&self.cursors, &mut self.content,self.focused);
        }

        /// Processes PageUp and PageDown, scrolling the page accordingly.
        fn scroll_page(&mut self, step:Step) {
            let page_height     = self.size().y;
            let scrolling       = match step {
                Step::PageUp   =>  page_height,
                Step::PageDown => -page_height,
                _              => 0.0
            };

            self.scroll(Vector2::new(0.0,scrolling));
        }

        /// Adjust the view to make the last cursor visible.
        fn adjust_view(&mut self) {
            let last_cursor      = self.cursors.last_cursor();
            let scroll_y         = self.scroll_position().y;
            let view_size        = self.size();
            let current_line     = last_cursor.current_line(&mut self.content);
            let current_line_pos = current_line.y_position();
            let next_line_pos    = current_line_pos + current_line.height;
            let y_scrolling      = (scroll_y - next_line_pos + view_size.y).min(0.0);
            let y_scrolling      = (scroll_y - current_line_pos).max(y_scrolling);
            let scrolling        = Vector2::new(0.0,y_scrolling);
            self.scroll(scrolling);
        }

        /// Move all cursors by given step.
        pub fn navigate_cursors(&mut self, step:Step, selecting:bool) {
            if !selecting {
                self.clear_word_occurrences()
            }
            let text_field_size = self.size();
            let content         = &mut self.content;
            let mut navigation  = CursorNavigation{content,selecting,text_field_size};
            self.cursors.navigate_all_cursors(&mut navigation,step);
            self.scroll_page(step);
            self.adjust_view();
            self.rendered.update_cursor_sprites(&self.cursors, &mut self.content,self.focused);
        }

        /// Discards all current content and replaces it with new one.
        /// Whenever possible, tries to maintain cursor positions.
        pub fn set_content(&mut self, text:&str) {
            self.clear_word_occurrences();
            self.content.set_content(text);
            self.cursors.recalculate_positions(&self.content);
            self.assignment_update().update_after_text_edit();
            self.rendered.update_glyphs(&mut self.content);
            self.rendered.update_cursor_sprites(&self.cursors, &mut self.content,self.focused);
        }

        /// Clear content.
        pub fn clear_content(&mut self) {
            self.set_content("");
        }

        /// Obtains the whole text content as a single String.
        pub fn get_content(&self) -> String {
            let mut line_strings = self.content.lines().iter().map(|l| l.to_string());
            line_strings.join("\n")
        }

        /// Get the selected text.
        pub fn get_selected_text(&self) -> String {
            self.cursors.get_selected_text(&self.content)
        }

        /// Text field has a selected text.
        pub fn has_selection(&self) -> bool {
            self.cursors.cursors.iter().any(|cursor| cursor.has_selection())
        }

        /// Transforms `absolute_position` to relative position from TextField's content origin.
        pub fn relative_position(&self, absolute_position:Vector2<f32>) -> Vector2<f32> {
            absolute_position - self.rendered.display_object.global_position().xy()
        }

        /// Block selects a text from active cursor's position to screen `position`.
        pub fn block_selection(&mut self, position:Vector2<f32>) {
            let point_on_text = self.relative_position(position);
            self.cursors.block_selection(&mut self.content, point_on_text);
            self.rendered.update_cursor_sprites(&self.cursors, &mut self.content,self.focused);
        }

        /// Selects the current word, if the cursor is inside a word, or select a next word if a
        /// word is already selected. For definition of word check `word_occurrence` module doc.
        pub fn select_next_word_occurrence(&mut self) {
            let not_multicursors = self.cursors.cursors.len() == 1;
            if self.word_occurrences.is_none() && not_multicursors {
                let cursor            = self.cursors.last_cursor();
                self.word_occurrences = WordOccurrences::new(&self.content,&cursor);
            }

            let has_selection             = self.has_selection();
            if let Some(word_occurrences) = &mut self.word_occurrences {
                if let Some(word) = word_occurrences.select_next() {
                    if has_selection {
                        self.cursors.add_cursor(TextLocation::at_document_begin());
                    }

                    let cursor = self.cursors.last_cursor_mut();
                    cursor.select_range(&word);
                    let focused = self.focused;
                    self.rendered.update_cursor_sprites(&self.cursors,&mut self.content,focused);
                }
            }
        }

        /// Check if given point on screen is inside this TextField.
        pub fn is_inside(&self, point:Vector2<f32>) -> bool {
            let position = self.display_object.global_position();
            let size     = self.properties.size;
            let x_range  = position.x ..= (position.x + size.x);
            let y_range  = (position.y - size.y) ..= position.y;
            x_range.contains(&point.x) && y_range.contains(&point.y)
        }

        /// Set text edit callback.
        ///
        /// This callback will be called once per `write` function call and all functions using it.
        /// That's include all edits being an effect of keyboard or mouse event.
        pub fn set_text_edit_callback<Callback:FnMut(TextChange) + 'static>
        (&mut self, callback:Callback) {
            self.text_change_callback = Some(Box::new(callback))
        }

        /// Tell if the TextField is focused.
        pub fn is_focused(&self) -> bool {
            self.focused
        }

        fn on_defocus(&mut self) {
            self.focused = false;
            self.rendered.update_cursor_sprites(&self.cursors,&mut self.content,self.focused);
        }
    }
}


// === Public ===

impl TextField {
    /// Create new empty TextField
    pub fn new<'t,S:Into<&'t Scene>>
    (scene:S, properties:TextFieldProperties,focus_manager:&FocusManager) -> Self {
        Self::new_with_content(scene,"",properties,focus_manager)
    }

    /// Create new TextField with predefined content.
    pub fn new_with_content<'t,S:Into<&'t Scene>>
    (scene:S, initial_content:&str, properties:TextFieldProperties,focus_manager:&FocusManager)
    -> Self {
        let scene = scene.into();
        let data = TextFieldData::new(scene,initial_content,properties,focus_manager);
        let rc   = Rc::new(RefCell::new(data));
        let this = Self {rc};
        let frp  = TextFieldFrp::new(scene,this.downgrade());
        this.with_borrowed(move |mut data| { data.frp = Some(frp); });
        this
    }

    pub fn set_focus(&self) {
        let focus_manager = self.with_borrowed(|data| data.focus_manager.clone_ref());
        focus_manager.set_focus_on(&self);
        self.with_borrowed(|data| data.focused = true);
    }
}


// === Editing text ===

impl TextField {
    /// Edit text.
    ///
    /// All the currently selected text will be removed, and the given string will be inserted
    /// by each cursor.
    pub fn write(&self, text:&str) {
        let trimmed    = text.trim_end_matches('\n');
        let cursor_ids = self.with_borrowed(|this| this.cursors.sorted_cursor_indices());
        // When we insert (e.g. paste) many lines in multicursor mode, under some circumnstances
        // we insert one line per cursor, instead of having all cursors inserting the whole
        // content. Such situation we call here Line Per Cursor Edit.
        let is_line_per_cursor_edit = trimmed.contains('\n') && cursor_ids.len() > 1;

        if is_line_per_cursor_edit {
            let cursor_with_line = cursor_ids.iter().cloned().zip(trimmed.split('\n'));
            self.write_per_cursor(cursor_with_line);
        } else {
            let cursor_with_line = cursor_ids.iter().map(|cursor_id| (*cursor_id,text));
            self.write_per_cursor(cursor_with_line);
        };
        self.with_borrowed(|this| {
            this.clear_word_occurrences();
            // TODO[ao] updates should be done only in one place and only once per frame
            // see https://github.com/luna/ide/issues/178
            this.assignment_update().update_after_text_edit();
            this.adjust_view();
            this.rendered.update_glyphs(&mut this.content);
            this.rendered.update_cursor_sprites(&this.cursors, &mut this.content, this.focused);
        });
    }

    /// Remove all text selected by all cursors.
    pub fn remove_selection(&self) {
        self.write("");
    }

    /// Do delete operation on text.
    ///
    /// For cursors with selection it will just remove the selected text. For the rest, it will
    /// remove all content covered by `step`.
    pub fn do_delete_operation(&self, step:Step) {
        let text_field_size = self.size();
        self.with_borrowed(|this| {
            let content           = &mut this.content;
            let selecting         = true;
            let mut navigation    = CursorNavigation{selecting,content,text_field_size};
            let without_selection = |c:&Cursor| !c.has_selection();
            this.cursors.navigate_cursors(&mut navigation,step,without_selection);
        });
        self.remove_selection();
    }
}


// === Private ===

impl TextField {

    fn write_per_cursor<'a,It>(&self, text_per_cursor:It)
        where It : Iterator<Item=(CursorId,&'a str)> {
        let mut location_change = TextLocationChange::default();
        let mut opt_callback    = self.with_borrowed(|this| std::mem::take(&mut this.text_change_callback));
        for (cursor_id,to_insert) in text_per_cursor {
            let notification = self.with_borrowed(|this| {
                this.apply_one_cursor_change(&mut location_change,cursor_id,to_insert)
            });
            if let Some(callback) = opt_callback.as_mut() {
                callback(notification);
            }
        }
        self.with_borrowed(|this| {
            if this.text_change_callback.is_none() {
                this.text_change_callback = opt_callback
            }
        });
    }
}

impl TextFieldData {
    fn new<'t,S:Into<&'t Scene>>
    ( scene           : S
    , initial_content : &str
    , properties      : TextFieldProperties
    , focus_manager   : &FocusManager
    ) -> Self {
        let logger               = Logger::new("TextField");
        let display_object       = display::object::Instance::new(logger);
        let content              = TextFieldContent::new(initial_content,&properties);
        let cursors              = Cursors::default();
        let rendered             = TextFieldSprites::new(scene,&properties);
        let frp                  = None;
        let word_occurrences     = None;
        let text_change_callback = None;
        let focus_manager        = focus_manager.clone_ref();
        let focused              = false;
        display_object.add_child(&rendered);

        Self {properties,content,cursors,rendered,display_object,frp,word_occurrences,
              text_change_callback,focus_manager,focused}.initialize()
    }

    fn initialize(mut self) -> Self {
        self.assignment_update().update_line_assignment();
        self.rendered.update_glyphs(&mut self.content);
        self.rendered.update_cursor_sprites(&self.cursors,&mut self.content,self.focused);
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

    /// Applies change for one cursor, updating its position, and returns struct which should be
    /// passed to `text_change_callback`.
    fn apply_one_cursor_change
    (&mut self, location_change:&mut TextLocationChange, cursor_id:CursorId, to_insert:&str)
    -> TextChange {
        let CursorId(id)   = cursor_id;
        let cursor         = &mut self.cursors.cursors[id];
        let replaced       = location_change.apply_to_range(cursor.selection_range());
        let replaced_chars = self.content.convert_location_range_to_char_index(&replaced);
        let change         = content::Change::replace(replaced,to_insert);
        location_change.add_change(&change);
        *cursor = Cursor::new(change.inserted_text_range().end);
        self.content.apply_change(change);
        TextChange::replace(replaced_chars, to_insert.to_string())
    }
}

// === Display Object ===

//impl From<&TextField> for display::object::Instance {
//    fn from(text_fields: &TextField) -> Self {
//        text_fields.rc.borrow().display_object.clone_ref()
//    }
//}
