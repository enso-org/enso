//! A single entry in Select
use crate::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatch;
use ensogl_text as text;
use ensogl_theme;
use ensogl_core::data::color;


// =================
// === Constants ===
// =================

/// Padding inside entry in pixels.
pub const PADDING:f32 = 14.0;
/// The overall entry's height (including padding).
pub const HEIGHT:f32 = 30.0;
/// The text size of entry's labe.
pub const LABEL_SIZE:f32 = 12.0;



// =============
// === Entry ===
// =============

/// Entry id. 0 is the first entry in component.
pub type Id = usize;


/// Display objects should implement this trait to be usable as list entries. The origin should be
/// at the center left of the object. The intended height of entries is given by [`Height`].
pub trait Entry: display::Object + Debug {

    /// Sets whether this entry is focused, which means that it is selected and the list view has
    /// focus. In this case, the entry can change its color or visual weight.
    fn set_focused(&self, selected:bool);

    /// Sets the width of the entry.
    fn set_width(&self, width:f32);
}

/// A wrapper around `Rc<dyn Entry>`.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct AnyEntry(Rc<dyn Entry>);

impl<T: Entry + 'static> From<T> for AnyEntry {
    fn from(entry:T) -> Self { Self(Rc::new(entry)) }
}

impl<T: Entry + 'static> From<Rc<T>> for AnyEntry {
    fn from(entry:Rc<T>) -> Self { Self(entry) }
}

impl display::Object for AnyEntry {
    fn display_object(&self) -> &display::object::Instance {
        self.0.display_object()
    }
}



// === Entry Provider ===

/// The Entry Provider for [`ListView`] component.
///
/// The select does not display all entries at once, instead it lazily ask for entries when they're
/// about to be displayed. So setting the select content is essentially providing implementor of
/// this trait.
pub trait EntryProvider: Debug {
    /// Number of all entries.
    fn entry_count(&self) -> usize;

    /// Get the entry with given id. The implementors should return `None` only when the requested
    /// id is greater or equal to the entry count.
    fn get(&self, app:&Application, id:Id) -> Option<AnyEntry>;
}

/// A wrapper for shared instance of some EntryProvider.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct AnyEntryProvider(Rc<dyn EntryProvider>);

impl<T: EntryProvider + 'static> From<T> for AnyEntryProvider {
    fn from(provider:T) -> Self { Self(Rc::new(provider)) }
}

impl<T: EntryProvider + 'static> From<Rc<T>> for AnyEntryProvider {
    fn from(provider:Rc<T>) -> Self { Self(provider) }
}

impl Default for AnyEntryProvider {
    fn default() -> Self {EmptyProvider.into()}
}


// === Empty Entry Provider ===

/// An Entry Provider giving no entries.
///
/// This is the default provider for new select components.
#[derive(Clone,CloneRef,Copy,Debug)]
pub struct EmptyProvider;

#[derive(Debug,Copy,Clone)]
enum EmptyProviderEntry {}

impl display::Object for EmptyProviderEntry {
    fn display_object(&self) -> &display::object::Instance {
        match *self {}
    }
}

impl Entry for EmptyProviderEntry {
    fn set_focused(&self, _selected: bool) {}

    fn set_width(&self, _width: f32) {}
}

impl EntryProvider for EmptyProvider {
    fn entry_count(&self) -> usize {
        0
    }

    fn get(&self, _:&Application, _:usize) -> Option<AnyEntry> {
        None
    }
}


// === StringEntry ===

#[derive(Debug)]
struct StringEntry {
    display_object : display::object::Instance,
    label          : text::Area,
}

impl StringEntry {
    fn new(app:&Application, string:&str) -> Self {
        let logger = Logger::new("StringEntry");
        let display_object = display::object::Instance::new(logger);

        let label = text::Area::new(app);
        label.add_to_scene_layer(&app.display.scene().layers.label);
        display_object.add_child(&label);
        let styles = StyleWatch::new(&app.display.scene().style_sheet);
        let text_color = styles.get_color(ensogl_theme::widget::list_view::text);
        label.set_default_color(text_color);
        label.set_default_text_size(text::Size(LABEL_SIZE));
        label.set_position_xy(Vector2(PADDING,LABEL_SIZE/2.0));
        label.set_content(string);

        Self {display_object,label}
    }
}

impl display::Object for StringEntry {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl Entry for StringEntry {
    fn set_focused(&self, focused: bool) {
        self.label.set_color_all(if focused {color::Rgba::white()} else {color::Rgba::black()});
    }

    fn set_width(&self, _width: f32) {}
}


// === VecEntryProvider ===

#[derive(Debug,Shrinkwrap)]
struct VecEntryProvider(Rc<Vec<String>>);

impl EntryProvider for VecEntryProvider {
    fn entry_count(&self) -> usize {
        self.len()
    }

    fn get(&self, app:&Application, id:usize) -> Option<AnyEntry> {
        let string = self.0.get(id)?;
        Some(StringEntry::new(app,string).into())
    }
}

impl From<Rc<Vec<String>>> for AnyEntryProvider {
    fn from(entries: Rc<Vec<String>>) -> Self {
        VecEntryProvider(entries).into()
    }
}



// =================
// === EntryList ===
// =================

/// The output of `entry_at_y_position`
#[allow(missing_docs)]
#[derive(Copy,Clone,Debug,Eq,Hash,PartialEq)]
pub enum IdAtYPosition {
    AboveFirst, UnderLast, Entry(Id)
}

impl IdAtYPosition {
    /// Returns id of entry if present.
    pub fn entry(&self) -> Option<Id> {
        if let Self::Entry(id) = self { Some(*id) }
        else                          { None      }
    }
}

/// A view containing an entry list, arranged in column.
///
/// Not all entries are displayed at once, only those visible.
///
/// [`set_selection`] can be used to set one entry as selected. [`set_focused`] can be used to mark
/// if this list has focus. The selected entry can change it's appearance based on this status.
#[derive(Clone,CloneRef,Debug)]
pub struct List {
    logger          : Logger,
    app             : Application,
    display_object  : display::object::Instance,
    visible_entries : Rc<RefCell<HashMap<Id,AnyEntry>>>,
    visible_range   : Rc<CloneCell<Range<f32>>>,
    provider        : Rc<CloneRefCell<AnyEntryProvider>>,
    selected_id     : Rc<Cell<Option<Id>>>,
    entry_width     : Rc<Cell<f32>>,
    focused         : Rc<Cell<bool>>,
}

impl List {
    /// Entry List View constructor.
    pub fn new(parent:impl AnyLogger, app:&Application) -> Self {
        let app             = app.clone_ref();
        let logger          = Logger::sub(parent,"entry::List");
        let visible_entries = default();
        let visible_range   = Rc::new(CloneCell::new(default()..default()));
        let display_object  = display::object::Instance::new(&logger);
        let provider        = default();
        let selected_id     = default();
        let entry_width     = default();
        let focused         = default();
        List {logger,app,display_object,visible_entries,visible_range,provider,selected_id,
            entry_width,focused}
    }

    /// The number of all entries in List, including not displayed.
    pub fn entry_count(&self) -> usize {
        self.provider.get().entry_count()
    }

    /// The number of all displayed entries in List.
    pub fn visible_entry_count(&self) -> usize {
        ((self.visible_range.get().end - self.visible_range.get().start) / HEIGHT) as usize
    }

    /// Y position of entry with given id, relative to Entry List position.
    pub fn position_y_of_entry(id:Id) -> f32 {
        id as f32 * -HEIGHT - 0.5 * HEIGHT
    }

    /// Y range of entry with given id, relative to Entry List position.
    pub fn y_range_of_entry(id:Id) -> Range<f32> {
        let position = Self::position_y_of_entry(id);
        (position - HEIGHT / 2.0)..(position + HEIGHT / 2.0)
    }

    /// Y range of all entries in this list, including not displayed.
    pub fn total_height(entry_count:usize) -> f32 {
        entry_count as f32 * HEIGHT
    }

    /// Get the entry id which lays on given y coordinate.
    pub fn entry_at_y_position(y:f32, entry_count:usize) -> IdAtYPosition {
        use IdAtYPosition::*;
        let height = Self::total_height(entry_count);
        if y > 0.0          { AboveFirst               }
        else if y < -height { UnderLast                }
        else                { Entry((-y/HEIGHT) as Id) }
    }

    /// Set y range of the list that is visible.
    pub fn set_visible_range(&self, range:Range<f32>) {
        self.visible_range.set(range);
        self.update_visible_entries();
    }

    /// Update displayed entries to show the given range.
    fn update_visible_entries(&self) {
        let entry_at_y_saturating = |y:f32| {
            match Self::entry_at_y_position(y,self.entry_count()) {
                IdAtYPosition::AboveFirst => 0,
                IdAtYPosition::UnderLast  => self.entry_count().saturating_sub(1),
                IdAtYPosition::Entry(id)  => id,
            }
        };
        let first_visible          = entry_at_y_saturating(self.visible_range.get().end);
        let last_visible           = entry_at_y_saturating(self.visible_range.get().start);
        let visible_ids: Range<Id> = first_visible..(last_visible +1);

        // It can be extremely slow to create or destroy objects, in particular text areas.
        // Therefore, we only destroy or create those that enter or leave the visible area.

        {  // Remove entries that went out of sight
            let mut visible_entries = self.visible_entries.borrow_mut();

            let removed_entries = visible_entries.drain_filter(|id, _| !visible_ids.contains(id));
            for (_, entry) in removed_entries {
                self.display_object.remove_child(&entry);
            }
        }

        // Add entries that came into sight
        for id in visible_ids {
            let needs_to_be_created = !self.visible_entries.borrow().contains_key(&id);
            if needs_to_be_created {
                if let Some(new_entry) = self.provider.get().get(&self.app,id) {
                    self.display_object.add_child(&new_entry);
                    self.visible_entries.borrow_mut().insert(id, new_entry);
                    self.update_entry_appearance(id);
                }
            }
        }
    }

    fn update_entry_appearance(&self, id:Id) {
        if let Some(entry) = self.visible_entries.borrow().get(&id) {
            entry.set_position_y(Self::position_y_of_entry(id));
            entry.set_width(self.entry_width.get());
            entry.set_focused(self.selected_id.get() == Some(id) && self.focused.get());
        }
    }

    /// Update displayed entries, giving new provider.
    pub fn set_provider(&self, provider:AnyEntryProvider) {
        const MAX_SAFE_ENTRIES_COUNT:usize = 1000;
        if provider.entry_count() > MAX_SAFE_ENTRIES_COUNT {
            error!(self.logger, "ListView entry count exceed {MAX_SAFE_ENTRIES_COUNT} - so big \
            number of entries can cause visual glitches, e.g. https://github.com/enso-org/ide/\
            issues/757 or https://github.com/enso-org/ide/issues/758");
        }
        self.visible_entries.borrow_mut().clear();
        self.provider.set(provider);
        self.update_visible_entries()
    }

    /// Set the selected entry.
    pub fn set_selection(&self, id:Option<Id>) {
        let old = self.selected_id.replace(id);
        if let Some(old) = old {
            self.update_entry_appearance(old);
        }
        if let Some(id) = id {
            self.update_entry_appearance(id);
        }
    }

    /// Set the width for all entries.
    pub fn set_entry_width(&self, width:f32) {
        self.entry_width.set(width);
        for entry in self.visible_entries.deref().borrow().values() {
            entry.set_width(width);
        }
    }

    /// Set whether the list is focused.
    pub fn set_focused(&self, focused:bool) {
        self.focused.set(focused);
        if let Some(selection) = self.selected_id.get() {
            self.update_entry_appearance(selection);
        }
    }
}

impl display::Object for List {
    fn display_object(&self) -> &display::object::Instance { &self.display_object }
}
