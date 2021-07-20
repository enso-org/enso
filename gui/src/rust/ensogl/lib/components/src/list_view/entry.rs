//! A single entry in Select
use crate::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::layer::LayerId;
use ensogl_core::display::shape::StyleWatch;
use ensogl_text as text;
use ensogl_theme;



// =================
// === Constants ===
// =================

/// Padding inside entry in pixels.
pub const PADDING:f32 = 14.0;
/// The overall entry's height (including padding).
pub const HEIGHT:f32 = 30.0;
/// The text size of entry's labe.
pub const LABEL_SIZE:f32 = 12.0;
/// The size in pixels of icons inside entries.
pub const ICON_SIZE:f32 = 0.0; // TODO[ao] restore when we create icons for the searcher.
/// The gap between icon and label.
pub const ICON_LABEL_GAP:f32 = 0.0; // TODO[ao] restore when we create icons for the searcher.



// ===================
// === Entry Model ===
// ===================

/// Entry id. 0 is the first entry in component.
pub type Id = usize;

/// A model on which the view bases.
#[allow(missing_docs)]
#[derive(Clone,Debug,Default)]
pub struct Model {
    pub label       : String,
    pub highlighted : Vec<text::Range<text::Bytes>>,
    pub icon        : Option<display::object::Any>,
}

impl Model {
    /// Create model of simple entry with given label.
    ///
    /// The model won't have icon nor higlighting, but those can be set using `highlight` and
    /// `with_icon`.
    pub fn new(label:impl Str) -> Self {
        Self {
            label       : label.into(),
            highlighted : default(),
            icon        : default(),
        }
    }

    /// Add highlighting to the entry and return it.
    pub fn highlight(mut self, bytes:impl IntoIterator<Item=text::Range<text::Bytes>>) -> Self {
        self.highlighted.extend(bytes.into_iter());
        self
    }

    /// Add icon to the entry and return it.
    pub fn with_icon(mut self, icon:impl display::Object + 'static) -> Self {
        self.icon = Some(icon.into_any());
        self
    }
}

impl<T:Display> From<T> for Model {
    fn from(item: T) -> Self {
        Model::new(item.to_string())
    }
}


// === Entry Model Provider ===

/// The Entry Model Provider for select component.
///
/// The select does not display all entries at once, instead it lazily ask for models of entries
/// when they're about to be displayed. So setting the select content is essentially providing
/// implementor of this trait.
pub trait ModelProvider : Debug {
    /// Number of all entries.
    fn entry_count(&self) -> usize;

    /// Get the model of entry with given id. The implementors should return `None` onlt when
    /// requested id greater or equal to entries count.
    fn get(&self, id:Id) -> Option<Model>;
}

/// A wrapper for shared instance of some ModelProvider.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct AnyModelProvider(Rc<dyn ModelProvider>);

impl<T:ModelProvider + 'static> From<T> for AnyModelProvider {
    fn from(provider:T) -> Self { Self(Rc::new(provider)) }
}

impl<T:ModelProvider + 'static> From<Rc<T>> for AnyModelProvider {
    fn from(provider:Rc<T>) -> Self { Self(provider) }
}

impl Default for AnyModelProvider {
    fn default() -> Self {EmptyProvider.into()}
}


// === Empty Model Provider ===

/// An Entry Model Provider giving no entries.
///
/// This is the default provider for new select components.
#[derive(Clone,CloneRef,Copy,Debug)]
pub struct EmptyProvider;

impl ModelProvider for EmptyProvider {
    fn entry_count(&self)          -> usize         { 0    }
    fn get        (&self, _:usize) -> Option<Model> { None }
}


// === Model Provider for Vectors ===

impl<T:Into<Model> + Debug + Clone> ModelProvider for Vec<T> {
    fn entry_count(&self) -> usize {
        self.len()
    }

    fn get(&self, id:usize) -> Option<Model> {
       Some(<[T]>::get(self, id)?.clone().into())
    }
}


// === Masked Model Provider ===

/// An Entry Model Provider that wraps a `AnyModelProvider` and allows the masking of a single item.
#[derive(Clone,Debug)]
pub struct SingleMaskedProvider {
    content : AnyModelProvider,
    mask    : Cell<Option<Id>>,
}

impl ModelProvider for SingleMaskedProvider {
    fn entry_count(&self) -> usize {
        match self.mask.get() {
            None    => self.content.entry_count(),
            Some(_) => self.content.entry_count().saturating_sub(1),
        }
    }

    fn get(&self, ix:usize) -> Option<Model> {
        let internal_ix = self.unmasked_index(ix);
        self.content.get(internal_ix)
    }
}

impl SingleMaskedProvider {

    /// Return the index to the unmasked underlying data. Will only be valid to use after
    /// calling `clear_mask`.
    ///
    /// Transform index of an element visible in the menu, to the index of the all the objects,
    /// accounting for the removal of the selected item.
    ///
    /// Example:
    /// ```text
    /// Mask              `Some(1)`
    /// Masked indices    [0,     1, 2]
    /// Unmasked Index    [0, 1,  2, 3]
    /// -------------------------------
    /// Mask              `None`
    /// Masked indices    [0, 1, 2, 3]
    /// Unmasked Index    [0, 1, 2, 3]
    /// ```
    pub fn unmasked_index(&self, ix:Id) -> Id {
        match self.mask.get() {
            None                 => ix,
            Some(id) if ix < id  => ix,
            Some(_)              => ix+1,
        }
    }

    /// Mask out the given index. All methods will now skip this item and the `SingleMaskedProvider`
    /// will behave as if it was not there.
    ///
    /// *Important:* The index is interpreted according to the _masked_ position of elements.
    pub fn set_mask(&self, ix:Id) {
        let internal_ix = self.unmasked_index(ix);
        self.mask.set(Some(internal_ix));
    }

    /// Mask out the given index. All methods will now skip this item and the `SingleMaskedProvider`
    /// will behave as if it was not there.
    ///
    /// *Important:* The index is interpreted according to the _unmasked_ position of elements.
    pub fn set_mask_raw(&self, ix:Id) {
        self.mask.set(Some(ix));
    }

    /// Clear the masked item.
    pub fn clear_mask(&self) {
        self.mask.set(None)
    }
}

impl From<AnyModelProvider> for SingleMaskedProvider {
    fn from(content:AnyModelProvider) -> Self {
        let mask = default();
        SingleMaskedProvider{content,mask}
    }
}



// =============
// === Entry ===
// =============

/// A displayed entry in select component.
///
/// The Display Object position of this component is docked to the middle of left entry's boundary.
/// It differs from usual behaviour of EnsoGl components, but makes the entries alignment much
/// simpler.
#[derive(Clone,CloneRef,Debug)]
pub struct Entry {
    app            : Application,
    id             : Rc<Cell<Option<Id>>>,
    label          : text::Area,
    icon           : Rc<CloneCell<Option<display::object::Any>>>,
    display_object : display::object::Instance,
}

impl Entry {
    /// Create new entry view.
    pub fn new(logger:impl AnyLogger, app:&Application) -> Self {
        let app            = app.clone_ref();
        let id             = default();
        let label          = app.new_view::<text::Area>();
        let icon           = Rc::new(CloneCell::new(None));
        let display_object = display::object::Instance::new(logger);
        display_object.add_child(&label);
        label.set_position_xy(Vector2(PADDING + ICON_SIZE + ICON_LABEL_GAP, LABEL_SIZE/2.0));
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
        let styles     = StyleWatch::new(&app.display.scene().style_sheet);
        let text_color = styles.get_color(ensogl_theme::widget::list_view::text);
        label.set_default_color(text_color);
        label.set_default_text_size(text::Size(LABEL_SIZE));
        Entry{app,id,label,icon,display_object}
    }

    /// Set the new model for this view.
    ///
    /// This function updates icon and label.
    pub fn set_model(&self, id:Id, model:&Model) {
        if let Some(old_icon) = self.icon.get() {
            self.remove_child(&old_icon);
        }
        if let Some(new_icon) = &model.icon {
            self.add_child(&new_icon);
            new_icon.set_position_xy(Vector2(PADDING + ICON_SIZE/2.0, 0.0));
        }
        self.id.set(Some(id));
        self.icon.set(model.icon.clone());
        self.label.set_content(&model.label);

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles    = StyleWatch::new(&self.app.display.scene().style_sheet);
        let highlight = styles.get_color(ensogl_theme::widget::list_view::text::highlight);
        for highlighted in &model.highlighted {
            self.label.set_color_bytes(highlighted,highlight);
        }
    }
}

impl display::Object for Entry {
    fn display_object(&self) -> &display::object::Instance { &self.display_object }
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
#[derive(Clone,CloneRef,Debug)]
pub struct List {
    logger         : Logger,
    app            : Application,
    display_object : display::object::Instance,
    entries        : Rc<RefCell<Vec<Entry>>>,
    entries_range  : Rc<CloneCell<Range<Id>>>,
    provider       : Rc<CloneRefCell<AnyModelProvider>>,
    label_layer    : Rc<Cell<LayerId>>,
}

impl List {
    /// Entry List View constructor.
    pub fn new(parent:impl AnyLogger, app:&Application) -> Self {
        let app            = app.clone_ref();
        let logger         = Logger::sub(parent,"entry::List");
        let entries        = default();
        let entries_range  = Rc::new(CloneCell::new(default()..default()));
        let display_object = display::object::Instance::new(&logger);
        let provider       = default();
        let label_layer    = Rc::new(Cell::new(app.display.scene().layers.label.id));
        List {logger,app,display_object,entries,entries_range,provider,label_layer}
    }

    /// The number of all entries in List, including not displayed.
    pub fn entry_count(&self) -> usize {
        self.provider.get().entry_count()
    }

    /// The number of all displayed entries in List.
    pub fn visible_entry_count(&self) -> usize {
        self.entries_range.get().len()
    }

    /// Y position of entry with given id, relative to Entry List position.
    pub fn position_y_of_entry(id:Id) -> f32 { id as f32 * -HEIGHT }

    /// Y range of entry with given id, relative to Entry List position.
    pub fn y_range_of_entry(id:Id) -> Range<f32> {
        let position = Self::position_y_of_entry(id);
        (position - HEIGHT / 2.0)..(position + HEIGHT / 2.0)
    }

    /// Y range of all entries in this list, including not displayed.
    pub fn y_range_of_all_entries(entry_count:usize) -> Range<f32> {
        let start = if entry_count > 0 {
            Self::position_y_of_entry(entry_count - 1) - HEIGHT / 2.0
        } else {
            HEIGHT / 2.0
        };
        let end   = HEIGHT / 2.0;
        start..end
    }

    /// Get the entry id which lays on given y coordinate.
    pub fn entry_at_y_position(y:f32, entry_count:usize) -> IdAtYPosition {
        use IdAtYPosition::*;
        let all_entries_start = Self::y_range_of_all_entries(entry_count).start;
        if y > HEIGHT/2.0             { AboveFirst                     }
        else if y < all_entries_start { UnderLast                      }
        else                          { Entry((-y/HEIGHT + 0.5) as Id) }
    }

    /// Update displayed entries to show the given range.
    pub fn update_entries(&self, mut range:Range<Id>) {
        range.end = range.end.min(self.provider.get().entry_count());
        if range != self.entries_range.get() {
            debug!(self.logger, "Update entries for {range:?}");
            let provider = self.provider.get();
            let current_entries:HashSet<Id> = with(self.entries.borrow_mut(), |mut entries| {
                entries.resize_with(range.len(),|| self.create_new_entry());
                entries.iter().filter_map(|entry| entry.id.get()).collect()
            });
            let missing = range.clone().filter(|id| !current_entries.contains(id));
            // The provider is provided by user, so we should not keep any borrow when calling its
            // methods.
            let models = missing.map(|id| (id,provider.get(id)));
            with(self.entries.borrow(), |entries| {
                let is_outdated = |e:&Entry| e.id.get().map_or(true, |i| !range.contains(&i));
                let outdated    = entries.iter().filter(|e| is_outdated(e));
                for (entry,(id,model)) in outdated.zip(models) {
                    Self::update_entry(&self.logger,entry,id,&model);
                }
            });
            self.entries_range.set(range);
        }
    }

    /// Update displayed entries, giving new provider.
    pub fn update_entries_new_provider
    (&self, provider:impl Into<AnyModelProvider> + 'static, mut range:Range<Id>) {
        const MAX_SAFE_ENTRIES_COUNT:usize = 1000;
        let provider = provider.into();
        if provider.entry_count() > MAX_SAFE_ENTRIES_COUNT {
            error!(self.logger, "ListView entry count exceed {MAX_SAFE_ENTRIES_COUNT} - so big \
            number of entries can cause visual glitches, e.g. https://github.com/enso-org/ide/\
            issues/757 or https://github.com/enso-org/ide/issues/758");
        }
        range.end       = range.end.min(provider.entry_count());
        let models      = range.clone().map(|id| (id,provider.get(id)));
        let mut entries = self.entries.borrow_mut();
        entries.resize_with(range.len(),|| self.create_new_entry());
        for (entry,(id,model)) in entries.iter().zip(models) {
            Self::update_entry(&self.logger,entry,id,&model);
        }
        self.entries_range.set(range);
        self.provider.set(provider);
    }

    /// Sets the scene layer where the labels will be placed.
    pub fn set_label_layer(&self, label_layer:LayerId) {
        if let Some(layer) = self.app.display.scene().layers.get(self.label_layer.get()) {
            for entry in &*self.entries.borrow() {
                entry.label.remove_from_scene_layer(&self.app.display.scene().layers.label);
                entry.label.add_to_scene_layer(&layer);
            }
        } else {
            error!(self.logger, "Cannot set layer {label_layer:?} for labels: the layer does not \
                exist in the scene");
        }
        self.label_layer.set(label_layer);
    }

    fn create_new_entry(&self) -> Entry {
        let entry = Entry::new(&self.logger,&self.app);
        if let Some(layer) = self.app.display.scene().layers.get(self.label_layer.get()) {
            entry.label.remove_from_scene_layer(&self.app.display.scene().layers.label);
            entry.label.add_to_scene_layer(&layer);
        } else {
            error!(self.logger, "Cannot set layer {self.label_layer:?} for labels: the layer does \
                not exist in the scene");
        }
        self.add_child(&entry);
        entry
    }

    fn update_entry(logger:&Logger, entry:&Entry, id:Id, model:&Option<Model>) {
        debug!(logger, "Setting new model {model:?} for entry {id}; \
                        old entry: {entry.id.get():?}.");
        match model {
            Some(model) => entry.set_model(id,model),
            None        => {
                error!(logger, "Model provider didn't return model for id {id}.");
                entry.set_model(id,&default())
            }
        };
        entry.set_position_y(Self::position_y_of_entry(id));
    }
}

impl display::Object for List {
    fn display_object(&self) -> &display::object::Instance { &self.display_object }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_masked_provider() {
        let test_data   = vec!["A", "B", "C", "D"];
        let test_models = test_data.into_iter().map(|label| Model::new(label)).collect_vec();
        let provider:AnyModelProvider     = test_models.into();
        let provider:SingleMaskedProvider = provider.into();

        assert_eq!(provider.entry_count(), 4);
        assert_eq!(provider.get(0).unwrap().label, "A");
        assert_eq!(provider.get(1).unwrap().label, "B");
        assert_eq!(provider.get(2).unwrap().label, "C");
        assert_eq!(provider.get(3).unwrap().label, "D");

        provider.set_mask_raw(0);
        assert_eq!(provider.entry_count(), 3);
        assert_eq!(provider.get(0).unwrap().label, "B");
        assert_eq!(provider.get(1).unwrap().label, "C");
        assert_eq!(provider.get(2).unwrap().label, "D");

        provider.set_mask_raw(1);
        assert_eq!(provider.entry_count(), 3);
        assert_eq!(provider.get(0).unwrap().label, "A");
        assert_eq!(provider.get(1).unwrap().label, "C");
        assert_eq!(provider.get(2).unwrap().label, "D");

        provider.set_mask_raw(2);
        assert_eq!(provider.entry_count(), 3);
        assert_eq!(provider.get(0).unwrap().label, "A");
        assert_eq!(provider.get(1).unwrap().label, "B");
        assert_eq!(provider.get(2).unwrap().label, "D");

        provider.set_mask_raw(3);
        assert_eq!(provider.entry_count(), 3);
        assert_eq!(provider.get(0).unwrap().label, "A");
        assert_eq!(provider.get(1).unwrap().label, "B");
        assert_eq!(provider.get(2).unwrap().label, "C");
    }
}
