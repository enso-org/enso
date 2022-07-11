//! A module defining entry [`List`] structure: a view of ListView entries arranged in column.

use crate::prelude::*;

use crate::entry;
use crate::entry::Entry;

use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::layer::Layer;
use ensogl_core::display::scene::layer::WeakLayer;
use ensogl_core::display::style;



// ======================
// === DisplayedEntry ===
// ======================

/// A displayed entry in select component.
///
/// The Display Object position of this component is docked to the middle of left entry's boundary.
/// It differs from usual behaviour of EnsoGL components, but makes the entries' alignment much
/// simpler: In the vast majority of cases we want to align list elements to the left.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
#[clone_ref(bound = "E:CloneRef")]
pub struct DisplayedEntry<E> {
    pub id:    Rc<Cell<Option<entry::Id>>>,
    pub entry: E,
}



// =================
// === EntryList ===
// =================

/// The output of `entry_at_y_position`
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum IdAtYPosition {
    AboveFirst,
    UnderLast,
    Entry(entry::Id),
}

impl IdAtYPosition {
    /// Returns id of entry if present.
    pub fn entry(&self) -> Option<entry::Id> {
        if let Self::Entry(id) = self {
            Some(*id)
        } else {
            None
        }
    }
}

/// A view containing an entry list, arranged in column.
///
/// Not all entries are displayed at once, only those visible.
pub type List<E> = ListData<E, <E as Entry>::Params>;

/// Data of a [`List`].
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
#[clone_ref(bound = "E:CloneRef")]
pub struct ListData<E, P> {
    logger:         Logger,
    app:            Application,
    display_object: display::object::Instance,
    entries:        Rc<RefCell<Vec<DisplayedEntry<E>>>>,
    entries_range:  Rc<CloneCell<Range<entry::Id>>>,
    entry_params:   Rc<RefCell<P>>,
    provider:       Rc<CloneRefCell<entry::AnyModelProvider<E>>>,
    label_layer:    Rc<RefCell<WeakLayer>>,
}

impl<E, P: Default> ListData<E, P> {
    /// Entry List View constructor.
    pub fn new(parent: impl AnyLogger, app: &Application) -> Self {
        let app = app.clone_ref();
        let logger = Logger::new_sub(parent, "entry::List");
        let entries = default();
        let entries_range = Rc::new(CloneCell::new(default()..default()));
        let entry_params = default();
        let display_object = display::object::Instance::new(&logger);
        let provider = default();
        let label_layer = Rc::new(RefCell::new(app.display.default_scene.layers.label.downgrade()));
        Self {
            logger,
            app,
            display_object,
            entries,
            entries_range,
            entry_params,
            provider,
            label_layer,
        }
    }
}

impl<E, P> ListData<E, P> {
    /// The number of all entries in List, including not displayed.
    pub fn entry_count(&self) -> usize {
        self.provider.get().entry_count()
    }

    /// The number of all displayed entries in List.
    pub fn visible_entry_count(&self) -> usize {
        self.entries_range.get().len()
    }

    /// Y position of entry with given id, relative to Entry List position.
    pub fn position_y_of_entry(id: entry::Id) -> f32 {
        id as f32 * -entry::HEIGHT
    }

    /// Y range of entry with given id, relative to Entry List position.
    pub fn y_range_of_entry(id: entry::Id) -> Range<f32> {
        let position = Self::position_y_of_entry(id);
        (position - entry::HEIGHT / 2.0)..(position + entry::HEIGHT / 2.0)
    }

    /// Y range of all entries in this list, including not displayed.
    pub fn y_range_of_all_entries(entry_count: usize) -> Range<f32> {
        let start = if entry_count > 0 {
            Self::position_y_of_entry(entry_count - 1) - entry::HEIGHT / 2.0
        } else {
            entry::HEIGHT / 2.0
        };
        let end = entry::HEIGHT / 2.0;
        start..end
    }

    /// Get the entry id which lays on given y coordinate.
    pub fn entry_at_y_position(y: f32, entry_count: usize) -> IdAtYPosition {
        use IdAtYPosition::*;
        let all_entries_start = Self::y_range_of_all_entries(entry_count).start;
        if y > entry::HEIGHT / 2.0 {
            AboveFirst
        } else if y < all_entries_start {
            UnderLast
        } else {
            Entry((-y / entry::HEIGHT + 0.5) as entry::Id)
        }
    }
}

impl<E: Entry, P> ListData<E, P> {
    /// Sets the scene layer where the labels will be placed.
    pub fn set_label_layer(&self, label_layer: &Layer) {
        for entry in &*self.entries.borrow() {
            entry.entry.set_label_layer(label_layer);
        }
        self.label_layer.replace(label_layer.downgrade());
    }
}

impl<E: Entry> ListData<E, E::Params> {
    /// Update displayed entries to show the given range and limit their display width to at most
    /// `max_width_px`. Any newly created entries will use the styles located at the `style_prefix`
    /// path in the application's style sheet.
    pub fn update_entries(
        &self,
        mut range: Range<entry::Id>,
        max_width_px: f32,
        style_prefix: &style::Path,
    ) {
        range.end = range.end.min(self.provider.get().entry_count());
        if range != self.entries_range.get() {
            debug!(self.logger, "Update entries for {range:?}");
            let provider = self.provider.get();
            let current_entries: HashSet<entry::Id> =
                with(self.entries.borrow_mut(), |mut entries| {
                    entries.resize_with(range.len(), || self.create_new_entry(style_prefix));
                    entries.iter().filter_map(|entry| entry.id.get()).collect()
                });
            let missing = range.clone().filter(|id| !current_entries.contains(id));
            // The provider is provided by user, so we should not keep any borrow when calling its
            // methods.
            let models = missing.map(|id| (id, provider.get(id)));
            with(self.entries.borrow(), |entries| {
                let is_outdated =
                    |e: &DisplayedEntry<E>| e.id.get().map_or(true, |i| !range.contains(&i));
                let outdated = entries.iter().filter(|e| is_outdated(e));
                for (entry, (id, model)) in outdated.zip(models) {
                    Self::update_entry(&self.logger, entry, id, &model);
                }
            });
            self.entries_range.set(range);
        }
        for entry in self.entries.borrow().iter() {
            entry.entry.set_max_width(max_width_px);
        }
    }

    /// Recreate the displayed entries to make them use the styles located at the `style_prefix`
    /// path in the application's style sheet.
    pub fn recreate_entries_with_style_prefix(&self, style_prefix: style::Path) {
        let mut entries = self.entries.borrow_mut();
        let provider = self.provider.get();
        for entry in entries.iter_mut() {
            self.remove_child(&entry.entry);
            let new_entry = self.create_new_entry(&style_prefix);
            if let Some(id) = entry.id.get() {
                let model = provider.get(id);
                Self::update_entry(&self.logger, &new_entry, id, &model);
            }
            *entry = new_entry;
        }
    }

    /// Set params used in the displayed entries and recreate all displayed entries. The entries
    /// will use the styles located at the `style_prefix` path in the application's style sheet.
    pub fn set_entry_params_and_recreate_entries(
        &self,
        params: E::Params,
        style_prefix: style::Path,
    ) {
        self.entry_params.replace(params);
        self.recreate_entries_with_style_prefix(style_prefix);
    }

    /// Get previously set entry params.
    pub fn entry_params(&self) -> E::Params {
        self.entry_params.borrow().clone_ref()
    }

    /// Update displayed entries, giving new provider. New entries created by the function have
    /// their maximum width set to `max_width_px` and use the styles located at the `style_prefix`
    /// path.
    pub fn update_entries_new_provider(
        &self,
        provider: impl Into<entry::AnyModelProvider<E>> + 'static,
        mut range: Range<entry::Id>,
        max_width_px: f32,
        style_prefix: style::Path,
    ) {
        const MAX_SAFE_ENTRIES_COUNT: usize = 1000;
        let provider = provider.into();
        if provider.entry_count() > MAX_SAFE_ENTRIES_COUNT {
            error!(
                self.logger,
                "ListView entry count exceed {MAX_SAFE_ENTRIES_COUNT} - so big \
            number of entries can cause visual glitches, e.g. https://github.com/enso-org/ide/\
            issues/757 or https://github.com/enso-org/ide/issues/758"
            );
        }
        range.end = range.end.min(provider.entry_count());
        let models = range.clone().map(|id| (id, provider.get(id)));
        let mut entries = self.entries.borrow_mut();
        let create_new_entry_with_max_width = || {
            let entry = self.create_new_entry(&style_prefix);
            entry.entry.set_max_width(max_width_px);
            entry
        };
        entries.resize_with(range.len(), create_new_entry_with_max_width);
        for (entry, (id, model)) in entries.iter().zip(models) {
            Self::update_entry(&self.logger, entry, id, &model);
        }
        self.entries_range.set(range);
        self.provider.set(provider);
    }

    fn create_new_entry(&self, style_prefix: &style::Path) -> DisplayedEntry<E> {
        let layers = &self.app.display.default_scene.layers;
        let layer = self.label_layer.borrow().upgrade().unwrap_or_else(|| {
            error!(
                self.logger,
                "Cannot set layer {self.label_layer:?} for labels: the layer does \
                not exist in the scene"
            );
            layers.main.clone_ref()
        });
        let entry = E::new(&self.app, style_prefix, &self.entry_params.borrow());
        let entry = DisplayedEntry { id: default(), entry };
        entry.entry.set_label_layer(&layer);
        self.add_child(&entry.entry);
        entry
    }

    fn update_entry(
        logger: &Logger,
        entry: &DisplayedEntry<E>,
        id: entry::Id,
        model: &Option<E::Model>,
    ) {
        debug!(
            logger,
            "Setting new model {model:?} for entry {id}; \
            old entry: {entry.id.get():?}."
        );
        entry.id.set(Some(id));
        match model {
            Some(model) => entry.entry.update(model),
            None => {
                error!(logger, "Model provider didn't return model for id {id}.");
                entry.entry.update(&default());
            }
        };
        entry.entry.set_position_y(Self::position_y_of_entry(id));
    }
}

impl<E, P> display::Object for ListData<E, P> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
