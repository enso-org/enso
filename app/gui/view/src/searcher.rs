//! A Searcher component.
//!
//! This component wraps the plain ListView in some searcher-specific logic, like committing
//! editing, or picking suggestion with Tab.

use crate::prelude::*;
use ensogl::display::shape::*;

use crate::documentation;

use enso_frp as frp;
use ensogl::application;
use ensogl::application::shortcut;
use ensogl::application::Application;
use ensogl::display;
use ensogl::DEPRECATED_Animation;
use ensogl_component::list_view;
use ensogl_component::list_view::ListView;


// ==============
// === Export ===
// ==============

pub use ensogl_component::list_view::entry;



// =================
// === Constants ===
// =================

/// Width of searcher panel in pixels.
pub const SEARCHER_WIDTH: f32 = 480.0;
/// Height of searcher panel in pixels.
///
/// Because we don't implement clipping yet, the best UX is when searcher height is almost multiple
/// of entry height + padding.
pub const SEARCHER_HEIGHT: f32 = 183.5;

const ACTION_LIST_WIDTH: f32 = 180.0;
const LIST_DOC_GAP: f32 = 15.0;
const DOCUMENTATION_WIDTH: f32 = SEARCHER_WIDTH - ACTION_LIST_WIDTH - LIST_DOC_GAP;
const ACTION_LIST_X: f32 = (ACTION_LIST_WIDTH - SEARCHER_WIDTH) / 2.0;
const DOCUMENTATION_X: f32 = (SEARCHER_WIDTH - DOCUMENTATION_WIDTH) / 2.0;



// ==============================
// === Documentation Provider ===
// ==============================

/// The Entry Model Provider.
///
/// This provider is used by searcher to print documentation of currently selected entry.
pub trait DocumentationProvider: Debug {
    /// Get documentation string to be displayed when no entry is selected.
    fn get(&self) -> Option<String> {
        None
    }

    /// Get documentation string for given entry, or `None` if entry or documentation does not
    /// exist.
    fn get_for_entry(&self, id: entry::Id) -> Option<String>;
}

impl DocumentationProvider for entry::EmptyProvider {
    fn get_for_entry(&self, _: entry::Id) -> Option<String> {
        None
    }
}


// === AnyDocumentationProvider ===

/// A wrapper for shared instance of some DocumentationProvider.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct AnyDocumentationProvider {
    rc: Rc<dyn DocumentationProvider>,
}

impl Default for AnyDocumentationProvider {
    fn default() -> Self {
        entry::EmptyProvider.into()
    }
}

impl<T: DocumentationProvider + 'static> From<T> for AnyDocumentationProvider {
    fn from(provider: T) -> Self {
        Self { rc: Rc::new(provider) }
    }
}

impl<T: DocumentationProvider + 'static> From<Rc<T>> for AnyDocumentationProvider {
    fn from(provider: Rc<T>) -> Self {
        Self { rc: provider }
    }
}


// =============
// === Model ===
// =============

/// A type of ListView entry used in searcher.
pub type Entry = list_view::entry::GlyphHighlightedLabel;

#[derive(Clone, CloneRef, Debug)]
struct Model {
    app:            Application,
    logger:         Logger,
    display_object: display::object::Instance,
    list:           ListView<Entry>,
    documentation:  documentation::View,
    doc_provider:   Rc<CloneRefCell<AnyDocumentationProvider>>,
}

impl Model {
    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let app = app.clone_ref();
        let logger = Logger::new("SearcherView");
        let display_object = display::object::Instance::new();
        let list = app.new_view::<ListView<Entry>>();
        list.deprecated_focus();
        let documentation = documentation::View::new(&app);
        let doc_provider = default();
        scene.layers.node_searcher.add(&list);
        display_object.add_child(&documentation);
        display_object.add_child(&list);

        // FIXME: StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        //  system (#795)
        let style = StyleWatch::new(&app.display.default_scene.style_sheet);
        let action_list_gap_path = ensogl_hardcoded_theme::application::searcher::action_list_gap;
        let action_list_gap = style.get_number_or(action_list_gap_path, 0.0);
        list.set_label_layer(&scene.layers.node_searcher_text);
        list.set_position_y(-action_list_gap);
        list.set_position_x(ACTION_LIST_X);
        documentation.set_position_x(DOCUMENTATION_X);
        documentation.set_position_y(-action_list_gap);
        Self { app, logger, display_object, list, documentation, doc_provider }
    }

    fn docs_for(&self, id: Option<entry::Id>) -> String {
        let doc_provider = self.doc_provider.get();
        let when_none_selected = || doc_provider.get().unwrap_or_else(|| " ".to_owned());
        id.map_or_else(when_none_selected, |id| doc_provider.get_for_entry(id).unwrap_or_default())
    }

    fn set_height(&self, h: f32) {
        self.list.resize(Vector2(ACTION_LIST_WIDTH, h));
        self.documentation.visualization_frp.inputs.set_size.emit(Vector2(DOCUMENTATION_WIDTH, h));
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Use the selected action as a suggestion and add it to the current input.
        use_as_suggestion (),
        set_actions       (entry::AnyModelProvider<list_view::entry::GlyphHighlightedLabel>,AnyDocumentationProvider),
        select_action     (entry::Id),
        show              (),
        hide              (),
    }

    Output {
        selected_entry     (Option<entry::Id>),
        used_as_suggestion (Option<entry::Id>),
        editing_committed  (Option<entry::Id>),
        size               (Vector2<f32>),
        is_visible         (bool),
        is_selected        (bool),
        is_empty           (bool),
    }
}



// ============
// === View ===
// ============

/// The Searcher Component.
///
/// This component covers only the list of actions. The Searcher input is displayed as an
/// additional graph node in edit mode, so we could easily display e.g. connections between selected
/// node and searcher input.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    pub frp: Frp,
    model:   Model,
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl View {
    /// Create new component.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app);
        Self { frp, model }.init()
    }

    /// Initialize the FRP network.
    fn init(self) -> Self {
        self.model.set_height(0.0);
        let network = &self.frp.network;
        let model = &self.model;
        let frp = &self.frp;
        let source = &self.frp.source;

        let height = DEPRECATED_Animation::<f32>::new(network);

        frp::extend! { network
            eval frp.set_actions ([model] ((entries,docs)) {
                model.doc_provider.set(docs.clone_ref());
                model.list.set_entries(entries);
            });
            selected_entry <- model.list.selected_entry.on_change();
            eval frp.select_action ((id) model.list.select_entry(id));
            source.selected_entry <+ selected_entry;
            source.size           <+ height.value.map(|h| Vector2(SEARCHER_WIDTH,*h));
            source.is_visible     <+ model.list.size.map(|size| size.x*size.y > std::f32::EPSILON);
            // source.is_selected    <+ model.documentation.frp.is_selected.map(|&value|value);
            source.is_empty       <+ frp.set_actions.map(|(entries,_)| entries.entry_count() == 0);

            eval height.value ((h)  model.set_height(*h));
            eval frp.show     ((()) height.set_target_value(SEARCHER_HEIGHT));
            eval frp.hide     ((()) height.set_target_value(-list_view::SHADOW_PX));

            is_selected               <- selected_entry.map(|e| e.is_some());
            is_enabled                <- bool(&frp.hide,&frp.show);
            is_entry_enabled          <- is_selected && is_enabled;
            displayed_doc             <- selected_entry.map(f!((id) model.docs_for(*id)));
            opt_picked_entry          <- selected_entry.sample(&frp.use_as_suggestion);
            source.used_as_suggestion <+ opt_picked_entry.gate(&is_entry_enabled);
            source.editing_committed  <+ model.list.chosen_entry.gate(&is_entry_enabled);

            eval displayed_doc ((data) model.documentation.frp.display_documentation(data));
        };

        self
    }

    /// The Documentation Panel View.
    pub fn documentation(&self) -> &documentation::View {
        &self.model.documentation
    }

    /// Set the action list displayed in searcher.
    ///
    /// The list is represented list-entry-model and documentation provider. It's a helper for FRP
    /// `set_suggestion` input (FRP nodes cannot be generic).
    pub fn set_actions(
        &self,
        provider: Rc<impl list_view::entry::ModelProvider<Entry> + DocumentationProvider + 'static>,
    ) {
        let entries: list_view::entry::AnyModelProvider<Entry> = provider.clone_ref().into();
        let documentation: AnyDocumentationProvider = provider.into();
        self.frp.set_actions(entries, documentation);
    }

    /// Clear the action list.
    ///
    /// It just set empty provider using FRP `set_actions` input.
    pub fn clear_actions(&self) {
        let provider = Rc::new(list_view::entry::EmptyProvider);
        self.set_actions(provider);
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for View {
    fn label() -> &'static str {
        "Searcher"
    }
    fn new(app: &Application) -> Self {
        Self::new(app)
    }
    fn app(&self) -> &Application {
        &self.model.app
    }
    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::*;
        [(Press, "tab", "use_as_suggestion")]
            .iter()
            .map(|(a, b, c)| Self::self_shortcut(*a, *b, *c))
            .collect()
    }
}
