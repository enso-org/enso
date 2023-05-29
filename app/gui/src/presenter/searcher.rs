//! The module containing [`Searcher`] presenter. See [`crate::presenter`] documentation to know
//! more about presenters in general.

use crate::prelude::*;

use crate::controller::graph::NewNodeInfo;
use crate::controller::searcher::action::Suggestion;
use crate::controller::searcher::component;
use crate::controller::searcher::Mode;
use crate::controller::searcher::Notification;
use crate::executor::global::spawn_stream_handler;
use crate::model::module::NodeMetadata;
use crate::model::suggestion_database::entry::Kind;
use crate::presenter;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;
use crate::presenter::searcher::provider::ControllerComponentsProviderExt;

use enso_frp as frp;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::documentation_ir::Placeholder;
use enso_text as text;
use ide_view as view;
use ide_view::component_browser::component_list_panel::grid as component_grid;
use ide_view::component_browser::component_list_panel::BreadcrumbId;
use ide_view::component_browser::component_list_panel::SECTION_NAME_CRUMB_INDEX;
use ide_view::graph_editor::GraphEditor;
use ide_view::project::SearcherParams;


// ==============
// === Export ===
// ==============

pub mod provider;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "No component group with the index {:?}.", _0)]
pub struct NoSuchComponent(component_grid::GroupEntryId);



// ========================
// === Helper Functions ===
// ========================

fn title_for_docs(suggestion: &model::suggestion_database::Entry) -> String {
    match suggestion.kind {
        Kind::Type => format!("Type {}", suggestion.name),
        Kind::Constructor => format!("Constructor {}", suggestion.name),
        Kind::Function => format!("Function {}", suggestion.name),
        Kind::Local => format!("Node {}", suggestion.name),
        Kind::Method => {
            let preposition = if suggestion.self_type.is_some() { " of " } else { "" };
            let self_type = suggestion.self_type.as_ref().map_or("", |tp| tp.name());
            format!("Method {}{}{}", suggestion.name, preposition, self_type)
        }
        Kind::Module => format!("Module {}", suggestion.name),
    }
}

fn doc_placeholder_for(suggestion: &model::suggestion_database::Entry) -> String {
    let title = title_for_docs(suggestion);
    format!("<div class=\"enso docs summary\"><p />{title} <p />No documentation available</div>")
}


// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
struct Model {
    controller: controller::Searcher,
    view:       view::project::View,
    provider:   Rc<RefCell<Option<provider::Component>>>,
    input_view: ViewNodeId,
}

impl Model {
    #[profile(Debug)]
    fn new(
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let provider = default();
        Self { controller, view, provider, input_view }
    }

    #[profile(Debug)]
    fn input_changed(&self, new_input: &str, cursor_position: text::Byte) {
        if let Err(err) = self.controller.set_input(new_input.to_owned(), cursor_position) {
            error!("Error while setting new searcher input: {err}.");
        }
    }

    fn suggestion_for_entry_id(
        &self,
        id: component_grid::GroupEntryId,
    ) -> FallibleResult<Suggestion> {
        let component: FallibleResult<_> = self
            .controller
            .provider()
            .component_by_view_id(id)
            .ok_or_else(|| NoSuchComponent(id).into());
        Ok(match component?.data {
            component::Data::FromDatabase { entry, .. } =>
                Suggestion::FromDatabase(entry.clone_ref()),
            component::Data::Virtual { snippet } => Suggestion::Hardcoded(snippet.clone_ref()),
        })
    }

    /// Should be called if a suggestion is selected but not used yet.
    fn suggestion_selected(&self, entry_id: Option<component_grid::GroupEntryId>) {
        let suggestion = entry_id.map(|id| self.suggestion_for_entry_id(id));
        let to_preview = match suggestion {
            Some(Ok(suggestion)) => Some(suggestion),
            Some(Err(err)) => {
                warn!("Error while previewing suggestion: {err}.");
                None
            }
            None => None,
        };
        if let Err(error) = self.controller.preview(to_preview) {
            error!("Failed to preview searcher input (selected suggestion: {entry_id:?}) because of error: {error}.");
        }
    }

    fn suggestion_accepted(
        &self,
        id: component_grid::GroupEntryId,
    ) -> Option<(ViewNodeId, text::Range<text::Byte>, ImString)> {
        let provider = self.controller.provider();
        let component: FallibleResult<_> =
            provider.component_by_view_id(id).ok_or_else(|| NoSuchComponent(id).into());
        let new_code = component.and_then(|component| {
            let suggestion = match component.data {
                component::Data::FromDatabase { entry, .. } =>
                    Suggestion::FromDatabase(entry.clone_ref()),
                component::Data::Virtual { snippet } => Suggestion::Hardcoded(snippet.clone_ref()),
            };
            self.controller.use_suggestion(suggestion)
        });
        match new_code {
            Ok(text::Change { range, text }) => {
                self.update_breadcrumbs();
                Some((self.input_view, range, text.into()))
            }
            Err(err) => {
                error!("Error while applying suggestion: {err}.");
                None
            }
        }
    }

    fn breadcrumb_selected(&self, id: BreadcrumbId) {
        self.controller.select_breadcrumb(id);
    }

    fn update_breadcrumbs(&self) {
        let names = self.controller.breadcrumbs().into_iter();
        let browser = self.view.searcher();
        // We only update the breadcrumbs starting from the second element because the first
        // one is reserved as a section name.
        let from = 1;
        let breadcrumbs_from = (names.map(Into::into).collect(), from);
        browser.model().list.model().breadcrumbs.set_entries_from(breadcrumbs_from);
    }

    fn show_breadcrumbs_ellipsis(&self, show: bool) {
        let browser = self.view.searcher();
        browser.model().list.model().breadcrumbs.show_ellipsis(show);
    }

    fn set_section_name_crumb(&self, text: ImString) {
        let browser = self.view.searcher();
        let breadcrumbs = &browser.model().list.model().breadcrumbs;
        breadcrumbs.set_entry((SECTION_NAME_CRUMB_INDEX, text.into()));
    }

    fn on_active_section_change(&self, section_id: component_grid::SectionId) {
        let components = self.controller.components();
        let mut section_names = components.top_module_section_names();
        let name = match section_id {
            component_grid::SectionId::Namespace(n) =>
                section_names.nth(n).map(|n| n.clone_ref()).unwrap_or_default(),
            component_grid::SectionId::Popular => "Popular".to_im_string(),
            component_grid::SectionId::LocalScope => "Local".to_im_string(),
        };
        self.set_section_name_crumb(name);
    }

    fn module_entered(&self, module: component_grid::ElementId) {
        self.enter_module(module);
    }

    fn enter_module(&self, module: component_grid::ElementId) -> Option<()> {
        let provider = self.controller.provider();
        let id = if let Some(entry) = module.as_entry_id() {
            let component = provider.component_by_view_id(entry)?;
            component.id()?
        } else {
            let group = provider.group_by_view_id(module.group)?;
            group.component_id?
        };
        self.controller.enter_module(&id);
        self.update_breadcrumbs();
        let show_ellipsis = self.controller.last_module_has_submodules();
        self.show_breadcrumbs_ellipsis(show_ellipsis);
        Some(())
    }

    fn expression_accepted(
        &self,
        entry_id: Option<component_grid::GroupEntryId>,
    ) -> Option<AstNodeId> {
        if let Some(entry_id) = entry_id {
            self.suggestion_accepted(entry_id);
        }
        if !self.controller.is_input_empty() {
            self.controller.commit_node().map(Some).unwrap_or_else(|err| {
                error!("Error while committing node expression: {err}.");
                None
            })
        } else {
            // if input is empty or contains spaces only, we cannot update the node (there is no
            // valid AST to assign). Because it is an expected thing, we also do not report error.
            None
        }
    }

    fn documentation_of_component(
        &self,
        id: view::component_browser::component_list_panel::grid::GroupEntryId,
    ) -> EntryDocumentation {
        let component = self.controller.provider().component_by_view_id(id);
        if let Some(component) = component {
            match component.data {
                component::Data::FromDatabase { id, .. } =>
                    self.controller.documentation_for_entry(*id),
                component::Data::Virtual { snippet } =>
                    snippet.documentation.clone().unwrap_or_default(),
            }
        } else {
            default()
        }
    }

    fn documentation_of_group(&self, id: component_grid::GroupId) -> EntryDocumentation {
        let group = self.controller.provider().group_by_view_id(id);
        if let Some(group) = group {
            if let Some(id) = group.component_id {
                self.controller.documentation_for_entry(id)
            } else {
                Placeholder::VirtualComponentGroup { name: group.name.clone() }.into()
            }
        } else {
            default()
        }
    }

    fn should_select_first_entry(&self) -> bool {
        self.controller.is_filtering() || self.controller.is_input_empty()
    }
}

/// The Searcher presenter, synchronizing state between searcher view and searcher controller.
///
/// The presenter should be created for one instantiated searcher controller (when node starts to
/// being edited). Alternatively, the [`setup_controller`] method covers constructing the controller
/// and the presenter.
#[derive(Debug)]
pub struct Searcher {
    _network: frp::Network,
    model:    Rc<Model>,
}

impl Searcher {
    /// Constructor. The returned structure works right away.
    #[profile(Task)]
    pub fn new(
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let model = Rc::new(Model::new(controller, view, input_view));
        let network = frp::Network::new("presenter::Searcher");

        let graph = &model.view.graph().frp;
        let browser = model.view.searcher();

        frp::extend! { network
            eval model.view.searcher_input_changed ([model]((expr, selections)) {
                let cursor_position = selections.last().map(|sel| sel.end).unwrap_or_default();
                model.input_changed(expr, cursor_position);
            });

            action_list_changed <- source::<()>();

            eval_ model.view.toggle_component_browser_private_entries_visibility (
                model.controller.reload_list());
        }

        let grid = &browser.model().list.model().grid;
        let navigator = &browser.model().list.model().section_navigator;
        let breadcrumbs = &browser.model().list.model().breadcrumbs;
        let documentation = &browser.model().documentation;
        frp::extend! { network
            eval_ action_list_changed ([model, grid, navigator] {
                model.provider.take();
                let controller_provider = model.controller.provider();
                let namespace_section_count = controller_provider.namespace_section_count();
                navigator.set_namespace_section_count.emit(namespace_section_count);
                let provider = provider::Component::provide_new_list(controller_provider, &grid);
                *model.provider.borrow_mut() = Some(provider);
            });
            grid.select_first_entry <+ action_list_changed.filter(f_!(model.should_select_first_entry()));
            input_edit <- grid.suggestion_accepted.filter_map(f!((e) model.suggestion_accepted(*e)));
            graph.edit_node_expression <+ input_edit;

            entry_selected <- grid.active.map(|&s| s?.as_entry_id());
            selected_entry_changed <- entry_selected.on_change().constant(());
            grid.unhover_element <+ any2(
                &selected_entry_changed,
                &model.view.toggle_component_browser_private_entries_visibility,
            );
            hovered_not_selected <- all_with(&grid.hovered, &grid.active, |h, s| {
                match (h, s) {
                    (Some(h), Some(s)) => h != s,
                    _ => false,
                }
            });
            documentation.frp.show_hovered_item_preview_caption <+ hovered_not_selected;
            docs_params <- all3(&action_list_changed, &grid.active, &grid.hovered);
            docs <- docs_params.filter_map(f!([model]((_, selected, hovered)) {
                let entry = hovered.as_ref().or(selected.as_ref());
                entry.map(|entry| {
                    if let Some(group_id) = entry.as_header() {
                        model.documentation_of_group(group_id)
                    } else {
                        let entry_id = entry.as_entry_id().expect("GroupEntryId");
                        model.documentation_of_component(entry_id)
                    }
                })
            }));
            documentation.frp.display_documentation <+ docs;

            eval_ grid.suggestion_accepted([]analytics::remote_log_event("component_browser::suggestion_accepted"));
            eval entry_selected((entry) model.suggestion_selected(*entry));
            eval grid.module_entered((id) model.module_entered(*id));
            eval breadcrumbs.selected((id) model.breadcrumb_selected(*id));
            active_section <- grid.active_section.filter_map(|s| *s);
            eval active_section((section) model.on_active_section_change(*section));
        }

        let weak_model = Rc::downgrade(&model);
        let notifications = model.controller.subscribe();
        spawn_stream_handler(weak_model, notifications, move |notification, _| {
            match notification {
                Notification::NewActionList => action_list_changed.emit(()),
            };
            std::future::ready(())
        });

        Self { model, _network: network }
    }

    /// Create a new input node for use in the searcher. Initiates a new node in the ast and
    /// associates it with the already existing view.
    ///
    /// Returns the new node id and optionally the source node which was selected/dragged when
    /// creating this node.
    fn create_input_node(
        parameters: SearcherParams,
        graph: &presenter::Graph,
        graph_editor: &GraphEditor,
        graph_controller: &controller::Graph,
    ) -> FallibleResult<(ast::Id, Option<ast::Id>)> {
        /// The expression to be used for newly created nodes when initialising the searcher without
        /// an existing node.
        const DEFAULT_INPUT_EXPRESSION: &str = "Nothing";
        let SearcherParams { input, source_node, .. } = parameters;

        let view_data = graph_editor.model.nodes.get_cloned_ref(&input);

        let position = view_data.map(|node| node.position().xy());
        let position = position.map(|vector| model::module::Position { vector });

        let metadata = NodeMetadata { position, ..default() };
        let mut new_node = NewNodeInfo::new_pushed_back(DEFAULT_INPUT_EXPRESSION);
        new_node.metadata = Some(metadata);
        new_node.introduce_pattern = false;
        let transaction_name = "Add code for created node's visualization preview.";
        let _transaction =
            graph_controller.undo_redo_repository().open_ignored_transaction(transaction_name);
        let created_node = graph_controller.add_node(new_node)?;

        graph.assign_node_view_explicitly(input, created_node);

        let source_node = source_node.and_then(|id| graph.ast_node_of_view(id.node));

        Ok((created_node, source_node))
    }

    /// Initiate the operating mode for the searcher based on the given [`SearcherParams`]. If the
    /// view associated with the input node given in the parameters does not yet exist, it will
    /// be created.
    ///
    /// Returns the [`Mode`] that should be used for the searcher.
    pub fn init_input_node(
        parameters: SearcherParams,
        graph: &presenter::Graph,
        graph_editor: &GraphEditor,
        graph_controller: &controller::Graph,
    ) -> FallibleResult<Mode> {
        let SearcherParams { input, .. } = parameters;
        let ast_node = graph.ast_node_of_view(input);

        let mode = match ast_node {
            Some(node_id) => Ok(Mode::EditNode { node_id }),
            None => {
                let (new_node, source_node) =
                    Self::create_input_node(parameters, graph, graph_editor, graph_controller)?;
                Ok(Mode::NewNode { node_id: new_node, source_node })
            }
        };
        let target_node = mode.as_ref().map(|mode| mode.node_id());
        if let Ok(target_node) = target_node {
            graph.allow_expression_auto_updates(target_node, false);
        }
        mode
    }

    /// Setup new, appropriate searcher controller for the edition of `node_view`, and construct
    /// presenter handling it.
    #[profile(Task)]
    pub fn setup_controller(
        ide_controller: controller::Ide,
        project_controller: controller::Project,
        graph_controller: controller::ExecutedGraph,
        graph_presenter: &presenter::Graph,
        view: view::project::View,
        parameters: SearcherParams,
    ) -> FallibleResult<Self> {
        // We get the position for searcher before initializing the input node, because the
        // added node will affect the AST, and the position will become incorrect.
        let position_in_code = graph_controller.graph().definition_end_location()?;

        let mode = Self::init_input_node(
            parameters,
            graph_presenter,
            view.graph(),
            &graph_controller.graph(),
        )?;

        let searcher_controller = controller::Searcher::new_from_graph_controller(
            ide_controller,
            &project_controller.model,
            graph_controller,
            mode,
            parameters.cursor_position,
            position_in_code,
        )?;

        // Clear input on a new node. By default this will be set to whatever is used as the default
        // content of the new node.
        if let Mode::NewNode { source_node, .. } = mode {
            if source_node.is_none() {
                if let Err(e) = searcher_controller.set_input("".to_string(), text::Byte(0)) {
                    error!("Failed to clear input when creating searcher for a new node: {e:?}.");
                }
            }
        }

        let input = parameters.input;
        Ok(Self::new(searcher_controller, view, input))
    }

    /// Expression accepted in Component Browser.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes. The `entry_id` might be none in case where the user want to accept
    /// the node input without any entry selected. If the commitment results in creating a new
    /// node, its AST ID is returned.
    pub fn expression_accepted(
        self,
        entry_id: Option<component_grid::GroupEntryId>,
    ) -> Option<AstNodeId> {
        self.model.expression_accepted(entry_id)
    }

    /// Abort editing, without taking any action.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes.
    pub fn abort_editing(self) {
        self.model.controller.abort_editing()
    }

    /// Returns the node view that is being edited by the searcher.
    pub fn input_view(&self) -> ViewNodeId {
        self.model.input_view
    }

    /// Returns true if the entry under given index is one of the examples.
    pub fn is_entry_an_example(&self, entry: view::searcher::entry::Id) -> bool {
        use crate::controller::searcher::action::Action::Example;

        let controller = &self.model.controller;
        let entry = controller.actions().list().and_then(|l| l.get_cloned(entry));
        entry.map_or(false, |e| matches!(e.action, Example(_)))
    }
}
