//! The module containing [`Searcher`] presenter. See [`crate::presenter`] documentation to know
//! more about presenters in general.

use crate::prelude::*;

use crate::controller::graph::NewNodeInfo;
use crate::controller::searcher::action::Suggestion;
use crate::controller::searcher::component;
use crate::controller::searcher::Mode;
use crate::controller::searcher::Notification;
use crate::controller::searcher::UserAction;
use crate::executor::global::spawn_stream_handler;
use crate::model::module::NodeMetadata;
use crate::model::suggestion_database::entry::Kind;
use crate::presenter;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;
use crate::presenter::searcher::provider::ControllerComponentsProviderExt;

use enso_frp as frp;
use ide_view as view;
use ide_view::component_browser::component_list_panel::grid as component_grid;
use ide_view::component_browser::component_list_panel::BreadcrumbId;
use ide_view::component_browser::component_list_panel::SECTION_NAME_CRUMB_INDEX;
use ide_view::graph_editor::component::node as node_view;
use ide_view::graph_editor::GraphEditor;
use ide_view::project::SearcherParams;
use ide_view::project::SearcherVariant;


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
    logger:     Logger,
    controller: controller::Searcher,
    view:       view::project::View,
    provider:   Rc<RefCell<Option<provider::Component>>>,
    input_view: ViewNodeId,
}

impl Model {
    #[profile(Debug)]
    fn new(
        parent: impl AnyLogger,
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let logger = parent.sub("presenter::Searcher");
        let provider = default();
        Self { logger, controller, view, provider, input_view }
    }

    #[profile(Debug)]
    fn input_changed(&self, new_input: &str) {
        if let Err(err) = self.controller.set_input(new_input.to_owned()) {
            error!("Error while setting new searcher input: {err}.");
        }
    }

    fn entry_used_as_suggestion(
        &self,
        entry_id: view::searcher::entry::Id,
    ) -> Option<(ViewNodeId, node_view::Expression)> {
        match self.controller.use_as_suggestion(entry_id) {
            Ok(new_code) => {
                let new_code_and_trees = node_view::Expression::new_plain(new_code);
                self.update_breadcrumbs();
                Some((self.input_view, new_code_and_trees))
            }
            Err(err) => {
                error!("Error while applying suggestion: {err}.");
                None
            }
        }
    }

    /// Should be called if an entry is selected but not used yet. Only used for the old searcher
    /// API.
    fn entry_selected_as_suggestion(&self, entry_id: view::searcher::entry::Id) {
        if let Err(error) = self.controller.preview_entry_as_suggestion(entry_id) {
            warn!("Failed to preview entry {entry_id:?} because of error: {error:?}.");
        }
    }

    fn commit_editing(&self, entry_id: Option<view::searcher::entry::Id>) -> Option<AstNodeId> {
        let result = match entry_id {
            Some(id) => self.controller.execute_action_by_index(id),
            None => self.controller.commit_node().map(Some),
        };
        result.unwrap_or_else(|err| {
            error!("Error while executing action: {err}.");
            None
        })
    }

    fn create_providers(&self) -> provider::Any {
        provider::create_providers_from_controller(&self.controller)
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
    fn suggestion_selected(&self, entry_id: component_grid::GroupEntryId) {
        match self.suggestion_for_entry_id(entry_id) {
            Ok(suggestion) =>
                if let Err(error) = self.controller.preview_suggestion(suggestion) {
                    warn!("Failed to preview suggestion {entry_id:?} because of error: {error:?}.");
                },
            Err(err) => warn!("Error while previewing suggestion: {err}."),
        }
    }

    fn suggestion_accepted(
        &self,
        id: component_grid::GroupEntryId,
    ) -> Option<(ViewNodeId, node_view::Expression)> {
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
            Ok(new_code) => {
                self.update_breadcrumbs();
                let new_code_and_trees = node_view::Expression::new_plain(new_code);
                Some((self.input_view, new_code_and_trees))
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
        if let SearcherVariant::ComponentBrowser(browser) = self.view.searcher() {
            // We only update the breadcrumbs starting from the second element because the first
            // one is reserved as a section name.
            let from = 1;
            let breadcrumbs_from = (names.map(Into::into).collect(), from);
            browser.model().list.model().breadcrumbs.set_entries_from(breadcrumbs_from);
        }
    }

    fn show_breadcrumbs_ellipsis(&self, show: bool) {
        if let SearcherVariant::ComponentBrowser(browser) = self.view.searcher() {
            browser.model().list.model().breadcrumbs.show_ellipsis(show);
        }
    }

    fn set_section_name_crumb(&self, text: &str) {
        if let SearcherVariant::ComponentBrowser(browser) = self.view.searcher() {
            let breadcrumbs = &browser.model().list.model().breadcrumbs;
            breadcrumbs.set_entry((SECTION_NAME_CRUMB_INDEX, ImString::new(text).into()));
        }
    }

    fn on_active_section_change(&self, section_id: component_grid::SectionId) {
        self.set_section_name_crumb(section_id.as_str());
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
        self.controller.commit_node().map(Some).unwrap_or_else(|err| {
            error!("Error while committing node expression: {err}.");
            None
        })
    }

    fn documentation_of_component(
        &self,
        id: view::component_browser::component_list_panel::grid::GroupEntryId,
    ) -> String {
        let component = self.controller.provider().component_by_view_id(id);
        if let Some(component) = component {
            match component.data {
                component::Data::FromDatabase { entry, .. } => {
                    if let Some(documentation) = &entry.documentation_html {
                        let title = title_for_docs(&entry);
                        format!(
                            "<div class=\"enso docs summary\"><p />{title}</div>{documentation}"
                        )
                    } else {
                        doc_placeholder_for(&entry)
                    }
                }
                component::Data::Virtual { snippet } => {
                    if let Some(documentation) = &snippet.documentation_html {
                        documentation.to_string()
                    } else {
                        default()
                    }
                }
            }
        } else {
            default()
        }
    }

    fn documentation_of_group(&self, id: component_grid::GroupId) -> String {
        let group = self.controller.provider().group_by_view_id(id);
        if let Some(group) = group {
            iformat!("<div class=\"enso docs summary\"><p />{group.name}</div>")
        } else {
            default()
        }
    }

    fn should_auto_select_first_action(&self) -> bool {
        let user_action = self.controller.current_user_action();
        let list_not_empty = matches!(self.controller.actions(), controller::searcher::Actions::Loaded {list} if list.matching_count() > 0);
        // Usually we don't want to select first entry and display docs when user finished typing
        // function or argument.
        let starting_typing = user_action == UserAction::StartingTypingArgument;
        !starting_typing && list_not_empty
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
        parent: impl AnyLogger,
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let model = Rc::new(Model::new(parent, controller, view, input_view));
        let network = frp::Network::new("presenter::Searcher");

        let graph = &model.view.graph().frp;

        frp::extend! { network
            eval model.view.searcher_input_changed ((expr) model.input_changed(expr));

            action_list_changed <- source::<()>();
            select_entry <- action_list_changed.filter(f_!(model.should_auto_select_first_action()));
        }

        match model.view.searcher() {
            SearcherVariant::ComponentBrowser(browser) => {
                let grid = &browser.model().list.model().grid;
                let breadcrumbs = &browser.model().list.model().breadcrumbs;
                let documentation = &browser.model().documentation;
                frp::extend! { network
                    eval_ action_list_changed ([model, grid] {
                        model.provider.take();
                        let controller_provider = model.controller.provider();
                        let provider = provider::Component::provide_new_list(controller_provider, &grid);
                        *model.provider.borrow_mut() = Some(provider);
                    });
                    new_input <- grid.suggestion_accepted.filter_map(f!((e) model.suggestion_accepted(*e)));
                    graph.set_node_expression <+ new_input;

                    entry_selected <- grid.active.filter_map(|&s| s?.as_entry_id());
                    entry_hovered <- grid.hovered.map(|&s| s?.as_entry_id());
                    entry_docs <- all_with3(&action_list_changed,
                        &entry_selected,
                        &entry_hovered,
                        f!([model](_, selected, hovered) {
                            let entry = hovered.as_ref().unwrap_or(selected);
                            model.documentation_of_component(*entry)
                        })
                    );
                    header_selected <- grid.active.filter_map(|element| {
                        use component_grid::content::ElementId;
                        use component_grid::content::ElementInGroup::Header;
                        match element {
                            Some(ElementId { element: Header, group}) => Some(*group),
                            _ => None
                        }
                    });
                    header_docs <- header_selected.map(f!((id) model.documentation_of_group(*id)));
                    documentation.frp.display_documentation <+ entry_docs;
                    documentation.frp.display_documentation <+ header_docs;

                    eval_ grid.suggestion_accepted([]analytics::remote_log_event("component_browser::suggestion_accepted"));
                    eval entry_selected((entry) model.suggestion_selected(*entry));
                    eval grid.module_entered((id) model.module_entered(*id));
                    eval breadcrumbs.selected((id) model.breadcrumb_selected(*id));
                    active_section <- grid.active_section.filter_map(|s| *s);
                    eval active_section((section) model.on_active_section_change(*section));
                }
            }
            SearcherVariant::OldNodeSearcher(searcher) => {
                let searcher = &searcher.frp;

                frp::extend! { network
                    new_providers <- action_list_changed.map(f_!(model.create_providers()));
                    searcher.set_actions <+ new_providers;
                    searcher.select_action <+ select_entry.constant(0);
                    used_as_suggestion <- searcher.used_as_suggestion.filter_map(|entry| *entry);
                    new_input <- used_as_suggestion.filter_map(f!((e) model.entry_used_as_suggestion(*e)));
                    graph.set_node_expression <+ new_input;
                    eval searcher.selected_entry([model](entry)
                        if let Some(id) = entry { model.entry_selected_as_suggestion(*id)});

                    eval_ searcher.used_as_suggestion([]analytics::remote_log_event("searcher::used_as_suggestion"));
                }
            }
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
        let SearcherParams { input, source_node } = parameters;

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
        parent: impl AnyLogger,
        ide_controller: controller::Ide,
        project_controller: controller::Project,
        graph_controller: controller::ExecutedGraph,
        graph_presenter: &presenter::Graph,
        view: view::project::View,
        parameters: SearcherParams,
    ) -> FallibleResult<Self> {
        let mode = Self::init_input_node(
            parameters,
            graph_presenter,
            view.graph(),
            &graph_controller.graph(),
        )?;

        let searcher_controller = controller::Searcher::new_from_graph_controller(
            &parent,
            ide_controller,
            &project_controller.model,
            graph_controller,
            mode,
        )?;

        // Clear input on a new node. By default this will be set to whatever is used as the default
        // content of the new node.
        if let Mode::NewNode { source_node, .. } = mode {
            if source_node.is_none() {
                if let Err(e) = searcher_controller.set_input("".to_string()) {
                    error!("Failed to clear input when creating searcher for a new node: {e:?}.");
                }
            }
        }

        let input = parameters.input;
        Ok(Self::new(parent, searcher_controller, view, input))
    }

    /// Commit editing in the old Node Searcher.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes. The `entry_id` might be none in case where the searcher should accept
    /// the node input without any entry selected. If the commitment results in creating a new
    /// node, its AST id is returned.
    #[profile(Task)]
    pub fn commit_editing(self, entry_id: Option<view::searcher::entry::Id>) -> Option<AstNodeId> {
        self.model.commit_editing(entry_id)
    }

    /// Expression accepted in Component Browser.
    ///
    /// This method takes `self`, as the presenter (with the searcher view) should be dropped once
    /// editing finishes. The `entry_id` might be none in case where the user want to accept
    /// the node input without any entry selected. If the commitment results in creating a new
    /// node, its AST id is returned.
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
