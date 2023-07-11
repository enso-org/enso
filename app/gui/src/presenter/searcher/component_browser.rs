//! The module containing [`ComponentBrowserSearcher`] presenter. See [`crate::presenter`]
//! documentation to know more about presenters in general.

use crate::prelude::*;

use crate::controller::searcher::action::Suggestion;
use crate::controller::searcher::component;
use crate::controller::searcher::Mode;
use crate::controller::searcher::Notification;
use crate::executor::global::spawn_stream_handler;
use crate::model::suggestion_database::entry::Kind;
use crate::presenter;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;
use crate::presenter::searcher::component_browser::provider::ControllerComponentsProviderExt;
use crate::presenter::searcher::SearcherPresenter;

use enso_frp as frp;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::documentation_ir::Placeholder;
use enso_text as text;
use ide_view as view;
use ide_view::component_browser;
use ide_view::component_browser::component_list_panel::grid as component_grid;
use ide_view::component_browser::component_list_panel::BreadcrumbId;
use ide_view::component_browser::component_list_panel::SECTION_NAME_CRUMB_INDEX;
use ide_view::graph_editor::NodeId;
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
    project:    view::project::View,
    provider:   Rc<RefCell<Option<provider::Component>>>,
    input_view: ViewNodeId,
    view:       component_browser::View,
}

impl Model {
    #[profile(Debug)]
    fn new(
        controller: controller::Searcher,
        project: view::project::View,
        input_view: ViewNodeId,
        view: component_browser::View,
    ) -> Self {
        let provider = default();
        Self { controller, project, view, provider, input_view }
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
        let browser = &self.view;
        // We only update the breadcrumbs starting from the second element because the first
        // one is reserved as a section name.
        let from = 1;
        let breadcrumbs_from = (names.map(Into::into).collect(), from);
        browser.model().list.model().breadcrumbs.set_entries_from(breadcrumbs_from);
    }

    fn show_breadcrumbs_ellipsis(&self, show: bool) {
        let browser = &self.view;
        browser.model().list.model().breadcrumbs.show_ellipsis(show);
    }

    fn set_section_name_crumb(&self, text: ImString) {
        let browser = &self.view;
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
        _node_id: NodeId,
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
/// The presenter should be created for one instantiated searcher controller (when a node starts
/// being edited). Alternatively, the [`setup_controller`] method covers constructing the controller
/// and the presenter.
#[derive(Debug)]
pub struct ComponentBrowserSearcher {
    _network: frp::Network,
    model:    Rc<Model>,
}


impl SearcherPresenter for ComponentBrowserSearcher {
    #[profile(Task)]
    fn setup_searcher(
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

    fn expression_accepted(
        self: Box<Self>,
        node_id: NodeId,
        entry_id: Option<component_grid::GroupEntryId>,
    ) -> Option<AstNodeId> {
        self.model.expression_accepted(node_id, entry_id)
    }


    fn abort_editing(self: Box<Self>) {
        self.model.controller.abort_editing()
    }

    fn input_view(&self) -> ViewNodeId {
        self.model.input_view
    }
}

impl ComponentBrowserSearcher {
    #[profile(Task)]
    fn new(
        controller: controller::Searcher,
        view: view::project::View,
        input_view: ViewNodeId,
    ) -> Self {
        let searcher_view = view.searcher().clone_ref();
        let model = Rc::new(Model::new(controller, view, input_view, searcher_view));
        let network = frp::Network::new("presenter::Searcher");

        let graph = &model.project.graph().frp;
        let browser = &model.view;

        frp::extend! { network
            eval model.project.searcher_input_changed ([model]((expr, selections)) {
                let cursor_position = selections.last().map(|sel| sel.end).unwrap_or_default();
                model.input_changed(expr, cursor_position);
            });

            action_list_changed <- any_mut::<()>();
            // When the searcher input is changed, we need to update immediately the list of
            // entries in the component browser (as opposed to waiting for a `NewActionList` event
            // which is delivered asynchronously). This is because the input may be accepted
            // before the asynchronous event is delivered and to accept the correct entry the list
            // must be up-to-date.
            action_list_changed <+ model.project.searcher_input_changed.constant(());

            eval_ model.project.toggle_component_browser_private_entries_visibility (
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
                &model.project.toggle_component_browser_private_entries_visibility,
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
}
