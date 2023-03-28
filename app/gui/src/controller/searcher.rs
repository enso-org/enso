//! This module contains all structures related to Searcher Controller.

use crate::model::traits::*;
use crate::prelude::*;

use crate::controller::graph::FailedToCreateNode;
use crate::controller::searcher::component::group;
use crate::model::module::NodeEditStatus;
use crate::model::module::NodeMetadata;
use crate::model::suggestion_database;

use breadcrumbs::Breadcrumbs;
use const_format::concatcp;
use double_representation::graph::GraphInfo;
use double_representation::graph::LocationHint;
use double_representation::import;
use double_representation::name::project;
use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use double_representation::node::NodeInfo;
use engine_protocol::language_server;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::entry::Id as EntryId;
use enso_text as text;
use enso_text::Byte;
use enso_text::Location;
use enso_text::Rope;
use flo_stream::Subscriber;


// ==============
// === Export ===
// ==============

pub mod action;
pub mod breadcrumbs;
pub mod component;
pub mod input;

pub use action::Action;



// =================
// === Constants ===
// =================

/// If enabled, searcher will assign names for all nodes created with it, not only when it is
/// needed. Currently enabled to trigger engine's caching of user-added nodes.
/// See: https://github.com/enso-org/ide/issues/1067
pub const ASSIGN_NAMES_FOR_NODES: bool = true;

/// The special module used for mock `Enso_Project.data` entry.
/// See also [`Searcher::add_enso_project_entries`].
const ENSO_PROJECT_SPECIAL_MODULE: &str =
    concatcp!(project::STANDARD_BASE_LIBRARY_PATH, ".Enso_Project");



// ==============
// === Errors ===
// ==============


#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "No action entry with the index {}.", index)]
pub struct NoSuchAction {
    index: usize,
}

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Action entry with index {} is not a suggestion.", index)]
pub struct NotASuggestion {
    index: usize,
}

#[allow(missing_docs)]
#[derive(Debug, Fail)]
#[fail(display = "An action \"{}\" is not supported: {}", action_label, reason)]
pub struct NotSupported {
    action_label: String,
    reason:       failure::Error,
}

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "An action cannot be executed when searcher is in \"edit node\" mode.")]
pub struct CannotExecuteWhenEditingNode;

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Cannot commit expression in current mode ({:?}).", mode)]
pub struct CannotCommitExpression {
    mode: Mode,
}


// =====================
// === Notifications ===
// =====================

/// The notification emitted by Searcher Controller
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Notification {
    /// A new Suggestion list is available.
    NewActionList,
}



// ===================
// === Suggestions ===
// ===================

/// List of actions available in Searcher.
#[derive(Clone, CloneRef, Debug)]
pub enum Actions {
    /// The action list is still gathering suggestions from the Language Server.
    Loading,
    /// The action list is ready.
    #[allow(missing_docs)]
    Loaded { list: Rc<action::List> },
    /// Loading suggestions from the Language Server resulted in error.
    Error(Rc<failure::Error>),
}

impl Actions {
    /// Check if list is still loading.
    pub fn is_loading(&self) -> bool {
        matches!(self, Self::Loading)
    }

    /// Check if retrieving suggestion list was unsuccessful
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }

    /// Get the list of actions. Returns None if still loading or error was returned.
    pub fn list(&self) -> Option<&action::List> {
        match self {
            Self::Loaded { list } => Some(list),
            _ => None,
        }
    }
}

impl Default for Actions {
    fn default() -> Self {
        Self::Loading
    }
}



// ======================
// === RequiredImport ===
// ======================

/// An import that is needed for the picked suggestion.
#[derive(Debug, Clone)]
pub enum RequiredImport {
    /// A specific entry needs to be imported.
    Entry(Rc<enso_suggestion_database::Entry>),
    /// An entry with a specific name needs to be imported.
    Name(QualifiedName),
}



// ================
// === ThisNode ===
// ================

/// Information about a node that is used as a `self` argument.
///
/// "This" node is either:
/// 1. A node that was selected when the searcher was brought up.
/// 2. A source node of the connection that was dropped on the scene to create a new node.
/// This affects suggestions for the first completion (to include methods of the node's returned
/// value) and the code inserted when the input is committed.
#[derive(Clone, Debug)]
pub struct ThisNode {
    /// Identifier of the node that will be connected if the initial suggestion is picked.
    pub id: double_representation::node::Id,
    /// Name of the variable that will be used to connect with the selected node.
    pub var: String,
    /// If the pattern with variable needs to be introduced on the node.
    pub needs_to_introduce_pattern: bool,
}

impl ThisNode {
    /// Retrieve information about the `self` node.
    ///
    /// Returns `None` if the given node's information cannot be retrieved or if the node does not
    /// introduce a variable.
    pub fn new(id: double_representation::node::Id, graph: &controller::Graph) -> Option<Self> {
        let node = graph.node(id).ok()?;
        let (var, needs_to_introduce_pattern) = if let Some(ast) = node.info.pattern() {
            // TODO [mwu]
            //   Here we just require that the whole node's pattern is a single var, like
            //   `var = expr`. This prevents using pattern subpart (like `x` in
            //   `Point x y = get_pos`), or basically any node that doesn't stick to `var = expr`
            //   form. If we wanted to support pattern subparts, the engine would need to send us
            //   value updates for matched pattern pieces. See the issue:
            //   https://github.com/enso-org/enso/issues/1038
            (ast::identifier::as_var(ast)?.to_owned(), false)
        } else {
            (graph.variable_name_for(&node.info).ok()?.repr(), true)
        };
        Some(ThisNode { id, var, needs_to_introduce_pattern })
    }

    /// Introduce a pattern with variable on the node serving as provider of "this" argument.
    ///
    /// Does nothing if node already has a pattern.
    pub fn introduce_pattern(&self, graph: controller::Graph) -> FallibleResult {
        if self.needs_to_introduce_pattern {
            graph.set_pattern_on(self.id, ast::Ast::var(&self.var))?;
        }
        Ok(())
    }
}



// ===========================
// === Searcher Controller ===
// ===========================

/// Describes how Searcher was brought to screen and how should behave when committing expression.
#[derive(Copy, Clone, Debug)]
#[allow(missing_docs)]
pub enum Mode {
    /// Searcher is working with a newly created node. `source_node` is either a selected node
    /// or a node from which the connection was dragged out before being dropped at the scene.
    NewNode { node_id: ast::Id, source_node: Option<ast::Id> },
    /// Searcher should edit existing node's expression.
    EditNode { node_id: ast::Id },
}

impl Mode {
    /// Return the ID of the node used as target for the Searcher.
    pub fn node_id(&self) -> ast::Id {
        match self {
            Mode::NewNode { node_id, .. } => *node_id,
            Mode::EditNode { node_id, .. } => *node_id,
        }
    }
}

/// A fragment filled by single picked suggestion.
///
/// We store such information in Searcher to better suggest the potential arguments, and to know
/// what imports should be added when inserting node.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct PickedSuggestion {
    pub entry:         action::Suggestion,
    pub inserted_code: String,
    pub import:        Option<RequiredImport>,
}

impl PickedSuggestion {
    /// Check if the picked fragment is still unmodified by user.
    fn is_still_unmodified(&self, input: &input::Input) -> bool {
        match &input.ast {
            input::InputAst::Line(ast) =>
                ast.elem.iter_recursive().any(|ast_node| ast_node.repr() == self.inserted_code),
            input::InputAst::Invalid(string) => *string == self.inserted_code,
        }
    }
}

/// A controller state.
#[derive(Clone, Debug, Default)]
pub struct Data {
    /// The current searcher's input.
    pub input:              input::Input,
    /// The action list which should be displayed.
    pub actions:            Actions,
    /// The component list which should be displayed.
    pub components:         component::List,
    /// All picked suggestions. If the user changes the generated code, it will be removed from
    /// this list.
    pub picked_suggestions: Vec<PickedSuggestion>,
}

impl Data {
    /// Initialize Searcher state when editing node.
    ///
    /// When searcher is brought by editing node, the input should be an expression of this node.
    /// Committing node will then edit the exiting node's expression instead of adding a new one.
    /// Additionally searcher should restore information about intended method, so we will be able
    /// to suggest arguments.
    #[profile(Debug)]
    fn new_with_edited_node(
        graph: &controller::Graph,
        edited_node_id: ast::Id,
        cursor_position: Byte,
    ) -> FallibleResult<Self> {
        let edited_node = graph.node(edited_node_id)?;
        let input_ast = ast::BlockLine { elem: edited_node.info.expression(), off: 0 };
        let input = input::Input::new(input_ast, cursor_position);
        let actions = default();
        let components = default();
        let picked_suggestions = default();
        Ok(Data { input, actions, components, picked_suggestions })
    }
}

/// A helper wrapper for the state needed to provide the list of visible components.
///
/// It wraps the [`component::List`] structure and provide API providing always currently visible
/// entries. Those in turn depends on current breadcrumbs state and presence of
/// ["this" argument](ThisNode).
#[derive(Clone, Debug, CloneRef)]
pub struct ComponentsProvider {
    breadcrumbs:  Breadcrumbs,
    list:         component::List,
    has_this_arg: Immutable<bool>,
}

/// Enum of top modules and their content to display.
#[derive(Debug)]
pub enum TopModules {
    /// The selected `Groups` list and the section number the groups belong to.
    Subset(group::AlphabeticalList, usize),
    /// Vector of all `Group` lists in the same order as the sections they belong to.
    All(Vec<group::AlphabeticalList>),
}

impl ComponentsProvider {
    /// The list of modules and their content displayed in `Submodules` section of the browser.
    pub fn top_modules(&self) -> TopModules {
        let components = self.components();
        if let Some(selected) = self.breadcrumbs.selected() {
            let section = components
                .module_qualified_name(selected)
                .and_then(|name| {
                    components.top_module_section_indices().get(&name.project().namespace).copied()
                })
                .unwrap_or_default();
            let submodules =
                components.submodules_of(selected).map(CloneRef::clone_ref).unwrap_or_default();
            TopModules::Subset(submodules, section)
        } else if *self.has_this_arg {
            TopModules::All(components.top_modules_flattened().collect())
        } else {
            TopModules::All(components.top_modules().collect())
        }
    }

    /// The list of components displayed in `Favorites` section of the browser.
    ///
    /// The favorites section is not empty only if the root module is selected.
    pub fn favorites(&self) -> group::List {
        if self.breadcrumbs.is_top_module() {
            self.components().favorites.clone_ref()
        } else {
            default()
        }
    }

    /// The list of components displayed in `Local Scope` section of the browser.
    pub fn local_scope(&self) -> group::Group {
        let components = self.components();
        if let Some(selected) = self.breadcrumbs.selected() {
            components.get_module_content(selected).map(CloneRef::clone_ref).unwrap_or_default()
        } else {
            components.local_scope.clone_ref()
        }
    }

    /// Returns true if providing a content of some module currently.
    pub fn displaying_module(&self) -> bool {
        self.breadcrumbs.selected().is_some()
    }

    fn components(&self) -> &component::List {
        &self.list
    }

    /// Returns the number of namespace sections.
    pub fn namespace_section_count(&self) -> usize {
        self.list.top_module_section_count()
    }

    /// Check if the component list is filtered.
    pub fn is_filtered(&self) -> bool {
        self.list.is_filtered()
    }
}

/// An information used for filtering entries.
#[derive(Debug, Clone, CloneRef)]
pub struct Filter {
    /// The part of the input used for filtering.
    pub pattern: ImString,
    /// Additional context. A string representation of the edited accessor chain.
    pub context: Option<ImString>,
}

/// Searcher Controller.
///
/// This is an object providing all required functionalities for Searcher View: mainly it is the
/// action list to display depending on the searcher input, and actions of picking suggestion and
/// executing entry.
///
/// For description of different actions, see `Action` type.
///
/// Each action can be _executed_, and some (like suggestions of code) may be _used as a
/// suggestion_. The exact outcome depends on action type.
///
/// Additionally user can accept the current searcher input as a new node (or new expression of
/// existing node).
#[derive(Clone, CloneRef, Debug)]
pub struct Searcher {
    data:             Rc<RefCell<Data>>,
    breadcrumbs:      Breadcrumbs,
    notifier:         notification::Publisher<Notification>,
    graph:            controller::ExecutedGraph,
    mode:             Immutable<Mode>,
    database:         Rc<model::SuggestionDatabase>,
    language_server:  Rc<language_server::Connection>,
    ide:              controller::Ide,
    this_arg:         Rc<Option<ThisNode>>,
    position_in_code: Immutable<Location<Byte>>,
    project:          model::Project,
    node_edit_guard:  Rc<Option<EditGuard>>,
}

impl Searcher {
    /// Create new Searcher Controller, when you have Executed Graph Controller handy.
    #[profile(Task)]
    pub fn new_from_graph_controller(
        ide: controller::Ide,
        project: &model::Project,
        graph: controller::ExecutedGraph,
        mode: Mode,
        cursor_position: Byte,
        position_in_code: Location<Byte>,
    ) -> FallibleResult<Self> {
        let project = project.clone_ref();
        let data = if let Mode::EditNode { node_id } = mode {
            Data::new_with_edited_node(&graph.graph(), node_id, cursor_position)?
        } else {
            default()
        };
        let node_metadata_guard = Rc::new(Some(EditGuard::new(&mode, graph.clone_ref())));
        let this_arg = Rc::new(match mode {
            Mode::NewNode { source_node: Some(node), .. } => ThisNode::new(node, &graph.graph()),
            _ => None,
        });
        let breadcrumbs = Breadcrumbs::new();
        let ret = Self {
            graph,
            this_arg,
            ide,
            data: Rc::new(RefCell::new(data)),
            breadcrumbs,
            notifier: default(),
            mode: Immutable(mode),
            database: project.suggestion_db(),
            language_server: project.json_rpc(),
            position_in_code: Immutable(position_in_code),
            project,
            node_edit_guard: node_metadata_guard,
        };
        Ok(ret.init())
    }

    fn init(self) -> Self {
        self.reload_list();
        self
    }

    /// Abort editing and perform cleanup.
    pub fn abort_editing(&self) {
        self.clear_temporary_imports();
    }

    /// Return true if user is currently filtering entries (the input has non-empty _pattern_ part).
    pub fn is_filtering(&self) -> bool {
        !self.data.borrow().input.filter().pattern.is_empty()
    }

    /// Subscribe to controller's notifications.
    pub fn subscribe(&self) -> Subscriber<Notification> {
        self.notifier.subscribe()
    }

    /// Get the current action list.
    pub fn actions(&self) -> Actions {
        self.data.borrow().actions.clone_ref()
    }

    /// Get the current component list.
    pub fn components(&self) -> component::List {
        self.data.borrow().components.clone_ref()
    }

    /// Get the documentation for the entry.
    pub fn documentation_for_entry(&self, id: EntryId) -> EntryDocumentation {
        self.database.documentation_for_entry(id)
    }

    /// Build a provider for this searcher.
    pub fn provider(&self) -> ComponentsProvider {
        ComponentsProvider {
            breadcrumbs:  self.breadcrumbs.clone_ref(),
            list:         self.components(),
            has_this_arg: Immutable(self.this_arg.is_some()),
        }
    }

    /// Enter the specified module. The displayed content of the browser will be updated.
    pub fn enter_module(&self, module: &component::Id) {
        let builder = breadcrumbs::Builder::new(&self.database, self.components());
        let breadcrumbs = builder.build(module);
        self.breadcrumbs.set_content(breadcrumbs);
        self.notifier.notify(Notification::NewActionList);
    }

    /// Whether the last module in the breadcrumbs list contains more descendants or not.
    pub fn last_module_has_submodules(&self) -> bool {
        let last_module = self.breadcrumbs.last();
        let components = self.components();
        let get_submodules = |module| components.submodules_of(module).map(CloneRef::clone_ref);
        let submodules = last_module.and_then(get_submodules);
        submodules.map_or(false, |submodules| !submodules.is_empty())
    }

    /// A list of breadcrumbs' text labels to be displayed. The list is updated by
    /// [`Self::enter_module`].
    pub fn breadcrumbs(&self) -> Vec<ImString> {
        self.breadcrumbs.names()
    }

    /// Select the breadcrumb with the index [`id`]. The displayed content of the browser will be
    /// updated.
    pub fn select_breadcrumb(&self, id: usize) {
        self.breadcrumbs.select(id);
        self.notifier.notify(Notification::NewActionList);
    }

    /// Set the Searcher Input.
    ///
    /// This function should be called each time user modifies Searcher input in view. It may result
    /// in a new action list (the appropriate notification will be emitted).
    #[profile(Debug)]
    pub fn set_input(&self, new_input: String, cursor_position: Byte) -> FallibleResult {
        debug!("Manually setting input to {new_input} with cursor position {cursor_position}");
        let parsed_input = input::Input::parse(self.ide.parser(), new_input, cursor_position);
        let new_context = parsed_input.context().map(|ctx| ctx.into_ast().repr());
        let new_literal = parsed_input.edited_literal().cloned();
        let old_input = mem::replace(&mut self.data.borrow_mut().input, parsed_input);
        let old_context = old_input.context().map(|ctx| ctx.into_ast().repr());
        let old_literal = old_input.edited_literal();

        self.invalidate_picked_suggestions();
        let context_changed = old_context != new_context;
        let literal_changed = old_literal != new_literal.as_ref();
        if context_changed || literal_changed {
            debug!("Reloading list.");
            self.reload_list();
        } else {
            let data = self.data.borrow();
            let filter = data.input.filter();
            data.components.update_filtering(filter.clone_ref());
            if let Actions::Loaded { list } = &data.actions {
                debug!("Update filtering.");
                list.update_filtering(filter.pattern);
                executor::global::spawn(self.notifier.publish(Notification::NewActionList));
            }
        }
        Ok(())
    }

    fn this_var(&self) -> Option<&str> {
        self.this_arg.deref().as_ref().map(|this| this.var.as_ref())
    }

    /// Pick a completion suggestion.
    ///
    /// This function should be called when user do the _use as suggestion_ action as a code
    /// suggestion (see struct documentation). The picked suggestion will be remembered, and the
    /// searcher's input will be updated and returned by this function.
    #[profile(Debug)]
    pub fn use_suggestion(
        &self,
        picked_suggestion: action::Suggestion,
    ) -> FallibleResult<text::Change<Byte, String>> {
        debug!("Picking suggestion: {picked_suggestion:?}.");
        let change = {
            let mut data = self.data.borrow_mut();
            let has_this = self.this_var().is_some();
            let inserted = data.input.after_inserting_suggestion(&picked_suggestion, has_this)?;
            let new_cursor_position = inserted.inserted_text.end;
            let inserted_code = inserted.new_input[inserted.inserted_code].to_owned();
            let import = inserted.import.clone();
            let parser = self.ide.parser();
            data.input = input::Input::parse(parser, &inserted.new_input, new_cursor_position);
            let suggestion = PickedSuggestion { entry: picked_suggestion, inserted_code, import };
            data.picked_suggestions.push(suggestion);
            inserted.input_change()
        };
        self.reload_list();
        self.breadcrumbs.set_content(iter::empty());
        Ok(change)
    }

    /// Use action at given index as a suggestion. The exact outcome depends on the action's type.
    pub fn use_as_suggestion(
        &self,
        index: usize,
    ) -> FallibleResult<enso_text::Change<Byte, String>> {
        let error = || NoSuchAction { index };
        let suggestion = {
            let data = self.data.borrow();
            let list = data.actions.list().ok_or_else(error)?;
            list.get_cloned(index).ok_or_else(error)?.action
        };
        match suggestion {
            Action::Suggestion(suggestion) => self.use_suggestion(suggestion),
            _ => Err(NotASuggestion { index }.into()),
        }
    }

    /// Preview the suggestion in the searcher.
    pub fn preview_entry_as_suggestion(&self, index: usize) -> FallibleResult {
        debug!("Previewing entry: {index:?}.");
        let error = || NoSuchAction { index };
        let suggestion = {
            let data = self.data.borrow();
            let list = data.actions.list().ok_or_else(error)?;
            list.get_cloned(index).ok_or_else(error)?.action
        };
        if let Action::Suggestion(picked_suggestion) = suggestion {
            self.preview_suggestion(picked_suggestion)?;
        };

        Ok(())
    }

    /// Use action at given index as a suggestion. The exact outcome depends on the action's type.
    pub fn preview_suggestion(&self, picked_suggestion: action::Suggestion) -> FallibleResult {
        let transaction_name = "Previewing Component Browser suggestion.";
        let _skip = self.graph.undo_redo_repository().open_ignored_transaction(transaction_name);

        debug!("Previewing suggestion: \"{picked_suggestion:?}\".");
        self.clear_temporary_imports();

        let has_this = self.this_var().is_some();
        let preview_change =
            self.data.borrow().input.after_inserting_suggestion(&picked_suggestion, has_this)?;
        let preview_ast = self.ide.parser().parse_line_ast(preview_change.new_input).ok();
        let expression = self.get_expression(preview_ast.as_ref());

        {
            // This block serves to limit the borrow of `self.data`.
            let data = self.data.borrow();
            let requirements = data.picked_suggestions.iter().filter_map(|ps| ps.import.clone());
            let all_requirements = requirements.chain(preview_change.import.iter().cloned());
            self.add_required_imports(all_requirements, false)?;
        }
        self.graph.graph().set_expression(self.mode.node_id(), expression)?;

        Ok(())
    }

    /// Execute given action.
    ///
    /// If the action results in adding new node to the graph, or changing an exiting node, its id
    /// will be returned by this function.
    #[profile(Task)]
    pub fn execute_action(&self, action: Action) -> FallibleResult<Option<ast::Id>> {
        match action {
            Action::Suggestion(suggestion) => {
                self.use_suggestion(suggestion)?;
                self.commit_node().map(Some)
            }
            Action::Example(example) => match *self.mode {
                Mode::NewNode { .. } => self.add_example(&example).map(Some),
                _ => Err(CannotExecuteWhenEditingNode.into()),
            },
            Action::ProjectManagement(action) => {
                match self.ide.manage_projects() {
                    Ok(_) => {
                        let ide = self.ide.clone_ref();
                        executor::global::spawn(async move {
                            // We checked that manage_projects returns Some just a moment ago, so
                            // unwrapping is safe.
                            let manage_projects = ide.manage_projects().unwrap();
                            let result = match action {
                                action::ProjectManagement::CreateNewProject =>
                                    manage_projects.create_new_project(None),
                                action::ProjectManagement::OpenProject { id, .. } =>
                                    manage_projects.open_project(*id),
                            };
                            if let Err(err) = result.await {
                                error!("Error when creating new project: {err}");
                            }
                        });
                        Ok(None)
                    }
                    Err(err) => Err(NotSupported {
                        action_label: Action::ProjectManagement(action).to_string(),
                        reason:       err,
                    }
                    .into()),
                }
            }
        }
    }

    /// See `execute_action` documentation.
    #[profile(Task)]
    pub fn execute_action_by_index(&self, index: usize) -> FallibleResult<Option<ast::Id>> {
        let error = || NoSuchAction { index };
        let action = {
            let data = self.data.borrow();
            let list = data.actions.list().ok_or_else(error)?;
            list.get_cloned(index).ok_or_else(error)?.action
        };
        self.execute_action(action.clone_ref())
    }

    /// Preview the action in the searcher.
    #[profile(Task)]
    pub fn preview_action_by_index(&self, index: usize) -> FallibleResult<()> {
        let error = || NoSuchAction { index };
        let action = {
            let data = self.data.borrow();
            let list = data.actions.list().ok_or_else(error)?;
            list.get_cloned(index).ok_or_else(error)?.action
        };
        debug!("Previewing action: {action:?}");
        Ok(())
    }

    /// Commit the current input as a new node expression.
    ///
    /// If the searcher was brought by editing existing node, the input is set as a new node
    /// expression, otherwise a new node is added. This will also add all imports required by
    /// picked suggestions.
    #[profile(Debug)]
    pub fn commit_node(&self) -> FallibleResult<ast::Id> {
        let _transaction_guard = self.graph.get_or_open_transaction("Commit node");
        self.clear_temporary_imports();

        // We add the required imports before we edit its content. This way, we avoid an
        // intermediate state where imports would already be in use but not yet available.
        {
            let data = self.data.borrow();
            let requirements = data.picked_suggestions.iter().filter_map(|ps| ps.import.clone());
            self.add_required_imports(requirements, true)?;
        }

        let node_id = self.mode.node_id();
        let expression = self.get_expression(self.data.borrow().input.ast());
        let graph = self.graph.graph();
        graph.set_expression(node_id, expression)?;
        if let Mode::NewNode { .. } = *self.mode {
            graph.introduce_name_on(node_id)?;
        }
        if let Some(this) = self.this_arg.deref().as_ref() {
            this.introduce_pattern(graph.clone_ref())?;
        }
        // Should go last, as we want to prevent a revert only when the committing process was
        // successful.
        if let Some(guard) = self.node_edit_guard.deref().as_ref() {
            guard.prevent_revert()
        }
        Ok(node_id)
    }

    fn get_expression(&self, input: Option<&Ast>) -> String {
        let expression = match (self.this_var(), input) {
            (Some(this_var), Some(input)) => apply_this_argument(this_var, input).repr(),
            (None, Some(input)) => input.repr(),
            (_, None) => "".to_owned(),
        };
        expression
    }

    /// Adds an example to the graph.
    ///
    /// The example piece of code will be inserted as a new function definition, and in current
    /// graph the node calling this function will appear.
    #[profile(Debug)]
    pub fn add_example(&self, example: &action::Example) -> FallibleResult<ast::Id> {
        // === Add new function definition ===
        let graph = self.graph.graph();
        let mut module = double_representation::module::Info { ast: graph.module.ast() };
        let graph_definition_name = graph.graph_info()?.source.name.item;
        let new_definition = example.definition_to_add(&module, self.ide.parser())?;
        let new_definition_name = Ast::var(new_definition.name.name.item.clone());
        let new_definition_place =
            double_representation::module::Placement::Before(graph_definition_name);
        module.add_method(new_definition, new_definition_place, self.ide.parser())?;


        // === Add new node ===
        let args = std::iter::empty();
        let this_expression = Ast::var(self.module_qualified_name().name());
        let node_expression =
            ast::prefix::Chain::new_with_this(new_definition_name, this_expression, args);
        let node_expression = node_expression.into_ast();
        let node = NodeInfo::from_main_line_ast(&node_expression).ok_or(FailedToCreateNode)?;
        let added_node_id = node.id();
        let graph_definition =
            double_representation::module::locate(&module.ast, &self.graph.graph().id)?;
        let mut graph_info = GraphInfo::from_definition(graph_definition.item);
        graph_info.add_node(&node, LocationHint::End)?;
        module.ast = module.ast.set_traversing(&graph_definition.crumbs, graph_info.ast())?;
        let position =
            self.graph.graph().node(self.mode.node_id())?.metadata.and_then(|md| md.position);
        let metadata = NodeMetadata { position, ..default() };


        // === Add imports ===
        for import in example.imports.iter().map(QualifiedName::from_text).filter_map(Result::ok) {
            let import = model::suggestion_database::entry::Import::Qualified { module: import };
            let already_imported = module.iter_imports().any(|info| import.covered_by(&info));
            if !already_imported {
                module.add_import(self.ide.parser(), import.into());
            }
        }
        graph.module.update_ast(module.ast)?;
        graph.module.set_node_metadata(added_node_id, metadata)?;

        Ok(added_node_id)
    }

    #[profile(Debug)]
    fn invalidate_picked_suggestions(&self) {
        let mut data = self.data.borrow_mut();
        let data = &mut *data;
        let input = &data.input;
        data.picked_suggestions.drain_filter(|frag| !frag.is_still_unmodified(input));
    }

    #[profile(Debug)]
    fn add_required_imports<'a>(
        &self,
        import_requirements: impl Iterator<Item = RequiredImport>,
        permanent: bool,
    ) -> FallibleResult {
        let imports = import_requirements
            .filter_map(|requirement| match requirement {
                RequiredImport::Entry(entry) => Some(
                    entry.required_imports(&self.database, self.module_qualified_name().as_ref()),
                ),
                RequiredImport::Name(name) => {
                    let (_id, entry) = self.database.lookup_by_qualified_name(&name)?;
                    let defined_in = self.module_qualified_name();
                    Some(entry.required_imports(&self.database, defined_in.as_ref()))
                }
            })
            .flatten();
        let mut module = self.module();
        // TODO[ao] this is a temporary workaround. See [`Searcher::add_enso_project_entries`]
        //     documentation.
        let enso_project_special_import = suggestion_database::entry::Import::Qualified {
            module: ENSO_PROJECT_SPECIAL_MODULE.try_into().unwrap(),
        };
        let without_enso_project = imports.filter(|i| *i != enso_project_special_import);
        for entry_import in without_enso_project {
            let already_imported =
                module.iter_imports().any(|existing| entry_import.covered_by(&existing));
            let import: import::Info = entry_import.into();
            let import_id = import.id();
            let already_inserted = module.contains_import(import_id);
            let need_to_insert = !already_imported;
            let old_import_became_permanent = permanent && already_inserted;
            let need_to_update_md = need_to_insert || old_import_became_permanent;
            if need_to_insert {
                module.add_import(self.ide.parser(), import);
            }
            if need_to_update_md {
                self.graph.graph().module.with_import_metadata(
                    import_id,
                    Box::new(|import_metadata| {
                        import_metadata.is_temporary = !permanent;
                    }),
                )?;
            }
        }
        self.graph.graph().module.update_ast(module.ast)
    }

    fn clear_temporary_imports(&self) {
        let transaction_name = "Clearing temporary imports after closing searcher.";
        let _skip = self.graph.undo_redo_repository().open_ignored_transaction(transaction_name);
        let mut module = self.module();
        let import_metadata = self.graph.graph().module.all_import_metadata();
        let metadata_to_remove = import_metadata
            .into_iter()
            .filter_map(|(id, import_metadata)| {
                import_metadata.is_temporary.then(|| {
                    if let Err(e) = module.remove_import_by_id(id) {
                        warn!("Failed to remove import because of: {e:?}");
                    }
                    id
                })
            })
            .collect_vec();
        if let Err(e) = self.graph.graph().module.update_ast(module.ast) {
            warn!("Failed to update module ast when removing imports because of: {e:?}");
        }
        for id in metadata_to_remove {
            if let Err(e) = self.graph.graph().module.remove_import_metadata(id) {
                warn!("Failed to remove import metadata for import id {id} because of: {e:?}");
            }
        }
    }


    /// Reload Action List.
    ///
    /// The current list will be set as "Loading" and Language Server will be requested for a new
    /// list - once it be retrieved, the new list will be set and notification will be emitted.
    #[profile(Debug)]
    pub fn reload_list(&self) {
        let edited_literal = self.data.borrow().input.edited_literal().cloned();
        if let Some(literal) = edited_literal {
            let components = component_list_for_literal(&literal, &self.database);
            let mut data = self.data.borrow_mut();
            data.components = components;
            data.actions = Actions::Loaded { list: default() };
        } else {
            let this_type = self.this_arg_type_for_next_completion();
            self.gather_actions_from_engine(this_type, None);
            self.data.borrow_mut().actions = Actions::Loading;
        }
        executor::global::spawn(self.notifier.publish(Notification::NewActionList));
    }

    /// Get the typename of "this" value for current completion context. Returns `Future`, as the
    /// type information might not have came yet from the Language Server.
    #[profile(Debug)]
    fn this_arg_type_for_next_completion(&self) -> impl Future<Output = Option<String>> {
        let replaced_range = {
            let input = &self.data.borrow().input;
            let default_range = (input.cursor_position..input.cursor_position).into();
            input.edited_name_range().unwrap_or(default_range)
        };
        let is_first_function = replaced_range.start == Byte(0);
        let graph = self.graph.clone_ref();
        let this = self.this_arg.clone_ref();
        async move {
            if is_first_function {
                let ThisNode { id, .. } = this.deref().as_ref()?;
                let opt_type = graph.expression_type(*id).await.map(Into::into);
                opt_type.map_none(move || error!("Failed to obtain type for this node."))
            } else {
                None
            }
        }
    }

    fn gather_actions_from_engine(
        &self,
        this_type: impl Future<Output = Option<String>> + 'static,
        tags: Option<Vec<language_server::SuggestionEntryType>>,
    ) {
        let ls = self.language_server.clone_ref();
        let graph = self.graph.graph();
        let position = self.my_utf16_location().span.into();
        let this = self.clone_ref();
        executor::global::spawn(async move {
            let this_type = this_type.await;
            let is_static = this_type.is_some().then_some(false);
            info!("Requesting new suggestion list. Type of `self` is {this_type:?}.");
            let file = graph.module.path().file_path();
            let response =
                ls.completion(file, &position, &this_type, &None, &tags, &is_static).await;
            match response {
                Ok(response) => {
                    info!("Received suggestions from Language Server.");
                    let list = this.make_action_list(&response);
                    let mut data = this.data.borrow_mut();
                    let filter = data.input.filter();
                    list.update_filtering(filter.pattern.clone_ref());
                    data.actions = Actions::Loaded { list: Rc::new(list) };
                    let completions = response.results;
                    data.components = this.make_component_list(completions, &this_type);
                    data.components.update_filtering(filter);
                }
                Err(err) => {
                    let msg = "Request for completions to the Language Server returned error";
                    error!("{msg}: {err}");
                    let mut data = this.data.borrow_mut();
                    data.actions = Actions::Error(Rc::new(err.into()));
                    data.components = this.make_component_list(this.database.keys(), &this_type);
                    data.components.update_filtering(data.input.filter());
                }
            }
            this.notifier.publish(Notification::NewActionList).await;
        });
    }

    /// Process multiple completion responses from the engine into a single list of suggestion.
    #[profile(Debug)]
    fn make_action_list(
        &self,
        completion_response: &language_server::response::Completion,
    ) -> action::List {
        let creating_new_node = matches!(self.mode.deref(), Mode::NewNode { .. });
        let should_add_additional_entries = creating_new_node && self.this_arg.is_none();
        let mut actions = action::ListWithSearchResultBuilder::new();
        let (libraries_icon, default_icon) =
            action::hardcoded::ICONS.with(|i| (i.libraries.clone_ref(), i.default.clone_ref()));
        if should_add_additional_entries && self.ide.manage_projects().is_ok() {
            let mut root_cat = actions.add_root_category("Projects", default_icon.clone_ref());
            let category = root_cat.add_category("Projects", default_icon.clone_ref());
            let create_project = action::ProjectManagement::CreateNewProject;
            category.add_action(Action::ProjectManagement(create_project));
        }
        let mut libraries_root_cat =
            actions.add_root_category("Libraries", libraries_icon.clone_ref());
        if should_add_additional_entries {
            let examples_cat =
                libraries_root_cat.add_category("Examples", default_icon.clone_ref());
            examples_cat.extend(self.database.iterate_examples().map(Action::Example));
        }
        let libraries_cat =
            libraries_root_cat.add_category("Libraries", libraries_icon.clone_ref());
        if should_add_additional_entries {
            Self::add_enso_project_entries(&libraries_cat);
        }
        let entries = completion_response.results.iter().filter_map(|id| {
            self.database
                .lookup(*id)
                .map(|entry| Action::Suggestion(action::Suggestion::FromDatabase(entry)))
                .handle_err(|e| {
                    error!(
                        "Response provided a suggestion ID that cannot be \
                    resolved: {e}."
                    )
                })
        });
        libraries_cat.extend(entries);

        actions.build()
    }

    #[profile(Debug)]
    fn make_component_list<'a>(
        &self,
        entry_ids: impl IntoIterator<Item = suggestion_database::entry::Id>,
        this_type: &Option<String>,
    ) -> component::List {
        let favorites = self.graph.component_groups();
        let module_name = self.module_qualified_name();
        let mut builder = component_list_builder_with_favorites(
            &self.database,
            module_name.as_ref(),
            &*favorites,
            self.ide.are_component_browser_private_entries_visible(),
        );
        add_virtual_entries_to_builder(&mut builder, this_type);
        builder.extend_list_and_allow_favorites_with_ids(&self.database, entry_ids);
        builder.build()
    }

    /// Convert a location within a current module (i.e. module being edited) to a location indexed
    /// by UTF-16 code units. This enables Language Server protocol compatibility.
    fn location_to_utf16(
        &self,
        location: Location<Byte>,
    ) -> suggestion_database::entry::ModuleSpan {
        let module: Rope = self.graph.graph().module.ast().repr().into();
        suggestion_database::entry::ModuleSpan {
            module: self.module_qualified_name(),
            span:   module.utf16_code_unit_location_of_location(location),
        }
    }

    /// Convert a position of the searcher in the code to an Engine-compatible UTF-16 location.
    fn my_utf16_location(&self) -> suggestion_database::entry::ModuleSpan {
        let location = self.position_in_code.deref().into();
        self.location_to_utf16(location)
    }

    fn module(&self) -> double_representation::module::Info {
        double_representation::module::Info { ast: self.graph.graph().module.ast() }
    }

    fn module_qualified_name(&self) -> QualifiedName {
        self.graph.module_qualified_name(&*self.project)
    }

    /// Add to the action list the special mocked entry of `Enso_Project.data`.
    ///
    /// This is a workaround for Engine bug https://github.com/enso-org/enso/issues/1605.
    //TODO[ao] this is a temporary workaround.
    fn add_enso_project_entries(libraries_cat_builder: &action::CategoryBuilder) {
        // We may unwrap here, because the constant is tested to be convertible to
        // [`QualifiedName`].
        let module = QualifiedName::from_text(ENSO_PROJECT_SPECIAL_MODULE).unwrap();
        let self_type = module.clone();
        for method in &["data", "root"] {
            let entry = model::suggestion_database::Entry {
                name:          (*method).to_owned(),
                kind:          model::suggestion_database::entry::Kind::Method,
                defined_in:    module.clone(),
                arguments:     vec![],
                return_type:   "Standard.Base.System.File.File".try_into().unwrap(),
                documentation: vec![],
                self_type:     Some(self_type.clone()),
                is_static:     true,
                scope:         model::suggestion_database::entry::Scope::Everywhere,
                icon_name:     None,
                reexported_in: None,
            };
            let action = Action::Suggestion(action::Suggestion::FromDatabase(Rc::new(entry)));
            libraries_cat_builder.add_action(action);
        }
    }
}


// === Searcher helpers ===

fn component_list_builder_with_favorites<'a>(
    suggestion_db: &model::SuggestionDatabase,
    local_scope_module: QualifiedNameRef,
    groups: impl IntoIterator<Item = &'a model::execution_context::ComponentGroup>,
    private_entries_visibile: bool,
) -> component::builder::List {
    let mut builder = if private_entries_visibile {
        component::builder::List::new_with_private_components()
    } else {
        component::builder::List::new()
    };
    if let Some((id, _)) = suggestion_db.lookup_by_qualified_name(local_scope_module) {
        builder = builder.with_local_scope_module_id(id);
    }
    builder.set_grouping_and_order_of_favorites(suggestion_db, groups);
    builder
}

fn add_virtual_entries_to_builder(
    builder: &mut component::builder::List,
    this_type: &Option<String>,
) {
    if this_type.is_none() {
        let snippets = component::hardcoded::INPUT_SNIPPETS.with(|s| s.clone());
        let group_name = component::hardcoded::INPUT_GROUP_NAME;
        let project = project::QualifiedName::standard_base_library();
        builder.insert_virtual_components_in_favorites_group(group_name, project, snippets);
    }
}


// === Node Edit Metadata Guard ===

/// On creation the `EditGuard` saves the current expression of the node to its metadata.
/// When dropped the metadata is cleared again and, by default, the node content is reverted to the
/// previous expression. The expression reversion can be prevented by calling `prevent_revert`.
///
/// This structure does not create any Undo Redo transactions, but may contribute to existing one
/// if opened somewhere else.
#[derive(Debug)]
struct EditGuard {
    node_id:           ast::Id,
    graph:             controller::ExecutedGraph,
    revert_expression: Cell<bool>,
}

impl EditGuard {
    pub fn new(mode: &Mode, graph: controller::ExecutedGraph) -> Self {
        debug!("Initialising EditGuard.");

        let ret = Self { node_id: mode.node_id(), graph, revert_expression: Cell::new(true) };
        ret.save_node_expression_to_metadata(mode).unwrap_or_else(|e| {
            error!("Failed to save the node edit metadata due to error: {}", e)
        });
        ret
    }

    pub fn prevent_revert(&self) {
        self.revert_expression.set(false);
    }

    /// Mark the node as edited in its metadata and save the current expression, so it can later
    /// be restored.
    fn save_node_expression_to_metadata(&self, mode: &Mode) -> FallibleResult {
        let transaction_name = "Storing edited node original expression.";
        let _skip = self.graph.undo_redo_repository().open_ignored_transaction(transaction_name);
        let module = &self.graph.graph().module;
        match mode {
            Mode::NewNode { .. } => module.with_node_metadata(
                self.node_id,
                Box::new(|m| {
                    m.edit_status = Some(NodeEditStatus::Created {});
                }),
            ),
            Mode::EditNode { .. } => {
                let node = self.graph.graph().node(self.node_id)?;
                let previous_expression = node.info.main_line.expression().to_string();
                module.with_node_metadata(
                    self.node_id,
                    Box::new(|metadata| {
                        metadata.edit_status = Some(NodeEditStatus::Edited { previous_expression });
                    }),
                )
            }
        }
    }

    /// Mark the node as no longer edited and discard the edit metadata.
    fn clear_node_edit_metadata(&self) -> FallibleResult {
        let transaction_name = "Storing edited node original expression.";
        let _skip = self.graph.undo_redo_repository().open_ignored_transaction(transaction_name);
        let module = &self.graph.graph().module;
        module.with_node_metadata(
            self.node_id,
            Box::new(|metadata| {
                metadata.edit_status = None;
            }),
        )
    }

    fn get_saved_expression(&self) -> FallibleResult<Option<NodeEditStatus>> {
        let module = &self.graph.graph().module;
        Ok(module.node_metadata(self.node_id)?.edit_status)
    }

    fn revert_node_expression_edit(&self) -> FallibleResult {
        let transaction_name = "Reverting node expression to original.";
        let _skip = self.graph.undo_redo_repository().open_ignored_transaction(transaction_name);
        let edit_status = self.get_saved_expression()?;
        match edit_status {
            None => {
                error!(
                    "Tried to revert the expression of the edited node, \
                but found no edit metadata."
                );
            }
            Some(NodeEditStatus::Created) => {
                debug!("Deleting temporary node {} after aborting edit.", self.node_id);
                self.graph.graph().remove_node(self.node_id)?;
            }
            Some(NodeEditStatus::Edited { previous_expression }) => {
                debug!(
                    "Reverting expression of node {} to {} after aborting edit.",
                    self.node_id, &previous_expression
                );
                let graph = self.graph.graph();
                graph.set_expression(self.node_id, previous_expression)?;
            }
        };
        Ok(())
    }
}

impl Drop for EditGuard {
    fn drop(&mut self) {
        if self.revert_expression.get() {
            self.revert_node_expression_edit().unwrap_or_else(|e| {
                error!("Failed to revert node edit after editing ended because of an error: {e}")
            });
        } else {
            debug!("Not reverting node expression after edit.")
        }
        if self.graph.graph().node_exists(self.node_id) {
            self.clear_node_edit_metadata().unwrap_or_else(|e| {
                error!(
                "Failed to clear node edit metadata after editing ended because of an error: {e}"
            )
            });
        }
    }
}


// === Helpers ===

fn apply_this_argument(this_var: &str, ast: &Ast) -> Ast {
    if let Ok(opr) = ast::known::Opr::try_from(ast) {
        let shape = ast::SectionLeft { arg: Ast::var(this_var), off: 1, opr: opr.into() };
        Ast::new(shape, None)
    } else if let Some(mut infix) = ast::opr::GeneralizedInfix::try_new(ast) {
        if let Some(ref mut larg) = &mut infix.left {
            larg.arg = apply_this_argument(this_var, &larg.arg);
        } else {
            infix.left = Some(ast::opr::ArgWithOffset { arg: Ast::var(this_var), offset: 1 });
        }
        infix.into_ast()
    } else if let Some(mut prefix_chain) = ast::prefix::Chain::from_ast(ast) {
        prefix_chain.func = apply_this_argument(this_var, &prefix_chain.func);
        prefix_chain.into_ast()
    } else {
        let shape = ast::Infix {
            larg: Ast::var(this_var),
            loff: 0,
            opr:  Ast::opr(ast::opr::predefined::ACCESS),
            roff: 0,
            rarg: ast.clone_ref(),
        };
        Ast::new(shape, None)
    }
}

/// Build a component list with a single component, representing the given literal. When used as a
/// suggestion, a number literal will be inserted without changes, but a string literal will be
/// surrounded by quotation marks.
fn component_list_for_literal(
    literal: &input::Literal,
    db: &enso_suggestion_database::SuggestionDatabase,
) -> component::List {
    let mut builder = component::builder::List::default();
    let project = project::QualifiedName::standard_base_library();
    let snippet = component::hardcoded::Snippet::from_literal(literal, db).into();
    builder.insert_virtual_components_in_favorites_group("Literals", project, vec![snippet]);
    builder.build()
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod test {
    use super::*;

    use crate::controller::ide::plain::ProjectOperationsNotSupported;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::test::mock::data::project_qualified_name;
    use crate::test::mock::data::MAIN_FINISH;
    use crate::test::mock::data::MODULE_NAME;

    use engine_protocol::language_server::types::test::value_update_with_type;
    use engine_protocol::language_server::SuggestionId;
    use enso_suggestion_database::entry::Argument;
    use enso_suggestion_database::mock_suggestion_database;
    use enso_suggestion_database::SuggestionDatabase;
    use json_rpc::expect_call;
    use parser::Parser;
    use std::assert_matches::assert_matches;


    #[test]
    fn enso_project_special_module_is_convertible_to_qualified_names() {
        QualifiedName::from_text(ENSO_PROJECT_SPECIAL_MODULE)
            .expect("ENSO_PROJECT_SPECIAL_MODULE should be convertible to QualifiedName.");
    }

    pub fn completion_response(results: &[SuggestionId]) -> language_server::response::Completion {
        language_server::response::Completion {
            results:         results.to_vec(),
            current_version: default(),
        }
    }

    pub fn expect_completion(client: &mut language_server::MockClient, results: &[SuggestionId]) {
        let response = completion_response(results);
        client.expect.completion(|_, _, _, _, _, _| Ok(response))
    }

    #[derive(Debug, Derivative)]
    #[derivative(Default)]
    struct MockData {
        graph:         controller::graph::executed::tests::MockData,
        /// If the node in `main` function will be selected while opening searcher.
        selected_node: bool,
        #[derivative(Default(value = "MAIN_FINISH"))]
        code_location: engine_protocol::language_server::Position,
    }

    impl MockData {
        fn change_main_body(&mut self, lines: &[&str]) {
            let code: Rope = crate::test::mock::main_from_lines(lines).into();
            let location = code.last_line_end_location();
            // TODO [mwu] Not nice that we ended up with duplicated mock data for code.
            self.graph.module.code = (&code).into();
            self.graph.graph.code = (&code).into();
            self.code_location = code.utf16_code_unit_location_of_location(location).into();
        }

        fn expect_completion(
            &self,
            client: &mut language_server::MockClient,
            self_type: Option<&str>,
            result: &[SuggestionId],
        ) {
            let completion_response = completion_response(result);
            let is_static = self_type.is_some().then_some(false);
            expect_call!(client.completion(
                module      = self.graph.module.path.file_path().clone(),
                position    = self.code_location,
                self_type   = self_type.map(Into::into),
                return_type = None,
                tag         = None,
                is_static   = is_static
            ) => Ok(completion_response));
        }
    }

    struct Fixture {
        #[allow(dead_code)]
        data:     MockData,
        database: Rc<SuggestionDatabase>,
        test:     TestWithLocalPoolExecutor,
        searcher: Searcher,
    }

    impl Fixture {
        fn new_custom<D, C>(database_setup: D, client_setup: C) -> Self
        where
            D: FnOnce(RangeInclusive<Location<enso_text::Utf16CodeUnit>>) -> Rc<SuggestionDatabase>,
            C: FnOnce(&mut MockData, &mut language_server::MockClient), {
            let test = TestWithLocalPoolExecutor::set_up();
            let mut data = MockData::default();
            let mut client = language_server::MockClient::default();
            client.require_all_calls();
            client_setup(&mut data, &mut client);
            let code = enso_text::Rope::from(&data.graph.module.code);
            let start_of_code = enso_text::Location::default();
            let end_of_code = code.location_of_text_end_utf16_code_unit();
            let code_range = start_of_code..=end_of_code;
            let graph = data.graph.controller();
            let node = &graph.graph().nodes().unwrap()[0];
            let searcher_target = graph.graph().nodes().unwrap().last().unwrap().id();
            let this = ThisNode::new(node.info.id(), &graph.graph());
            let this = data.selected_node.and_option(this);
            let database = database_setup(code_range);
            let mut ide = controller::ide::MockAPI::new();
            let mut project = model::project::MockAPI::new();
            let project_qname = project_qualified_name();
            let project_name = project_qname.project.clone();
            project.expect_qualified_name().returning_st(move || project_qname.clone());
            project.expect_name().returning_st(move || project_name.clone());
            let project = Rc::new(project);
            ide.expect_parser().return_const(Parser::new());
            let current_project = project.clone_ref();
            ide.expect_current_project().returning_st(move || Some(current_project.clone_ref()));
            ide.expect_manage_projects()
                .returning_st(move || Err(ProjectOperationsNotSupported.into()));
            ide.expect_are_component_browser_private_entries_visible().returning_st(|| false);
            let node_metadata_guard = default();
            let breadcrumbs = Breadcrumbs::new();
            let searcher = Searcher {
                graph,
                database: database.clone_ref(),
                ide: Rc::new(ide),
                data: default(),
                breadcrumbs,
                notifier: default(),
                mode: Immutable(Mode::NewNode { node_id: searcher_target, source_node: None }),
                language_server: language_server::Connection::new_mock_rc(client),
                this_arg: Rc::new(this),
                position_in_code: Immutable(code.last_line_end_location()),
                project: project.clone_ref(),
                node_edit_guard: node_metadata_guard,
            };
            Fixture { data, test, searcher, database }
        }

        fn new() -> Self {
            Self::new_custom(|_| default(), |_, _| {})
        }

        fn lookup(&self, name: &QualifiedName) -> Rc<enso_suggestion_database::Entry> {
            self.database.lookup_by_qualified_name(name).expect("Database lookup failed.").1
        }

        fn lookup_str(&self, name: &str) -> Rc<enso_suggestion_database::Entry> {
            self.database.lookup_by_qualified_name_str(name).expect("Database lookup failed.")
        }
    }

    fn test_function_1_name() -> QualifiedName {
        crate::test::mock::data::module_qualified_name().new_child("testFunction1")
    }

    fn test_function_2_name() -> QualifiedName {
        crate::test::mock::data::module_qualified_name().new_child("testFunction2")
    }

    fn test_method_name() -> QualifiedName {
        crate::test::mock::data::module_qualified_name().new_child("test_method")
    }

    fn test_var_1_name() -> QualifiedName {
        crate::test::mock::data::module_qualified_name().new_child("test_var_1")
    }

    fn test_method_3_name() -> &'static str {
        "test.Test.Test.test_method3"
    }

    fn suggestion_database_with_mock_entries(
        scope: RangeInclusive<Location<enso_text::Utf16CodeUnit>>,
    ) -> Rc<SuggestionDatabase> {
        let database = mock_suggestion_database! {
            test.Test {
                mod Test {
                    static fn test_method3(this: Standard.Base.Any, arg: Standard.Base.Text) -> Standard.Base.Text;
                }
            }
            mock_namespace.Mock_Project {
                mod Mock_Module {
                    static fn test_method(this: Standard.Base.Any, arg: Standard.Base.Number) -> Standard.Base.Number;
                    static fn test_method2() -> Standard.Base.Any;
                }
            }
        };

        let module_name = crate::test::mock::data::module_qualified_name();
        let return_type = QualifiedName::from_text("Standard.Base.Number").unwrap();
        let function = suggestion_database::Entry::new_function(
            module_name.clone(),
            "testFunction1",
            return_type.clone(),
            scope.clone(),
        );
        let local = suggestion_database::Entry::new_local(
            module_name.clone(),
            "test_var_1",
            return_type.clone(),
            scope.clone(),
        );
        let arguments = vec![
            Argument::new("text_arg", "Standard.Base.Text"),
            Argument::new("num_arg", "Standard.Base.Number"),
        ];
        let function2 = suggestion_database::Entry::new_function(
            module_name,
            "testFunction2",
            return_type,
            scope,
        )
        .with_arguments(arguments);

        database.put_entry(101, function);
        database.put_entry(102, local);
        database.put_entry(103, function2);
        Rc::new(database)
    }

    /// Test checks that:
    /// 1) if the selected node is assigned to a single variable (or can be assigned), the list is
    ///    not immediately presented;
    /// 2) instead the searcher model obtains the type information for the selected node and uses it
    ///    to query Language Server for the suggestion list;
    /// 3) The query for argument type takes the this-argument presence into consideration.
    #[test]
    fn loading_list_w_self() {
        let mock_type = crate::test::mock::data::TYPE_NAME;

        struct Case {
            /// The single line of the initial `main` body.
            node_line: &'static str,
            /// If the searcher should enter "connect to this" mode at all and wait for type info.
            sets_this: bool,
        }

        let cases = [
            Case { node_line: "2+2", sets_this: true },
            Case { node_line: "the_sum = 2 + 2", sets_this: true },
            Case { node_line: "[x,y] = 2 + 2", sets_this: false },
        ];

        for case in &cases {
            let mut fixture =
                Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
                    data.change_main_body(&[case.node_line]);
                    data.selected_node = true;
                    // We expect following calls:
                    // 1) for the function - with the "this" filled (if the test case says so);
                    // 2) for subsequent completions - without "self"
                    data.expect_completion(client, case.sets_this.as_some(mock_type), &[1, 5, 9]);

                    data.expect_completion(client, None, &[1, 5, 9]);
                    data.expect_completion(client, None, &[1, 5, 9]);
                });
            let test_function_1 = fixture.lookup(&test_function_1_name());
            let test_function_2 = fixture.lookup(&test_function_2_name());
            let searcher = &mut fixture.searcher;

            searcher.reload_list();

            // The suggestion list should stall only if we actually use "this" argument.
            if case.sets_this {
                assert!(searcher.actions().is_loading());
                fixture.test.run_until_stalled();
                // Nothing appeared, because we wait for type information for this node.
                assert!(searcher.actions().is_loading());

                let this_node_id = searcher.this_arg.deref().as_ref().unwrap().id;
                let update = value_update_with_type(this_node_id, mock_type);
                searcher.graph.computed_value_info_registry().apply_updates(vec![update]);
                assert!(searcher.actions().is_loading());
            }

            fixture.test.run_until_stalled();
            assert!(!searcher.actions().is_loading());
            let suggestion2 = action::Suggestion::FromDatabase(test_function_2.clone_ref());
            searcher.use_suggestion(suggestion2).unwrap();
            let suggestion1 = action::Suggestion::FromDatabase(test_function_1.clone_ref());
            searcher.use_suggestion(suggestion1).unwrap();
            let expected_input = format!("{} {}", test_function_2.name, test_function_1.name);
            assert_eq!(searcher.data.borrow().input.ast().repr(), expected_input);
        }
    }

    #[test]
    fn arguments_suggestions_for_picked_method() {
        let mut fixture =
            Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
                data.expect_completion(client, None, &[20]);
            });
        let test_method = fixture.lookup(&test_method_name());
        let Fixture { test, searcher, .. } = &mut fixture;
        searcher.use_suggestion(action::Suggestion::FromDatabase(test_method.clone_ref())).unwrap();
        assert!(searcher.actions().is_loading());
        test.run_until_stalled();
        assert!(!searcher.actions().is_loading());
    }

    #[test]
    fn arguments_suggestions_for_picked_function() {
        let mut fixture =
            Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
                data.expect_completion(client, None, &[]); // Function suggestion.
            });


        let test_function_2 = fixture.lookup(&test_function_2_name());
        let Fixture { searcher, .. } = &mut fixture;
        searcher
            .use_suggestion(action::Suggestion::FromDatabase(test_function_2.clone_ref()))
            .unwrap();
        assert_eq!(searcher.data.borrow().input.ast().unwrap().repr(), "testFunction2");
        searcher.set_input("testFunction2 'foo' ".to_owned(), Byte(20)).unwrap();
        searcher.set_input("testFunction2 'foo' 10 ".to_owned(), Byte(23)).unwrap();
    }

    #[test]
    fn non_picked_function_arg_suggestions() {
        let mut fixture =
            Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
                data.graph.module.code.insert_str(0, "import test.Test.Test\n\n");
                data.code_location.line += 2;
                data.expect_completion(client, None, &[1]);
                data.expect_completion(client, None, &[]);
                data.expect_completion(client, None, &[]);
            });
        let Fixture { searcher, .. } = &mut fixture;

        // Known functions cases
        searcher.set_input("Test.test_method ".to_string(), Byte(16)).unwrap();
        searcher.set_input(format!("{MODULE_NAME}.test_method "), Byte(16)).unwrap();
        searcher.set_input("testFunction2 \"str\" ".to_string(), Byte(16)).unwrap();

        // Unknown functions case
        searcher.set_input("unknownFunction ".to_string(), Byte(14)).unwrap();
    }

    #[test]
    fn loading_list() {
        let mut fixture =
            Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
                // entry with id 99999 does not exist, so only two actions from suggestions db
                // should be displayed in searcher.
                data.expect_completion(client, None, &[101, 99999, 103]);
            });

        let test_function_1 = fixture.lookup(&test_function_1_name());
        let test_function_2 = fixture.lookup(&test_function_2_name());
        let searcher = &mut fixture.searcher;
        let mut subscriber = searcher.subscribe();
        searcher.reload_list();
        assert!(searcher.actions().is_loading());
        fixture.test.run_until_stalled();
        let list = searcher.actions().list().unwrap().to_action_vec();
        // There are 8 entries, because: 2 were returned from `completion` method, two are mocked,
        // and all of these are repeated in "All Search Result" category.
        assert_eq!(list.len(), 8);
        assert_eq!(list[2], Action::Suggestion(action::Suggestion::FromDatabase(test_function_1)));
        assert_eq!(list[3], Action::Suggestion(action::Suggestion::FromDatabase(test_function_2)));
        let notification = subscriber.next().boxed_local().expect_ready();
        assert_eq!(notification, Some(Notification::NewActionList));
    }

    #[test]
    fn loading_components() {
        // Prepare a sample component group to be returned by a mock Language Server client.
        let module_qualified_name = crate::test::mock::data::module_qualified_name().to_string();
        let sample_ls_component_group = language_server::LibraryComponentGroup {
            library: project::QualifiedName::standard_base_library().to_string(),
            name:    "Test Group 1".to_string(),
            color:   None,
            icon:    None,
            exports: vec![
                language_server::LibraryComponent {
                    name:     module_qualified_name.clone() + ".test_method2",
                    shortcut: None,
                },
                language_server::LibraryComponent {
                    name:     module_qualified_name + ".test_method",
                    shortcut: None,
                },
            ],
        };
        // Create a test fixture with mocked Engine responses.
        let mut fixture =
            Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
                // Entry with id 99999 does not exist, so only two actions from suggestions db
                // should be displayed in searcher.
                data.expect_completion(client, None, &[5, 99999, 103]);
                data.graph.ctx.component_groups = vec![sample_ls_component_group];
            });
        let test_function_2 = fixture.lookup(&test_function_2_name());
        let test_method = fixture.lookup(&test_method_name());
        let searcher = &mut fixture.searcher;
        // Reload the components list in the Searcher.
        searcher.reload_list();
        fixture.test.run_until_stalled();
        // Verify the contents of the components list loaded by the Searcher.
        let components = searcher.components();
        if let [module_group] = &components.top_modules().next().unwrap()[..] {
            let expected_group_name = format!(
                "{}.{}",
                test_method.defined_in.project().project,
                test_method.defined_in.name()
            );
            assert_eq!(module_group.name, expected_group_name);
            let entries = module_group.entries.borrow();
            assert_matches!(
                entries.as_slice(),
                [e1, e2] if e1.name() == test_method.name && e2.name() == test_function_2.name
            );
        } else {
            panic!(
                "Wrong top modules in Component List: {:?}",
                components.top_modules().collect::<Vec<_>>()
            );
        }
        let favorites = &components.favorites;
        assert_eq!(favorites.len(), 2);
        let favorites_group_0 = &favorites[0];
        assert_eq!(favorites_group_0.name, component::hardcoded::INPUT_GROUP_NAME);
        let favorites_group_1 = &favorites[1];
        assert_eq!(favorites_group_1.name, "Test Group 1");
        let favorites_entries = favorites_group_1.entries.borrow();
        assert_eq!(favorites_entries.len(), 1);
        assert_eq!(favorites_entries[0].id().unwrap(), 5);
    }

    fn are_same(
        action: &action::Suggestion,
        entry: &Rc<model::suggestion_database::Entry>,
    ) -> bool {
        match action {
            action::Suggestion::FromDatabase(lhs) => Rc::ptr_eq(lhs, entry),
            action::Suggestion::Hardcoded(_) => false,
        }
    }

    #[test]
    fn picked_completions_list_maintaining() {
        let fixture = Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
            data.expect_completion(client, None, &[]);
            data.expect_completion(client, None, &[]);
            data.expect_completion(client, None, &[]);
        });
        let test_function_1 = fixture.lookup(&test_function_1_name());
        let test_var_1 = fixture.lookup(&test_var_1_name());
        let Fixture { test: _test, searcher, .. } = fixture;
        let picked_suggestions = || Ref::map(searcher.data.borrow(), |d| &d.picked_suggestions);

        // Picking first suggestion.
        let suggestion = action::Suggestion::FromDatabase(test_function_1.clone_ref());
        let new_input = searcher.use_suggestion(suggestion).unwrap();
        assert_eq!(new_input.text, "testFunction1 ");
        let (func,) = picked_suggestions().iter().cloned().expect_tuple();
        assert!(are_same(&func.entry, &test_function_1));

        // Typing more args by hand.
        searcher.set_input("testFunction1 some_arg pat".to_string(), Byte(26)).unwrap();
        let (func,) = picked_suggestions().iter().cloned().expect_tuple();
        assert!(are_same(&func.entry, &test_function_1));

        // Picking argument's suggestion.
        let suggestion1 = action::Suggestion::FromDatabase(test_var_1.clone_ref());
        let new_input = searcher.use_suggestion(suggestion1.clone()).unwrap();
        assert_eq!(new_input.text, "test_var_1 ");
        let new_input = searcher.use_suggestion(suggestion1).unwrap();
        assert_eq!(new_input.text, "test_var_1 ");
        let (function, arg1, arg2) = picked_suggestions().iter().cloned().expect_tuple();
        assert!(are_same(&function.entry, &test_function_1));
        assert!(are_same(&arg1.entry, &test_var_1));
        assert!(are_same(&arg2.entry, &test_var_1));
    }

    #[test]
    fn applying_this_var() {
        #[derive(Copy, Clone, Debug)]
        struct Case {
            before: &'static str,
            after:  &'static str,
        }

        impl Case {
            fn new(before: &'static str, after: &'static str) -> Self {
                Case { before, after }
            }

            fn run(&self) {
                let parser = Parser::new();
                let ast = parser.parse_line_ast(self.before).unwrap();
                let new_ast = apply_this_argument("foo", &ast);
                assert_eq!(new_ast.repr(), self.after, "Case {self:?} failed: {ast:?}");
            }
        }

        let cases = [
            Case::new("bar", "foo.bar"),
            Case::new("bar.baz", "foo.bar.baz"),
            Case::new("bar baz", "foo.bar baz"),
            Case::new("+ 2", "foo + 2"),
            Case::new("+ 2 + 3", "foo + 2 + 3"),
            Case::new("+ 2 - 3", "foo + 2 - 3"),
            Case::new("+ bar baz", "foo + bar baz"),
            Case::new("map x-> x.characters.length", "foo.map x-> x.characters.length"),
            Case::new("at 3 == y", "foo.at 3 == y"),
        ];

        for case in &cases {
            case.run();
        }
    }

    #[test]
    fn adding_node_introducing_this_var() {
        struct Case {
            line:              &'static str,
            result:            String,
            expect_completion: bool,
            run:               Box<dyn FnOnce(&mut Fixture, action::Suggestion)>,
        }

        impl Case {
            fn new(
                line: &'static str,
                result: &[&str],
                expect_completion: bool,
                run: impl FnOnce(&mut Fixture, action::Suggestion) + 'static,
            ) -> Self {
                Case {
                    line,
                    expect_completion,
                    result: crate::test::mock::main_from_lines(result),
                    run: Box::new(run),
                }
            }
        }

        let cases = vec![
            // Completion was picked.
            Case::new(
                "2 + 2",
                &["sum1 = 2 + 2", "operator1 = sum1.testFunction1"],
                true,
                |f, s| {
                    f.searcher.use_suggestion(s).unwrap();
                },
            ),
            // The input was manually written (not picked).
            Case::new(
                "2 + 2",
                &["sum1 = 2 + 2", "operator1 = sum1.testFunction1"],
                false,
                |f, _| {
                    f.searcher.set_input("testFunction1".to_owned(), Byte(13)).unwrap();
                },
            ),
            // Completion was picked and edited.
            Case::new(
                "2 + 2",
                &["sum1 = 2 + 2", "operator1 = sum1.var.testFunction1"],
                true,
                |f, s| {
                    f.searcher.use_suggestion(s).unwrap();
                    let parser = f.searcher.ide.parser();
                    let expr = "var.testFunction1";
                    let new_parsed_input = input::Input::parse(parser, expr, Byte(expr.len()));
                    f.searcher.data.borrow_mut().input = new_parsed_input;
                },
            ),
            // Variable name already present, need to use it. And not break it.
            Case::new(
                "my_var = 2 + 2",
                &["my_var = 2 + 2", "operator1 = my_var.testFunction1"],
                true,
                |f, s| {
                    f.searcher.use_suggestion(s).unwrap();
                },
            ),
            // Variable names unusable (subpatterns are not yet supported).
            // Don't use "this" argument adjustments at all.
            Case::new(
                "[x,y] = 2 + 2",
                &["[x,y] = 2 + 2", "testfunction11 = testFunction1"],
                true,
                |f, s| {
                    f.searcher.use_suggestion(s).unwrap();
                },
            ),
        ];

        for case in cases.into_iter() {
            let mut fixture =
                Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
                    data.selected_node = true;
                    // The last node will be used as searcher target.
                    data.change_main_body(&[case.line, "Nothing"]);
                    if case.expect_completion {
                        data.expect_completion(client, None, &[]);
                    }
                });
            let entry = fixture.lookup(&test_function_1_name());
            let suggestion = action::Suggestion::FromDatabase(entry);
            (case.run)(&mut fixture, suggestion);
            fixture.searcher.commit_node().unwrap();
            let updated_def = fixture.searcher.graph.graph().definition().unwrap().item;
            assert_eq!(updated_def.ast.repr(), case.result);
        }
    }

    #[test]
    fn adding_imports_with_nodes() {
        fn expect_inserted_import_for(
            entry: &Rc<model::suggestion_database::Entry>,
            expected_import: Vec<&QualifiedName>,
        ) {
            let Fixture { test: _test, mut searcher, .. } =
                Fixture::new_custom(suggestion_database_with_mock_entries, |_, _| {});
            let module = searcher.graph.graph().module.clone_ref();
            let parser = searcher.ide.parser().clone_ref();

            let picked_method = PickedSuggestion {
                entry:         action::Suggestion::FromDatabase(entry.clone_ref()),
                inserted_code: default(),
                import:        Some(RequiredImport::Entry(entry.clone_ref())),
            };
            with(searcher.data.borrow_mut(), |mut data| {
                data.picked_suggestions.push(picked_method);
                data.input = input::Input::parse(&parser, entry.name.to_string(), default());
            });

            // Add new node.
            let node_id = searcher.mode.node_id();
            searcher.mode = Immutable(Mode::NewNode { node_id, source_node: None });
            searcher.commit_node().unwrap();

            let module_info = module.info();
            let imported_names = module_info
                .iter_imports()
                .map(|import| import.qualified_module_name().unwrap())
                .collect_vec();

            let expected_import = expected_import.into_iter().cloned().collect_vec();
            assert_eq!(imported_names, expected_import);
        }

        let fixture = Fixture::new_custom(suggestion_database_with_mock_entries, |_, _| {});
        let test_function_1 = fixture.lookup(&test_function_1_name());
        let test_var_1 = fixture.lookup(&test_var_1_name());
        let test_method = fixture.lookup(&test_function_1_name());
        let test_method_3 = fixture.lookup_str(test_method_3_name());
        expect_inserted_import_for(&test_function_1, vec![]);
        expect_inserted_import_for(&test_var_1, vec![]);
        expect_inserted_import_for(&test_method, vec![]);
        expect_inserted_import_for(&test_method_3, vec![&test_method_3.defined_in]);
    }

    #[test]
    fn committing_node() {
        let mut fixture =
            Fixture::new_custom(suggestion_database_with_mock_entries, |data, _client| {
                data.change_main_body(&["2 + 2", "Nothing"]); // The last node will be used as
                                                              // searcher target.
            });
        let test_method_3 = fixture.lookup_str(test_method_3_name());
        let searcher = &mut fixture.searcher;

        let (node1, searcher_target) = searcher.graph.graph().nodes().unwrap().expect_tuple();

        let module = searcher.graph.graph().module.clone_ref();
        // Setup searcher.
        let parser = Parser::new();
        let picked_method = PickedSuggestion {
            entry:         action::Suggestion::FromDatabase(test_method_3.clone_ref()),
            inserted_code: String::from("Test.test_method"),
            import:        Some(RequiredImport::Entry(test_method_3.clone_ref())),
        };
        with(searcher.data.borrow_mut(), |mut data| {
            data.picked_suggestions.push(picked_method);
            data.input = input::Input::parse(&parser, "Test.test_method".to_string(), Byte(16));
        });

        // Add new node.
        searcher.mode =
            Immutable(Mode::NewNode { node_id: searcher_target.id(), source_node: None });
        searcher.commit_node().unwrap();

        let expected_code =
            "import test.Test.Test\nmain =\n    2 + 2\n    operator1 = Test.test_method";
        assert_eq!(module.ast().repr(), expected_code);

        // Edit existing node.
        searcher.mode = Immutable(Mode::EditNode { node_id: node1.info.id() });
        searcher.commit_node().unwrap();
        let expected_code =
            "import test.Test.Test\nmain =\n    Test.test_method\n    operator1 = Test.test_method";
        assert_eq!(module.ast().repr(), expected_code);
    }

    #[test]
    fn adding_example() {
        let Fixture { test: _test, searcher, .. } = Fixture::new();
        let module = searcher.graph.graph().module.clone_ref();
        let example = model::suggestion_database::example::Example {
            name:               "Test Example".to_owned(),
            code:               "x = 2 + 2\nx + 4".to_owned(),
            imports:            vec![],
            documentation_html: "Lorem ipsum".to_owned(),
        };
        let expected_code = "test_example1 =\n    x = 2 + 2\n    x + 4\n\n\
            main = \n    2 + 2\n    Mock_Module.test_example1";
        searcher.add_example(&Rc::new(example)).unwrap();
        assert_eq!(module.ast().repr(), expected_code);
    }

    #[test]
    fn adding_example_twice() {
        let Fixture { test: _test, searcher, .. } = Fixture::new();
        let module = searcher.graph.graph().module.clone_ref();
        let example = model::suggestion_database::example::Example {
            name:               "Test Example".to_owned(),
            code:               "[1,2,3,4,5]".to_owned(),
            imports:            vec!["std.Base.Network.Http".to_owned()],
            documentation_html: "Lorem ipsum".to_owned(),
        };
        let expected_code = "import std.Base.Network.Http\n\
            test_example1 = [1,2,3,4,5]\n\ntest_example2 = [1,2,3,4,5]\n\n\
            main = \n    2 + 2\n    Mock_Module.test_example1\n    Mock_Module.test_example2";
        let example = Rc::new(example);
        searcher.add_example(&example).unwrap();
        searcher.add_example(&example).unwrap();
        assert_eq!(module.ast().repr(), expected_code);
    }

    #[test]
    fn edit_guard() {
        let Fixture { test: _test, mut searcher, .. } = Fixture::new();
        let graph = searcher.graph.graph();
        let node = graph.nodes().unwrap().last().unwrap().clone();
        let initial_node_expression = node.main_line.expression();
        let node_id = node.info.id();
        searcher.mode = Immutable(Mode::EditNode { node_id });
        searcher.node_edit_guard =
            Rc::new(Some(EditGuard::new(&searcher.mode, searcher.graph.clone_ref())));

        // Apply an edit to the node.
        graph.set_expression(node_id, "Edited Node").unwrap();

        // Verify the metadata was initialised after the guard creation.
        let module = graph.module.clone_ref();
        module
            .with_node_metadata(
                node_id,
                Box::new(|metadata| {
                    assert_eq!(
                        metadata.edit_status,
                        Some(NodeEditStatus::Edited {
                            previous_expression: node.info.expression().to_string(),
                        })
                    );
                }),
            )
            .unwrap();

        // Verify the metadata is cleared after the searcher is dropped.
        drop(searcher);
        module
            .with_node_metadata(
                node_id,
                Box::new(|metadata| {
                    assert_eq!(metadata.edit_status, None);
                }),
            )
            .unwrap();
        // Verify the node was reverted.

        let node = graph.nodes().unwrap().last().unwrap().clone();
        let final_node_expression = node.main_line.expression();
        assert_eq!(initial_node_expression.to_string(), final_node_expression.to_string());
    }

    #[test]
    fn edit_guard_no_revert() {
        let Fixture { test: _test, mut searcher, .. } = Fixture::new();
        let graph = searcher.graph.graph();
        let node = graph.nodes().unwrap().last().unwrap().clone();
        let node_id = node.info.id();
        searcher.mode = Immutable(Mode::EditNode { node_id });
        searcher.node_edit_guard =
            Rc::new(Some(EditGuard::new(&searcher.mode, searcher.graph.clone_ref())));

        // Apply an edit to the node.
        let new_expression = "Edited Node";
        graph.set_expression(node_id, new_expression).unwrap();
        // Prevent reverting the node by calling the `prevent_revert` method.
        searcher.node_edit_guard.deref().as_ref().unwrap().prevent_revert();

        // Verify the node is not reverted after the searcher is dropped.
        drop(searcher);
        let node = graph.nodes().unwrap().last().unwrap().clone();
        let final_node_expression = node.main_line.expression();
        assert_eq!(final_node_expression.to_string(), new_expression);
    }

    /// Test recognition of qualified names in the searcher's input.
    #[test]
    fn recognize_qualified_names() {
        fn database() -> Rc<SuggestionDatabase> {
            mock_suggestion_database! {
                mock_namespace.MockProject {
                    mod Foo {
                        mod Bar {
                            type Baz {
                                fn baz_method() -> Standard.Base.Number;
                            }

                            static fn bar_method() -> Standard.Base.Number;
                        }
                    }

                    mod Table {
                        mod Data {
                            mod Table {
                                type Table {
                                    static fn new() -> mock_namespace.MockProject.Table.Data.Table.Table;
                                }
                            }
                        }
                    }

                    static fn project_method() -> Standard.Base.Number;
                }
            }
            .into()
        }

        #[derive(Debug)]
        struct Case {
            entry:            String,
            input:            String,
            expected_code:    String,
            expected_imports: Vec<String>,
        }

        let cases = vec![
            Case {
                entry:            "mock_namespace.MockProject.Foo.Bar.Baz.baz_method".to_string(),
                input:            "Foo.Bar.".to_string(),
                expected_code:    "operator1 = Foo.Bar.Baz.baz_method".to_string(),
                expected_imports: vec!["import mock_namespace.MockProject.Foo".to_string()],
            },
            Case {
                entry:            "mock_namespace.MockProject.Foo.Bar.Baz.baz_method".to_string(),
                input:            "Bar.".to_string(),
                expected_code:    "operator1 = Bar.Baz.baz_method".to_string(),
                expected_imports: vec!["import mock_namespace.MockProject.Foo.Bar".to_string()],
            },
            Case {
                entry:            "mock_namespace.MockProject.Foo.Bar.bar_method".to_string(),
                input:            "Bar.".to_string(),
                expected_code:    "operator1 = Bar.bar_method".to_string(),
                expected_imports: vec!["import mock_namespace.MockProject.Foo.Bar".to_string()],
            },
            Case {
                entry:            "mock_namespace.MockProject.Foo.Bar.bar_method".to_string(),
                input:            "Foo.Gee.".to_string(),
                expected_code:    "operator1 = Bar.bar_method".to_string(),
                expected_imports: vec!["import mock_namespace.MockProject.Foo.Bar".to_string()],
            },
            Case {
                entry:            "mock_namespace.MockProject.Foo.Bar.bar_method".to_string(),
                input:            "mock_namespace.MockProject.Foo.Bar.".to_string(),
                expected_code:    "operator1 = mock_namespace.MockProject.Foo.Bar.bar_method"
                    .to_string(),
                expected_imports: vec![],
            },
            Case {
                entry:            "mock_namespace.MockProject.Table.Data.Table.Table.new"
                    .to_string(),
                input:            "Table.".to_string(),
                expected_code:    "operator1 = Table.new".to_string(),
                expected_imports: vec![
                    "from mock_namespace.MockProject.Table.Data.Table import Table".to_string(),
                ],
            },
            Case {
                entry:            "mock_namespace.MockProject.project_method".to_string(),
                input:            "mock_namespace.MockProject.".to_string(),
                expected_code:    "operator1 = mock_namespace.MockProject.project_method"
                    .to_string(),
                expected_imports: vec![],
            },
            Case {
                entry:            "mock_namespace.MockProject.project_method".to_string(),
                input:            "MockProject.".to_string(),
                expected_code:    "operator1 = MockProject.project_method".to_string(),
                expected_imports: vec!["import mock_namespace.MockProject".to_string()],
            },
            Case {
                entry:            "mock_namespace.MockProject.Foo.Bar.bar_method".to_string(),
                input:            "MockProject.".to_string(),
                expected_code:    "operator1 = MockProject.Foo.Bar.bar_method".to_string(),
                expected_imports: vec!["import mock_namespace.MockProject".to_string()],
            },
        ];

        for case in cases {
            let mut fixture = Fixture::new_custom(
                |_| database(),
                |data, client| {
                    // data.selected_node = true;
                    data.change_main_body(&["Nothing"]);
                    data.expect_completion(client, None, &[]);
                    data.expect_completion(client, None, &[]);
                },
            );
            let entry = fixture.lookup_str(&case.entry);
            let searcher = &mut fixture.searcher;

            searcher.set_input(case.input.clone(), Byte(case.input.len())).unwrap();
            let suggestion = action::Suggestion::FromDatabase(entry.clone_ref());
            searcher.preview_suggestion(suggestion.clone()).unwrap();
            searcher.use_suggestion(suggestion.clone()).unwrap();
            searcher.commit_node().unwrap();
            let updated_def = searcher.graph.graph().definition().unwrap().item;
            let expected = crate::test::mock::main_from_lines(&[case.expected_code.clone()]);
            assert_eq!(updated_def.ast.repr(), expected, "{case:?}");
            let module_info = &searcher.graph.graph().module.info();
            let imports = module_info.iter_imports();
            let imports = imports.map(|i| i.to_string()).collect_vec();
            assert_eq!(imports, case.expected_imports, "{case:?}");
        }
    }
}
