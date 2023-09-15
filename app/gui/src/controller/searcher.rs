//! This module contains all structures related to Searcher Controller.

use crate::model::traits::*;
use crate::prelude::*;

use crate::controller::graph::ImportType;
use crate::controller::graph::RequiredImport;
use crate::controller::searcher::breadcrumbs::BreadcrumbEntry;
use crate::model::execution_context::GroupQualifiedName;
use crate::model::module::NodeEditStatus;
use crate::model::suggestion_database;
use crate::presenter::searcher;

use breadcrumbs::Breadcrumbs;
use double_representation::name::project;
use double_representation::name::QualifiedName;
use engine_protocol::language_server;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_text as text;
use enso_text::Byte;
use enso_text::Location;
use enso_text::Rope;
use flo_stream::Subscriber;


// ==============
// === Export ===
// ==============

pub mod breadcrumbs;
pub mod component;
pub mod input;
pub mod search;



// =================
// === Constants ===
// =================

/// If enabled, searcher will assign names for all nodes created with it, not only when it is
/// needed. Currently enabled to trigger engine's caching of user-added nodes.
/// See: https://github.com/enso-org/ide/issues/1067
pub const ASSIGN_NAMES_FOR_NODES: bool = true;
/// A name of a component group containing entries representing literals.
pub const LITERALS_GROUP_NAME: &str = "Literals";


// ==============
// === Errors ===
// ==============


#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "No component with the index {}.", index)]
pub struct NoSuchComponent {
    index: usize,
}

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Component with the index {} cannot be entered.", index)]
pub struct NotEnterable {
    index: usize,
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
    /// A new component list is available.
    NewComponentList,
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

        let existing_var = node.variable_name().ok()?.map(|name| name.to_owned());
        let needs_to_introduce_pattern = existing_var.is_none();
        let make_new_var = || graph.variable_name_for(&node.info).ok().map(|var| var.repr());
        let var = existing_var.or_else(make_new_var)?;

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

    /// Return the ID of the node used as source for the Searcher.
    pub fn source_node(&self) -> Option<ast::Id> {
        match self {
            Mode::NewNode { source_node, .. } => *source_node,
            Mode::EditNode { .. } => None,
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
    pub suggestion:    component::Suggestion,
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
    /// The component list which should be displayed.
    pub components:         Rc<component::List>,
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
        let components = default();
        let picked_suggestions = default();
        Ok(Data { input, components, picked_suggestions })
    }
}

/// An information used for filtering entries.
#[derive(Debug, Clone, CloneRef, Eq, PartialEq)]
pub struct Filter {
    /// The part of the input used for filtering.
    pub pattern: ImString,
    /// Additional context. A string representation of the edited accessor chain.
    pub context: Option<ImString>,
    /// The name of the currently active module. This is necessary since the module influences what
    /// code to generate. At the time of writing, this is only the case when importing a module
    /// method of a main module: the module is referred to as `Main` from within the same module
    /// or by the project name when referenced elsewhere. See
    /// `enso_suggestion_database::Entry::code_with_static_this` for its usage.
    module_name: Rc<QualifiedName>,
}

/// Component Browser Controller.
///
/// This controller provides list of suggestions for given searcher input and controls how Component
/// Browser interacts with the code (applying suggestion, comitting/aborting editing, updating code
/// for preview).
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

    /// Dump the suggestion database to the console in JSON format.
    pub fn dump_database_as_json(&self) {
        console_log!("{}", self.database.dump_as_json());
    }

    /// Abort editing and perform cleanup.
    pub fn abort_editing(&self) {
        self.clear_temporary_imports();
    }

    /// Return true if user is currently filtering entries (the input has non-empty _pattern_ part).
    pub fn is_filtering(&self) -> bool {
        !self.filter().pattern.is_empty()
    }

    /// Return true if the current searcher input is empty.
    pub fn is_input_empty(&self) -> bool {
        self.data.borrow().input.is_empty()
    }

    /// Subscribe to controller's notifications.
    pub fn subscribe(&self) -> Subscriber<Notification> {
        self.notifier.subscribe()
    }

    /// Get the current component list.
    pub fn components(&self) -> Rc<component::List> {
        self.data.borrow().components.clone_ref()
    }

    /// Get the documentation for the entry.
    pub fn documentation_for_entry(&self, index: usize) -> EntryDocumentation {
        let data = self.data.borrow();
        let component = data.components.displayed().get(index);
        if let Some(component) = component {
            match &component.suggestion {
                component::Suggestion::FromDatabase { id, .. } =>
                    self.database.documentation_for_entry(*id),
                component::Suggestion::Virtual { snippet } =>
                    snippet.documentation.clone().unwrap_or_default(),
            }
        } else {
            default()
        }
    }

    /// Enter the specified module. The displayed content of the browser will be updated.
    pub fn enter_entry(&self, _entry: usize) -> FallibleResult {
        self.reload_list();
        Ok(())
    }

    /// A list of breadcrumbs' text labels to be displayed. The list is updated by
    /// [`Self::enter_entry`].
    pub fn breadcrumbs(&self) -> Vec<ImString> {
        self.breadcrumbs.names()
    }

    /// Set the selected breadcrumb. The `id` is the index of the breadcrumb from left to right.
    pub fn select_breadcrumb(&self, id: usize) {
        self.breadcrumbs.select(id);
    }

    /// Set the breadcrumbs to match the component at the given `index`. The index refers to the
    /// displayed list of components. Returns the full breadcrumb for the entry, if there is one.
    pub fn update_breadcrumbs(&self, index: usize) -> Option<Vec<BreadcrumbEntry>> {
        let data = self.data.borrow();
        if let Some(component) = data.components.displayed().get(index) {
            if let Some(id) = component.id() {
                let bc_builder = breadcrumbs::Builder::new(&self.database);
                let breadcrumbs = bc_builder.build(id).collect_vec();
                assert!(breadcrumbs.iter().all(|e| self.database.lookup(e.id()).is_ok()));
                self.breadcrumbs.set_content(breadcrumbs.clone().into_iter());
                Some(breadcrumbs)
            } else {
                warn!(
                    "Cannot update breadcrumbs with component that has no suggestion database \
                entry. Invalid component: {:?}",
                    component.suggestion.name()
                );
                None
            }
        } else {
            warn!("Update breadcrumbs called with invalid index: {}", index);
            None
        }
    }

    /// Return the documentation for the breadcrumb.
    pub fn documentation_for_selected_breadcrumb(&self) -> Option<EntryDocumentation> {
        let selected = self.breadcrumbs.selected();
        let component = selected?;
        assert!(self.database.lookup(component).is_ok());
        let docs = self.database.documentation_for_entry(component);
        Some(docs)
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
        let old_filter = self.filter();
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
            let filter = self.filter();
            if filter != old_filter {
                let mut data = self.data.borrow_mut();
                Rc::make_mut(&mut data.components).update_filtering(filter.clone_ref());
                executor::global::spawn(self.notifier.publish(Notification::NewComponentList));
            }
        }
        Ok(())
    }

    fn this_var(&self) -> Option<&str> {
        self.this_arg.deref().as_ref().map(|this| this.var.as_ref())
    }

    /// Pick a component suggestion and apply it to the input.
    #[profile(Debug)]
    pub fn use_suggestion(
        &self,
        suggestion: component::Suggestion,
    ) -> FallibleResult<text::Change<Byte, String>> {
        debug!("Picking suggestion: {suggestion:?}.");
        let change = {
            let mut data = self.data.borrow_mut();
            let has_this = self.this_var().is_some();
            let module_name = self.module_qualified_name();
            let inserted = data.input.after_inserting_suggestion(
                &suggestion,
                has_this,
                module_name.as_ref(),
            )?;
            let new_cursor_position = inserted.inserted_text.end;
            let inserted_code = inserted.new_input[inserted.inserted_code].to_owned();
            let import = inserted.import.clone();
            let parser = self.ide.parser();
            data.input = input::Input::parse(parser, &inserted.new_input, new_cursor_position);
            let picked = PickedSuggestion { suggestion, inserted_code, import };
            data.picked_suggestions.push(picked);
            inserted.input_change()
        };
        self.reload_list();
        self.breadcrumbs.set_content(iter::empty());
        Ok(change)
    }

    /// Use action at given index as a suggestion. The exact outcome depends on the action's type.
    pub fn use_suggestion_by_index(
        &self,
        index: usize,
    ) -> FallibleResult<enso_text::Change<Byte, String>> {
        let error = || NoSuchComponent { index };
        let suggestion = self.data.with_borrowed(|data| {
            let component = data.components.displayed().get(index);
            component.map(|c| c.suggestion.clone()).ok_or_else(error)
        })?;
        self.use_suggestion(suggestion)
    }

    /// Preview the suggestion in the searcher by given index, or preview the current searcher input
    /// if index is `None`.
    pub fn preview_by_index(&self, index: Option<usize>) -> FallibleResult {
        debug!("Previewing entry: {index:?}.");
        let data = self.data.borrow();
        let suggestion = index
            .map(|index| {
                let error = || NoSuchComponent { index };
                data.components.displayed().get(index).map(|c| &c.suggestion).ok_or_else(error)
            })
            .transpose()?;
        self.preview(suggestion)?;
        Ok(())
    }

    /// Preview the current suggestion input.
    pub fn preview_input(&self) -> FallibleResult {
        self.preview(None)
    }

    /// Update the edited node's code with the current preview.
    ///
    /// If `suggestion` is specified, the preview will contains code after applying it.
    /// Otherwise it will be just the current searcher input.
    pub fn preview(&self, suggestion: Option<&component::Suggestion>) -> FallibleResult {
        let transaction_name = "Previewing Component Browser suggestion.";
        let _skip = self
            .graph
            .undo_redo_repository()
            .open_ignored_transaction_or_ignore_current(transaction_name);

        debug!("Updating node preview. Previewed suggestion: \"{suggestion:?}\".");
        self.clear_temporary_imports();
        let has_this = self.this_var().is_some();
        let preview_change_result = suggestion.map(|suggestion| {
            let module_name = self.module_qualified_name();
            self.data.borrow().input.after_inserting_suggestion(
                suggestion,
                has_this,
                module_name.as_ref(),
            )
        });

        let suggestion_change = preview_change_result.transpose()?;
        let empty_node_ast = || Ast::cons(ast::constants::keywords::NOTHING).with_new_id();
        let preview_ast = match &suggestion_change {
            Some(change) if change.new_input.trim().is_empty() => empty_node_ast(),
            Some(change) => self.ide.parser().parse_line_ast(&change.new_input)?,
            None => self.data.borrow().input.ast().cloned().unwrap_or_else(empty_node_ast),
        };
        let expression = self.get_expression(preview_ast);

        {
            // This block serves to limit the borrow of `self.data`.
            let data = self.data.borrow();
            let current_input_requirements =
                data.picked_suggestions.iter().filter_map(|ps| ps.import.clone());
            let picked_suggestion_requirement = suggestion_change.and_then(|change| change.import);
            let all_requirements =
                current_input_requirements.chain(picked_suggestion_requirement.iter().cloned());
            self.graph.graph().add_required_imports(all_requirements, ImportType::Temporary)?;
        }
        self.graph.graph().set_expression_ast(self.mode.node_id(), expression)?;

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

        let expression = match &self.data.borrow().input.ast {
            input::InputAst::Line(ast) => ast.clone(),
            input::InputAst::Invalid(input) => self.ide.parser().parse_line(input)?,
        };

        // We add the required imports before we edit its content. This way, we avoid an
        // intermediate state where imports would already be in use but not yet available.
        {
            let data = self.data.borrow();
            let requirements = data.picked_suggestions.iter().filter_map(|ps| ps.import.clone());
            self.graph.graph().add_required_imports(requirements, ImportType::Permanent)?;
        }

        let node_id = self.mode.node_id();
        let graph = self.graph.graph();
        graph.set_expression_ast(node_id, self.get_expression(expression.elem))?;
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

    fn get_expression(&self, input: Ast) -> Ast {
        match self.this_var() {
            Some(this_var) => searcher::apply_this_argument(this_var, &input),
            None => input,
        }
    }

    #[profile(Debug)]
    fn invalidate_picked_suggestions(&self) {
        let mut data = self.data.borrow_mut();
        let data = &mut *data;
        let input = &data.input;
        data.picked_suggestions.drain_filter(|frag| !frag.is_still_unmodified(input));
    }

    fn clear_temporary_imports(&self) {
        let transaction_name = "Clearing temporary imports after closing searcher.";
        let _skip = self
            .graph
            .undo_redo_repository()
            .open_ignored_transaction_or_ignore_current(transaction_name);
        self.graph.graph().clear_temporary_imports();
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
            self.data.borrow_mut().components = Rc::new(components);
        } else {
            let this_type = self.this_arg_type_for_next_completion();
            self.gather_actions_from_engine(this_type, None);
        }
        executor::global::spawn(self.notifier.publish(Notification::NewComponentList));
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
            let is_static = Some(this_type.is_none());
            info!("Requesting new suggestion list. Type of `self` is {this_type:?}.");
            let file = graph.module.path().file_path();
            let response =
                ls.completion(file, &position, &this_type, &None, &tags, &is_static).await;
            let new_list = match response {
                Ok(response) => {
                    info!("Received suggestions from Language Server.");
                    let completions = response.results;
                    this.make_component_list(completions, &this_type)
                }
                Err(err) => {
                    let msg = "Request for completions to the Language Server returned error";
                    error!("{msg}: {err}");
                    this.make_component_list(this.database.keys(), &this_type)
                }
            };
            this.data.borrow_mut().components = new_list;
            this.notifier.publish(Notification::NewComponentList).await;
        });
    }

    #[profile(Debug)]
    fn make_component_list<'a>(
        &self,
        entry_ids: impl IntoIterator<Item = suggestion_database::entry::Id>,
        this_type: &Option<String>,
    ) -> Rc<component::List> {
        let db = &*self.database;
        let groups = self.graph.component_groups();
        let mut builder = match (this_type, self.breadcrumbs.selected()) {
            (Some(tp), _) => component::Builder::new_with_this_type(db, &groups, tp),
            (None, Some(module)) => component::Builder::new_inside_module(db, &groups, module),
            _ => {
                let mut builder = component::Builder::new(db, &groups);
                add_virtual_entries_to_builder(&mut builder);
                builder
            }
        };

        builder.add_components_from_db(entry_ids);
        let mut list = builder.build();
        list.update_filtering(self.filter());
        Rc::new(list)
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

    fn module_qualified_name(&self) -> QualifiedName {
        self.graph.module_qualified_name_with_project(&*self.project)
    }

    fn filter(&self) -> Filter {
        self.data.borrow().input.filter(self.module_qualified_name())
    }
}


// === Searcher helpers ===

fn add_virtual_entries_to_builder(builder: &mut component::Builder) {
    let snippets = component::hardcoded::INPUT_SNIPPETS.with(|s| s.clone());
    // Unwrap is safe because conversion from INPUT_GROUP_NAME is tested.
    let group_name = GroupQualifiedName::try_from(component::hardcoded::INPUT_GROUP_NAME).unwrap();
    builder.add_virtual_entries_to_group(group_name, snippets);
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
        let _skip = self
            .graph
            .undo_redo_repository()
            .open_ignored_transaction_or_ignore_current(transaction_name);
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
                let previous_expression = node.info.expression().to_string();
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
        let _skip = self
            .graph
            .undo_redo_repository()
            .open_ignored_transaction_or_ignore_current(transaction_name);
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
        let _skip = self
            .graph
            .undo_redo_repository()
            .open_ignored_transaction_or_ignore_current(transaction_name);
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

/// Build a component list with a single component, representing the given literal. When used as a
/// suggestion, a number literal will be inserted without changes, but a string literal will be
/// surrounded by quotation marks.
fn component_list_for_literal(
    literal: &input::Literal,
    db: &enso_suggestion_database::SuggestionDatabase,
) -> component::List {
    let mut builder = component::builder::Builder::new_empty(db);
    let project = project::QualifiedName::standard_base_library();
    let group_name = GroupQualifiedName::new(project, LITERALS_GROUP_NAME);
    let snippet = component::hardcoded::Snippet::from_literal(literal, db).into();
    builder.add_virtual_entries_to_group(group_name, vec![snippet]);
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
    use crate::presenter::searcher::apply_this_argument;
    use crate::test::mock;
    use crate::test::mock::data::MAIN_FINISH;

    use engine_protocol::language_server::types::test::value_update_with_type;
    use engine_protocol::language_server::LibraryComponent;
    use engine_protocol::language_server::LibraryComponentGroup;
    use engine_protocol::language_server::SuggestionId;
    use enso_suggestion_database::entry::Argument;
    use enso_suggestion_database::mock_suggestion_database;
    use enso_suggestion_database::SuggestionDatabase;
    use json_rpc::expect_call;
    use parser::Parser;


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
            let is_static = Some(self_type.is_none());
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
            let database = database_setup(code_range);
            let graph = data.graph.controller_with_db(database.clone_ref());
            let node = &graph.graph().nodes().unwrap()[0];
            let searcher_target = graph.graph().nodes().unwrap().last().unwrap().id();
            let this = ThisNode::new(node.info.id(), &graph.graph());
            let this = data.selected_node.and_option(this);
            let mut ide = controller::ide::MockAPI::new();
            let mut project = model::project::MockAPI::new();
            let project_qname = mock::data::project_qualified_name();
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

        fn lookup_suggestion(&self, name: &QualifiedName) -> component::Suggestion {
            let (id, entry) =
                self.database.lookup_by_qualified_name(name).expect("Database lookup failed.");
            component::Suggestion::FromDatabase { id, entry }
        }

        fn test_function_1_suggestion(&self) -> component::Suggestion {
            let name = mock::data::module_qualified_name().new_child("testFunction1");
            self.lookup_suggestion(&name)
        }

        fn test_function_2_suggestion(&self) -> component::Suggestion {
            let name = mock::data::module_qualified_name().new_child("testFunction2");
            self.lookup_suggestion(&name)
        }

        fn test_method_3_suggestion(&self) -> component::Suggestion {
            self.lookup_suggestion(&"test.Test.Test.test_method3".try_into().unwrap())
        }

        fn test_var_1_suggestion(&self) -> component::Suggestion {
            let name = mock::data::module_qualified_name().new_child("test_var_1");
            self.lookup_suggestion(&name)
        }
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

                    type Mock_Type {
                        Mock_Constructor;

                        fn test_method4() -> Standard.Base.Any;
                        static fn test_method5() -> Standard.Base.Any;
                    }
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
                    data.expect_completion(client, case.sets_this.as_some(mock_type), &[9]);

                    data.expect_completion(client, None, &[0, 1, 2, 3, 4, 5]);
                    data.expect_completion(client, None, &[0, 1, 2, 3, 4, 5]);
                });
            let searcher = &fixture.searcher;

            searcher.reload_list();

            // The suggestion list should stall only if we actually use "this" argument.
            if case.sets_this {
                assert!(searcher.components().displayed().is_empty());
                fixture.test.run_until_stalled();
                // Nothing appeared, because we wait for type information for this node.
                assert!(searcher.components().displayed().is_empty());

                let this_node_id = searcher.this_arg.deref().as_ref().unwrap().id;
                let update = value_update_with_type(this_node_id, mock_type);
                searcher.graph.computed_value_info_registry().apply_updates(vec![update]);
                assert!(searcher.components().displayed().is_empty());
            }

            fixture.test.run_until_stalled();
            assert!(!searcher.components().displayed().is_empty());
            let suggestion2 = fixture.test_function_2_suggestion();
            let suggestion1 = fixture.test_function_1_suggestion();
            let expected_input = "testFunction2 testFunction1";
            searcher.use_suggestion(suggestion2).unwrap();
            searcher.use_suggestion(suggestion1).unwrap();
            assert_eq!(searcher.data.borrow().input.ast().repr(), expected_input);
        }
    }

    #[test]
    fn arguments_suggestions_for_function() {
        let fixture = Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
            data.expect_completion(client, None, &[]); // Function suggestion.
        });

        let Fixture { searcher, .. } = &fixture;
        searcher.use_suggestion(fixture.test_function_2_suggestion()).unwrap();
        assert_eq!(searcher.data.borrow().input.ast().unwrap().repr(), "testFunction2");
        searcher.set_input("testFunction2 'foo' ".to_owned(), Byte(20)).unwrap();
        searcher.set_input("testFunction2 'foo' 10 ".to_owned(), Byte(23)).unwrap();
    }

    #[test]
    fn loading_list() {
        let mut fixture =
            Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
                let module_name = mock::data::module_qualified_name();
                data.graph.ctx.component_groups = vec![LibraryComponentGroup {
                    library: mock::data::project_qualified_name().to_string(),
                    name:    "Group".to_owned(),
                    color:   None,
                    icon:    None,
                    exports: vec![
                        LibraryComponent {
                            name:     module_name.clone().new_child("testFunction1").into(),
                            shortcut: None,
                        },
                        LibraryComponent {
                            name:     module_name.new_child("testFunction2").into(),
                            shortcut: None,
                        },
                    ],
                }];
                // entry with id 99999 does not exist, so only two actions from suggestions db
                // should be displayed in searcher.
                data.expect_completion(client, None, &[101, 99999, 103]);
            });

        let searcher = &fixture.searcher;
        let mut subscriber = searcher.subscribe();
        searcher.reload_list();
        assert!(searcher.components().displayed().is_empty());
        fixture.test.run_until_stalled();
        let list = dbg!(searcher.components());
        // There are 4 entries, because: 2 were returned from `completion` method, and two are
        // virtual entries.
        let components = list.displayed();
        assert_eq!(components.len(), 4);
        assert_eq!(components[0].suggestion, fixture.test_function_1_suggestion());
        assert_eq!(components[1].suggestion, fixture.test_function_2_suggestion());
        let notification = subscriber.next().boxed_local().expect_ready();
        assert_eq!(notification, Some(Notification::NewComponentList));
    }

    #[test]
    fn picked_completions_list_maintaining() {
        let fixture = Fixture::new_custom(suggestion_database_with_mock_entries, |data, client| {
            data.expect_completion(client, None, &[]);
            data.expect_completion(client, None, &[]);
            data.expect_completion(client, None, &[]);
        });
        let test_function_1 = fixture.test_function_1_suggestion();
        let test_var_1 = fixture.test_var_1_suggestion();
        let Fixture { test: _test, searcher, .. } = fixture;
        let picked_suggestions = || Ref::map(searcher.data.borrow(), |d| &d.picked_suggestions);

        // Picking first suggestion.
        let new_input = searcher.use_suggestion(test_function_1.clone()).unwrap();
        assert_eq!(new_input.text, "testFunction1 ");
        let (func,) = picked_suggestions().iter().cloned().expect_tuple();
        assert_eq!(&func.suggestion, &test_function_1);

        // Typing more args by hand.
        searcher.set_input("testFunction1 some_arg pat".to_string(), Byte(26)).unwrap();
        let (func,) = picked_suggestions().iter().cloned().expect_tuple();
        assert_eq!(&func.suggestion, &test_function_1);

        // Picking argument's suggestion.
        let new_input = searcher.use_suggestion(test_var_1.clone()).unwrap();
        assert_eq!(new_input.text, "test_var_1 ");
        let new_input = searcher.use_suggestion(test_var_1.clone()).unwrap();
        assert_eq!(new_input.text, "test_var_1 ");
        let (function, arg1, arg2) = picked_suggestions().iter().cloned().expect_tuple();
        assert_eq!(&function.suggestion, &test_function_1);
        assert_eq!(&arg1.suggestion, &test_var_1);
        assert_eq!(&arg2.suggestion, &test_var_1);
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
            run:               Box<dyn FnOnce(&mut Fixture, component::Suggestion)>,
        }

        impl Case {
            fn new(
                line: &'static str,
                result: &[&str],
                expect_completion: bool,
                run: impl FnOnce(&mut Fixture, component::Suggestion) + 'static,
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
            let suggestion = fixture.test_function_1_suggestion();
            (case.run)(&mut fixture, suggestion);
            fixture.searcher.commit_node().unwrap();
            let updated_def = fixture.searcher.graph.graph().definition().unwrap().item;
            assert_eq!(updated_def.ast.repr(), case.result);
        }
    }

    #[test]
    fn adding_imports_with_nodes() {
        fn expect_inserted_import_for(
            suggestion: component::Suggestion,
            expected_import: Vec<&QualifiedName>,
        ) {
            let Fixture { test: _test, mut searcher, .. } =
                Fixture::new_custom(suggestion_database_with_mock_entries, |_, _| {});
            let module = searcher.graph.graph().module.clone_ref();
            let parser = searcher.ide.parser().clone_ref();
            let expression = suggestion.name().to_owned();
            let import = suggestion.required_import();

            let picked_method = PickedSuggestion { suggestion, inserted_code: default(), import };
            with(searcher.data.borrow_mut(), |mut data| {
                data.picked_suggestions.push(picked_method);
                data.input = input::Input::parse(&parser, expression.as_str(), default());
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
        expect_inserted_import_for(fixture.test_function_1_suggestion(), vec![]);
        expect_inserted_import_for(fixture.test_var_1_suggestion(), vec![]);
        expect_inserted_import_for(fixture.test_method_3_suggestion(), vec![&"test.Test.Test"
            .try_into()
            .unwrap()]);
    }

    #[test]
    fn committing_node() {
        let mut fixture =
            Fixture::new_custom(suggestion_database_with_mock_entries, |data, _client| {
                data.change_main_body(&["2 + 2", "Nothing"]); // The last node will be used as
                                                              // searcher target.
            });
        let test_method_3 = fixture.test_method_3_suggestion();
        let searcher = &mut fixture.searcher;

        let (node1, searcher_target) = searcher.graph.graph().nodes().unwrap().expect_tuple();

        let module = searcher.graph.graph().module.clone_ref();
        // Setup searcher.
        let parser = Parser::new();
        let import = test_method_3.required_import();
        let picked_method = PickedSuggestion {
            suggestion: test_method_3,
            inserted_code: String::from("Test.test_method"),
            import,
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
    fn edit_guard() {
        let Fixture { test: _test, mut searcher, .. } = Fixture::new();
        let graph = searcher.graph.graph();
        let node = graph.nodes().unwrap().last().unwrap().clone();
        let initial_node_expression = node.expression();
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
        let final_node_expression = node.expression();
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
        let final_node_expression = node.expression();
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
            let suggestion = fixture.lookup_suggestion(&case.entry.as_str().try_into().unwrap());
            let searcher = &mut fixture.searcher;

            searcher.set_input(case.input.clone(), Byte(case.input.len())).unwrap();
            searcher.preview(Some(&suggestion)).unwrap();
            searcher.use_suggestion(suggestion).unwrap();
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
