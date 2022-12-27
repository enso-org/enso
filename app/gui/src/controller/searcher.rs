//! This module contains all structures related to Searcher Controller.

use crate::model::traits::*;
use crate::prelude::*;

use crate::controller::graph::FailedToCreateNode;
use crate::controller::searcher::component::group;
use crate::model::module::MethodId;
use crate::model::module::NodeEditStatus;
use crate::model::module::NodeMetadata;
use crate::model::suggestion_database;
use crate::notification;

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
use enso_text::Byte;
use enso_text::Location;
use enso_text::Rope;
use flo_stream::Subscriber;
use parser_scala::Parser;


// ==============
// === Export ===
// ==============

pub mod action;
pub mod breadcrumbs;
pub mod component;

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

const MINIMUM_PATTERN_OFFSET: usize = 1;


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



// ===================
// === Input Parts ===
// ===================

/// An identification of input fragment filled by picking suggestion.
///
/// Essentially, this is a crumb for ParsedInput's expression.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum CompletedFragmentId {
    /// The called "function" part, defined as a `func` element in Prefix Chain
    /// (see `ast::prefix::Chain`).
    Function,
    /// The `id`th argument of the called function.
    Argument { index: usize },
}

/// The enum summarizing what user is currently doing basing on the searcher input.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UserAction {
    /// User is about to type/chose function (the input is likely empty).
    StartingTypingFunction,
    /// User is about to type next argument.
    StartingTypingArgument,
    /// User is in the middle of typing function.
    TypingFunction,
    /// User is in the middle of typing argument.
    TypingArgument,
}

/// A Searcher Input which is parsed to the _expression_ and _pattern_ parts.
///
/// We parse the input for better understanding what user wants to add.
#[derive(Clone, Debug, Default)]
pub struct ParsedInput {
    /// The part of input which is treated as completed function and some set of arguments.
    ///
    /// The expression is kept as prefix chain, as it allows us to easily determine what kind of
    /// entity we can put at this moment (is it a function or argument? What type of the
    /// argument?).
    pub expression:     Option<ast::Shifted<ast::prefix::Chain>>,
    /// An offset between expression and pattern.
    pub pattern_offset: usize,
    /// The part of input being a function/argument which is still typed by user. It is used
    /// for filtering actions.
    pub pattern:        String,
}

impl ParsedInput {
    /// Constructor from the plain input.
    #[profile(Debug)]
    fn new(input: impl Into<String>, parser: &Parser) -> FallibleResult<Self> {
        let mut input = input.into();
        let leading_spaces = input.chars().take_while(|c| *c == ' ').count();
        // To properly guess what is "still typed argument" we simulate type of one letter by user.
        // This letter will be added to the last argument (or function if there is no argument), or
        // will be a new argument (so the user starts filling a new argument).
        //
        // See also `parsed_input` test to see all cases we want to cover.
        input.push('a');
        let ast = parser.parse_line_ast(input.trim_start())?;
        let mut prefix = ast::prefix::Chain::from_ast_non_strict(&ast);
        if let Some(last_arg) = prefix.args.pop() {
            let mut last_arg_repr = last_arg.sast.wrapped.repr();
            last_arg_repr.pop();
            Ok(ParsedInput {
                expression:     Some(ast::Shifted::new(leading_spaces, prefix)),
                pattern_offset: last_arg.sast.off,
                pattern:        last_arg_repr,
            })
        } else {
            let mut func_repr = prefix.func.repr();
            func_repr.pop();
            Ok(ParsedInput {
                expression:     None,
                pattern_offset: leading_spaces,
                pattern:        func_repr,
            })
        }
    }

    fn new_from_ast(ast: &Ast) -> Self {
        let prefix = ast::prefix::Chain::from_ast_non_strict(ast);
        ParsedInput {
            expression:     Some(ast::Shifted::new(default(), prefix)),
            pattern_offset: 0,
            pattern:        default(),
        }
    }

    /// Returns the id of the next fragment potentially filled by picking completion suggestion.
    fn next_completion_id(&self) -> CompletedFragmentId {
        match &self.expression {
            None => CompletedFragmentId::Function,
            Some(expression) => CompletedFragmentId::Argument { index: expression.args.len() },
        }
    }

    /// Get the picked fragment from the Searcher's input.
    pub fn completed_fragment(&self, fragment: CompletedFragmentId) -> Option<String> {
        use CompletedFragmentId::*;
        match (fragment, &self.expression) {
            (_, None) => None,
            (Function, Some(expr)) => Some(expr.func.repr()),
            (Argument { index }, Some(expr)) => Some(expr.args.get(index)?.sast.wrapped.repr()),
        }
    }

    /// Get the user action basing of this input (see `UserAction` docs).
    pub fn user_action(&self) -> UserAction {
        use UserAction::*;
        let empty_pattern = self.pattern.is_empty();
        match self.next_completion_id() {
            CompletedFragmentId::Function if empty_pattern => StartingTypingFunction,
            CompletedFragmentId::Function => TypingFunction,
            CompletedFragmentId::Argument { .. } if empty_pattern => StartingTypingArgument,
            CompletedFragmentId::Argument { .. } => TypingArgument,
        }
    }

    /// Convert the current input to Prefix Chain representation.
    pub fn as_prefix_chain(&self, parser: &Parser) -> Option<ast::Shifted<ast::prefix::Chain>> {
        let parsed_pattern = parser.parse_line_ast(&self.pattern).ok();
        let pattern_sast = parsed_pattern.map(|p| ast::Shifted::new(self.pattern_offset, p));
        // If there is an expression part of input, we add current pattern as the last argument.
        if let Some(chain) = &self.expression {
            let mut chain = chain.clone();
            if let Some(sast) = pattern_sast {
                let prefix_id = None;
                let argument = ast::prefix::Argument { sast, prefix_id };
                chain.wrapped.args.push(argument);
            }
            Some(chain)
        // If there isn't any expression part, the pattern is the whole input.
        } else if let Some(sast) = pattern_sast {
            let chain = ast::prefix::Chain::from_ast_non_strict(&sast.wrapped);
            Some(ast::Shifted::new(self.pattern_offset, chain))
        } else {
            None
        }
    }
}

impl HasRepr for ParsedInput {
    fn repr(&self) -> String {
        let mut repr = self.expression.as_ref().map_or("".to_string(), HasRepr::repr);
        repr.extend(itertools::repeat_n(' ', self.pattern_offset));
        repr.push_str(&self.pattern);
        repr
    }
}

impl Display for ParsedInput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr())
    }
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
            Mode::EditNode { node_id } => *node_id,
        }
    }
}

/// A fragment filled by single picked suggestion.
///
/// We store such information in Searcher to better suggest the potential arguments, and to know
/// what imports should be added when inserting node.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct FragmentAddedByPickingSuggestion {
    pub id:                CompletedFragmentId,
    pub picked_suggestion: action::Suggestion,
}

impl FragmentAddedByPickingSuggestion {
    /// Check if the picked fragment is still unmodified by user.
    fn is_still_unmodified(&self, input: &ParsedInput) -> bool {
        let expected = self.code_to_insert(&None);
        input.completed_fragment(self.id).contains(&expected)
    }

    fn code_to_insert(&self, this_node: &Option<ThisNode>) -> Cow<str> {
        let generate_this = self.id != CompletedFragmentId::Function || this_node.is_none();
        self.picked_suggestion.code_to_insert(generate_this)
    }

    fn required_imports(
        &self,
        db: &model::SuggestionDatabase,
        current_module: QualifiedNameRef,
    ) -> impl IntoIterator<Item = suggestion_database::entry::Import> {
        self.picked_suggestion.required_imports(db, current_module)
    }
}

/// A controller state.
#[derive(Clone, Debug, Default)]
pub struct Data {
    /// The current searcher's input.
    pub input: ParsedInput,
    /// The action list which should be displayed.
    pub actions: Actions,
    /// The component list which should be displayed.
    pub components: component::List,
    /// All fragments of input which were added by picking suggestions. If the fragment will be
    /// changed by user, it will be removed from this list.
    pub fragments_added_by_picking: Vec<FragmentAddedByPickingSuggestion>,
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
        database: &model::SuggestionDatabase,
        edited_node_id: ast::Id,
    ) -> FallibleResult<Self> {
        let edited_node = graph.node(edited_node_id)?;
        let input = ParsedInput::new_from_ast(&edited_node.info.expression());
        let actions = default();
        let components = default();
        let intended_method = edited_node.metadata.and_then(|md| md.intended_method);
        let initial_entry = intended_method.and_then(|metadata| database.lookup_method(metadata));
        let initial_fragment = initial_entry.and_then(|entry| {
            let fragment = FragmentAddedByPickingSuggestion {
                id:                CompletedFragmentId::Function,
                picked_suggestion: action::Suggestion::FromDatabase(entry),
            };
            // This is meant to work with single function calls (without "this" argument).
            // In other case we should know what the function is from the engine, as the function
            // should be resolved.
            fragment.is_still_unmodified(&input).then_some(fragment)
        });
        let mut fragments_added_by_picking = Vec::<FragmentAddedByPickingSuggestion>::new();
        initial_fragment.for_each(|f| fragments_added_by_picking.push(f));
        Ok(Data { input, actions, components, fragments_added_by_picking })
    }

    fn find_picked_fragment(
        &self,
        id: CompletedFragmentId,
    ) -> Option<&FragmentAddedByPickingSuggestion> {
        self.fragments_added_by_picking.iter().find(|fragment| fragment.id == id)
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

impl ComponentsProvider {
    /// The list of modules and their content displayed in `Submodules` section of the browser.
    pub fn top_modules(&self) -> group::AlphabeticalList {
        let components = self.components();
        if let Some(selected) = self.breadcrumbs.selected() {
            components.submodules_of(selected).map(CloneRef::clone_ref).unwrap_or_default()
        } else if *self.has_this_arg {
            components.top_modules_flattened().clone_ref()
        } else {
            components.top_modules().clone_ref()
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
    logger:           Logger,
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
    /// Create new Searcher Controller.
    pub async fn new(
        parent: impl AnyLogger,
        ide: controller::Ide,
        project: &model::Project,
        method: language_server::MethodPointer,
        mode: Mode,
    ) -> FallibleResult<Self> {
        let graph = controller::ExecutedGraph::new(&parent, project.clone_ref(), method).await?;
        Self::new_from_graph_controller(parent, ide, project, graph, mode)
    }

    /// Abort editing and perform cleanup.
    pub fn abort_editing(&self) {
        self.clear_temporary_imports();
    }

    /// Create new Searcher Controller, when you have Executed Graph Controller handy.
    #[profile(Task)]
    pub fn new_from_graph_controller(
        parent: impl AnyLogger,
        ide: controller::Ide,
        project: &model::Project,
        graph: controller::ExecutedGraph,
        mode: Mode,
    ) -> FallibleResult<Self> {
        let project = project.clone_ref();
        let logger = Logger::new_sub(parent, "Searcher Controller");
        let database = project.suggestion_db();
        let data = if let Mode::EditNode { node_id } = mode {
            Data::new_with_edited_node(&graph.graph(), &database, node_id)?
        } else {
            default()
        };
        let node_metadata_guard = Rc::new(Some(EditGuard::new(&mode, graph.clone_ref())));
        let module_ast = graph.graph().module.ast();
        let def_id = graph.graph().id;
        let def_span = double_representation::module::definition_span(&module_ast, &def_id)?;
        let module_repr: Rope = module_ast.repr().into();
        let position = module_repr.offset_to_location_snapped(def_span.end);
        let this_arg = Rc::new(match mode {
            Mode::NewNode { source_node: Some(node), .. } => ThisNode::new(node, &graph.graph()),
            _ => None,
        });
        let breadcrumbs = Breadcrumbs::new();
        let ret = Self {
            logger,
            graph,
            this_arg,
            ide,
            data: Rc::new(RefCell::new(data)),
            breadcrumbs,
            notifier: default(),
            mode: Immutable(mode),
            database: project.suggestion_db(),
            language_server: project.json_rpc(),
            position_in_code: Immutable(position),
            project,
            node_edit_guard: node_metadata_guard,
        };
        Ok(ret.init())
    }

    fn init(self) -> Self {
        self.reload_list();
        self
    }

    /// Return true if user is currently filtering entries (the input has non-empty _pattern_ part).
    pub fn is_filtering(&self) -> bool {
        !self.data.borrow().input.pattern.is_empty()
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
    pub fn set_input(&self, new_input: String) -> FallibleResult {
        debug!("Manually setting input to {new_input}.");
        let parsed_input = ParsedInput::new(new_input, self.ide.parser())?;
        let old_expr = self.data.borrow().input.expression.repr();
        let new_expr = parsed_input.expression.repr();

        self.data.borrow_mut().input = parsed_input;
        self.invalidate_fragments_added_by_picking();
        let expression_changed = old_expr != new_expr;
        if expression_changed {
            debug!("Reloading list.");
            self.reload_list();
        } else {
            let data = self.data.borrow();
            data.components.update_filtering(&data.input.pattern);
            if let Actions::Loaded { list } = &data.actions {
                debug!("Update filtering.");
                list.update_filtering(&data.input.pattern);
                executor::global::spawn(self.notifier.publish(Notification::NewActionList));
            }
        }
        Ok(())
    }

    fn this_var(&self) -> Option<&str> {
        self.this_arg.deref().as_ref().map(|this| this.var.as_ref())
    }

    /// Code that will be inserted by expanding given suggestion at given location.
    ///
    /// Code depends on the location, as the first fragment can introduce `self` variable access,
    /// and then we don't want to put any module name.
    fn code_to_insert<'a>(&self, fragment: &'a FragmentAddedByPickingSuggestion) -> Cow<'a, str> {
        fragment.code_to_insert(&self.this_arg)
    }

    /// Pick a completion suggestion.
    ///
    /// This function should be called when user do the _use as suggestion_ action as a code
    /// suggestion (see struct documentation). The picked suggestion will be remembered, and the
    /// searcher's input will be updated and returned by this function.
    #[profile(Debug)]
    pub fn use_suggestion(&self, picked_suggestion: action::Suggestion) -> FallibleResult<String> {
        info!("Picking suggestion: {picked_suggestion:?}.");
        let id = self.data.borrow().input.next_completion_id();
        let picked_completion = FragmentAddedByPickingSuggestion { id, picked_suggestion };
        let code_to_insert = self.code_to_insert(&picked_completion);
        debug!("Code to insert: \"{code_to_insert}\"");
        let added_ast = self.ide.parser().parse_line_ast(code_to_insert)?;
        let pattern_offset = self.data.borrow().input.pattern_offset;
        let new_expression_chain = self.create_new_expression_chain(added_ast, pattern_offset);
        let new_parsed_input = ParsedInput {
            expression:     Some(new_expression_chain),
            pattern_offset: 1,
            pattern:        "".to_string(),
        };
        let new_input = new_parsed_input.repr();
        self.data.borrow_mut().input = new_parsed_input;
        self.data.borrow_mut().fragments_added_by_picking.push(picked_completion);
        self.reload_list();
        self.breadcrumbs.set_content(iter::empty());
        Ok(new_input)
    }

    fn create_new_expression_chain(
        &self,
        added_ast: Ast,
        pattern_offset: usize,
    ) -> ast::Shifted<ast::prefix::Chain> {
        match self.data.borrow().input.expression.clone() {
            None => {
                let ast = ast::prefix::Chain::from_ast_non_strict(&added_ast);
                ast::Shifted::new(pattern_offset, ast)
            }
            Some(mut expression) => {
                let new_argument = ast::prefix::Argument {
                    sast:      ast::Shifted::new(
                        pattern_offset.max(MINIMUM_PATTERN_OFFSET),
                        added_ast,
                    ),
                    prefix_id: default(),
                };
                expression.args.push(new_argument);
                expression
            }
        }
    }

    /// Use action at given index as a suggestion. The exact outcome depends on the action's type.
    pub fn use_as_suggestion(&self, index: usize) -> FallibleResult<String> {
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
        debug!("Previewing suggestion: \"{picked_suggestion:?}\".");
        self.clear_temporary_imports();

        let id = self.data.borrow().input.next_completion_id();
        let picked_completion = FragmentAddedByPickingSuggestion { id, picked_suggestion };
        let code_to_insert = self.code_to_insert(&picked_completion);
        debug!("Code to insert: \"{code_to_insert}\".",);
        let added_ast = self.ide.parser().parse_line_ast(code_to_insert)?;
        let pattern_offset = self.data.borrow().input.pattern_offset;
        {
            // This block serves to limit the borrow of `self.data`.
            let current_fragments = &self.data.borrow().fragments_added_by_picking;
            let fragments_added_by_picking =
                current_fragments.iter().chain(iter::once(&picked_completion));
            self.add_required_imports(fragments_added_by_picking, false)?;
        }
        let new_expression_chain = self.create_new_expression_chain(added_ast, pattern_offset);
        let expression = self.get_expression(Some(new_expression_chain));
        let intended_method = self.intended_method();

        self.graph.graph().module.with_node_metadata(
            self.mode.node_id(),
            Box::new(|md| md.intended_method = intended_method),
        )?;
        debug!("Previewing expression: \"{:?}\".", expression);
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
        //TODO[MM] the actual functionality here will be implemented as part of task #182634050.
        let error = || NoSuchAction { index };
        let action = {
            let data = self.data.borrow();
            let list = data.actions.list().ok_or_else(error)?;
            list.get_cloned(index).ok_or_else(error)?.action
        };
        debug!("Previewing action: {action:?}");
        Ok(())
    }

    /// Check if the first fragment in the input (i.e. the one representing the called function)
    /// is still unmodified.
    ///
    /// False if it was modified after picking or if it wasn't picked at all.
    pub fn is_function_fragment_unmodified(&self) -> bool {
        with(self.data.borrow(), |data| {
            data.fragments_added_by_picking.first().contains_if(|frag| {
                let is_function = frag.id == CompletedFragmentId::Function;
                is_function && frag.is_still_unmodified(&data.input)
            })
        })
    }

    /// Commit the current input as a new node expression.
    ///
    /// If the searcher was brought by editing existing node, the input is set as a new node
    /// expression, otherwise a new node is added. This will also add all imports required by
    /// picked suggestions.
    #[profile(Debug)]
    pub fn commit_node(&self) -> FallibleResult<ast::Id> {
        let _transaction_guard = self.graph.get_or_open_transaction("Commit node");
        if let Some(guard) = self.node_edit_guard.deref().as_ref() {
            guard.prevent_revert()
        }
        self.clear_temporary_imports();
        // We add the required imports before we edit its content. This way, we avoid an
        // intermediate state where imports would already be in use but not yet available.
        let data_borrowed = self.data.borrow();
        let fragments = data_borrowed.fragments_added_by_picking.iter();
        self.add_required_imports(fragments, true)?;

        let node_id = self.mode.node_id();
        let input_chain = self.data.borrow().input.as_prefix_chain(self.ide.parser());
        let expression = self.get_expression(input_chain);
        let intended_method = self.intended_method();
        self.graph.graph().set_expression(node_id, expression)?;
        if let Mode::NewNode { .. } = self.mode.as_ref() {
            self.graph.graph().introduce_name_on(node_id)?;
        }
        self.graph
            .graph()
            .module
            .with_node_metadata(node_id, Box::new(|md| md.intended_method = intended_method))?;
        let graph = self.graph.graph();
        if let Some(this) = self.this_arg.deref().as_ref() {
            this.introduce_pattern(graph.clone_ref())?;
        }
        Ok(node_id)
    }

    fn get_expression(&self, input_chain: Option<ast::Shifted<ast::prefix::Chain>>) -> String {
        let expression = match (self.this_var(), input_chain) {
            (Some(this_var), Some(input)) =>
                apply_this_argument(this_var, &input.wrapped.into_ast()).repr(),
            (None, Some(input)) => input.wrapped.into_ast().repr(),
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
    fn invalidate_fragments_added_by_picking(&self) {
        let mut data = self.data.borrow_mut();
        let data = data.deref_mut();
        let input = &data.input;
        data.fragments_added_by_picking.drain_filter(|frag| !frag.is_still_unmodified(input));
    }

    #[profile(Debug)]
    fn add_required_imports<'a>(
        &self,
        fragments: impl Iterator<Item = &'a FragmentAddedByPickingSuggestion>,
        permanent: bool,
    ) -> FallibleResult {
        let imports = fragments.flat_map(|frag| {
            frag.required_imports(&self.database, self.module_qualified_name().as_ref())
        });
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
    fn reload_list(&self) {
        let this_type = self.this_arg_type_for_next_completion();
        let return_types = match self.data.borrow().input.next_completion_id() {
            CompletedFragmentId::Function => vec![],
            CompletedFragmentId::Argument { index } =>
                self.return_types_for_argument_completion(index),
        };
        self.gather_actions_from_engine(this_type, return_types, None);
        self.data.borrow_mut().actions = Actions::Loading;
        executor::global::spawn(self.notifier.publish(Notification::NewActionList));
    }

    /// Get the typename of "this" value for current completion context. Returns `Future`, as the
    /// type information might not have came yet from the Language Server.
    #[profile(Debug)]
    fn this_arg_type_for_next_completion(&self) -> impl Future<Output = Option<String>> {
        let next_id = self.data.borrow().input.next_completion_id();
        let graph = self.graph.clone_ref();
        let this = self.this_arg.clone_ref();
        async move {
            let is_function_fragment = next_id == CompletedFragmentId::Function;
            if !is_function_fragment {
                return None;
            }
            let ThisNode { id, .. } = this.deref().as_ref()?;
            let opt_type = graph.expression_type(*id).await.map(Into::into);
            opt_type.map_none(move || error!("Failed to obtain type for this node."))
        }
    }

    /// Get the type that suggestions for the next completion should return.
    ///
    /// Generally this corresponds to the type of the currently filled function argument. Returns
    /// empty list if no type could be determined.
    fn return_types_for_argument_completion(&self, arg_index: usize) -> Vec<String> {
        let suggestions = if let Some(intended) = self.intended_function_suggestion() {
            std::iter::once(intended).collect()
        } else {
            self.possible_function_calls()
        };
        suggestions
            .into_iter()
            .filter_map(|suggestion| {
                let arg_index = arg_index + if self.this_arg.is_some() { 1 } else { 0 };
                let arg_index = arg_index + if self.has_this_argument() { 1 } else { 0 };
                let parameter = suggestion.argument_types().into_iter().nth(arg_index)?;
                Some(parameter)
            })
            .collect()
    }

    fn gather_actions_from_engine(
        &self,
        this_type: impl Future<Output = Option<String>> + 'static,
        return_types: impl IntoIterator<Item = String>,
        tags: Option<Vec<language_server::SuggestionEntryType>>,
    ) {
        let ls = self.language_server.clone_ref();
        let graph = self.graph.graph();
        let position = self.my_utf16_location().span.into();
        let this = self.clone_ref();
        let return_types = return_types.into_iter().collect_vec();
        let return_types_for_engine = if return_types.is_empty() {
            vec![None]
        } else {
            return_types.iter().cloned().map(Some).collect()
        };
        executor::global::spawn(async move {
            let this_type = this_type.await;
            info!("Requesting new suggestion list. Type of `self` is {this_type:?}.");
            let requests = return_types_for_engine.into_iter().map(|return_type| {
                info!("Requesting suggestions for returnType {return_type:?}.");
                let file = graph.module.path().file_path();
                ls.completion(file, &position, &this_type, &return_type, &tags)
            });
            let responses: Result<Vec<language_server::response::Completion>, _> =
                futures::future::join_all(requests).await.into_iter().collect();
            match responses {
                Ok(responses) => {
                    info!("Received suggestions from Language Server.");
                    let list = this.make_action_list(responses.iter());
                    let mut data = this.data.borrow_mut();
                    data.actions = Actions::Loaded { list: Rc::new(list) };
                    let completions = responses.iter().flat_map(|r| r.results.iter().cloned());
                    data.components =
                        this.make_component_list(completions, &this_type, &return_types);
                }
                Err(err) => {
                    let msg = "Request for completions to the Language Server returned error";
                    error!("{msg}: {err}");
                    let mut data = this.data.borrow_mut();
                    data.actions = Actions::Error(Rc::new(err.into()));
                    data.components =
                        this.make_component_list(this.database.keys(), &this_type, &return_types);
                }
            }
            this.notifier.publish(Notification::NewActionList).await;
        });
    }

    /// Process multiple completion responses from the engine into a single list of suggestion.
    #[profile(Debug)]
    fn make_action_list<'a>(
        &self,
        completion_responses: impl IntoIterator<Item = &'a language_server::response::Completion>,
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
        for response in completion_responses {
            let entries = response.results.iter().filter_map(|id| {
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
        }

        actions.build()
    }

    #[profile(Debug)]
    fn make_component_list<'a>(
        &self,
        entry_ids: impl IntoIterator<Item = suggestion_database::entry::Id>,
        this_type: &Option<String>,
        return_types: &[String],
    ) -> component::List {
        let favorites = self.graph.component_groups();
        let module_name = self.module_qualified_name();
        let mut builder = component_list_builder_with_favorites(
            &self.database,
            module_name.as_ref(),
            &*favorites,
        );
        add_virtual_entries_to_builder(&mut builder, this_type, return_types);
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

    fn possible_function_calls(&self) -> Vec<action::Suggestion> {
        let opt_result = || {
            let call_ast = self.data.borrow().input.expression.as_ref()?.func.clone_ref();
            let call = SimpleFunctionCall::try_new(&call_ast)?;
            if let Some(module) = self.module_whose_method_is_called(&call) {
                let name = call.function_name;
                let entry = self.database.lookup_module_method(name, &module);
                Some(entry.into_iter().map(action::Suggestion::FromDatabase).collect())
            } else {
                let name = &call.function_name;
                let location = self.my_utf16_location();
                let entries = self.database.lookup_at(name, &location);
                Some(entries.into_iter().map(action::Suggestion::FromDatabase).collect())
            }
        };
        opt_result().unwrap_or_default()
    }

    /// For the simple function call checks if the function is called on the module (if it can be
    /// easily determined) and returns the module's qualified name if it is.
    fn module_whose_method_is_called(&self, call: &SimpleFunctionCall) -> Option<QualifiedName> {
        let location = self.my_utf16_location();
        let this_name = ast::identifier::name(call.this_argument.as_ref()?)?;
        let matching_locals = self.database.lookup_locals_at(this_name, &location);
        let module_name = location.module;
        let not_local_name = matching_locals.is_empty();
        not_local_name.and_option_from(|| {
            if this_name == module_name.name().deref() {
                Some(module_name)
            } else {
                self.module().iter_imports().find_map(|import| {
                    import
                        .qualified_module_name()
                        .ok()
                        .filter(|module| module.name().deref() == this_name)
                })
            }
        })
    }

    /// Get the suggestion that was selected by the user into the function.
    ///
    /// This suggestion shall be used to request better suggestions from the engine.
    pub fn intended_function_suggestion(&self) -> Option<action::Suggestion> {
        let id = CompletedFragmentId::Function;
        let fragment = self.data.borrow().find_picked_fragment(id).cloned();
        fragment.map(|f| f.picked_suggestion.clone_ref())
    }

    /// Returns the Id of method user intends to be called in this node.
    ///
    /// The method may be picked by user from suggestion, but there are many methods with the same
    /// name.
    fn intended_method(&self) -> Option<MethodId> {
        self.intended_function_suggestion()?.method_id()
    }

    /// If the function fragment has been filled and also contains the initial "this" argument.
    ///
    /// While we maintain chain consisting of target and applied arguments, the target itself might
    /// need to count for a one argument, as it may of form `this.method`.
    fn has_this_argument(&self) -> bool {
        self.data
            .borrow()
            .input
            .expression
            .as_ref()
            .and_then(|expr| ast::opr::as_access_chain(&expr.func))
            .is_some()
    }

    fn module(&self) -> double_representation::module::Info {
        double_representation::module::Info { ast: self.graph.graph().module.ast() }
    }

    fn module_qualified_name(&self) -> QualifiedName {
        self.graph.module_qualified_name(&*self.project)
    }

    /// Get the user action basing of current input (see `UserAction` docs).
    pub fn current_user_action(&self) -> UserAction {
        self.data.borrow().input.user_action()
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
                name:               (*method).to_owned(),
                kind:               model::suggestion_database::entry::Kind::Method,
                defined_in:         module.clone(),
                arguments:          vec![],
                return_type:        "Standard.Base.System.File.File".try_into().unwrap(),
                documentation:      vec![],
                documentation_html: None,
                self_type:          Some(self_type.clone()),
                is_static:          true,
                scope:              model::suggestion_database::entry::Scope::Everywhere,
                icon_name:          None,
                reexported_in:      None,
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
) -> component::builder::List {
    let mut builder = component::builder::List::new();
    if let Some((id, _)) = suggestion_db.lookup_by_qualified_name(local_scope_module) {
        builder = builder.with_local_scope_module_id(id);
    }
    builder.set_grouping_and_order_of_favorites(suggestion_db, groups);
    builder
}

fn add_virtual_entries_to_builder(
    builder: &mut component::builder::List,
    this_type: &Option<String>,
    return_types: &[String],
) {
    if this_type.is_none() {
        let snippets = if return_types.is_empty() {
            component::hardcoded::INPUT_SNIPPETS.with(|s| s.clone())
        } else {
            let parse_type_qn = |s| QualifiedName::from_text(s).ok();
            let rt_qns = return_types.iter().filter_map(parse_type_qn);
            component::hardcoded::input_snippets_with_matching_return_type(rt_qns)
        };
        let group_name = component::hardcoded::INPUT_GROUP_NAME;
        let project = project::QualifiedName::standard_base_library();
        builder.insert_virtual_components_in_favorites_group(group_name, project, snippets);
    }
}


// === Node Edit Metadata Guard ===

/// On creation the `EditGuard` saves the current expression of the node to its metadata.
/// When dropped the metadata is cleared again and, by default, the node content is reverted to the
/// previous expression. The expression reversion can be prevented by calling `prevent_revert`.
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
                        let previous_intended_method = metadata.intended_method.clone();
                        metadata.edit_status = Some(NodeEditStatus::Edited {
                            previous_expression,
                            previous_intended_method,
                        });
                    }),
                )
            }
        }
    }

    /// Mark the node as no longer edited and discard the edit metadata.
    fn clear_node_edit_metadata(&self) -> FallibleResult {
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
        let mut edit_status = None;
        module.with_node_metadata(
            self.node_id,
            Box::new(|metadata| {
                edit_status = metadata.edit_status.clone();
            }),
        )?;
        Ok(edit_status)
    }

    fn revert_node_expression_edit(&self) -> FallibleResult {
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
            Some(NodeEditStatus::Edited { previous_expression, previous_intended_method }) => {
                debug!(
                    "Reverting expression of node {} to {} after aborting edit.",
                    self.node_id, &previous_expression
                );
                let graph = self.graph.graph();
                graph.set_expression(self.node_id, previous_expression)?;
                let module = &self.graph.graph().module;
                module.with_node_metadata(
                    self.node_id,
                    Box::new(|metadata| {
                        metadata.intended_method = previous_intended_method;
                    }),
                )?;
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



// === SimpleFunctionCall ===

/// A simple function call is an AST where function is a single identifier with optional
/// argument applied by `ACCESS` operator (dot).
struct SimpleFunctionCall {
    this_argument: Option<Ast>,
    function_name: String,
}

impl SimpleFunctionCall {
    fn try_new(call: &Ast) -> Option<Self> {
        if let Some(name) = ast::identifier::name(call) {
            Some(Self { this_argument: None, function_name: name.to_owned() })
        } else {
            let infix = ast::opr::to_access(call)?;
            let name = ast::identifier::name(&infix.rarg)?;
            Some(Self {
                this_argument: Some(infix.larg.clone_ref()),
                function_name: name.to_owned(),
            })
        }
    }
}

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



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod test {
    use super::*;

    use crate::controller::ide::plain::ProjectOperationsNotSupported;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::mock_suggestion_database;
    use crate::model::suggestion_database::entry::Argument;
    use crate::model::SuggestionDatabase;
    use crate::test::mock::data::project_qualified_name;
    use crate::test::mock::data::MAIN_FINISH;
    use crate::test::mock::data::MODULE_NAME;

    use engine_protocol::language_server::types::test::value_update_with_type;
    use engine_protocol::language_server::SuggestionId;
    use json_rpc::expect_call;
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
        client.expect.completion(|_, _, _, _, _| Ok(response))
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
            return_type: Option<&str>,
            result: &[SuggestionId],
        ) {
            let completion_response = completion_response(result);
            expect_call!(client.completion(
                module      = self.graph.module.path.file_path().clone(),
                position    = self.code_location,
                self_type   = self_type.map(Into::into),
                return_type = return_type.map(Into::into),
                tag         = None
            ) => Ok(completion_response));
        }
    }

    struct Fixture {
        #[allow(dead_code)]
        data:     MockData,
        test:     TestWithLocalPoolExecutor,
        searcher: Searcher,
        entry1:   Rc<model::suggestion_database::Entry>,
        entry2:   Rc<model::suggestion_database::Entry>,
        entry3:   Rc<model::suggestion_database::Entry>,
        entry4:   Rc<model::suggestion_database::Entry>,
        entry9:   Rc<model::suggestion_database::Entry>,
    }

    impl Fixture {
        fn new_custom<F>(client_setup: F) -> Self
        where F: FnOnce(&mut MockData, &mut language_server::MockClient) {
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
            let logger = Logger::new("Searcher"); // new_empty
            let module_name = crate::test::mock::data::module_qualified_name();
            let database = suggestion_database_with_mock_entries(code_range);
            let mut ide = controller::ide::MockAPI::new();
            let mut project = model::project::MockAPI::new();
            let project_qname = project_qualified_name();
            let project_name = project_qname.project.clone();
            project.expect_qualified_name().returning_st(move || project_qname.clone());
            project.expect_name().returning_st(move || project_name.clone());
            let project = Rc::new(project);
            ide.expect_parser().return_const(Parser::new_or_panic());
            let current_project = project.clone_ref();
            ide.expect_current_project().returning_st(move || Some(current_project.clone_ref()));
            ide.expect_manage_projects()
                .returning_st(move || Err(ProjectOperationsNotSupported.into()));
            let node_metadata_guard = default();
            let breadcrumbs = Breadcrumbs::new();
            let searcher = Searcher {
                graph,
                logger,
                database,
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
            let (_, entry1) = searcher
                .database
                .lookup_by_qualified_name(&module_name.clone().new_child("testFunction1"))
                .unwrap();
            let (_, entry2) = searcher
                .database
                .lookup_by_qualified_name(&module_name.clone().new_child("test_var_1"))
                .unwrap();
            let (_, entry3) = searcher
                .database
                .lookup_by_qualified_name(&module_name.clone().new_child("test_method"))
                .unwrap();
            let entry4 = searcher
                .database
                .lookup_by_qualified_name_str("test.Test.Test.test_method")
                .unwrap();
            let (_, entry9) = searcher
                .database
                .lookup_by_qualified_name(&module_name.new_child("testFunction2"))
                .unwrap();
            Fixture { data, test, searcher, entry1, entry2, entry3, entry4, entry9 }
        }

        fn new() -> Self {
            Self::new_custom(|_, _| {})
        }
    }

    fn suggestion_database_with_mock_entries(
        scope: RangeInclusive<Location<enso_text::Utf16CodeUnit>>,
    ) -> Rc<SuggestionDatabase> {
        let database = mock_suggestion_database! {
            test.Test {
                mod Test {
                    static fn test_method(this: Standard.Base.Any, arg: Standard.Base.Text) -> Standard.Base.Text;
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
    #[wasm_bindgen_test]
    fn loading_list_w_self() {
        let mock_type = crate::test::mock::data::TYPE_NAME;

        /// The case is: `main` contains a single, selected node. Searcher is brought up.
        /// Mock `entry1` suggestion is picked twice.
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
            let Fixture { mut test, searcher, entry1, entry9, .. } =
                Fixture::new_custom(|data, client| {
                    data.change_main_body(&[case.node_line]);
                    data.selected_node = true;
                    // We expect following calls:
                    // 1) for the function - with the "this" filled (if the test case says so);
                    // 2) for subsequent completions - without "self"
                    data.expect_completion(client, case.sets_this.as_some(mock_type), None, &[
                        1, 5, 9,
                    ]);
                    // If we are about to add the self type, then we expect the second argument
                    // first, and then none. Otherwise we expect all arguments
                    // starting from the first.
                    let expected_types = if case.sets_this {
                        [Some("Standard.Base.Number"), None]
                    } else {
                        [Some("Standard.Base.Text"), Some("Standard.Base.Number")]
                    };

                    data.expect_completion(client, None, expected_types[0], &[1, 5, 9]);
                    data.expect_completion(client, None, expected_types[1], &[1, 5, 9]);
                });

            searcher.reload_list();

            // The suggestion list should stall only if we actually use "this" argument.
            if case.sets_this {
                assert!(searcher.actions().is_loading());
                test.run_until_stalled();
                // Nothing appeared, because we wait for type information for this node.
                assert!(searcher.actions().is_loading());

                let this_node_id = searcher.this_arg.deref().as_ref().unwrap().id;
                let update = value_update_with_type(this_node_id, mock_type);
                searcher.graph.computed_value_info_registry().apply_updates(vec![update]);
                assert!(searcher.actions().is_loading());
            }

            test.run_until_stalled();
            assert!(!searcher.actions().is_loading());
            searcher.use_suggestion(action::Suggestion::FromDatabase(entry9.clone_ref())).unwrap();
            searcher.use_suggestion(action::Suggestion::FromDatabase(entry1.clone_ref())).unwrap();
            let expected_input = format!("{} {} ", entry9.name, entry1.name);
            assert_eq!(searcher.data.borrow().input.repr(), expected_input);
        }
    }

    #[wasm_bindgen_test]
    fn arguments_suggestions_for_picked_method() {
        let mut fixture = Fixture::new_custom(|data, client| {
            data.expect_completion(client, None, Some("Standard.Base.Number"), &[20]);
        });
        let Fixture { test, searcher, entry3, .. } = &mut fixture;
        searcher.use_suggestion(action::Suggestion::FromDatabase(entry3.clone_ref())).unwrap();
        assert!(searcher.actions().is_loading());
        test.run_until_stalled();
        assert!(!searcher.actions().is_loading());
    }

    #[wasm_bindgen_test]
    fn arguments_suggestions_for_picked_function() {
        let mut fixture = Fixture::new_custom(|data, client| {
            data.expect_completion(client, None, Some("Standard.Base.Text"), &[]); // First arg suggestion.
            data.expect_completion(client, None, Some("Standard.Base.Number"), &[]); // Second arg suggestion.
            data.expect_completion(client, None, None, &[]); // Oversaturated arg position.
        });


        let Fixture { searcher, entry9, .. } = &mut fixture;
        searcher.use_suggestion(action::Suggestion::FromDatabase(entry9.clone_ref())).unwrap();
        assert_eq!(searcher.data.borrow().input.repr(), "testFunction2 ");
        searcher.set_input("testFunction2 'foo' ".to_owned()).unwrap();
        searcher.set_input("testFunction2 'foo' 10 ".to_owned()).unwrap();
    }

    #[wasm_bindgen_test]
    fn non_picked_function_arg_suggestions() {
        let mut fixture = Fixture::new_custom(|data, client| {
            data.graph.module.code.insert_str(0, "import test.Test.Test\n\n");
            data.code_location.line += 2;
            data.expect_completion(client, None, Some("Standard.Base.Text"), &[1]);
            data.expect_completion(client, None, Some("Standard.Base.Number"), &[]);
            data.expect_completion(client, None, Some("Standard.Base.Number"), &[]);
            data.expect_completion(client, None, None, &[1, 2, 3, 4, 9]);
        });
        let Fixture { searcher, .. } = &mut fixture;

        // Known functions cases
        searcher.set_input("Test.test_method ".to_string()).unwrap();
        searcher.set_input(iformat!("{MODULE_NAME}.test_method ")).unwrap();
        searcher.set_input("testFunction2 \"str\" ".to_string()).unwrap();

        // Unknown functions case
        searcher.set_input("unknownFunction ".to_string()).unwrap();
    }

    #[wasm_bindgen_test]
    fn non_picked_function_arg_suggestion_ambiguous() {
        fn run_case(input: impl Str, setup: impl FnOnce(&mut Fixture)) {
            // In each case we expect that we can pick two methods with the same name, but different
            // second argument, so the controller should call Engine for each type.
            const EXPECTED_REQUESTS: usize = 2;
            let requested_types: Rc<RefCell<HashSet<Option<String>>>> = default();
            let requested_types2 = requested_types.clone();
            let mut fixture = Fixture::new_custom(move |data, client| {
                data.graph.module.code.insert_str(0, "import test.Test.Test\n\n");
                data.code_location.line += 2;
                for _ in 0..EXPECTED_REQUESTS {
                    let requested_types = requested_types2.clone();
                    client.expect.completion(
                        move |_path, _position, _self_type, return_type, _tags| {
                            requested_types.borrow_mut().insert(return_type.clone());
                            Ok(completion_response(&[]))
                        },
                    );
                }
            });
            setup(&mut fixture);
            let Fixture { test, searcher, .. } = &mut fixture;
            searcher.set_input(input.into()).unwrap();
            test.run_until_stalled();
            assert_eq!(requested_types.borrow().len(), EXPECTED_REQUESTS);
            assert!(requested_types.borrow().contains(&Some("Standard.Base.Number".to_string())));
            assert!(requested_types.borrow().contains(&Some("Standard.Base.Text".to_string())));
        }

        run_case("test_method (foo bar) ".to_string(), |_| {});
        run_case("(foo bar).test_method ".to_string(), |_| {});
        // Here the "Test" module is shadowed by local, so the call is ambiguous
        run_case("Test.test_method ".to_string(), |fixture| {
            let shadowing = model::suggestion_database::Entry {
                name: "test".to_string(),
                ..(*fixture.entry2).clone()
            };
            fixture.searcher.database.put_entry(133, shadowing)
        });
    }

    #[wasm_bindgen_test]
    fn loading_list() {
        let Fixture { mut test, searcher, entry1, entry9, .. } =
            Fixture::new_custom(|data, client| {
                // entry with id 99999 does not exist, so only two actions from suggestions db
                // should be displayed in searcher.
                data.expect_completion(client, None, None, &[101, 99999, 103]);
            });

        let mut subscriber = searcher.subscribe();
        searcher.reload_list();
        assert!(searcher.actions().is_loading());
        test.run_until_stalled();
        let list = searcher.actions().list().unwrap().to_action_vec();
        // There are 8 entries, because: 2 were returned from `completion` method, two are mocked,
        // and all of these are repeated in "All Search Result" category.
        assert_eq!(list.len(), 8);
        assert_eq!(list[2], Action::Suggestion(action::Suggestion::FromDatabase(entry1)));
        assert_eq!(list[3], Action::Suggestion(action::Suggestion::FromDatabase(entry9)));
        let notification = subscriber.next().boxed_local().expect_ready();
        assert_eq!(notification, Some(Notification::NewActionList));
    }

    #[wasm_bindgen_test]
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
        let Fixture { mut test, searcher, entry3, entry9, .. } =
            Fixture::new_custom(|data, client| {
                // Entry with id 99999 does not exist, so only two actions from suggestions db
                // should be displayed in searcher.
                data.expect_completion(client, None, None, &[5, 99999, 103]);
                data.graph.ctx.component_groups = vec![sample_ls_component_group];
            });
        // Reload the components list in the Searcher.
        searcher.reload_list();
        test.run_until_stalled();
        // Verify the contents of the components list loaded by the Searcher.
        let components = searcher.components();
        if let [module_group] = &components.top_modules()[..] {
            let expected_group_name =
                format!("{}.{}", entry3.defined_in.project().project, entry3.defined_in.name());
            assert_eq!(module_group.name, expected_group_name);
            let entries = module_group.entries.borrow();
            assert_matches!(entries.as_slice(), [e1, e2] if e1.name() == entry3.name && e2.name()
    == entry9.name);
        } else {
            ipanic!("Wrong top modules in Component List: {components.top_modules():?}");
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

    #[wasm_bindgen_test]
    fn parsed_input() {
        let parser = Parser::new_or_panic();

        fn args_reprs(prefix: &ast::prefix::Chain) -> Vec<String> {
            prefix.args.iter().map(|arg| arg.repr()).collect()
        }

        let input = "";
        let parsed = ParsedInput::new(input.to_string(), &parser).unwrap();
        assert!(parsed.expression.is_none());
        assert_eq!(parsed.pattern.as_str(), "");

        let input = "foo";
        let parsed = ParsedInput::new(input.to_string(), &parser).unwrap();
        assert!(parsed.expression.is_none());
        assert_eq!(parsed.pattern.as_str(), "foo");

        let input = " foo";
        let parsed = ParsedInput::new(input.to_string(), &parser).unwrap();
        assert!(parsed.expression.is_none());
        assert_eq!(parsed.pattern_offset, 1);
        assert_eq!(parsed.pattern.as_str(), "foo");

        let input = "foo  ";
        let parsed = ParsedInput::new(input.to_string(), &parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off, 0);
        assert_eq!(expression.func.repr(), "foo");
        assert_eq!(args_reprs(&expression), Vec::<String>::new());
        assert_eq!(parsed.pattern_offset, 2);
        assert_eq!(parsed.pattern.as_str(), "");

        let input = "foo bar";
        let parsed = ParsedInput::new(input.to_string(), &parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off, 0);
        assert_eq!(expression.func.repr(), "foo");
        assert_eq!(args_reprs(&expression), Vec::<String>::new());
        assert_eq!(parsed.pattern_offset, 1);
        assert_eq!(parsed.pattern.as_str(), "bar");

        let input = "foo  bar  baz";
        let parsed = ParsedInput::new(input.to_string(), &parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off, 0);
        assert_eq!(expression.func.repr(), "foo");
        assert_eq!(args_reprs(&expression), vec!["  bar".to_string()]);
        assert_eq!(parsed.pattern_offset, 2);
        assert_eq!(parsed.pattern.as_str(), "baz");

        let input = "  foo bar baz ";
        let parsed = ParsedInput::new(input.to_string(), &parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off, 2);
        assert_eq!(expression.func.repr(), "foo");
        assert_eq!(args_reprs(&expression), vec![" bar".to_string(), " baz".to_string()]);
        assert_eq!(parsed.pattern_offset, 1);
        assert_eq!(parsed.pattern.as_str(), "");

        let input = "foo bar (baz ";
        let parsed = ParsedInput::new(input.to_string(), &parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off, 0);
        assert_eq!(expression.func.repr(), "foo");
        assert_eq!(args_reprs(&expression), vec![" bar".to_string()]);
        assert_eq!(parsed.pattern_offset, 1);
        assert_eq!(parsed.pattern.as_str(), "(baz ");
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

    #[wasm_bindgen_test]
    fn picked_completions_list_maintaining() {
        let Fixture { test: _test, searcher, entry1, entry2, .. } =
            Fixture::new_custom(|data, client| {
                data.expect_completion(client, None, None, &[]);
                data.expect_completion(client, None, None, &[]);
                data.expect_completion(client, None, None, &[]);
                data.expect_completion(client, None, None, &[]);
                data.expect_completion(client, None, None, &[]);
                data.expect_completion(client, None, None, &[]);
            });
        let frags_borrow = || Ref::map(searcher.data.borrow(), |d| &d.fragments_added_by_picking);

        // Picking first suggestion.
        let new_input =
            searcher.use_suggestion(action::Suggestion::FromDatabase(entry1.clone_ref())).unwrap();
        assert_eq!(new_input, "testFunction1 ");
        let (func,) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(func.id, CompletedFragmentId::Function);
        assert!(are_same(&func.picked_suggestion, &entry1));

        // Typing more args by hand.
        searcher.set_input("testFunction1 some_arg pat".to_string()).unwrap();
        let (func,) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(func.id, CompletedFragmentId::Function);
        assert!(are_same(&func.picked_suggestion, &entry1));

        // Picking argument's suggestion.
        let new_input =
            searcher.use_suggestion(action::Suggestion::FromDatabase(entry2.clone_ref())).unwrap();
        assert_eq!(new_input, "testFunction1 some_arg test_var_1 ");
        let new_input =
            searcher.use_suggestion(action::Suggestion::FromDatabase(entry2.clone_ref())).unwrap();
        assert_eq!(new_input, "testFunction1 some_arg test_var_1 test_var_1 ");
        let (function, arg1, arg2) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(function.id, CompletedFragmentId::Function);
        assert!(are_same(&function.picked_suggestion, &entry1));
        assert_eq!(arg1.id, CompletedFragmentId::Argument { index: 1 });
        assert!(are_same(&arg1.picked_suggestion, &entry2));
        assert_eq!(arg2.id, CompletedFragmentId::Argument { index: 2 });
        assert!(are_same(&arg2.picked_suggestion, &entry2));

        // Backspacing back to the second arg.
        searcher.set_input("testFunction1 some_arg test_var_1 test_v".to_string()).unwrap();
        let (picked, arg) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(picked.id, CompletedFragmentId::Function);
        assert!(are_same(&picked.picked_suggestion, &entry1));
        assert_eq!(arg.id, CompletedFragmentId::Argument { index: 1 });
        assert!(are_same(&arg.picked_suggestion, &entry2));

        // Editing the picked function.
        searcher.set_input("testFunction2 some_arg test_var_1 test_v".to_string()).unwrap();
        let (arg,) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(arg.id, CompletedFragmentId::Argument { index: 1 });
        assert!(are_same(&arg.picked_suggestion, &entry2));
    }

    #[wasm_bindgen_test]
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
                let parser = Parser::new_or_panic();
                let ast = parser.parse_line_ast(self.before).unwrap();
                let new_ast = apply_this_argument("foo", &ast);
                assert_eq!(new_ast.repr(), self.after, "Case {:?} failed: {:?}", self, ast);
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

    #[wasm_bindgen_test]
    fn adding_node_introducing_this_var() {
        struct Case {
            line:   &'static str,
            result: String,
            run:    Box<dyn FnOnce(&mut Fixture)>,
        }

        impl Case {
            fn new(
                line: &'static str,
                result: &[&str],
                run: impl FnOnce(&mut Fixture) + 'static,
            ) -> Self {
                Case {
                    line,
                    result: crate::test::mock::main_from_lines(result),
                    run: Box::new(run),
                }
            }
        }

        let cases = vec![
            // Completion was picked.
            Case::new("2 + 2", &["sum1 = 2 + 2", "operator1 = sum1.testFunction1"], |f| {
                f.searcher
                    .use_suggestion(action::Suggestion::FromDatabase(f.entry1.clone()))
                    .unwrap();
            }),
            // The input was manually written (not picked).
            Case::new("2 + 2", &["sum1 = 2 + 2", "operator1 = sum1.testFunction1"], |f| {
                f.searcher.set_input("testFunction1 ".to_owned()).unwrap();
            }),
            // Completion was picked and edited.
            Case::new("2 + 2", &["sum1 = 2 + 2", "operator1 = sum1.var.testFunction1"], |f| {
                f.searcher
                    .use_suggestion(action::Suggestion::FromDatabase(f.entry1.clone()))
                    .unwrap();
                let new_parsed_input =
                    ParsedInput::new("var.testFunction1", f.searcher.ide.parser());
                f.searcher.data.borrow_mut().input = new_parsed_input.unwrap();
            }),
            // Variable name already present, need to use it. And not break it.
            Case::new(
                "my_var = 2 + 2",
                &["my_var = 2 + 2", "operator1 = my_var.testFunction1"],
                |f| {
                    f.searcher
                        .use_suggestion(action::Suggestion::FromDatabase(f.entry1.clone()))
                        .unwrap();
                },
            ),
            // Variable names unusable (subpatterns are not yet supported).
            // Don't use "this" argument adjustments at all.
            Case::new("[x,y] = 2 + 2", &["[x,y] = 2 + 2", "testfunction11 = testFunction1"], |f| {
                f.searcher
                    .use_suggestion(action::Suggestion::FromDatabase(f.entry1.clone()))
                    .unwrap();
            }),
        ];

        for case in cases.into_iter() {
            let mut fixture = Fixture::new_custom(|data, client| {
                data.selected_node = true;
                // The last node will be used as searcher target.
                data.change_main_body(&[case.line, "Nothing"]);
                data.expect_completion(client, None, None, &[]);
            });
            (case.run)(&mut fixture);
            fixture.searcher.commit_node().unwrap();
            let updated_def = fixture.searcher.graph.graph().definition().unwrap().item;
            assert_eq!(updated_def.ast.repr(), case.result);
        }
    }

    #[wasm_bindgen_test]
    fn adding_imports_with_nodes() {
        fn expect_inserted_import_for(
            entry: &Rc<model::suggestion_database::Entry>,
            expected_import: Vec<&QualifiedName>,
        ) {
            let Fixture { test: _test, mut searcher, .. } = Fixture::new();
            let module = searcher.graph.graph().module.clone_ref();
            let parser = searcher.ide.parser().clone_ref();

            let picked_method = FragmentAddedByPickingSuggestion {
                id:                CompletedFragmentId::Function,
                picked_suggestion: action::Suggestion::FromDatabase(entry.clone_ref()),
            };
            with(searcher.data.borrow_mut(), |mut data| {
                data.fragments_added_by_picking.push(picked_method);
                data.input = ParsedInput::new(entry.name.to_string(), &parser).unwrap();
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

        let Fixture { entry1, entry2, entry3, entry4, .. } = Fixture::new();
        expect_inserted_import_for(&entry1, vec![]);
        expect_inserted_import_for(&entry2, vec![]);
        expect_inserted_import_for(&entry3, vec![]);
        expect_inserted_import_for(&entry4, vec![&entry4.defined_in]);
    }

    #[wasm_bindgen_test]
    fn committing_node() {
        let Fixture { test: _test, mut searcher, entry4, .. } =
            Fixture::new_custom(|data, _client| {
                data.change_main_body(&["2 + 2", "Nothing"]); // The last node will be used as
                                                              // searcher target.
            });

        let (node1, searcher_target) = searcher.graph.graph().nodes().unwrap().expect_tuple();

        let module = searcher.graph.graph().module.clone_ref();
        // Setup searcher.
        let parser = Parser::new_or_panic();
        let picked_method = FragmentAddedByPickingSuggestion {
            id:                CompletedFragmentId::Function,
            picked_suggestion: action::Suggestion::FromDatabase(entry4),
        };
        with(searcher.data.borrow_mut(), |mut data| {
            data.fragments_added_by_picking.push(picked_method);
            data.input = ParsedInput::new("Test.test_method".to_string(), &parser).unwrap();
        });

        // Add new node.
        searcher.mode =
            Immutable(Mode::NewNode { node_id: searcher_target.id(), source_node: None });
        searcher.commit_node().unwrap();

        let expected_code =
            "import test.Test.Test\nmain =\n    2 + 2\n    operator1 = Test.test_method";
        assert_eq!(module.ast().repr(), expected_code);
        let expected_intended_method = Some(MethodId {
            module:          "test.Test.Test".to_string().try_into().unwrap(),
            defined_on_type: "test.Test.Test".to_string().try_into().unwrap(),
            name:            "test_method".to_string(),
        });
        let (_, searcher_target) = searcher.graph.graph().nodes().unwrap().expect_tuple();
        assert_eq!(searcher_target.metadata.unwrap().intended_method, expected_intended_method);

        // Edit existing node.
        searcher.mode = Immutable(Mode::EditNode { node_id: node1.info.id() });
        searcher.commit_node().unwrap();
        let expected_code =
            "import test.Test.Test\nmain =\n    Test.test_method\n    operator1 = Test.test_method";
        let (node1, _) = searcher.graph.graph().nodes().unwrap().expect_tuple();
        assert_eq!(node1.metadata.unwrap().intended_method, expected_intended_method);
        assert_eq!(module.ast().repr(), expected_code);
    }

    #[wasm_bindgen_test]
    fn initialized_data_when_editing_node() {
        let Fixture { test: _test, searcher, entry4, .. } = Fixture::new();

        let graph = searcher.graph.graph();
        let (node,) = graph.nodes().unwrap().expect_tuple();
        let node_id = node.info.id();
        let database = searcher.database;

        // Node had not intended method.
        let searcher_data = Data::new_with_edited_node(&graph, &database, node_id).unwrap();
        assert_eq!(searcher_data.input.repr(), node.info.expression().repr());
        assert!(searcher_data.fragments_added_by_picking.is_empty());
        assert!(searcher_data.actions.is_loading());

        // Node had intended method, but it's outdated.
        let intended_method = MethodId {
            module:          "test.Test.Test".to_string().try_into().unwrap(),
            defined_on_type: "test.Test.Test".to_string().try_into().unwrap(),
            name:            "test_method".to_string(),
        };
        graph
            .module
            .with_node_metadata(
                node_id,
                Box::new(|md| {
                    md.intended_method = Some(intended_method);
                }),
            )
            .unwrap();
        let searcher_data = Data::new_with_edited_node(&graph, &database, node_id).unwrap();
        assert_eq!(searcher_data.input.repr(), node.info.expression().repr());
        assert!(searcher_data.fragments_added_by_picking.is_empty());
        assert!(searcher_data.actions.is_loading());

        // Node had up-to-date intended method.
        graph.set_expression(node_id, "Test.test_method 12").unwrap();
        // We set metadata in previous section.
        let searcher_data = Data::new_with_edited_node(&graph, &database, node_id).unwrap();
        assert_eq!(searcher_data.input.repr(), "Test.test_method 12");
        assert!(searcher_data.actions.is_loading());
        let (initial_fragment,) = searcher_data.fragments_added_by_picking.expect_tuple();
        assert!(are_same(&initial_fragment.picked_suggestion, &entry4))
    }

    #[wasm_bindgen_test]
    fn simple_function_call_parsing() {
        let parser = Parser::new_or_panic();

        let ast = parser.parse_line_ast("foo").unwrap();
        let call = SimpleFunctionCall::try_new(&ast).expect("Returned None for \"foo\"");
        assert!(call.this_argument.is_none());
        assert_eq!(call.function_name, "foo");

        let ast = parser.parse_line_ast("Main.foo").unwrap();
        let call = SimpleFunctionCall::try_new(&ast).expect("Returned None for \"Main.foo\"");
        assert_eq!(call.this_argument.unwrap().repr(), "Main");
        assert_eq!(call.function_name, "foo");

        let ast = parser.parse_line_ast("(2 + 3).foo").unwrap();
        let call = SimpleFunctionCall::try_new(&ast).expect("Returned None for \"(2 + 3).foo\"");
        assert_eq!(call.this_argument.unwrap().repr(), "(2 + 3)");
        assert_eq!(call.function_name, "foo");

        let ast = parser.parse_line_ast("foo + 3").unwrap();
        assert!(SimpleFunctionCall::try_new(&ast).is_none());

        let ast = parser.parse_line_ast("foo bar baz").unwrap();
        assert!(SimpleFunctionCall::try_new(&ast).is_none());

        let ast = parser.parse_line_ast("Main . (foo bar)").unwrap();
        assert!(SimpleFunctionCall::try_new(&ast).is_none());
    }

    #[wasm_bindgen_test]
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

    #[wasm_bindgen_test]
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

    #[wasm_bindgen_test]
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
                            previous_expression:      node.info.expression().to_string(),
                            previous_intended_method: None,
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

    #[wasm_bindgen_test]
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
}
