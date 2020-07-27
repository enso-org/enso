//! This module contains all structures related to Searcher Controller.

use crate::prelude::*;

use crate::controller::graph::NewNodeInfo;
use crate::double_representation::module::ImportInfo;
use crate::model::module::MethodId;
use crate::model::module::NodeMetadata;
use crate::model::module::Position;
use crate::notification;

use data::text::TextLocation;
use enso_protocol::language_server;
use flo_stream::Subscriber;
use parser::Parser;



// =======================
// === Suggestion List ===
// =======================

/// Suggestion for input completion: possible functions, arguments, etc.
pub type CompletionSuggestion = Rc<model::suggestion_database::Entry>;

/// A single suggestion on the Searcher suggestion list.
#[derive(Clone,CloneRef,Debug,Eq,PartialEq)]
pub enum Suggestion {
    /// Suggestion for input completion: possible functions, arguments, etc.
    Completion(CompletionSuggestion)
    // In future, other suggestion types will be added (like suggestions of actions, etc.).
}

/// List of suggestions available in Searcher.
#[derive(Clone,CloneRef,Debug)]
pub enum Suggestions {
    /// The suggestion list is still loading from the Language Server.
    Loading,
    /// The suggestion list is loaded.
    #[allow(missing_docs)]
    Loaded {
        list : Rc<Vec<Suggestion>>
    },
    /// Loading suggestion list resulted in error.
    Error(Rc<failure::Error>)
}

impl Suggestions {
    /// Check if suggestion list is still loading.
    pub fn is_loading(&self) -> bool {
        match self {
            Self::Loading => true,
            _             => false,
        }
    }

    /// Check if retrieving suggestion list was unsuccessful
    pub fn is_error(&self) -> bool {
        match self {
            Self::Error(_) => true,
            _              => false,
        }
    }

    /// Get the list of suggestions. Returns None if still loading or error was returned.
    pub fn list(&self) -> Option<&Vec<Suggestion>> {
        match self {
            Self::Loaded {list} => Some(list),
            _                   => None,
        }
    }
}

impl Default for Suggestions {
    fn default() -> Self {
        Self::Loading
    }
}



// =====================
// === Notifications ===
// =====================

/// The notification emitted by Searcher Controller
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum Notification {
    /// A new Suggestion list is available.
    NewSuggestionList
}


// ===================
// === Input Parts ===
// ===================

/// An identification of input fragment filled by picking suggestion.
///
/// Essentially, this is a crumb for ParsedInput's expression.
#[derive(Clone,Copy,Debug,Eq,PartialEq)]
#[allow(missing_docs)]
pub enum CompletedFragmentId {
    /// The called "function" part, defined as a `func` element in Prefix Chain
    /// (see `ast::prefix::Chain`).
    Function,
    /// The `id`th argument of the called function.
    Argument{index:usize}
}

/// A Searcher Input which is parsed to the _expression_ and _pattern_ parts.
///
/// We parse the input for better understanding what user wants to add.
#[derive(Clone,Debug,Default)]
pub struct ParsedInput {
    /// The part of input which is treated as completed function and some set of arguments.
    ///
    /// The expression is kept as prefix chain, as it allows us to easily determine what kind of
    /// entity we can put at this moment (is it a function or argument? What type of the argument?).
    pub expression : Option<ast::Shifted<ast::prefix::Chain>>,
    /// An offset between expression and pattern.
    pub pattern_offset : usize,
    /// The part of input being a function/argument which is still typed by user. It is used
    /// for filtering suggestions.
    pub pattern : String,
}

impl ParsedInput {
    /// Constructor from the plain input.
    fn new(mut input:String, parser:&Parser) -> FallibleResult<Self> {
        let leading_spaces = input.chars().take_while(|c| *c == ' ').count();
        // To properly guess what is "still typed argument" we simulate type of one letter by user.
        // This letter will be added to the last argument (or function if there is no argument), or
        // will be a new argument (so the user starts filling a new argument).
        //
        // See also `parsed_input` test to see all cases we want to cover.
        input.push('a');
        let ast        = parser.parse_line(input.trim_start())?;
        let mut prefix = ast::prefix::Chain::new_non_strict(&ast);
        if let Some(last_arg) = prefix.args.pop() {
            let mut last_arg_repr = last_arg.sast.wrapped.repr();
            last_arg_repr.pop();
            Ok(ParsedInput {
                expression     : Some(ast::Shifted::new(leading_spaces,prefix)),
                pattern_offset : last_arg.sast.off,
                pattern        : last_arg_repr,
            })
        } else {
            let mut func_repr = prefix.func.repr();
            func_repr.pop();
            Ok(ParsedInput {
                expression     : None,
                pattern_offset : leading_spaces,
                pattern        : func_repr
            })
        }
    }

    fn new_from_ast(ast:&Ast) -> Self {
        let prefix = ast::prefix::Chain::new_non_strict(&ast);
        ParsedInput {
            expression     : Some(ast::Shifted::new(default(),prefix)),
            pattern_offset : 0,
            pattern        : default(),
        }
    }

    /// Returns the id of the next fragment potentially filled by picking completion suggestion.
    fn next_completion_id(&self) -> CompletedFragmentId {
        match &self.expression {
            None             => CompletedFragmentId::Function,
            Some(expression) => CompletedFragmentId::Argument {index:expression.args.len()},
        }
    }

    /// Get the picked fragment from the Searcher's input.
    pub fn completed_fragment(&self,fragment:CompletedFragmentId) -> Option<String> {
        use CompletedFragmentId::*;
        match (fragment,&self.expression) {
            (_              ,None)       => None,
            (Function       ,Some(expr)) => Some(expr.func.repr()),
            (Argument{index},Some(expr)) => Some(expr.args.get(index)?.sast.wrapped.repr()),
        }
    }
}

impl HasRepr for ParsedInput {
    fn repr(&self) -> String {
        let mut repr = self.expression.as_ref().map_or("".to_string(), HasRepr::repr);
        repr.extend(itertools::repeat_n(' ',self.pattern_offset));
        repr.push_str(&self.pattern);
        repr
    }
}



// ===========================
// === Searcher Controller ===
// ===========================

/// Describes how Searcher was brought to screen and how should behave when committing expression.
#[derive(Copy,Clone,Debug)]
#[allow(missing_docs)]
pub enum Mode {
    /// Searcher should add a new node at given position.
    NewNode {position:Option<Position>},
    /// Searcher should edit existing node's expression.
    EditNode {node_id:ast::Id}
}

/// A fragment filled by single picked completion suggestion.
///
/// We store such information in Searcher to better suggest the potential arguments, and to know
/// what imports should be added when inserting node.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct FragmentAddedByPickingSuggestion {
    pub id                : CompletedFragmentId,
    pub picked_suggestion : CompletionSuggestion,
}

impl FragmentAddedByPickingSuggestion {
    /// Check if the picked fragment is still unmodified by user.
    fn is_still_unmodified(&self, input:&ParsedInput) -> bool {
        input.completed_fragment(self.id).contains(&self.picked_suggestion.code_to_insert())
    }
}

/// A controller state.
#[derive(Clone,Debug,Default)]
pub struct Data {
    /// The current searcher's input.
    pub input : ParsedInput,
    /// The suggestion list which should be displayed.
    pub suggestions : Suggestions,
    /// All fragments of input which were added by picking suggestions. If the fragment will be
    /// changed by user, it will be removed from this list.
    pub fragments_added_by_picking : Vec<FragmentAddedByPickingSuggestion>,
}

impl Data {
    /// Initialize Searcher state when editing node.
    ///
    /// When searcher is brought by editing node, the input should be an expression of this node.
    /// Committing node will then edit the exiting node's expression instead of adding a new one.
    /// Additionally searcher should restore information about intended method, so we will be able
    /// to suggest arguments.
    fn new_with_edited_node
    (graph:&controller::Graph, database:&model::SuggestionDatabase, edited_node_id:ast::Id)
    -> FallibleResult<Self> {
        let edited_node      = graph.node(edited_node_id)?;
        let input            = ParsedInput::new_from_ast(edited_node.info.expression());
        let suggestions      = default();
        let intended_method  = edited_node.metadata.and_then(|md| md.intended_method);
        let initial_entry    = intended_method.and_then(|m| database.lookup_method(m));
        let initial_fragment = initial_entry.and_then(|entry| {
            let fragment = FragmentAddedByPickingSuggestion {
                id                : CompletedFragmentId::Function,
                picked_suggestion : entry
            };
            fragment.is_still_unmodified(&input).and_option(Some(fragment))
        });
        let mut fragments_added_by_picking = Vec::<FragmentAddedByPickingSuggestion>::new();
        initial_fragment.for_each(|f| fragments_added_by_picking.push(f));
        Ok(Data {input,suggestions,fragments_added_by_picking})
    }
}


/// Searcher Controller.
///
/// This is an object providing all required functionalities for Searcher View: mainly it is the
/// suggestion list to display depending on the searcher input, and actions of picking one or
/// accepting the Searcher input (pressing "Enter").
#[derive(Clone,CloneRef,Debug)]
pub struct Searcher {
    logger          : Logger,
    data            : Rc<RefCell<Data>>,
    notifier        : notification::Publisher<Notification>,
    graph           : controller::ExecutedGraph,
    mode            : Immutable<Mode>,
    database        : Rc<model::SuggestionDatabase>,
    language_server : Rc<language_server::Connection>,
    parser          : Parser,
}

impl Searcher {
    /// Create new Searcher Controller.
    pub async fn new
    ( parent  : impl AnyLogger
    , project : &model::Project
    , method  : language_server::MethodPointer
    , mode    : Mode
    ) -> FallibleResult<Self> {
        let graph = controller::ExecutedGraph::new(&parent,project.clone_ref(),method).await?;
        Self::new_from_graph_controller(parent,project,graph,mode)
    }

    /// Create new Searcher Controller, when you have Executed Graph Controller handy.
    pub fn new_from_graph_controller
    ( parent  : impl AnyLogger
    , project : &model::Project
    , graph   : controller::ExecutedGraph
    , mode    : Mode
    ) -> FallibleResult<Self> {
        let logger   = Logger::sub(parent,"Searcher Controller");
        let database = project.suggestion_db();
        let data     = if let Mode::EditNode{node_id} = mode {
            Data::new_with_edited_node(&graph.graph(),&*database,node_id)?
        } else {
            default()
        };
        let this = Self {
            logger,graph,
            data            : Rc::new(RefCell::new(data)),
            notifier        : default(),
            mode            : Immutable(mode),
            database        : project.suggestion_db(),
            language_server : project.json_rpc(),
            parser          : project.parser(),
        };
        this.reload_list();
        Ok(this)
    }

    /// Subscribe to controller's notifications.
    pub fn subscribe(&self) -> Subscriber<Notification> {
        self.notifier.subscribe()
    }

    /// Get the current suggestion list.
    pub fn suggestions(&self) -> Suggestions {
        self.data.borrow().suggestions.clone_ref()
    }

    /// Set the Searcher Input.
    ///
    /// This function should be called each time user modifies Searcher input in view. It may result
    /// in a new suggestion list (the aprriopriate notification will be emitted).
    pub fn set_input(&self, new_input:String) -> FallibleResult<()> {
        let parsed_input = ParsedInput::new(new_input,&self.parser)?;
        let old_id       = self.data.borrow().input.next_completion_id();
        let new_id       = parsed_input.next_completion_id();

        self.data.borrow_mut().input = parsed_input;
        self.invalidate_fragments_added_by_picking();
        if old_id != new_id {
            self.reload_list()
        }
        Ok(())
    }

    /// Pick a completion suggestion.
    ///
    /// This function should be called when user chooses some completion suggestion. The picked
    /// suggestion will be remembered, and the searcher's input will be updated and returned by this
    /// function.
    pub fn pick_completion
    (&self, picked_suggestion:CompletionSuggestion) -> FallibleResult<String> {
        let added_ast         = self.parser.parse_line(&picked_suggestion.code_to_insert())?;
        let id                = self.data.borrow().input.next_completion_id();
        let picked_completion = FragmentAddedByPickingSuggestion {id,picked_suggestion};
        let pattern_offset    = self.data.borrow().input.pattern_offset;
        let new_expression    = match self.data.borrow_mut().input.expression.take() {
            None => {
                let ast = ast::prefix::Chain::new_non_strict(&added_ast);
                ast::Shifted::new(pattern_offset,ast)
            },
            Some(mut expression) => {
                let new_argument = ast::prefix::Argument {
                    sast      : ast::Shifted::new(pattern_offset,added_ast),
                    prefix_id : default(),
                };
                expression.args.push(new_argument);
                expression
            }
        };
        let new_parsed_input = ParsedInput {
            expression     : Some(new_expression),
            pattern_offset : 1,
            pattern        : "".to_string()
        };
        let new_input = new_parsed_input.repr();
        self.data.borrow_mut().input = new_parsed_input;
        self.data.borrow_mut().fragments_added_by_picking.push(picked_completion);
        self.reload_list();
        Ok(new_input)
    }

    /// Commit the current input as a new node expression.
    ///
    /// If the searcher was brought by editing existing node, the input is set as a new node
    /// expression, otherwise a new node is added. This will also add all imports required by
    /// picked suggestions.
    pub fn commit_node(&self) -> FallibleResult<ast::Id> {
        let data_borrowed   = self.data.borrow();
        let expression      = data_borrowed.input.repr();
        let intended_method = self.intended_method();

        let id = match *self.mode {
            Mode::NewNode {position} => {
                let mut new_node    = NewNodeInfo::new_pushed_back(expression);
                new_node.metadata   = Some(NodeMetadata {position,intended_method});
                self.graph.graph().add_node(new_node)?
            },
            Mode::EditNode {node_id} => {
                self.graph.graph().set_expression(node_id,expression)?;
                self.graph.graph().module.with_node_metadata(node_id,Box::new(|md| {
                    md.intended_method = intended_method
                }));
                node_id
            }
        };
        self.add_required_imports();
        Ok(id)
    }

    fn invalidate_fragments_added_by_picking(&self) {
        let mut data = self.data.borrow_mut();
        let data     = data.deref_mut();
        let input    = &data.input;
        data.fragments_added_by_picking.drain_filter(|frag| !frag.is_still_unmodified(input));
    }

    fn add_required_imports(&self) {
        let data_borrowed = self.data.borrow();
        let fragments     = data_borrowed.fragments_added_by_picking.iter();
        let imports       = fragments.map(|frag| &frag.picked_suggestion.module);
        let module_ast    = self.graph.graph().module.ast();
        let mut module    = double_representation::module::Info {ast:module_ast};
        for import in imports {
            let import        = ImportInfo::from_qualified_name(&import);
            let already_there = module.iter_imports().any(|imp| imp == import);
            if !already_there {
                module.add_import(&self.parser,import);
            }
        }
        self.graph.graph().module.update_ast(module.ast);
    }

    /// Reload Suggestion List.
    ///
    /// The current list will be set as "Loading" and Language Server will be requested for a new
    /// list - once it be retrieved, the new list will be set and notification will be emitted.
    fn reload_list(&self) {
        let next_completion = self.data.borrow().input.next_completion_id();
        let new_suggestions = if next_completion == CompletedFragmentId::Function {
            if let Err(err) = self.get_suggestion_list_from_engine(None,None) {
                error!(self.logger,"Cannot request engine for suggestions: {err}");
            }
            Suggestions::Loading
        } else {
            // TODO[ao] Requesting for argument.
            Suggestions::Loaded {list:default()}
        };
        self.data.borrow_mut().suggestions = new_suggestions;
    }

    fn get_suggestion_list_from_engine
    (&self, return_type:Option<String>, tags:Option<Vec<language_server::SuggestionEntryType>>)
    -> FallibleResult<()> {
        let ls         = &self.language_server;
        let self_type  = None;
        let graph      = self.graph.graph();
        let file       = graph.module.path().file_path();
        let module_ast = graph.module.ast();
        let def_span   = double_representation::module::definition_span(&module_ast,&graph.id)?;
        let position   = TextLocation::convert_span(module_ast.repr(),&def_span).end.into();
        let request    = ls.completion(&file,&position,&self_type,&return_type,&tags);
        let data       = self.data.clone_ref();
        let database   = self.database.clone_ref();
        let logger     = self.logger.clone_ref();
        let notifier   = self.notifier.clone_ref();
        executor::global::spawn(async move {
            info!(logger,"Requesting new suggestion list.");
            let response = request.await;
            info!(logger,"Received suggestions from Language Server.");
            let new_suggestions = match response {
                Ok(list) => {
                    let entry_ids   = list.results.into_iter();
                    let entries     = entry_ids.filter_map(|id| {
                        let entry = database.get(id);
                        if entry.is_none() {
                            error!(logger,"Missing entry {id} in Suggestion Database.");
                        }
                        entry
                    });
                    let suggestions = entries.map(Suggestion::Completion);
                    Suggestions::Loaded {list:Rc::new(suggestions.collect())}
                },
                Err(error) => Suggestions::Error(Rc::new(error.into()))
            };
            data.borrow_mut().suggestions = new_suggestions;
            notifier.publish(Notification::NewSuggestionList).await;
        });
        Ok(())
    }

    /// Returns the Id of method user intends to be called in this node.
    ///
    /// The method may be picked by user from suggestion, but there are many methods with the same
    /// name.
    fn intended_method(&self) -> Option<MethodId> {
        let borrowed_data = self.data.borrow();
        let mut fragments = borrowed_data.fragments_added_by_picking.iter();
        let function_frag = fragments.find(|frag| frag.id == CompletedFragmentId::Function)?;
        function_frag.picked_suggestion.method_id()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use controller::graph::executed::tests::MockData as ExecutedGraphMockData;
    use json_rpc::expect_call;
    use utils::test::traits::*;

    struct Fixture {
        test     : TestWithLocalPoolExecutor,
        searcher : Searcher,
        entry1   : CompletionSuggestion,
        entry2   : CompletionSuggestion,
        entry3   : CompletionSuggestion,
        entry9   : CompletionSuggestion,
    }

    impl Fixture {
        fn new_custom<F>(client_setup:F) -> Self
        where F : FnOnce(&mut ExecutedGraphMockData,&mut language_server::MockClient) {
            let test           = TestWithLocalPoolExecutor::set_up();
            let mut graph_data = controller::graph::executed::tests::MockData::default();
            let mut client     = language_server::MockClient::default();
            client_setup(&mut graph_data,&mut client);
            let searcher = Searcher {
                logger          : default(),
                data            : default(),
                notifier        : default(),
                graph           : graph_data.controller(),
                mode            : Immutable(Mode::NewNode {position:default()}),
                database        : default(),
                language_server : language_server::Connection::new_mock_rc(client),
                parser          : Parser::new_or_panic(),
            };
            let entry1 = model::suggestion_database::Entry {
                name          : "testFunction1".to_string(),
                kind          : model::suggestion_database::EntryKind::Function,
                module        : "Test.Test".to_string().try_into().unwrap(),
                arguments     : vec![],
                return_type   : "Number".to_string(),
                documentation : default(),
                self_type     : None
            };
            let entry2 = model::suggestion_database::Entry {
                name : "TestVar1".to_string(),
                kind : model::suggestion_database::EntryKind::Local,
                ..entry1.clone()
            };
            let entry3 = model::suggestion_database::Entry {
                name          : "testMethod1".to_string(),
                kind          : model::suggestion_database::EntryKind::Method,
                self_type     : Some("Test".to_string()),
                ..entry1.clone()
            };
            let entry9 = entry1.clone().with_name("testFunction2");

            searcher.database.put_entry(1,entry1);
            let entry1 = searcher.database.get(1).unwrap();
            searcher.database.put_entry(2,entry2);
            let entry2 = searcher.database.get(2).unwrap();
            searcher.database.put_entry(3,entry3);
            let entry3 = searcher.database.get(3).unwrap();
            searcher.database.put_entry(9,entry9);
            let entry9 = searcher.database.get(9).unwrap();
            Fixture{test,searcher,entry1,entry2,entry3,entry9}
        }

        fn new() -> Self {
            Self::new_custom(|_,_| {})
        }
    }


    #[wasm_bindgen_test]
    fn loading_list() {
        let Fixture{mut test,searcher,entry1,entry9,..} = Fixture::new_custom(|data,client| {
            let completion_response = language_server::response::Completion {
                results: vec![1,5,9],
                current_version: default(),
            };

            let file_end          = data.module.code.chars().count();
            let expected_location = TextLocation {line:0, column:file_end};
            expect_call!(client.completion(
                module      = data.module.path.file_path().clone(),
                position    = expected_location.into(),
                self_type   = None,
                return_type = None,
                tag         = None
            ) => Ok(completion_response));
        });

        let mut subscriber = searcher.subscribe();
        searcher.reload_list();
        assert!(searcher.suggestions().is_loading());
        test.run_until_stalled();
        let expected_list = vec![Suggestion::Completion(entry1),Suggestion::Completion(entry9)];
        assert_eq!(searcher.suggestions().list(), Some(&expected_list));
        let notification = subscriber.next().boxed_local().expect_ready();
        assert_eq!(notification, Some(Notification::NewSuggestionList));
    }

    #[wasm_bindgen_test]
    fn parsed_input() {
        let parser = Parser::new_or_panic();

        fn args_reprs(prefix:&ast::prefix::Chain) -> Vec<String> {
            prefix.args.iter().map(|arg| arg.repr()).collect()
        }

        let input  = "";
        let parsed = ParsedInput::new(input.to_string(),&parser).unwrap();
        assert!(parsed.expression.is_none());
        assert_eq!(parsed.pattern.as_str(), "");

        let input  = "foo";
        let parsed = ParsedInput::new(input.to_string(),&parser).unwrap();
        assert!(parsed.expression.is_none());
        assert_eq!(parsed.pattern.as_str(), "foo");

        let input  = " foo";
        let parsed = ParsedInput::new(input.to_string(),&parser).unwrap();
        assert!(parsed.expression.is_none());
        assert_eq!(parsed.pattern_offset,   1);
        assert_eq!(parsed.pattern.as_str(), "foo");

        let input  = "foo  ";
        let parsed = ParsedInput::new(input.to_string(),&parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off         , 0);
        assert_eq!(expression.func.repr() , "foo");
        assert_eq!(args_reprs(&expression) , Vec::<String>::new());
        assert_eq!(parsed.pattern_offset,   2);
        assert_eq!(parsed.pattern.as_str(), "");

        let input  = "foo bar";
        let parsed = ParsedInput::new(input.to_string(),&parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off         , 0);
        assert_eq!(expression.func.repr() , "foo");
        assert_eq!(args_reprs(&expression) , Vec::<String>::new());
        assert_eq!(parsed.pattern_offset,   1);
        assert_eq!(parsed.pattern.as_str(), "bar");

        let input  = "foo  bar  baz";
        let parsed = ParsedInput::new(input.to_string(),&parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off         , 0);
        assert_eq!(expression.func.repr() , "foo");
        assert_eq!(args_reprs(&expression) , vec!["  bar".to_string()]);
        assert_eq!(parsed.pattern_offset  , 2);
        assert_eq!(parsed.pattern.as_str(), "baz");

        let input  = "  foo bar baz ";
        let parsed = ParsedInput::new(input.to_string(),&parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off         , 2);
        assert_eq!(expression.func.repr() , "foo");
        assert_eq!(args_reprs(&expression) , vec![" bar".to_string()," baz".to_string()]);
        assert_eq!(parsed.pattern_offset,   1);
        assert_eq!(parsed.pattern.as_str(), "");

        let input  = "foo bar (baz ";
        let parsed = ParsedInput::new(input.to_string(),&parser).unwrap();
        let expression = parsed.expression.unwrap();
        assert_eq!(expression.off         , 0);
        assert_eq!(expression.func.repr() , "foo");
        assert_eq!(args_reprs(&expression) , vec![" bar".to_string()]);
        assert_eq!(parsed.pattern_offset,   1);
        assert_eq!(parsed.pattern.as_str(), "(baz ");
    }

    #[wasm_bindgen_test]
    fn picked_completions_list_maintaining() {
        let Fixture{test:_test,searcher,entry1,entry2,..} = Fixture::new();
        let frags_borrow = || Ref::map(searcher.data.borrow(),|d| &d.fragments_added_by_picking);

        // Picking first suggestion.
        let new_input = searcher.pick_completion(entry1.clone_ref()).unwrap();
        assert_eq!(new_input, "testFunction1 ");
        let (func,) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(func.id, CompletedFragmentId::Function);
        assert!(Rc::ptr_eq(&func.picked_suggestion,&entry1));

        // Typing more args by hand.
        searcher.set_input("testFunction1 some_arg pat".to_string()).unwrap();
        let (func,) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(func.id, CompletedFragmentId::Function);
        assert!(Rc::ptr_eq(&func.picked_suggestion,&entry1));

        // Picking argument's suggestion.
        let new_input = searcher.pick_completion(entry2.clone_ref()).unwrap();
        assert_eq!(new_input, "testFunction1 some_arg TestVar1 ");
        let new_input = searcher.pick_completion(entry2.clone_ref()).unwrap();
        assert_eq!(new_input, "testFunction1 some_arg TestVar1 TestVar1 ");
        let (function,arg1,arg2) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(function.id, CompletedFragmentId::Function);
        assert!(Rc::ptr_eq(&function.picked_suggestion,&entry1));
        assert_eq!(arg1.id, CompletedFragmentId::Argument {index:1});
        assert!(Rc::ptr_eq(&arg1.picked_suggestion,&entry2));
        assert_eq!(arg2.id, CompletedFragmentId::Argument {index:2});
        assert!(Rc::ptr_eq(&arg2.picked_suggestion,&entry2));

        // Backspacing back to the second arg.
        searcher.set_input("testFunction1 some_arg TestVar1 TestV".to_string()).unwrap();
        let (picked,arg) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(picked.id, CompletedFragmentId::Function);
        assert!(Rc::ptr_eq(&picked.picked_suggestion,&entry1));
        assert_eq!(arg.id, CompletedFragmentId::Argument {index:1});
        assert!(Rc::ptr_eq(&arg.picked_suggestion,&entry2));

        // Editing the picked function.
        searcher.set_input("testFunction2 some_arg TestVar1 TestV".to_string()).unwrap();
        let (arg,) = frags_borrow().iter().cloned().expect_tuple();
        assert_eq!(arg.id, CompletedFragmentId::Argument {index:1});
        assert!(Rc::ptr_eq(&arg.picked_suggestion,&entry2));
    }

    #[wasm_bindgen_test]
    fn committing_node() {
        let Fixture{test:_test,mut searcher,entry3,..} = Fixture::new();
        let module                                     = searcher.graph.graph().module.clone_ref();
        // Setup searcher.
        let parser        = Parser::new_or_panic();
        let picked_method = FragmentAddedByPickingSuggestion {
            id                : CompletedFragmentId::Function,
            picked_suggestion : entry3,
        };
        with(searcher.data.borrow_mut(), |mut data| {
            data.fragments_added_by_picking.push(picked_method);
            data.input = ParsedInput::new("Test.testMethod1".to_string(),&parser).unwrap();
        });

        // Add new node.
        let position  = Some(Position::new(4.0, 5.0));
        searcher.mode = Immutable(Mode::NewNode {position});
        searcher.commit_node().unwrap();

        let expected_code = "import Test.Test\nmain = \n    2 + 2\n    Test.testMethod1";
        assert_eq!(module.ast().repr(), expected_code);
        let (node1,node2) = searcher.graph.graph().nodes().unwrap().expect_tuple();
        let expected_intended_method = Some(MethodId {
            module          : "Test.Test".to_string().try_into().unwrap(),
            defined_on_type : "Test".to_string(),
            name            : "testMethod1".to_string(),
        });
        assert_eq!(node2.metadata.unwrap().intended_method, expected_intended_method);

        // Edit existing node.
        searcher.mode = Immutable(Mode::EditNode {node_id:node1.info.id()});
        searcher.commit_node().unwrap();
        let expected_code = "import Test.Test\nmain = \n    Test.testMethod1\n    Test.testMethod1";
        let (node1,_)     = searcher.graph.graph().nodes().unwrap().expect_tuple();
        assert_eq!(node1.metadata.unwrap().intended_method, expected_intended_method);
        assert_eq!(module.ast().repr(), expected_code);
    }

    #[wasm_bindgen_test]
    fn initialized_data_when_editing_node() {
        let Fixture{test:_test,searcher,entry3,..} = Fixture::new();

        let graph    = searcher.graph.graph();
        let (node,)  = graph.nodes().unwrap().expect_tuple();
        let node_id  = node.info.id();
        let database = searcher.database;

        // Node had not intended method.
        let searcher_data = Data::new_with_edited_node(&graph,&database,node_id).unwrap();
        assert_eq!(searcher_data.input.repr(), node.info.expression().repr());
        assert!(searcher_data.fragments_added_by_picking.is_empty());
        assert!(searcher_data.suggestions.is_loading());

        // Node had intended method, but it's outdated.
        let intended_method = MethodId {
            module          : "Test.Test".to_string().try_into().unwrap(),
            defined_on_type : "Test".to_string(),
            name            : "testMethod1".to_string()
        };
        graph.module.with_node_metadata(node_id, Box::new(|md| {
            md.intended_method = Some(intended_method);
        }));
        let searcher_data = Data::new_with_edited_node(&graph,&database,node_id).unwrap();
        assert_eq!(searcher_data.input.repr(), node.info.expression().repr());
        assert!(searcher_data.fragments_added_by_picking.is_empty());
        assert!(searcher_data.suggestions.is_loading());

        // Node had up-to-date intended method.
        graph.set_expression(node_id,"Test.testMethod1 12").unwrap();
        // We set metadata in previous section.
        let searcher_data = Data::new_with_edited_node(&graph,&database,node_id).unwrap();
        assert_eq!(searcher_data.input.repr(), "Test.testMethod1 12");
        assert!(searcher_data.suggestions.is_loading());
        let (initial_fragment,) = searcher_data.fragments_added_by_picking.expect_tuple();
        assert!(Rc::ptr_eq(&initial_fragment.picked_suggestion,&entry3))
    }
}
