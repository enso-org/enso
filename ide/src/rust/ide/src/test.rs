#![cfg(test)]
//! Module for support code for writing tests.

use crate::prelude::*;

use crate::double_representation::module;
use crate::model::suggestion_database;
use crate::executor::test_utils::TestWithLocalPoolExecutor;

use enso_frp::data::bitfield::BitField;
use enso_frp::data::bitfield::BitField32;
use enso_protocol::types::Sha3_224;
use enso_protocol::language_server;
use enso_protocol::language_server::CapabilityRegistration;
use json_rpc::expect_call;
use utils::test::traits::*;

/// Utilities for mocking IDE components.
pub mod mock {
    use super::*;

    /// Data used to create mock IDE components.
    ///
    /// Contains a number of constants and functions building more complex structures from them.
    /// The purpose is to allow different parts of tests that mock different models using
    /// consistent data.
    #[allow(missing_docs)]
    pub mod data {
        use super::*;

        use enso_protocol::language_server::Position;
        use uuid::Uuid;

        pub const ROOT_ID         : Uuid     = Uuid::from_u128(100);
        pub const PROJECT_NAME    : &str     = "MockProject";
        pub const MODULE_NAME     : &str     = "Mock_Module";
        pub const CODE            : &str     = "main = \n    2 + 2";
        pub const DEFINITION_NAME : &str     = "main";
        pub const TYPE_NAME       : &str     = "Mock_Type";
        pub const MAIN_FINISH     : Position = Position {line:1, character:9};
        pub const CONTEXT_ID      : Uuid     = Uuid::from_u128(0xFE);

        pub fn module_path() -> crate::model::module::Path {
            crate::model::module::Path::from_name_segments(ROOT_ID, &[MODULE_NAME]).unwrap()
        }

        pub fn module_qualified_name() -> module::QualifiedName {
            module_path().qualified_module_name(PROJECT_NAME)
        }

        pub fn definition_name() -> crate::double_representation::definition::DefinitionName {
            crate::double_representation::definition::DefinitionName::new_plain(DEFINITION_NAME)
        }

        pub fn graph_id() -> crate::double_representation::graph::Id {
            crate::double_representation::graph::Id::new_plain_name(DEFINITION_NAME)
        }

        pub fn foo_method_parameter() -> suggestion_database::Argument {
            suggestion_database::Argument {
                name          : "this".to_owned(),
                repr_type     : "Base".to_owned(),
                is_suspended  : false,
                has_default   : false,
                default_value : None,
            }
        }

        pub fn foo_method_parameter2() -> suggestion_database::Argument {
            suggestion_database::Argument {
                name          : "param1".to_owned(),
                repr_type     : "Number".to_owned(),
                is_suspended  : false,
                has_default   : false,
                default_value : None,
            }
        }

        pub fn bar_method_parameter() -> suggestion_database::Argument {
            suggestion_database::Argument {
                name          : "this".to_owned(),
                repr_type     : "Other".to_owned(),
                is_suspended  : false,
                has_default   : false,
                default_value : None,
            }
        }

        pub fn suggestion_entry_foo() -> suggestion_database::Entry {
            suggestion_database::Entry {
                name      : "foo".to_owned(),
                module    : module::QualifiedName::from_segments("Std",&["Base"]).unwrap(),
                self_type : Some("Base".to_owned()),
                arguments : vec![foo_method_parameter(),foo_method_parameter2()],
                return_type   : "Any".to_owned(),
                kind          : suggestion_database::EntryKind::Method,
                scope         : suggestion_database::Scope::Everywhere,
                documentation : None
            }
        }

        pub fn suggestion_entry_bar() -> suggestion_database::Entry {
            suggestion_database::Entry {
                name      : "bar".to_owned(),
                module    : module::QualifiedName::from_segments("Std",&["Other"]).unwrap(),
                self_type : Some("Other".to_owned()),
                arguments : vec![bar_method_parameter()],
                return_type   : "Any".to_owned(),
                kind          : suggestion_database::EntryKind::Method,
                scope         : suggestion_database::Scope::Everywhere,
                documentation : None
            }
        }
    }

    /// This mock data represents a rudimentary enviromment consisting of a project with a single
    /// module. The module contents is provided by default by [data::CODE], can be overwritten by
    /// calling [set_code] or [set_inline_code].
    #[derive(Clone,Debug)]
    pub struct Unified {
        pub logger        : Logger,
        pub project_name  : String,
        pub module_path   : model::module::Path,
        pub suggestions   : HashMap<suggestion_database::EntryId,suggestion_database::Entry>,
        pub context_id    : model::execution_context::Id,
        pub parser        : parser::Parser,
        code              : String,
        id_map            : ast::IdMap,
        metadata          : crate::model::module::Metadata,
        root_definition   : double_representation::definition::DefinitionName,
    }

    impl Unified {
        pub fn set_inline_code(&mut self, code:impl AsRef<str>) {
            let method = self.method_pointer();
            self.code = format!("{} = {}",method.name,code.as_ref())
        }

        pub fn get_code(&self) -> &str {
            &self.code
        }

        pub fn set_code(&mut self, code:impl Into<String>) {
            self.code     = code.into();
            self.id_map   = default();
            self.metadata = default();
        }

        pub fn new() -> Self {
            use crate::test::mock::data::*;
            let mut suggestions = HashMap::new();
            suggestions.insert(1,suggestion_entry_foo());
            suggestions.insert(2,suggestion_entry_bar());
            Unified {
                suggestions,
                logger          : Logger::new("Unified"),
                project_name    : PROJECT_NAME.to_owned(),
                module_path     : module_path(),
                code            : CODE.to_owned(),
                id_map          : default(),
                metadata        : default(),
                context_id      : CONTEXT_ID,
                root_definition : definition_name(),
                parser          : parser::Parser::new_or_panic(),
            }
        }

        pub fn module(&self) -> crate::model::Module {
            let ast    = self.parser.parse_module(self.code.clone(),self.id_map.clone()).unwrap();
            let module = crate::model::module::Plain::new(self.module_path.clone(),ast,self.metadata.clone());
            Rc::new(module)
        }

        pub fn module_qualified_name(&self) -> module::QualifiedName {
            self.module_path.qualified_module_name(&self.project_name)
        }

        pub fn definition_id(&self) -> double_representation::definition::Id {
            double_representation::definition::Id::new_single_crumb(self.root_definition.clone())
        }

        pub fn method_pointer(&self) -> enso_protocol::language_server::MethodPointer {
            enso_protocol::language_server::MethodPointer {
                module          : self.module_qualified_name().to_string(),
                defined_on_type : self.module_path.module_name().to_string(),
                name            : self.root_definition.to_string(),
            }
        }

        /// Create a graph controller from the current mock data.
        pub fn graph
        (&self, logger:impl AnyLogger, module:model::Module, db:Rc<model::SuggestionDatabase>)
         -> crate::controller::Graph {
            let parser      = self.parser.clone_ref();
            let method      = self.method_pointer();
            let definition  = module.lookup_method(&method).unwrap();
            crate::controller::Graph::new(logger,module,db,parser,definition).unwrap()
        }

        pub fn execution_context(&self) -> model::ExecutionContext {
            let logger = Logger::sub(&self.logger,"Mocked Execution Context");
            Rc::new(model::execution_context::Plain::new(logger,self.method_pointer()))
        }

        pub fn project
        ( &self
        , module              : model::Module
        , execution_context   : model::ExecutionContext
        , suggestion_database : Rc<model::SuggestionDatabase>
        , json_client         : language_server::MockClient
        ) -> model::Project {
            let mut project = model::project::MockAPI::new();
            model::project::test::expect_name(&mut project,&self.project_name);
            model::project::test::expect_parser(&mut project,&self.parser);
            model::project::test::expect_module(&mut project,module);
            model::project::test::expect_execution_ctx(&mut project,execution_context);
            // Root ID is needed to generate module path used to get the module.
            model::project::test::expect_root_id(&mut project,crate::test::mock::data::ROOT_ID);
            model::project::test::expect_suggestion_db(&mut project,suggestion_database);
            let json_rpc = language_server::Connection::new_mock_rc(json_client);
            model::project::test::expect_json_rpc(&mut project,json_rpc);
            Rc::new(project)
        }

        pub fn fixture(&self) -> Fixture {
            self.fixture_customize(|_,_| {})
        }

        pub fn fixture_customize
        (&self, customize_json_rpc:impl FnOnce(&Self,&mut language_server::MockClient))
        -> Fixture {
            let mut json_client = language_server::MockClient::default();
            // Creating a searcher controller always triggers a query for completion.
            controller::searcher::test::expect_completion(&mut json_client, &[]);
            customize_json_rpc(self,&mut json_client);

            let logger        = Logger::new("UnifiedMock");
            let module        = self.module();
            let suggestion_db = Rc::new(model::SuggestionDatabase::new_from_entries(&logger,
                &self.suggestions));
            let graph     = self.graph(&logger,module.clone_ref(),suggestion_db.clone_ref());
            let execution = self.execution_context();
            let project   = self.project(module.clone_ref(),execution.clone_ref(),
                suggestion_db.clone_ref(),json_client);
            let executed_graph = controller::ExecutedGraph::new_internal(graph.clone_ref(),
                project.clone_ref(),execution.clone_ref());
            let executor       = TestWithLocalPoolExecutor::set_up();
            let data           = self.clone();
            let selected_nodes = Vec::new();
            let searcher_mode  = controller::searcher::Mode::NewNode {position:None};
            let searcher       = controller::Searcher::new_from_graph_controller(&logger,&project,
                executed_graph.clone_ref(),searcher_mode,selected_nodes).unwrap();
            Fixture {
                executor,
                data,
                module,
                graph,
                executed_graph,
                execution,
                suggestion_db,
                project,
                searcher,
            }
        }

        /// Register an expectation that the module described by this mock data will be opened.
        pub fn expect_opening_module
        (&self, client:&mut enso_protocol::language_server::MockClient) {
            let content          = self.code.clone();
            let current_version  = Sha3_224::new(content.as_bytes());
            let path             = self.module_path.file_path().clone();
            let write_capability = Some(CapabilityRegistration::create_can_edit_text_file(path));
            let open_resp        = language_server::response::OpenTextFile {
                write_capability,
                content,
                current_version,
            };

            let path = self.module_path.file_path().clone();
            expect_call!(client.open_text_file(path=path) => Ok(open_resp));
        }

        /// Register an expectation that the module described by this mock data will be closed.
        pub fn expect_closing_module
        (&self, client:&mut enso_protocol::language_server::MockClient) {
            let path = self.module_path.file_path().clone();
            expect_call!(client.close_text_file(path=path) => Ok(()));
        }
    }

    #[derive(Debug)]
    pub struct Fixture {
        pub executor       : TestWithLocalPoolExecutor,
        pub data           : Unified,
        pub module         : model::Module,
        pub graph          : controller::Graph,
        pub execution      : model::ExecutionContext,
        pub executed_graph : controller::ExecutedGraph,
        pub suggestion_db  : Rc<model::SuggestionDatabase>,
        pub project        : model::Project,
        pub searcher       : controller::Searcher,
    }

    impl Fixture {
        /// Runs all tasks in the pool and returns if no more progress can be made on any task.
        pub fn run_until_stalled(&mut self) {
            self.executor.run_until_stalled();
        }

        /// Create a synchronized module model.
        ///
        /// For this to work, some earlier customizations are needed, at least
        /// `[Unified::expect_opening_the_module]`, as the synchronized model makes calls to the
        /// language server API. Most likely also closing and initial edit (that adds metadata)
        /// should be expected. See usage for examples.
        pub fn synchronized_module(&self) -> Rc<model::module::Synchronized> {
            let parser        = self.data.parser.clone();
            let path          = self.data.module_path.clone();
            let ls            = self.project.json_rpc().clone();
            let module_future = model::module::Synchronized::open(path,ls,parser);
            // We can `expect_ready`, because in fact this is synchronous in test conditions.
            // (there's no real asynchronous connection beneath, just the `MockClient`)
            module_future.boxed_local().expect_ready().unwrap()
        }

        /// Create a synchronized module model and a module controller paired with it.
        ///
        /// Same considerations need to be made as with `[synchronized_module]`.
        pub fn synchronized_module_w_controller(&self) -> (Rc<model::module::Synchronized>,controller::Module) {
            let parser = self.data.parser.clone();
            let path   = self.data.module_path.clone();
            let ls     = self.project.json_rpc().clone();
            let module_fut = model::module::Synchronized::open(path,ls,parser);
            // We can `expect_ready`, because in fact this is synchronous.
            // (there's no real asynchronous connection beneath, just the `MockClient`)
            let model = module_fut.boxed_local().expect_ready().unwrap();

            let controller = controller::module::Handle {
                language_server : self.project.json_rpc(),
                model           : model.clone(),
                parser          : self.data.parser.clone(),
                logger          : Logger::sub(&self.data.logger,"MockModuleController"),
            };
            (model,controller)
        }
    }

    pub fn indent(line:impl AsRef<str>) -> String {
        iformat!("    {line.as_ref()}")
    }

    pub fn main_from_lines(lines:impl IntoIterator<Item:AsRef<str>>) -> String {
        def_from_lines("main",lines)
    }

    pub fn def_from_lines
    (name:impl Display, lines:impl IntoIterator<Item:AsRef<str>>) -> String {
        let body = lines.into_iter().map(indent).join("\n");
        iformat!("{name} =\n{body}")
    }
}

/// Check that given `CalledMethodInfo` is consistent with suggestion database `Entry`.
pub fn assert_call_info
( info:span_tree::generate::context::CalledMethodInfo
, entry:&model::suggestion_database::Entry
) {
    assert_eq!(info.parameters.len(),entry.arguments.len());
    for (encountered,expected) in info.parameters.iter().zip(entry.arguments.iter()) {
        let expected_info = model::suggestion_database::to_span_tree_param(expected);
        assert_eq!(encountered,&expected_info);
    }
}



// ==============
// === Runner ===
// ==============

/// Helper that runs given test multiple times simulating different executor interleaving.
/// The `Runner` can be used to write tests against the race conditions dependent on whether some
/// code path gets interrupted by executor's run or not.
///
/// Typical use-case is to call `Runner::run(|runner| { …test code… })`.
/// For testing a single, specific iteration `run_with` or `run_nth` should be used.
///
/// The test when run is given a handle to the runner object that offers `perhaps_run_until_stalled`
/// method. It should be invoked (with a fixture object) in places where typically the test code
/// would use the fixture's `run_until_stalled`. The runner will then either call the
/// `run_until_stalled` or do nothing.
///
/// The `run` method will run test multiple times, to cover all possible combinations.
/// The test should call `perhaps_run_until_stalled` the same number of times on each iteration.
/// Otherwise, the `perhaps_run_until_stalled` behavior is unspecified (but still well-formed).
#[derive(Clone,Copy,Debug,Default)]
pub struct Runner {
    /// Incremented each time when the runnee calls an interruption point.
    /// Reset to 0 after each run.
    current : u32,
    /// Bitmap that encodes behavior of subsequent `run_until_stalled` calls. True means running.
    seed : BitField32,
}

impl Runner {
    fn new(seed:BitField32) -> Self {
        let current = 0;
        Self {current,seed}
    }

    /// Call's the fixture's `run_until_stalled`. Or does not call. Depends on the current seed
    /// (defined by the iteration number) and the number of previous calls to this method.
    ///
    /// See the `[Runner]` documentation.
    pub fn perhaps_run_until_stalled(&mut self, fixture:&mut crate::test::mock::Fixture) {
        let index     = self.current;
        self.current += 1;
        if dbg!(self.seed.get_bit(index as usize)) {
            fixture.run_until_stalled();
        }
    }

    /// Calls the `test` function multiple times, to cover all possible combinations of
    /// `run_until_stalled` method's behavior.
    ///
    /// The number of runs is determined by the number of calls to the method in the first
    /// iteration.
    ///
    /// If this function fails only on a specific iteration, replacing it with `run_nth` might be
    /// helpful in debugging the issue.
    ///
    /// NOTE: The number of runs will grow *exponentially* with the `run_until_stalled` methods!
    /// Be certain to use it sparingly, to cover really specific scenarios. It is not meant for
    /// general usage in big tests in multiple places.
    pub fn run(mut test:impl FnMut(&mut Runner)) {
        let count         = Self::run_nth(0,&mut test);
        let possibilities = 2u32.pow(count);
        // Just to prevent accidentally generating too many runs.
        assert!(count < 5, "Consider reducing number of calls to `run_until_stalled` or bump this \
        limit if it doesn't cause slowdowns during the testing.");
        for i in 1 ..possibilities {
            Self::run_nth(i,&mut test);
        }
    }

    /// Calls the `test` function once. The executor behavior is defined by the `seed`.
    /// Returns the number of calls made to `perhaps_run_until_stalled`.
    pub fn run_with(seed:BitField32, mut test:impl FnMut(&mut Runner)) -> u32 {
        let mut runner = Runner::new(seed);
        test(&mut runner);
        runner.current
    }

    /// Calls the `test` function once. The executor behavior is defined by the `n` parameter.
    /// Returns the number of calls made to `perhaps_run_until_stalled`.
    pub fn run_nth(n:u32, test:impl FnMut(&mut Runner)) -> u32 {
        println!("Runner: Iteration {}",n);
        Self::run_with(BitField32 {raw:n}, test)
    }
}
