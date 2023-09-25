//! Module for support code for writing tests.

#![cfg(test)]

use crate::prelude::*;

use crate::executor::test_utils::TestWithLocalPoolExecutor;
use crate::model::suggestion_database;
use crate::model::undo_redo;

use double_representation::name::project;
use engine_protocol::binary;
use engine_protocol::language_server;
use engine_protocol::language_server::CapabilityRegistration;
use engine_protocol::types::Sha3_224;
use enso_frp::data::bitfield::BitField;
use enso_frp::data::bitfield::BitField32;
use json_rpc::expect_call;



/// Utilities for mocking IDE components.
pub mod mock {
    use super::*;
    use double_representation::name::QualifiedName;

    /// Data used to create mock IDE components.
    ///
    /// Contains a number of constants and functions building more complex structures from them.
    /// The purpose is to allow different parts of tests that mock different models using
    /// consistent data.
    #[allow(missing_docs)]
    pub mod data {
        use super::*;

        use double_representation::name::QualifiedName;
        use engine_protocol::language_server::Position;
        use uuid::Uuid;

        pub const ROOT_ID: Uuid = Uuid::from_u128(100);
        pub const NAMESPACE_NAME: &str = "mock_namespace";
        pub const PROJECT_NAME: &str = "Mock_Project";
        pub const MODULE_NAME: &str = "Mock_Module";
        pub const CODE: &str = "main = \n    2 + 2";
        pub const DEFINITION_NAME: &str = "main";
        pub const TYPE_NAME: &str = "mock_namespace.MockProject.Mock_Module.Mock_Type";
        pub const MAIN_FINISH: Position = Position { line: 1, character: 9 };
        pub const CONTEXT_ID: Uuid = Uuid::from_u128(0xFE);

        pub fn module_path() -> crate::model::module::Path {
            crate::model::module::Path::from_name_segments(ROOT_ID, &[MODULE_NAME]).unwrap()
        }

        pub fn project_qualified_name() -> project::QualifiedName {
            project::QualifiedName::new(NAMESPACE_NAME, PROJECT_NAME)
        }

        pub fn module_qualified_name() -> QualifiedName {
            module_path().qualified_module_name(project_qualified_name())
        }

        pub fn definition_name() -> double_representation::definition::DefinitionName {
            double_representation::definition::DefinitionName::new_plain(DEFINITION_NAME)
        }

        pub fn graph_id() -> double_representation::graph::Id {
            double_representation::graph::Id::new_plain_name(DEFINITION_NAME)
        }

        pub fn foo_method_parameter() -> suggestion_database::entry::Argument {
            suggestion_database::entry::Argument {
                name:          "self".to_owned(),
                repr_type:     "Standard.Base".try_into().unwrap(),
                is_suspended:  false,
                has_default:   false,
                default_value: None,
                tag_values:    Vec::new(),
            }
        }

        pub fn foo_method_parameter2() -> suggestion_database::entry::Argument {
            suggestion_database::entry::Argument {
                name:          "param1".to_owned(),
                repr_type:     "Standard.Base.Number".try_into().unwrap(),
                is_suspended:  false,
                has_default:   false,
                default_value: None,
                tag_values:    Vec::new(),
            }
        }

        pub fn bar_method_parameter() -> suggestion_database::entry::Argument {
            suggestion_database::entry::Argument {
                name:          "self".to_owned(),
                repr_type:     "Other".to_owned(),
                is_suspended:  false,
                has_default:   false,
                default_value: None,
                tag_values:    Vec::new(),
            }
        }

        pub fn suggestion_entry_foo() -> suggestion_database::Entry {
            let project_name = project::QualifiedName::standard_base_library();
            let entry = suggestion_database::Entry::new_method(
                QualifiedName::new_main(project_name),
                "Standard.Base.Main".try_into().unwrap(),
                "foo",
                "Standard.Base.Any".try_into().unwrap(),
                true,
            );
            entry.with_arguments(vec![foo_method_parameter(), foo_method_parameter2()])
        }

        pub fn suggestion_entry_bar() -> suggestion_database::Entry {
            let project_name = project::QualifiedName::standard_base_library();
            let entry = suggestion_database::Entry::new_method(
                QualifiedName::new_main(project_name).new_child("Other"),
                "Standard.Base.Other".try_into().unwrap(),
                "bar",
                "Standard.Base.Any".try_into().unwrap(),
                true,
            );
            entry.with_arguments(vec![bar_method_parameter()])
        }
    }

    /// This mock data represents a rudimentary environment consisting of a project with a single
    /// module. The module contents is provided by default by [data::CODE], can be overwritten by
    /// calling [set_code] or [set_inline_code].
    #[derive(Clone, Debug)]
    pub struct Unified {
        pub project_name: project::QualifiedName,
        pub module_path:  model::module::Path,
        pub suggestions:  HashMap<suggestion_database::entry::Id, suggestion_database::Entry>,
        pub context_id:   model::execution_context::Id,
        pub parser:       parser::Parser,
        pub read_only:    Rc<Cell<bool>>,
        code:             String,
        id_map:           ast::IdMap,
        metadata:         crate::model::module::Metadata,
        root_definition:  double_representation::definition::DefinitionName,
    }

    impl Unified {
        pub fn set_inline_code(&mut self, code: impl AsRef<str>) {
            let method = self.method_pointer();
            self.code = format!("{} = {}", method.name, code.as_ref())
        }

        pub fn get_code(&self) -> &str {
            &self.code
        }

        pub fn set_code(&mut self, code: impl Into<String>) {
            self.code = code.into();
            self.id_map = default();
            self.metadata = default();
        }

        pub fn new() -> Self {
            use crate::test::mock::data::*;
            let mut suggestions = HashMap::new();
            suggestions.insert(1, suggestion_entry_foo());
            suggestions.insert(2, suggestion_entry_bar());
            Unified {
                suggestions,
                project_name: project_qualified_name(),
                module_path: module_path(),
                code: CODE.to_owned(),
                id_map: default(),
                metadata: default(),
                context_id: CONTEXT_ID,
                root_definition: definition_name(),
                parser: parser::Parser::new(),
                read_only: default(),
            }
        }

        pub fn undo_redo_manager(&self) -> Rc<undo_redo::Manager> {
            Rc::new(model::undo_redo::Manager::new())
        }

        pub fn module(&self, urm: Rc<undo_redo::Manager>) -> crate::model::Module {
            let ast = self.parser.parse_module(&self.code, self.id_map.clone()).unwrap();
            let path = self.module_path.clone();
            let metadata = self.metadata.clone();
            let repository = urm.repository.clone_ref();
            let module = Rc::new(model::module::Plain::new(
                path,
                ast,
                metadata,
                repository,
                self.read_only.clone_ref(),
            ));
            urm.module_opened(module.clone_ref());
            module
        }

        pub fn module_qualified_name(&self) -> QualifiedName {
            self.module_path.qualified_module_name(self.project_name.clone())
        }

        pub fn definition_id(&self) -> double_representation::definition::Id {
            double_representation::definition::Id::new_single_crumb(self.root_definition.clone())
        }

        pub fn method_pointer(&self) -> engine_protocol::language_server::MethodPointer {
            engine_protocol::language_server::MethodPointer {
                module:          self.module_qualified_name().to_string(),
                defined_on_type: self.module_qualified_name().to_string(),
                name:            self.root_definition.to_string(),
            }
        }

        /// Create a graph controller from the current mock data.
        pub fn graph(
            &self,
            module: model::Module,
            db: Rc<model::SuggestionDatabase>,
        ) -> controller::Graph {
            let parser = self.parser.clone_ref();
            let method = self.method_pointer();
            let definition =
                module.lookup_method(self.project_name.clone(), &method).expect("Lookup failed.");
            let project_name = self.project_name.clone_ref();
            controller::Graph::new(module, db, parser, definition, project_name)
                .expect("Graph could not be created")
        }

        pub fn execution_context(&self) -> Rc<model::execution_context::Plain> {
            Rc::new(model::execution_context::Plain::new(self.method_pointer()))
        }

        pub fn project(
            &self,
            urm: Rc<undo_redo::Manager>,
            module: model::Module,
            execution_context: model::ExecutionContext,
            suggestion_database: Rc<model::SuggestionDatabase>,
            json_client: language_server::MockClient,
            binary_client: binary::MockClient,
        ) -> model::Project {
            let mut project = model::project::MockAPI::new();
            model::project::test::expect_name(&mut project, &self.project_name.project);
            model::project::test::expect_qualified_name(&mut project, &self.project_name);
            model::project::test::expect_qualified_module_name(&mut project);
            model::project::test::expect_parser(&mut project, &self.parser);
            model::project::test::expect_module(&mut project, module);
            model::project::test::expect_execution_ctx(&mut project, execution_context);
            // Root ID is needed to generate module path used to get the module.
            model::project::test::expect_root_id(&mut project, crate::test::mock::data::ROOT_ID);
            model::project::test::expect_suggestion_db(&mut project, suggestion_database);
            model::project::test::expect_read_only(&mut project, self.read_only.clone_ref());
            let json_rpc = language_server::Connection::new_mock_rc(json_client);
            model::project::test::expect_json_rpc(&mut project, json_rpc);
            let binary_rpc = binary::Connection::new_mock_rc(binary_client);
            model::project::test::expect_binary_rpc(&mut project, binary_rpc);
            project.expect_urm().returning_st(move || urm.clone_ref());
            Rc::new(project)
        }

        pub fn ide(&self, project: &model::Project) -> controller::Ide {
            Rc::new(controller::ide::Plain::new(project.clone_ref()))
        }

        pub fn fixture(&self) -> Fixture {
            self.fixture_customize(|_, _, _| {})
        }

        pub fn fixture_customize<Fun>(&self, customize_rpc: Fun) -> Fixture
        where Fun: FnOnce(&Self, &mut language_server::MockClient, &mut binary::MockClient)
        {
            let mut json_client = language_server::MockClient::default();
            let mut binary_client = binary::MockClient::new();
            // Creating a searcher controller always triggers a query for completion.
            controller::searcher::test::expect_completion(&mut json_client, &[]);
            customize_rpc(self, &mut json_client, &mut binary_client);

            let urm = self.undo_redo_manager();
            let module = self.module(urm.clone());
            let suggestion_db =
                Rc::new(model::SuggestionDatabase::new_from_entries(&self.suggestions));
            let graph = self.graph(module.clone_ref(), suggestion_db.clone_ref());
            let execution = self.execution_context();
            let project = self.project(
                urm,
                module.clone_ref(),
                execution.clone_ref(),
                suggestion_db.clone_ref(),
                json_client,
                binary_client,
            );
            let ide = self.ide(&project);
            let executed_graph = controller::ExecutedGraph::new_internal(
                graph.clone_ref(),
                project.clone_ref(),
                execution.clone_ref(),
            );
            let mut executor = TestWithLocalPoolExecutor::set_up();
            let data = self.clone();
            let searcher_target = executed_graph.graph().nodes().unwrap().last().unwrap().id();
            let searcher_mode = controller::searcher::Mode::EditNode {
                original_node_id: searcher_target,
                edited_node_id:   default(),
            };
            let position_in_code = executed_graph.graph().definition_end_location().unwrap();
            let searcher = controller::Searcher::new_from_graph_controller(
                ide.clone_ref(),
                &project,
                executed_graph.clone_ref(),
                searcher_mode,
                enso_text::Byte(0),
                position_in_code,
            )
            .unwrap();
            let read_only = self.read_only.clone_ref();
            executor.run_until_stalled();
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
                ide,
                read_only,
            }
        }

        /// Register an expectation that the module described by this mock data will be opened.
        pub fn expect_opening_module(&self, client: &mut language_server::MockClient) {
            let content = self.code.clone();
            self.expect_opening_module_with_content(client, move || Ok(content))
        }

        pub fn expect_opening_module_with_content(
            &self,
            client: &mut language_server::MockClient,
            content_getter: impl FnOnce() -> json_rpc::Result<String> + 'static,
        ) {
            let expected_path = self.module_path.file_path().clone();
            client.expect.open_text_file(move |path| {
                assert_eq!(path, &expected_path);
                let content = content_getter()?;
                let current_version = Sha3_224::new(content.as_bytes());
                let write_capability =
                    Some(CapabilityRegistration::create_can_edit_text_file(expected_path));
                Ok(language_server::response::OpenTextFile {
                    write_capability,
                    content,
                    current_version,
                })
            });
        }

        /// Register an expectation that the module described by this mock data will be closed.
        pub fn expect_closing_module(
            &self,
            client: &mut engine_protocol::language_server::MockClient,
        ) {
            let path = self.module_path.file_path().clone();
            expect_call!(client.close_text_file(path=path) => Ok(()));
        }
    }

    impl Default for Unified {
        fn default() -> Self {
            Self::new()
        }
    }

    #[derive(Debug, Deref, DerefMut)]
    pub struct Fixture {
        pub data:           Unified,
        pub module:         model::Module,
        pub graph:          controller::Graph,
        pub execution:      Rc<model::execution_context::Plain>,
        pub executed_graph: controller::ExecutedGraph,
        pub suggestion_db:  Rc<model::SuggestionDatabase>,
        pub project:        model::Project,
        pub read_only:      Rc<Cell<bool>>,
        pub ide:            controller::Ide,
        pub searcher:       controller::Searcher,
        #[deref]
        #[deref_mut]
        pub executor:       TestWithLocalPoolExecutor, // Last to drop the executor as last.
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
            let parser = self.data.parser.clone();
            let path = self.data.module_path.clone();
            let ls = self.project.json_rpc();
            let repository = self.project.urm().repository.clone_ref();
            let ro = self.read_only.clone_ref();
            let module_future = model::module::Synchronized::open(path, ls, parser, repository, ro);
            // We can `expect_ready`, because in fact this is synchronous in test conditions.
            // (there's no real asynchronous connection beneath, just the `MockClient`)
            let module = module_future.boxed_local().expect_ready().unwrap();
            self.project.urm().module_opened(module.clone());
            module
        }

        /// Create a synchronized module model and a module controller paired with it.
        ///
        /// Same considerations need to be made as with `[synchronized_module]`.
        pub fn synchronized_module_w_controller(
            &self,
        ) -> (Rc<model::module::Synchronized>, controller::Module) {
            let model = self.synchronized_module();
            let controller = controller::module::Handle {
                language_server: self.project.json_rpc(),
                model:           model.clone(),
                parser:          self.data.parser.clone(),
                project_name:    self.project.qualified_name(),
            };
            (model, controller)
        }

        pub fn module_name(&self) -> QualifiedName {
            self.module.path().qualified_module_name(self.project.qualified_name())
        }
    }

    pub fn indent(line: impl AsRef<str>) -> String {
        format!("    {}", line.as_ref())
    }

    pub fn main_from_lines(lines: impl IntoIterator<Item: AsRef<str>>) -> String {
        def_from_lines("main", lines)
    }

    pub fn def_from_lines(
        name: impl Display,
        lines: impl IntoIterator<Item: AsRef<str>>,
    ) -> String {
        let body = lines.into_iter().map(indent).join("\n");
        format!("{name} =\n{body}")
    }
}

/// Check that given `CalledMethodInfo` is consistent with suggestion database `Entry`.
pub fn assert_call_info(
    info: span_tree::generate::context::CalledMethodInfo,
    entry: &model::suggestion_database::Entry,
) {
    assert_eq!(info.parameters.len(), entry.arguments.len());
    let parser = parser::Parser::new();
    let db = model::suggestion_database::SuggestionDatabase::new_empty();
    for (encountered, expected) in info.parameters.iter().zip(entry.arguments.iter()) {
        let in_module = default();
        let expected_info = model::suggestion_database::entry::to_span_tree_param(
            expected, &db, &parser, in_module,
        );
        assert_eq!(encountered, &expected_info);
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
#[derive(Clone, Copy, Debug, Default)]
pub struct Runner {
    /// Incremented each time when the runnee calls an interruption point.
    /// Reset to 0 after each run.
    current: u32,
    /// Bitmap that encodes behavior of subsequent `run_until_stalled` calls. True means running.
    seed:    BitField32,
}

impl Runner {
    fn new(seed: BitField32) -> Self {
        let current = 0;
        Self { current, seed }
    }

    /// Call's the fixture's `run_until_stalled`. Or does not call. Depends on the current seed
    /// (defined by the iteration number) and the number of previous calls to this method.
    ///
    /// See the `[Runner]` documentation.
    pub fn perhaps_run_until_stalled(&mut self, fixture: &mut crate::test::mock::Fixture) {
        let index = self.current;
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
    pub fn run(mut test: impl FnMut(&mut Runner)) {
        let count = Self::run_nth(0, &mut test);
        let possibilities = 2u32.pow(count);
        // Just to prevent accidentally generating too many runs.
        assert!(
            count < 5,
            "Consider reducing number of calls to `run_until_stalled` or bump this \
        limit if it doesn't cause slowdowns during the testing."
        );
        for i in 1..possibilities {
            Self::run_nth(i, &mut test);
        }
    }

    /// Calls the `test` function once. The executor behavior is defined by the `seed`.
    /// Returns the number of calls made to `perhaps_run_until_stalled`.
    pub fn run_with(seed: BitField32, mut test: impl FnMut(&mut Runner)) -> u32 {
        let mut runner = Runner::new(seed);
        test(&mut runner);
        runner.current
    }

    /// Calls the `test` function once. The executor behavior is defined by the `n` parameter.
    /// Returns the number of calls made to `perhaps_run_until_stalled`.
    pub fn run_nth(n: u32, test: impl FnMut(&mut Runner)) -> u32 {
        debug!("Runner: Iteration {}", n);
        Self::run_with(BitField32 { raw: n }, test)
    }
}
