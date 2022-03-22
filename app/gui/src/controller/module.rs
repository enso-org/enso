//! Module Controller.

use crate::prelude::*;

use crate::model::module::Path;
use crate::model::module::TextChange;

use ast;
use ast::HasIdMap;
use double_representation::module;
use double_representation::project;
use double_representation::text::apply_code_change_to_id_map;
use engine_protocol::language_server;
use engine_protocol::types::Sha3_224;
use parser::Parser;



// ==============
// === Errors ===
// ==============

/// Error returned when graph id invalid.
#[derive(Clone, Debug, Fail)]
#[fail(display = "Invalid graph id: {:?}.", _0)]
pub struct InvalidGraphId(controller::graph::Id);



// =========================
// === Module Controller ===
// =========================

/// A Handle for Module Controller.
///
/// This struct contains all information and handles to do all module controller operations.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Handle {
    pub model:           model::Module,
    pub language_server: Rc<language_server::Connection>,
    pub parser:          Parser,
    pub logger:          Logger,
}

impl Handle {
    /// Create a module controller for given path.
    #[profile(Task)]
    pub async fn new(
        parent: impl AnyLogger,
        path: Path,
        project: &dyn model::project::API,
    ) -> FallibleResult<Self> {
        let logger = Logger::new_sub(parent, format!("Module Controller {}", path));
        let model = project.module(path).await?;
        let language_server = project.json_rpc();
        let parser = project.parser();
        Ok(Handle { model, language_server, parser, logger })
    }

    /// Save the module to file.
    pub fn save_file(&self) -> impl Future<Output = FallibleResult> {
        let content = self.model.serialized_content();
        let path = self.model.path().clone_ref();
        let ls = self.language_server.clone_ref();
        async move {
            let version = Sha3_224::new(content?.content.as_bytes());
            Ok(ls.client.save_text_file(path.file_path(), &version).await?)
        }
    }

    /// Updates AST after code change.
    ///
    /// May return Error when new code causes parsing errors, or when parsed code does not produce
    /// Module ast.
    pub fn apply_code_change(&self, change: TextChange) -> FallibleResult {
        let mut id_map = self.model.ast().id_map();
        apply_code_change_to_id_map(&mut id_map, &change, &self.model.ast().repr());
        self.model.apply_code_change(change, &self.parser, id_map)
    }

    /// Read module code.
    pub fn code(&self) -> String {
        self.model.ast().repr()
    }

    /// Check if current module state is synchronized with given code. If it's not, log error,
    /// and update module state to match the `code` passed as argument.
    pub fn check_code_sync(&self, code: String) -> FallibleResult {
        let my_code = self.code();
        if code != my_code {
            error!(
                self.logger,
                "The module controller ast was not synchronized with text editor \
                content!\n >>> Module: {my_code}\n >>> Editor: {code}"
            );
            let actual_ast = self.parser.parse(code, default())?.try_into()?;
            self.model.update_ast(actual_ast)?;
        }
        Ok(())
    }

    /// Returns a graph controller for graph in this module's subtree identified by `id`.
    pub fn graph_controller(
        &self,
        id: double_representation::graph::Id,
        suggestion_db: Rc<model::SuggestionDatabase>,
    ) -> FallibleResult<controller::Graph> {
        controller::Graph::new(
            &self.logger,
            self.model.clone_ref(),
            suggestion_db,
            self.parser.clone_ref(),
            id,
        )
    }

    /// Returns a graph controller for graph in this module's subtree identified by `id` without
    /// checking if the graph exists.
    pub fn graph_controller_unchecked(
        &self,
        id: double_representation::graph::Id,
        suggestion_db: Rc<model::SuggestionDatabase>,
    ) -> controller::Graph {
        controller::Graph::new_unchecked(
            &self.logger,
            self.model.clone_ref(),
            suggestion_db,
            self.parser.clone_ref(),
            id,
        )
    }

    /// Get the module's qualified name.
    pub fn qualified_name(&self, project_name: project::QualifiedName) -> module::QualifiedName {
        module::QualifiedName::new(project_name, self.model.id())
    }

    /// Modify module by modifying its `Info` description (which is a wrapper directly over module's
    /// AST).
    pub fn modify<R>(&self, f: impl FnOnce(&mut module::Info) -> R) -> FallibleResult<R> {
        let mut module = self.module_info();
        let ret = f(&mut module);
        self.model.update_ast(module.ast)?;
        Ok(ret)
    }

    /// Obtains the `Info` value describing this module's AST.
    pub fn module_info(&self) -> module::Info {
        let ast = self.model.ast();
        double_representation::module::Info { ast }
    }

    /// Adds a new import to the module.
    ///
    /// May create duplicate entries if such import was already present.
    pub fn add_import(&self, target: &module::QualifiedName) -> FallibleResult {
        let import = module::ImportInfo::from_qualified_name(target);
        self.modify(|info| info.add_import(&self.parser, import))?;
        Ok(())
    }

    /// Removes an import declaration that brings given target.
    ///
    /// Fails, if there was no such declaration found.
    pub fn remove_import(&self, target: &module::QualifiedName) -> FallibleResult {
        let import = module::ImportInfo::from_qualified_name(target);
        self.modify(|info| info.remove_import(&import))?
    }

    /// Retrieve a vector describing all import declarations currently present in the module.
    pub fn imports(&self) -> Vec<module::ImportInfo> {
        let module = self.module_info();
        module.iter_imports().collect()
    }

    /// Creates a mocked module controller.
    pub fn new_mock(
        path: Path,
        code: &str,
        id_map: ast::IdMap,
        language_server: Rc<language_server::Connection>,
        parser: Parser,
        repository: Rc<model::undo_redo::Repository>,
    ) -> FallibleResult<Self> {
        let logger = Logger::new("Mocked Module Controller");
        let ast = parser.parse(code.to_string(), id_map)?.try_into()?;
        let metadata = default();
        let model = Rc::new(model::module::Plain::new(&logger, path, ast, metadata, repository));
        Ok(Handle { model, language_server, parser, logger })
    }

    #[cfg(test)]
    pub fn expect_code(&self, expected_code: impl Str) {
        let code = self.code();
        assert_eq!(code, expected_code.as_ref());
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use ast;
    use ast::Ast;
    use ast::BlockLine;
    use enso_text::traits::*;
    use parser::Parser;
    use uuid::Uuid;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn update_ast_after_text_change() {
        TestWithLocalPoolExecutor::set_up().run_task(async {
            let ls = language_server::Connection::new_mock_rc(default());
            let parser = Parser::new().unwrap();
            let location = Path::from_mock_module_name("Test");
            let code = "2+2";
            let uuid1 = Uuid::new_v4();
            let uuid2 = Uuid::new_v4();
            let uuid3 = Uuid::new_v4();
            let uuid4 = Uuid::new_v4();
            let id_map = ast::IdMap::new(vec![
                ((0.bytes()..1.bytes()).into(), uuid1),
                ((1.bytes()..2.bytes()).into(), uuid2),
                ((2.bytes()..3.bytes()).into(), uuid3),
                ((0.bytes()..3.bytes()).into(), uuid4),
            ]);
            let controller =
                Handle::new_mock(location, code, id_map, ls, parser, default()).unwrap();

            // Change code from "2+2" to "22+2"
            let change = enso_text::Change::inserted(0.bytes(), "2".to_string());
            controller.apply_code_change(change).unwrap();
            let expected_ast = Ast::new_no_id(ast::Module {
                lines: vec![BlockLine {
                    elem: Some(Ast::new(
                        ast::Infix {
                            larg: Ast::new(
                                ast::Number { base: None, int: "22".to_string() },
                                Some(uuid1),
                            ),
                            loff: 0,
                            opr:  Ast::new(ast::Opr { name: "+".to_string() }, Some(uuid2)),
                            roff: 0,
                            rarg: Ast::new(
                                ast::Number { base: None, int: "2".to_string() },
                                Some(uuid3),
                            ),
                        },
                        Some(uuid4),
                    )),
                    off:  0,
                }],
            });
            assert_eq!(expected_ast, controller.model.ast().into());
        });
    }
}
