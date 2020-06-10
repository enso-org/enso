//! Module Controller.
//!
//! The module controller keeps cached module state (module state is AST+Metadata or equivalent),
//! and uses it for synchronizing state for text and graph representations. It provides method
//! for registering text and graph changes. If for example text represntation will be changed, there
//! will be notifications for both text change and graph change.

use crate::prelude::*;

use crate::double_representation::text::apply_code_change_to_id_map;
use crate::model::module::Path;

use ast;
use ast::HasIdMap;
use data::text::*;
use double_representation as dr;
use enso_protocol::language_server;
use enso_protocol::types::Sha3_224;
use parser::Parser;



// ==============
// === Errors ===
// ==============

/// Error returned when graph id invalid.
#[derive(Clone,Debug,Fail)]
#[fail(display="Invalid graph id: {:?}.",_0)]
pub struct InvalidGraphId(controller::graph::Id);



// =========================
// === Module Controller ===
// =========================

/// A Handle for Module Controller.
///
/// This struct contains all information and handles to do all module controller operations.
#[allow(missing_docs)]
#[derive(Clone,CloneRef,Debug)]
pub struct Handle {
    pub path            : Rc<Path>,
    pub model           : Rc<model::synchronized::Module>,
    pub language_server : Rc<language_server::Connection>,
    pub parser          : Parser,
    pub logger          : Logger,
}

impl Handle {
    /// Create a module controller for given path.
    ///
    /// This function won't load module from file - it just get the state in `model` argument.
    pub fn new
    ( parent          : impl AnyLogger
    , path            : Path
    , model           : Rc<model::synchronized::Module>
    , language_server : Rc<language_server::Connection>
    , parser          : Parser
    ) -> Self {
        let logger = Logger::sub(parent,format!("Module Controller {}", path));
        let path   = Rc::new(path);
        Handle {path,model,language_server,parser,logger}
    }

    /// Save the module to file.
    pub fn save_file(&self) -> impl Future<Output=FallibleResult<()>> {
        let content = self.model.serialized_content();
        let path    = self.path.clone_ref();
        let ls      = self.language_server.clone();
        async move {
            let version = Sha3_224::new(content?.content.as_bytes());
            Ok(ls.client.save_text_file(path.file_path(),&version).await?)
        }
    }

    /// Updates AST after code change.
    ///
    /// May return Error when new code causes parsing errors, or when parsed code does not produce
    /// Module ast.
    pub fn apply_code_change(&self,change:TextChange) -> FallibleResult<()> {
        let mut id_map       = self.model.ast().id_map();
        let replaced_size    = change.replaced.end - change.replaced.start;
        let replaced_span    = Span::new(change.replaced.start,replaced_size);

        apply_code_change_to_id_map(&mut id_map,&replaced_span,&change.inserted);
        self.model.apply_code_change(change,&self.parser,id_map)
    }

    /// Read module code.
    pub fn code(&self) -> String {
        self.model.ast().repr()
    }

    /// Check if current module state is synchronized with given code. If it's not, log error,
    /// and update module state to match the `code` passed as argument.
    pub fn check_code_sync(&self, code:String) -> FallibleResult<()> {
        let my_code = self.code();
        if code != my_code {
            error!(self.logger,"The module controller ast was not synchronized with text editor \
                content!\n >>> Module: {my_code}\n >>> Editor: {code}");
            let actual_ast = self.parser.parse(code,default())?.try_into()?;
            self.model.update_ast(actual_ast);
        }
        Ok(())
    }

    /// Returns a graph controller for graph in this module's subtree identified by `id`.
    pub fn graph_controller(&self, id:dr::graph::Id) -> FallibleResult<controller::Graph> {
        controller::Graph::new(&self.logger, self.model.clone_ref(), self.parser.clone_ref(), id)
    }

    /// Returns a executed graph controller for graph in this module's subtree identified by id.
    /// The execution context will be rooted at definition of this graph.
    ///
    /// This function wont check if the definition under id exists.
    pub async fn executed_graph_controller_unchecked
    (&self, id:dr::graph::Id, project:&controller::Project)
    -> FallibleResult<controller::ExecutedGraph> {
        let definition_name = id.crumbs.last().cloned().ok_or_else(|| InvalidGraphId(id.clone()))?;
        let graph           = self.graph_controller_unchecked(id);
        let path            = self.path.clone_ref();
        let execution_ctx   = project.create_execution_context(path,definition_name).await?;
        Ok(controller::ExecutedGraph::new(graph,execution_ctx))
    }

    /// Returns a graph controller for graph in this module's subtree identified by `id` without
    /// checking if the graph exists.
    pub fn graph_controller_unchecked(&self, id:dr::graph::Id) -> controller::Graph {
        controller::Graph::new_unchecked(&self.logger, self.model.clone_ref(),
                                         self.parser.clone_ref(), id)
    }

    #[cfg(test)]
    pub fn new_mock
    ( path            : Path
    , code            : &str
    , id_map          : ast::IdMap
    , language_server : Rc<language_server::Connection>
    , parser          : Parser
    ) -> FallibleResult<Self> {
        let logger = Logger::new("Mocked Module Controller");
        let ast    = parser.parse(code.to_string(),id_map.clone())?.try_into()?;
        let model  = model::Module::new(ast, default());
        let model  = model::synchronized::Module::mock(path.clone(),model);
        let path   = Rc::new(path);
        Ok(Handle {path,model,language_server,parser,logger})
    }

    #[cfg(test)]
    pub fn expect_code(&self, expected_code:impl Str) {
        let code = self.code();
        assert_eq!(code,expected_code.as_ref());
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
    use ast::BlockLine;
    use ast::Ast;
    use data::text::Span;
    use parser::Parser;
    use uuid::Uuid;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn update_ast_after_text_change() {
        TestWithLocalPoolExecutor::set_up().run_task(async {
            let ls       = language_server::Connection::new_mock_rc(default());
            let parser   = Parser::new().unwrap();
            let location = Path::from_mock_module_name("Test");

            let uuid1    = Uuid::new_v4();
            let uuid2    = Uuid::new_v4();
            let uuid3    = Uuid::new_v4();
            let uuid4    = Uuid::new_v4();
            let module   = "2+2";
            let id_map   = ast::IdMap::new(vec!
                [ (Span::new(Index::new(0),Size::new(1)),uuid1)
                , (Span::new(Index::new(1),Size::new(1)),uuid2)
                , (Span::new(Index::new(2),Size::new(1)),uuid3)
                , (Span::new(Index::new(0),Size::new(3)),uuid4)
                ]);

            let controller = Handle::new_mock(location,module,id_map,ls,parser).unwrap();

            // Change code from "2+2" to "22+2"
            let change = TextChange::insert(Index::new(1),"2".to_string());
            controller.apply_code_change(change).unwrap();
            let expected_ast = Ast::new_no_id(ast::Module {
                lines: vec![BlockLine {
                    elem: Some(Ast::new(ast::Infix {
                        larg : Ast::new(ast::Number{base:None, int:"22".to_string()}, Some(uuid1)),
                        loff : 0,
                        opr  : Ast::new(ast::Opr {name:"+".to_string()}, Some(uuid2)),
                        roff : 0,
                        rarg : Ast::new(ast::Number{base:None, int:"2".to_string()}, Some(uuid3)),
                    }, Some(uuid4))),
                    off: 0
                }]
            });
            assert_eq!(expected_ast, controller.model.ast().into());
        });
    }
}
