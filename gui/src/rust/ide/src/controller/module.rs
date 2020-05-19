//! Module Controller.
//!
//! The module controller keeps cached module state (module state is AST+Metadata or equivalent),
//! and uses it for synchronizing state for text and graph representations. It provides method
//! for registering text and graph changes. If for example text represntation will be changed, there
//! will be notifications for both text change and graph change.

use crate::prelude::*;

use crate::controller::FilePath;
use crate::double_representation::text::apply_code_change_to_id_map;
use crate::model::synchronized::ExecutionContext;

use ast;
use ast::HasIdMap;
use data::text::*;
use double_representation as dr;
use enso_protocol::language_server;
use parser::Parser;
use failure::_core::fmt::Formatter;



// ==============
// === Errors ===
// ==============

/// Error returned when module path is invalid, i.e. cannot obtain module name from it.
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Invalid module path.")]
pub struct InvalidModulePath {}

/// Error returned when graph id invalid.
#[derive(Clone,Debug,Fail)]
#[fail(display="Invalid graph id: {:?}.",_0)]
pub struct InvalidGraphId(controller::graph::Id);



// ============
// === Path ===
// ============

/// Path identifying module's file in the Language Server.
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub struct Path {
    file_path : FilePath,
}

impl Path {
    /// Create a path from the file path. Returns None if given path is not a valid module file.
    pub fn from_file_path(file_path:FilePath) -> Option<Self> {
        let has_proper_ext   = file_path.extension() == Some(constants::LANGUAGE_FILE_EXTENSION);
        let capitalized_name = file_path.file_name()?.chars().next()?.is_uppercase();
        let is_module        = has_proper_ext && capitalized_name;
        is_module.and_option_from(|| Some(Path{file_path}))
    }

    /// Get the file path.
    pub fn file_path(&self) -> &FilePath {
        &self.file_path
    }

    /// Get the module name from path.
    ///
    /// The module name is a filename without extension.
    pub fn module_name(&self) -> &str {
        // The file stem existence should be checked during construction.
        self.file_path.file_stem().unwrap()
    }
    /// Create a module path consisting of a single segment, based on a given module name.
    #[cfg(test)]
    pub fn from_module_name(name:impl Str) -> Self {
        let name:String = name.into();
        let file_name   = format!("{}.{}",name,constants::LANGUAGE_FILE_EXTENSION);
        let file_path   = FilePath::new(default(),&[file_name]);
        Self::from_file_path(file_path).unwrap()
    }
}

impl TryFrom<FilePath> for Path {
    type Error = InvalidModulePath;

    fn try_from(value:FilePath) -> Result<Self, Self::Error> {
        Path::from_file_path(value).ok_or(InvalidModulePath{})
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.file_path, f)
    }
}



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
    pub model           : Rc<model::Module>,
    pub language_server : Rc<language_server::Connection>,
    pub parser          : Parser,
    pub logger          : Logger,
}

impl Handle {
    /// Create a module controller for given path.
    ///
    /// This function won't load module from file - it just get the state in `model` argument.
    pub fn new
    (path:Path, model:Rc<model::Module>, language_server:Rc<language_server::Connection>, parser:Parser)
    -> Self {
        let logger = Logger::new(format!("Module Controller {}", path));
        let path   = Rc::new(path);
        Handle {path,model,language_server,parser,logger}
    }

    /// Load or reload module content from file.
    pub async fn load_file(&self) -> FallibleResult<()> {
        self.logger.info(|| "Loading module file");
        let path    = self.path.file_path().clone();
        let content = self.language_server.client.read_file(&path).await?.contents;
        self.logger.info(|| "Parsing code");
        // TODO[ao] We should not fail here when metadata are malformed, but discard them and set
        //  default instead.
        let parsed = self.parser.parse_with_metadata(content)?;
        self.logger.info(|| "Code parsed");
        self.logger.trace(|| format!("The parsed ast is {:?}", parsed.ast));
        self.model.update_whole(parsed);
        Ok(())
    }

    /// Save the module to file.
    pub fn save_file(&self) -> impl Future<Output=FallibleResult<()>> {
        let path    = self.path.file_path().clone();
        let ls      = self.language_server.clone();
        let content = self.model.source_as_string();
        async move { Ok(ls.client.write_file(&path,&content?).await?) }
    }

    /// Updates AST after code change.
    ///
    /// May return Error when new code causes parsing errors, or when parsed code does not produce
    /// Module ast.
    pub fn apply_code_change(&self,change:&TextChange) -> FallibleResult<()> {
        let mut code         = self.code();
        let mut id_map       = self.model.ast().id_map();
        let replaced_size    = change.replaced.end - change.replaced.start;
        let replaced_span    = Span::new(change.replaced.start,replaced_size);
        let replaced_indices = change.replaced.start.value..change.replaced.end.value;

        code.replace_range(replaced_indices,&change.inserted);
        apply_code_change_to_id_map(&mut id_map,&replaced_span,&change.inserted);
        let ast = self.parser.parse(code, id_map)?.try_into()?;
        self.logger.trace(|| format!("Applied change; Ast is now {:?}", ast));
        self.model.update_ast(ast);

        Ok(())
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
            self.logger.error(|| format!("The module controller ast was not synchronized with \
                text editor content!\n >>> Module: {:?}\n >>> Editor: {:?}",my_code,code));
            let actual_ast = self.parser.parse(code,default())?.try_into()?;
            self.model.update_ast(actual_ast);
        }
        Ok(())
    }

    /// Returns a graph controller for graph in this module's subtree identified by `id`.
    pub fn graph_controller(&self, id:dr::graph::Id) -> FallibleResult<controller::Graph> {
        controller::Graph::new(self.model.clone_ref(), self.parser.clone_ref(), id)
    }

    /// Returns a executed graph controller for graph in this module's subtree identified by id.
    /// The execution context will be rooted at definition of this graph.
    ///
    /// This function wont check if the definition under id exists.
    pub async fn executed_graph_controller_unchecked
    (&self, id:dr::graph::Id) -> FallibleResult<controller::ExecutedGraph> {
        let definition_name = id.crumbs.last().cloned().ok_or_else(|| InvalidGraphId(id.clone()))?;
        let graph           = self.graph_controller_unchecked(id);
        let language_server = self.language_server.clone_ref();
        let path            = self.path.clone_ref();
        let execution_ctx   = ExecutionContext::create(language_server,path,definition_name).await?;
        Ok(controller::ExecutedGraph::new(graph,execution_ctx))
    }

    /// Returns a graph controller for graph in this module's subtree identified by `id` without
    /// checking if the graph exists.
    pub fn graph_controller_unchecked(&self, id:dr::graph::Id) -> controller::Graph {
        controller::Graph::new_unchecked(self.model.clone_ref(), self.parser.clone_ref(), id)
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
        let model  = Rc::new(model::Module::new(ast, default()));
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

    use crate::notification;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use ast;
    use ast::BlockLine;
    use ast::Ast;
    use data::text::Span;
    use parser::Parser;
    use uuid::Uuid;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[test]
    fn module_path_conversion() {
        let path = FilePath::new(default(), &["src","Main.enso"]);
        assert!(Path::from_file_path(path).is_some());

        let path = FilePath::new(default(), &["src","Main.txt"]);
        assert!(Path::from_file_path(path).is_none());

        let path = FilePath::new(default(), &["src","main.txt"]);
        assert!(Path::from_file_path(path).is_none());
    }

    #[wasm_bindgen_test]
    fn update_ast_after_text_change() {
        TestWithLocalPoolExecutor::set_up().run_task(async {
            let ls       = language_server::Connection::new_mock_rc(default());
            let parser   = Parser::new().unwrap();
            let location = Path::from_module_name("Test");

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

            let mut text_notifications  = controller.model.subscribe_text_notifications();
            let mut graph_notifications = controller.model.subscribe_graph_notifications();

            // Change code from "2+2" to "22+2"
            let change = TextChange::insert(Index::new(1),"2".to_string());
            controller.apply_code_change(&change).unwrap();
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

            // Check emitted notifications
            assert_eq!(Some(notification::Text::Invalidate ), text_notifications.next().await );
            assert_eq!(Some(notification::Graphs::Invalidate), graph_notifications.next().await);
        });
    }
}
