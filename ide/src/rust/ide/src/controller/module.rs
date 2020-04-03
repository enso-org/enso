//! Module Controller.
//!
//! The module controller keeps cached module state (module state is AST+Metadata or equivalent),
//! and uses it for synchronizing state for text and graph representations. It provides method
//! for registering text and graph changes. If for example text represntation will be changed, there
//! will be notifications for both text change and graph change.

use crate::prelude::*;

use crate::double_representation::text::apply_code_change_to_id_map;

use ast;
use ast::HasIdMap;
use data::text::*;
use double_representation as dr;
use file_manager_client as fmc;
use parser::Parser;



// =======================
// === Module Location ===
// =======================

/// Structure uniquely identifying module location in the project.
/// Mappable to filesystem path.
#[derive(Clone,CloneRef,Debug,Display,Eq,Hash,PartialEq)]
pub struct Location(pub Rc<String>);

impl Location {
    /// Create new location from string.
    pub fn new(string:impl Str) -> Self {
        Location(Rc::new(string.into()))
    }

    /// Get the module location from filesystem path. Returns None if path does not lead to
    /// module file.
    pub fn from_path(path:&fmc::Path) -> Option<Self> {
        // TODO [ao] See function `to_path`
        let fmc::Path(path_str) = path;
        let suffix = format!(".{}", constants::LANGUAGE_FILE_EXTENSION);
        path_str.ends_with(suffix.as_str()).and_option_from(|| {
            let cut_from = path_str.len() - suffix.len();
            Some(Self::new(&path_str[..cut_from]))
        })
    }

    /// Obtains path (within a project context) to the file with this module.
    pub fn to_path(&self) -> file_manager_client::Path {
        // TODO [mwu] Extremely provisional. When multiple files support is
        //            added, needs to be fixed, if not earlier.
        let Location(string) = self;
        let result = format!("./{}.{}", string, constants::LANGUAGE_FILE_EXTENSION);
        file_manager_client::Path::new(result)
    }
}



// =========================
// === Module Controller ===
// =========================

/// A Handle for Module Controller
///
/// This struct contains all information and handles to do all module controller operations.
#[derive(Clone,CloneRef,Debug)]
pub struct Handle {
    /// This module's location.
    pub location : Location,
    /// The current state of module.
    pub model: Rc<model::Module>,
    /// The File Manager Client handle.
    pub file_manager : fmc::Handle,
    /// The Parser handle.
    parser : Parser,
    /// The logger handle.
    pub logger : Logger,
}

impl Handle {
    /// Create a module controller for given location.
    ///
    /// It may wait for module content, because the module must initialize its state.
    pub fn new
    (location:Location, model:Rc<model::Module>, file_manager:fmc::Handle, parser:Parser)
    -> Self {
        let logger = Logger::new(format!("Module Controller {}", location));
        Handle {location,model,file_manager,parser,logger}
    }

    /// Load or reload module content from file.
    pub async fn load_file(&self) -> FallibleResult<()> {
        self.logger.info(|| "Loading module file");
        let path    = self.location.to_path();
        let content = self.file_manager.read(path).await?;
        self.logger.info(|| "Parsing code");
        // TODO[ao] we should not fail here when metadata are malformed, but discard them and set
        // default instead.
        let parsed = self.parser.parse_with_metadata(content)?;
        self.logger.info(|| "Code parsed");
        self.logger.trace(|| format!("The parsed ast is {:?}", parsed.ast));
        self.model.update_whole(parsed);
        Ok(())
    }

    /// Save the module to file.
    pub fn save_file(&self) -> impl Future<Output=FallibleResult<()>> {
        let path    = self.location.to_path();
        let fm      = self.file_manager.clone_ref();
        let content = self.model.source_as_string();
        async move { Ok(fm.write(path,content?).await?) }
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

    /// Returns a graph controller for graph in this module's subtree identified by `id` without
    /// checking if the graph exists.
    pub fn graph_controller_unchecked(&self, id:dr::graph::Id) -> controller::Graph {
        controller::Graph::new_unchecked(self.model.clone_ref(), self.parser.clone_ref(), id)
    }

    #[cfg(test)]
    pub fn new_mock
    ( location     : Location
    , code         : &str
    , id_map       : ast::IdMap
    , file_manager : fmc::Handle
    , parser       : Parser
    ) -> FallibleResult<Self> {
        let logger = Logger::new("Mocked Module Controller");
        let ast    = parser.parse(code.to_string(),id_map.clone())?.try_into()?;
        let model  = Rc::new(model::Module::new(ast, default()));
        Ok(Handle {location,model,file_manager,parser,logger})
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
    use file_manager_client::Path;
    use json_rpc::test_util::transport::mock::MockTransport;
    use parser::Parser;
    use uuid::Uuid;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[test]
    fn get_location_from_path() {
        let module     = Path::new(format!("test.{}", constants::LANGUAGE_FILE_EXTENSION));
        let not_module = Path::new("test.txt");

        let expected_loc = Location::new("test");
        assert_eq!(Some(expected_loc),Location::from_path(&module    ));
        assert_eq!(None,              Location::from_path(&not_module));
    }

    #[wasm_bindgen_test]
    fn update_ast_after_text_change() {
        TestWithLocalPoolExecutor::set_up().run_task(async {
            let transport    = MockTransport::new();
            let file_manager = fmc::Handle::new(transport);
            let parser       = Parser::new().unwrap();
            let location     = Location::new("Test");

            let uuid1        = Uuid::new_v4();
            let uuid2        = Uuid::new_v4();
            let uuid3        = Uuid::new_v4();
            let module       = "2+2";
            let id_map       = ast::IdMap::new(vec!
                [ (Span::new(Index::new(0),Size::new(1)),uuid1.clone())
                , (Span::new(Index::new(2),Size::new(1)),uuid2)
                , (Span::new(Index::new(0),Size::new(3)),uuid3)
                ]);

            let controller   = Handle::new_mock
            (location,module,id_map,file_manager,parser).unwrap();

            let mut text_notifications  = controller.model.subscribe_text_notifications();
            let mut graph_notifications = controller.model.subscribe_graph_notifications();

            // Change code from "2+2" to "22+2"
            let change = TextChange::insert(Index::new(1),"2".to_string());
            controller.apply_code_change(&change).unwrap();
            let expected_ast = Ast::new(ast::Module {
                lines: vec![BlockLine {
                    elem: Some(Ast::new(ast::Infix {
                        larg : Ast::new(ast::Number{base:None, int:"22".to_string()}, Some(uuid1)),
                        loff : 0,
                        opr  : Ast::new(ast::Opr {name:"+".to_string()}, None),
                        roff : 0,
                        rarg : Ast::new(ast::Number{base:None, int:"2".to_string()}, Some(uuid2)),
                    }, Some(uuid3))),
                    off: 0
                }]
            }, None);
            assert_eq!(expected_ast, controller.model.ast().into());

            // Check emitted notifications
            assert_eq!(Some(notification::Text::Invalidate ), text_notifications.next().await );
            assert_eq!(Some(notification::Graphs::Invalidate), graph_notifications.next().await);
        });
    }
}
