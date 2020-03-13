//! Module Controller.
//!
//! The module controller keeps cached module state (module state is AST+Metadata or equivalent),
//! and uses it for synchronizing state for text and graph representations. It provides method
//! for registering text and graph changes. If for example text represntation will be changed, there
//! will be notifications for both text change and graph change.

use crate::prelude::*;

use crate::controller::FallibleResult;
use crate::controller::notification;
use crate::double_representation::text::apply_code_change_to_id_map;
use crate::executor::global::spawn;

use parser::api::SourceFile;
use ast;
use ast::Ast;
use ast::HasRepr;
use ast::IdMap;
use data::text::*;
use file_manager_client as fmc;
use flo_stream::MessagePublisher;
use flo_stream::Subscriber;
use json_rpc::error::RpcError;
use parser::api::IsParser;
use parser::Parser;

use serde::Serialize;
use serde::Deserialize;
use shapely::shared;



// ==============
// == Metadata ==
// ==============

/// Mapping between ID and metadata.
#[derive(Debug,Clone,Default,Deserialize,Serialize)]
pub struct Metadata {
    /// Metadata used within ide.
    #[serde(default="default")]
    pub ide : serde_json::Value,
    #[serde(flatten)]
    /// Metadata of other users of SourceFile<Metadata> API.
    /// Ide should not modify this part of metadata.
    rest : serde_json::Value,
}

impl parser::api::Metadata for Metadata {}



// =======================
// === Module Location ===
// =======================

/// Structure uniquely identifying module location in the project.
/// Mappable to filesystem path.
#[derive(Clone,Debug,Display,Eq,Hash,PartialEq)]
pub struct Location(pub String);

impl Location {
    /// Get the module location from filesystem path. Returns None if path does not lead to
    /// module file.
    pub fn from_path(path:&fmc::Path) -> Option<Self> {
        // TODO [ao] See function `to_path`
        let fmc::Path(path_str) = path;
        let suffix = format!(".{}", constants::LANGUAGE_FILE_EXTENSION);
        path_str.ends_with(suffix.as_str()).and_option_from(|| {
            let cut_from = path_str.len() - suffix.len();
            Some(Location(path_str[..cut_from].to_string()))
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

shared! { Handle
    /// State data of the module controller.
    #[derive(Debug)]
    pub struct Controller {
        /// This module's location.
        location: Location,
        /// The current module used by synchronizing both module representations.
        module: SourceFile<Metadata>,
        /// The id map of current ast
        // TODO: written for test purposes, should be removed once generating id_map from AST will
        // be implemented.
        id_map: IdMap,
        /// The File Manager Client handle.
        file_manager: fmc::Handle,
        /// The Parser handle.
        parser: Parser,
        /// Publisher of "text changed" notifications
        text_notifications  : notification::Publisher<notification::Text>,
        /// Publisher of "graph changed" notifications
        graph_notifications : notification::Publisher<notification::Graph>,
        /// The logger handle.
        logger: Logger,
    }

    impl {
        /// Obtain clone of location.
        pub fn location(&self) -> Location {
            self.location.clone()
        }

        /// Updates AST after code change.
        pub fn apply_code_change(&mut self,change:&TextChange) -> FallibleResult<()> {
            let mut code         = self.code();
            let replaced_size    = change.replaced.end - change.replaced.start;
            let replaced_span    = Span::new(change.replaced.start,replaced_size);
            let replaced_indices = change.replaced.start.value..change.replaced.end.value;

            code.replace_range(replaced_indices,&change.inserted);
            apply_code_change_to_id_map(&mut self.id_map,&replaced_span,&change.inserted);
            let ast = self.parser.parse(code, self.id_map.clone())?;
            self.update_ast(ast);
            self.logger.trace(|| format!("Applied change; Ast is now {:?}", self.module.ast));

            Ok(())
        }

        /// Read module code.
        pub fn code(&self) -> String {
            self.module.ast.repr()
        }

        /// Check if current module state is synchronized with given code. If it's not, log error,
        /// and update module state to match the `code` passed as argument.
        pub fn check_code_sync(&mut self, code:String) -> FallibleResult<()> {
            let my_code = self.code();
            if code != my_code {
                self.logger.error(|| format!("The module controller ast was not synchronized with \
                    text editor content!\n >>> Module: {:?}\n >>> Editor: {:?}",my_code,code));
                self.module.ast = self.parser.parse(code,default())?;
                self.id_map     = default();
            }
            Ok(())
        }

        /// Get subscriber receiving notifications about changes in module's text representation.
        pub fn subscribe_text_notifications(&mut self) -> Subscriber<notification::Text> {
            self.text_notifications.subscribe()
        }

        /// Get subscriber receiving notifications about changes in module's graph representation.
        pub fn subscribe_graph_notifications(&mut self) -> Subscriber<notification::Graph> {
            self.graph_notifications.subscribe()
        }
    }
}

impl Handle {
    /// Create a module controller for given location.
    ///
    /// It may wait for module content, because the module must initialize its state.
    pub async fn new(location:Location, file_manager:fmc::Handle, parser:Parser)
    -> FallibleResult<Self> {
        let logger              = Logger::new(format!("Module Controller {}", location));
        let ast                 = Ast::new(ast::Module{lines:default()},None);
        let module              = SourceFile {ast, metadata:default()};
        let id_map              = default();
        let text_notifications  = default();
        let graph_notifications = default();

        let data = Controller {location,module,file_manager,parser,id_map,logger,text_notifications,
            graph_notifications};
        let handle = Handle::new_from_data(data);
        handle.load_file().await?;
        Ok(handle)
    }

    /// Load or reload module content from file.
    pub async fn load_file(&self) -> FallibleResult<()> {
        let (logger,path,mut fm,mut parser) = self.with_borrowed(|data| {
            ( data.logger.clone()
            , data.location.to_path()
            , data.file_manager.clone_ref()
            , data.parser.clone_ref()
            )
        });
        logger.info(|| "Loading module file");
        let content = fm.read(path).await?;
        logger.info(|| "Parsing code");
        let SourceFile{ast,metadata} = parser.parse_with_metadata(content)?;
        logger.info(|| "Code parsed");
        logger.trace(|| format!("The parsed ast is {:?}", ast));
        self.with_borrowed(|data| data.module.metadata = metadata);
        self.with_borrowed(|data| data.update_ast(ast));
        Ok(())
    }

    /// Save the module to file.
    pub fn save_file(&self) -> impl Future<Output=Result<(),RpcError>> {
        let (path,mut fm,code) = self.with_borrowed(|data| {
            let path = data.location.to_path();
            let fm   = data.file_manager.clone_ref();
            let code = String::try_from(&data.module);
            (path,fm,code)
        });
        async move { fm.write(path.clone(),code?).await }
    }

    #[cfg(test)]
    pub fn new_mock
    ( location     : Location
    , code         : &str
    , id_map       : IdMap
    , file_manager : fmc::Handle
    , mut parser   : Parser
    ) -> FallibleResult<Self> {
        let logger = Logger::new("Mocked Module Controller");
        let ast    = parser.parse(code.to_string(),id_map.clone())?;
        let module = SourceFile{ast, metadata:Metadata::default()};
        let text_notifications  = default();
        let graph_notifications = default();
        let data   = Controller {location,module,file_manager,parser,id_map,logger,
            text_notifications,graph_notifications};
        Ok(Handle::new_from_data(data))
    }
}

impl Controller {
    /// Update current ast in module controller and emit notification about overall invalidation.
    fn update_ast(&mut self,ast:Ast) {
        self.module.ast  = ast;
        let text_change  = notification::Text::Invalidate;
        let graph_change = notification::Graph::Invalidate;
        let code_notify  = self.text_notifications.publish(text_change);
        let graph_notify = self.graph_notifications.publish(graph_change);
        spawn(async move { futures::join!(code_notify,graph_notify); });
    }
}


#[cfg(test)]
mod test {
    use super::*;

    use crate::controller::notification;
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
        let module     = Path(format!("test.{}", constants::LANGUAGE_FILE_EXTENSION));
        let not_module = Path("test.txt".to_string());

        let expected_loc = Location("test".to_string());
        assert_eq!(Some(expected_loc),Location::from_path(&module    ));
        assert_eq!(None,              Location::from_path(&not_module));
    }

    #[wasm_bindgen_test]
    fn update_ast_after_text_change() {
        TestWithLocalPoolExecutor::set_up().run_test(async {
            let transport    = MockTransport::new();
            let file_manager = file_manager_client::Handle::new(transport);
            let parser       = Parser::new().unwrap();
            let location     = Location("Test".to_string());

            let uuid1        = Uuid::new_v4();
            let uuid2        = Uuid::new_v4();
            let uuid3        = Uuid::new_v4();
            let module       = "2+2";
            let id_map       = IdMap::new(vec!
                [ (Span::from((0,1)),uuid1.clone())
                , (Span::from((2,1)),uuid2)
                , (Span::from((0,3)),uuid3)
                ]);

            let controller   = Handle::new_mock
            (location,module,id_map,file_manager,parser).unwrap();

            let mut text_notifications  = controller.subscribe_text_notifications();
            let mut graph_notifications = controller.subscribe_graph_notifications();

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
            assert_eq!(expected_ast, controller.with_borrowed(|data| data.module.ast.clone()));

            // Check emitted notifications
            assert_eq!(Some(notification::Text::Invalidate ), text_notifications.next().await );
            assert_eq!(Some(notification::Graph::Invalidate), graph_notifications.next().await);
        });
    }
}
