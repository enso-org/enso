//! A Wrapper for module which synchronizes opening/closing and all changes with Language Server.

use crate::prelude::*;

use crate::model::module::Notification;

use enso_protocol::types::Sha3_224;
use enso_protocol::language_server;
use data::text::TextLocation;
use parser::api::SourceFile;
use parser::Parser;
use enso_protocol::language_server::TextEdit;



// =======================
// === Content Summary ===
// =======================

/// The minimal information about module's content, required to do properly invalidation of opened
/// module in Language Server.
#[derive(Clone,Debug,Eq,PartialEq)]
struct ContentSummary {
    digest      : Sha3_224,
    end_of_file : TextLocation,
}

/// The information about module's content. In addition to minimal summery defined in
/// `ContentSummary` it adds information about sections, what enables efficient updates after code
/// and metadata changes.
#[derive(Clone,Debug,Eq,PartialEq,Shrinkwrap)]
struct ParsedContentSummary {
    #[shrinkwrap(main_field)]
    summary  : ContentSummary,
    code     : Range<TextLocation>,
    id_map   : Range<TextLocation>,
    metadata : Range<TextLocation>,
}

impl ParsedContentSummary {
    /// Get summary from `SourceFile`.
    fn from_source(source:&SourceFile) -> Self {
        let summary = ContentSummary {
            digest      : Sha3_224::new(source.content.as_bytes()),
            end_of_file : TextLocation::at_document_end(&source.content)
        };
        ParsedContentSummary {
            summary,
            code        : TextLocation::convert_byte_range(&source.content,&source.code),
            id_map      : TextLocation::convert_byte_range(&source.content,&source.id_map),
            metadata    : TextLocation::convert_byte_range(&source.content,&source.metadata),
        }
    }
}

/// The information about state of the module currently held in LanguageServer.
#[derive(Clone,Debug)]
enum LanguageServerContent {
    /// The content is synchronized with our module state after last fully handled notification.
    Synchronized(ParsedContentSummary),
    /// The content is not synchronized with our module state after last fully handled notificaiton,
    /// probably due to connection error when sending update.
    Desynchronized(ContentSummary)
}

impl LanguageServerContent {
    fn summary(&self) -> &ContentSummary {
        match self {
            LanguageServerContent::Synchronized(content)   => &content.summary,
            LanguageServerContent::Desynchronized(content) => content,
        }
    }
}



// ===========================
// === Synchronized Module ===
// ===========================

/// A Module which state is synchronized with Language Server using its textual API.
///
/// This struct owns  `model::Module`, load the state during creation and updates LS about all
/// changes done to it. On drop the module is closed in Language Server.
///
/// See also (enso protocol documentation)
/// [https://github.com/luna/enso/blob/main/docs/language-server/protocol-language-server.md].
#[derive(Debug)]
pub struct Module {
    /// Path to the module file.
    pub path        : model::module::Path,
    /// The module handle.
    pub model       : model::Module,
    language_server : Rc<language_server::Connection>,
    logger          : Logger,
}


// === Public API ===

impl Module {
    /// Open the module.
    ///
    /// This function will open the module in Language Server and schedule task which will send
    /// updates about module's change to Language Server.
    pub async fn open
    ( path            : model::module::Path
    , language_server : Rc<language_server::Connection>
    , parser          : Parser
    ) -> FallibleResult<Rc<Self>> {
        let logger        = Logger::new(iformat!("Module {path}"));
        let file_path     = path.file_path().clone();
        info!(logger, "Opening module {file_path}");
        let opened = language_server.client.open_text_file(&file_path).await?;
        info!(logger, "Read content of the module {path}, digest is {opened.current_version:?}");
        let end_of_file = TextLocation::at_document_end(&opened.content);
        // TODO[ao] We should not fail here when metadata are malformed, but discard them and set
        //  default instead.
        let source  = parser.parse_with_metadata(opened.content)?;
        let digest  = opened.current_version;
        let summary = ContentSummary {digest,end_of_file};
        let model   = model::Module::new(source.ast,source.metadata);
        let this    = Rc::new(Module {path,model,language_server,logger});
        executor::global::spawn(Self::runner(this.clone_ref(),summary));
        Ok(this)
    }

    /// Create a module mock.
    pub fn mock(path:model::module::Path, model:model::Module) -> Rc<Self> {
        let logger = Logger::new(iformat!("Mocked Module {path}"));
        let client = language_server::MockClient::default();
        client.expect.close_text_file(|_| Ok(()));
        // We don't expect any other call, because we don't execute `runner()`.
        let language_server = language_server::Connection::new_mock_rc(client);
        Rc::new(Module{path,model,language_server,logger})
    }
}


// === Synchronizing Language Server ===

impl Module {
    /// The asynchronous task scheduled during struct creation which listens for all module changes
    /// and send proper updates to Language Server.
    async fn runner(self:Rc<Self>, initial_ls_content: ContentSummary) {
        let first_invalidation = self.full_invalidation(&initial_ls_content).await;
        let mut ls_content     = self.new_ls_content_info(initial_ls_content, first_invalidation);
        let mut subscriber     = self.model.subscribe();
        let weak               = Rc::downgrade(&self);
        drop(self);

        loop {
            let notification = subscriber.next().await;
            let this = weak.upgrade();
            match (notification,this) {
                (Some(notification),Some(this)) => {
                    debug!(this.logger,"Processing a notification: {notification:?}");
                    let result = this.handle_notification(&ls_content,notification).await;
                    ls_content = this.new_ls_content_info(ls_content.summary().clone(),result)
                }
                _ => break,
            }
        }
    }

    /// Get the updated Language Server content summary basing on result of some updating function
    /// (`handle_notification` or `full_invalidation`. If the result is Error, then we assume that
    /// any change was not applied to Language Server state, and mark the state as `Desynchronized`,
    /// so any new update attempt should perform full invalidation.
    fn new_ls_content_info
    (&self, old_content:ContentSummary, new_content:FallibleResult<ParsedContentSummary>)
    -> LanguageServerContent {
        match new_content {
            Ok(new_content) => LanguageServerContent::Synchronized(new_content),
            Err(err)        => {
                error!(self.logger,"Error during sending text change to Language Server: {err}");
                LanguageServerContent::Desynchronized(old_content)
            }
        }
    }

    /// Send to LanguageServer update about received notification about module. Returns the new
    /// content summery of Language Server state.
    async fn handle_notification
    (&self, content:&LanguageServerContent, notification:Notification)
    -> FallibleResult<ParsedContentSummary> {
        debug!(self.logger,"Handling notification: {content:?}.");
        match content {
            LanguageServerContent::Desynchronized(summary) => self.full_invalidation(summary).await,
            LanguageServerContent::Synchronized(summary)   => match notification {
                Notification::Invalidate => self.full_invalidation(&summary.summary).await,
                Notification::CodeChanged{change,replaced_location} =>
                    self.notify_language_server(&summary.summary, |content| {
                        let code_change = TextEdit {
                            range : replaced_location.into(),
                            text  : change.inserted,
                        };
                        let id_map_change = TextEdit {
                            range : summary.id_map.clone().into(),
                            text  : content.id_map_slice().to_string(),
                        };
                        //id_map goes first, because code change may alter it's position.
                        vec![id_map_change,code_change]
                    }).await,
                Notification::MetadataChanged =>
                    self.notify_language_server(&summary.summary, |content| vec![TextEdit {
                        range : summary.metadata.clone().into(),
                        text  : content.metadata_slice().to_string(),
                    }]).await,
            },
        }
    }

    /// Send update to Language Server with the entire file content. Returns the new content summary
    /// of Language Server state.
    async fn full_invalidation
    (&self, ls_content:&ContentSummary) -> FallibleResult<ParsedContentSummary> {
        debug!(self.logger,"Handling full invalidation: {ls_content:?}.");
        let range = TextLocation::at_document_begin()..ls_content.end_of_file;
        self.notify_language_server(ls_content,|content| vec![TextEdit {
            range : range.into(),
            text  : content.content
        }]).await
    }

    /// This is a helper function with all common logic regarding sending the update to
    /// Language Server. Returns the new summary of Language Server state.
    async fn notify_language_server
    ( &self
    , ls_content        : &ContentSummary
    , edits_constructor : impl FnOnce(SourceFile) -> Vec<TextEdit>
    ) -> FallibleResult<ParsedContentSummary> {
        let content = self.model.serialized_content()?;
        let summary = ParsedContentSummary::from_source(&content);
        let edit    = language_server::types::FileEdit {
            path        : self.path.file_path().clone(),
            edits       : edits_constructor(content),
            old_version : ls_content.digest.clone(),
            new_version : summary.digest.clone()
        };
        debug!(self.logger,"Notifying LS with edit: {edit:?}.");
        self.language_server.client.apply_text_file_edit(&edit).await?;
        Ok(summary)
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        let file_path       = self.path.file_path().clone();
        let language_server = self.language_server.clone_ref();
        let logger          = self.logger.clone_ref();
        executor::global::spawn(async move {
            let result = language_server.client.close_text_file(&file_path).await;
            if let Err(err) = result {
                error!(logger,"Error when closing module file {file_path}: {err}");
            }
        });
    }
}

impl Deref for Module {
    type Target = model::Module;

    fn deref(&self) -> &Self::Target {
        &self.model
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
pub mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use data::text::TextChange;
    use data::text;
    use enso_protocol::language_server::CapabilityRegistration;
    use json_rpc::error::RpcError;
    use json_rpc::expect_call;
    use utils::test::ExpectTuple;
    use wasm_bindgen_test::wasm_bindgen_test;



    /// A helper structure for setting up the Language Server Mock Client calls during opening
    /// and invalidating module file.
    ///
    /// When setting new calls (e.g. using `expect_invalidate` function) it assumes the Language
    /// Server state with all previous;y set calls successfully applied.
    struct LsClientSetup {
        file_path          : language_server::Path,
        current_ls_code    : Rc<CloneCell<String>>,
        current_ls_version : Rc<CloneCell<Sha3_224>>,
        client             : language_server::MockClient,
    }

    impl LsClientSetup {
        fn new(file_path:language_server::Path, initial_content:impl Str) -> Self {
            let initial_content = initial_content.into();
            let initial_version = Sha3_224::new(initial_content.as_bytes());
            let client          = language_server::MockClient::default();

            let capability = CapabilityRegistration::create_can_edit_text_file(file_path.clone());
            let open_resp  = language_server::response::OpenTextFile {
                write_capability : Some(capability),
                content          : initial_content.clone(),
                current_version  : initial_version.clone(),
            };
            expect_call!(client.open_text_file(path=file_path.clone()) => Ok(open_resp));

            let current_ls_code    = Rc::new(CloneCell::new(initial_content));
            let current_ls_version = Rc::new(CloneCell::new(initial_version));
            LsClientSetup { file_path,client,current_ls_code,current_ls_version}
        }

        fn expect_edit<EditApplier>(&self, result:json_rpc::Result<()>, edit_applier:EditApplier)
        where EditApplier : FnOnce(&[TextEdit]) -> String + 'static {
            let path       = self.file_path.clone();
            let ls_version = self.current_ls_version.clone_ref();
            let ls_code    = self.current_ls_code.clone_ref();
            self.client.expect.apply_text_file_edit(move |edit| {
                let new_content = edit_applier(&edit.edits);
                // check the requested edit content:
                assert_eq!(edit.path       , path);
                assert_eq!(edit.old_version, ls_version.get());
                assert_eq!(edit.new_version, Sha3_224::new(new_content.as_bytes()));
                // Update state:
                if result.is_ok() {
                    ls_code.set(new_content);
                    ls_version.set(edit.new_version.clone());
                }
                result
            });
        }

        fn expect_invalidate(&self, result:json_rpc::Result<()>) {
            let end_of_file = TextLocation::at_document_end(self.current_ls_code.get());
            self.expect_edit(result, move |edits| {
                let (edit,)      = edits.iter().expect_tuple();
                let expected_range = language_server::types::TextRange {
                    start : language_server::types::Position { line:0,character:0  },
                    end   : end_of_file.into(),
                };
                assert_eq!(edit.range, expected_range);
                edit.text.clone()
            });
        }

        fn finish(self) -> Rc<language_server::Connection> {
            let client = self.client;
            expect_call!(client.close_text_file(path=self.file_path) => Ok(()));
            language_server::Connection::new_mock_rc(client)
        }
    }

    #[wasm_bindgen_test]
    fn handling_notifications() {
        let path            = model::module::Path::from_mock_module_name("TestModule");
        let parser          = Parser::new_or_panic();
        let initial_content = "main =\n    println \"Hello World!\"";

        let setup           = LsClientSetup::new(path.file_path().clone(),initial_content);
        setup.expect_invalidate(Ok(()));
        setup.expect_invalidate(Ok(()));
        setup.expect_edit(Ok(()), |edits| {
            // Check that it's not invalidate:
            assert_ne!(edits[0].range.start, TextLocation::at_document_begin().into());
            "main =\n    println \"Test 2\"".to_string()
        });
        let connection                             = setup.finish();
        let mut test                               = TestWithLocalPoolExecutor::set_up();
        let module:Rc<RefCell<Option<Rc<Module>>>> = default();
        let module_ref1                            = module.clone();
        let module_ref2                            = module.clone();
        let module_ref3                            = module.clone();
        test.run_task(async move {
            let module = Module::open(path,connection,Parser::new_or_panic()).await.unwrap();
            *module_ref1.borrow_mut() = Some(module);
        });
        test.when_stalled(move || {
            let module_ref  = module_ref2.borrow_mut();
            let module      = module_ref.as_ref().unwrap();
            let new_content = "main =\n    println \"Test\"".to_string();
            let new_ast     = parser.parse_module(new_content.clone(),default()).unwrap();
            module.update_ast(new_ast);
        });
        test.when_stalled(move || {
            let module_ref = module_ref3.borrow_mut();
            let module     = module_ref.as_ref().unwrap();
            let change     = TextChange {
                replaced : text::Index::new(20)..text::Index::new(24),
                inserted : "Test 2".to_string(),
            };
            module.apply_code_change(change,&Parser::new_or_panic(),default()).unwrap()
        });
        test.when_stalled(move || *module.borrow_mut() = None);
    }

    #[wasm_bindgen_test]
    fn handling_notification_after_failure() {
        let path            = model::module::Path::from_mock_module_name("TestModule");
        let initial_content = "main =\n    println \"Hello World!\"";

        let setup           = LsClientSetup::new(path.file_path().clone(),initial_content);
        setup.expect_invalidate(Err(RpcError::LostConnection));
        setup.expect_invalidate(Ok(()));
        let connection                             = setup.finish();
        let mut test                               = TestWithLocalPoolExecutor::set_up();
        let module:Rc<RefCell<Option<Rc<Module>>>> = default();
        let module_ref1                            = module.clone();
        let module_ref2                            = module.clone();
        test.run_task(async move {
            let module = Module::open(path,connection,Parser::new_or_panic()).await.unwrap();
            *module_ref1.borrow_mut() = Some(module);
        });
        test.when_stalled(move || {
            let module_ref = module_ref2.borrow_mut();
            let module     = module_ref.as_ref().unwrap();
            let change     = TextChange {
                replaced : text::Index::new(20)..text::Index::new(24),
                inserted : "Test 2".to_string(),
            };
            module.apply_code_change(change,&Parser::new_or_panic(),default()).unwrap()
        });
        test.when_stalled(move || *module.borrow_mut() = None);
    }
}
