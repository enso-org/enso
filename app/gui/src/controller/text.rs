//! Text Controller module.
//!
//! Facade over filesystem API or module text API for text editor. Does discerning between a module
//! file and plain text file. In case of the module, idmap and metadata are hidden for the user.

use crate::prelude::*;

use crate::controller::FilePath;
use crate::model::module::TextChange;

use engine_protocol::language_server;
use json_rpc::error::RpcError;
use std::pin::Pin;



// ====================
// === Notification ===
// ====================

/// A notification about changes of file content.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Notification {
    /// The content should be fully reloaded.
    Invalidate,
}



// =======================
// === Text Controller ===
// =======================

/// A handle for file.
///
/// This makes distinction between module and plain text files. The module files are handled by
/// Module Controller, the plain text files are handled directly by File Manager Client.
#[derive(Clone, CloneRef, Debug)]
enum FileHandle {
    PlainText { path: Rc<FilePath>, language_server: Rc<language_server::Connection> },
    Module { controller: controller::Module },
}

/// A Text Controller Handle.
///
/// This struct contains all information and handles to do all module controller operations.
#[derive(Clone, CloneRef, Debug)]
pub struct Handle {
    logger: Logger,
    file:   FileHandle,
}

impl Handle {
    /// Create a Text Controller for file.
    ///
    /// This constructor checks what kind of file we read, and load it as a module file or plain
    /// text file.
    pub async fn new(
        parent: impl AnyLogger,
        project: &model::Project,
        path: FilePath,
    ) -> FallibleResult<Self> {
        let logger = Logger::new_sub(parent, format!("Text Controller {}", path));
        let file = if let Ok(path) = model::module::Path::from_file_path(path.clone()) {
            FileHandle::Module {
                controller: controller::Module::new(logger.clone_ref(), path, &**project).await?,
            }
        } else {
            FileHandle::PlainText {
                path:            Rc::new(path),
                language_server: project.json_rpc(),
            }
        };
        Ok(Self { logger, file })
    }

    /// Get clone of file path handled by this controller.
    pub fn file_path(&self) -> &FilePath {
        match &self.file {
            FileHandle::PlainText { path, .. } => &*path,
            FileHandle::Module { controller } => controller.model.path().file_path(),
        }
    }

    /// Read file's content.
    pub async fn read_content(&self) -> Result<String, RpcError> {
        use FileHandle::*;
        match &self.file {
            PlainText { path, language_server } => {
                let response = language_server.read_file(path).await;
                response.map(|response| response.contents)
            }
            Module { controller } => Ok(controller.code()),
        }
    }

    /// Store the given content to file.
    pub fn store_content(&self, content: String) -> impl Future<Output = FallibleResult> {
        let file_handle = self.file.clone_ref();
        async move {
            match file_handle {
                FileHandle::PlainText { path, language_server } =>
                    language_server.write_file(&path, &content).await?,
                FileHandle::Module { controller } => {
                    controller.check_code_sync(content)?;
                    controller.save_file().await?
                }
            }
            Ok(())
        }
    }

    /// Apply text change.
    ///
    /// This function should be called by view on every user interaction changing the text content
    /// of file. It will e.g. update the Module Controller state and notify other views about
    /// update in case of module files.
    pub fn apply_text_change(&self, change: TextChange) -> FallibleResult {
        if let FileHandle::Module { controller } = &self.file {
            controller.apply_code_change(change)
        } else {
            Ok(())
        }
    }

    /// Get a stream of text changes notifications.
    pub fn subscribe(&self) -> Pin<Box<dyn Stream<Item = Notification>>> {
        match &self.file {
            FileHandle::PlainText { .. } => StreamExt::boxed(futures::stream::empty()),
            FileHandle::Module { controller } => {
                let subscriber = controller.model.subscribe();
                subscriber.filter_map(Self::map_module_notification).boxed()
            }
        }
    }

    async fn map_module_notification(
        notification: model::module::Notification,
    ) -> Option<Notification> {
        match notification.kind {
            model::module::NotificationKind::Invalidate
            | model::module::NotificationKind::CodeChanged { .. } => Some(Notification::Invalidate),
            model::module::NotificationKind::MetadataChanged => None,
        }
    }
}



// === Test Utilities ===

#[cfg(test)]
impl Handle {
    /// Get Language Server RPC Client used by this controller.
    pub fn language_server(&self) -> Rc<language_server::Connection> {
        match &self.file {
            FileHandle::PlainText { language_server, .. } => language_server.clone_ref(),
            FileHandle::Module { controller } => controller.language_server.clone_ref(),
        }
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use enso_text::traits::*;
    use parser::Parser;
    use wasm_bindgen_test::wasm_bindgen_test;

    fn setup_mock_project(setup: impl FnOnce(&mut model::project::MockAPI)) -> model::Project {
        let json_client = language_server::MockClient::default();
        let ls = engine_protocol::language_server::Connection::new_mock_rc(json_client);
        let ls_clone = ls.clone_ref();
        let mut project = model::project::MockAPI::new();
        setup(&mut project);
        project.expect_json_rpc().returning_st(move || ls_clone.clone_ref());
        Rc::new(project)
    }

    #[wasm_bindgen_test]
    fn passing_notifications_from_module() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let ls = language_server::Connection::new_mock_rc(default());
            let path = model::module::Path::from_mock_module_name("Test");
            let parser = Parser::new().unwrap();
            let module_res =
                controller::Module::new_mock(path, "main = 2+2", default(), ls, parser, default());
            let module = module_res.unwrap();
            let controller = Handle {
                logger: Logger::new("Test text controller"),
                file:   FileHandle::Module { controller: module.clone() },
            };
            let mut sub = controller.subscribe();

            let change = enso_text::Change::inserted(8.bytes(), "2".to_string());
            module.apply_code_change(change).unwrap();
            assert_eq!(Some(Notification::Invalidate), sub.next().await);
        })
    }

    #[wasm_bindgen_test]
    fn obtain_plain_text_controller() {
        TestWithLocalPoolExecutor::set_up().run_task(async move {
            let project = setup_mock_project(|_| {});
            let root_id = default();
            let path = FilePath::new(root_id, &["TestPath"]);
            let another_path = FilePath::new(root_id, &["TestPath2"]);
            let log = Logger::new("Test");
            let text_ctrl = Handle::new(&log, &project, path.clone()).await.unwrap();
            let another_ctrl = Handle::new(&log, &project, another_path.clone()).await.unwrap();

            assert!(Rc::ptr_eq(&another_ctrl.language_server(), &text_ctrl.language_server()));
            assert!(Rc::ptr_eq(&another_ctrl.language_server(), &project.json_rpc()));
            assert!(Rc::ptr_eq(&another_ctrl.language_server(), &project.json_rpc()));
            assert_eq!(path, *text_ctrl.file_path());
            assert_eq!(another_path, *another_ctrl.file_path());
        });
    }

    #[wasm_bindgen_test]
    fn obtain_text_controller_for_module() {
        let parser = parser::Parser::new_or_panic();
        TestWithLocalPoolExecutor::set_up().run_task(async move {
            let code = "2 + 2".to_string();
            let undo = default();
            let module = model::module::test::MockData { code, ..default() }.plain(&parser, undo);
            let module_clone = module.clone_ref();
            let project = setup_mock_project(move |project| {
                model::project::test::expect_module(project, module_clone);
                model::project::test::expect_parser(project, &parser);
            });
            let file_path = module.path().file_path();
            let log = Logger::new("Test");
            let text_ctrl = controller::Text::new(&log, &project, file_path.clone()).await.unwrap();
            let content = text_ctrl.read_content().await.unwrap();
            assert_eq!("2 + 2", content.as_str());
        });
    }
}
