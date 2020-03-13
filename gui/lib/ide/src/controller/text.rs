//! Text Controller module.
//!
//! Facade over filesystem API or module text API for text editor. Does discerning between Luna
//! module file and plain text file. In case of luna module idmap and metadata are hidden for the
//! user.

use crate::prelude::*;
use crate::controller::FallibleResult;
use crate::controller::notification;
use crate::executor::global::spawn;

use data::text::TextChange;
use file_manager_client as fmc;
use flo_stream::MessagePublisher;
use flo_stream::Subscriber;
use json_rpc::error::RpcError;
use shapely::shared;
use utils::channel::process_stream_with_handle;


// =======================
// === Text Controller ===
// =======================

/// A handle for file.
///
/// This makes distinction between module and plain text files. The module files are handled by
/// Module Controller, the plain text files are handled directly by File Manager Client.
#[derive(Clone,Debug)]
enum FileHandle {
    PlainText {path:fmc::Path, file_manager:fmc::Handle},
    Module    {controller:controller::module::Handle},
}


shared! { Handle

    /// Data stored by the text controller.
    #[derive(Debug)]
    pub struct Controller {
        file: FileHandle,
        /// Sink where we put events to be consumed by the view.
        notifications: notification::Publisher<notification::Text>,
    }

    impl {
        /// Get subscriber receiving controller's notifications.
        pub fn subscribe(&mut self) -> Subscriber<notification::Text> {
            self.notifications.subscribe()
        }

        /// Get clone of file path handled by this controller.
        pub fn file_path(&self) -> fmc::Path {
            match &self.file {
                FileHandle::PlainText{path,..} => path.clone(),
                FileHandle::Module{controller} => controller.location().to_path(),
            }
        }
    }
}

impl Handle {
    /// Create controller managing plain text file (which is not a module).
    pub fn new_for_plain_text(path:fmc::Path, file_manager:fmc::Handle) -> Self {
        Self::new(FileHandle::PlainText {path,file_manager})
    }
    /// Create controller managing Luna module file.
    pub fn new_for_module(controller:controller::module::Handle) -> Self {
        let text_notifications = controller.subscribe_text_notifications();
        let handle             = Self::new(FileHandle::Module {controller});
        let weak               = handle.downgrade();
        spawn(process_stream_with_handle(text_notifications,weak,|notification,this| {
            this.with_borrowed(move |data| data.notifications.publish(notification))
        }));
        handle
    }

    /// Read file's content.
    pub async fn read_content(&self) -> Result<String,RpcError> {
        use FileHandle::*;
        match self.file_handle() {
            PlainText {path,mut file_manager} => file_manager.read(path).await,
            Module    {controller}            => Ok(controller.code())
        }
    }

    /// Store the given content to file.
    pub fn store_content(&self, content:String) -> impl Future<Output=FallibleResult<()>> {
        let file_handle = self.file_handle();
        async move {
            match file_handle {
                FileHandle::PlainText {path,mut file_manager} => {
                    file_manager.write(path,content).await?
                },
                FileHandle::Module {controller} => {
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
    pub fn apply_text_change(&self, change:&TextChange) -> FallibleResult<()> {
        if let FileHandle::Module {controller} =  self.file_handle() {
            controller.apply_code_change(change)
        } else {
            Ok(())
        }
    }
}


// === Private functions ===

impl Handle {
    /// Create controller managing plain text file.
    fn new(file_handle:FileHandle) -> Self {
        let state = Controller {
            file          : file_handle,
            notifications : default(),
        };
        Self {rc:Rc::new(RefCell::new(state))}
    }

    fn file_handle(&self) -> FileHandle {
        self.with_borrowed(|state| state.file.clone())
    }
}


// === Test Utilities ===

#[cfg(test)]
impl Handle {
    /// Get FileManagerClient handle used by this controller.
    pub fn file_manager(&self) -> fmc::Handle {
        self.with_borrowed(|state| {
            match &state.file {
                FileHandle::PlainText {file_manager,..} => file_manager.clone_ref(),
                FileHandle::Module {..} =>
                    panic!("Cannot get FileManagerHandle from module file"),
            }
        })
    }
}



#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use data::text::Index;
    use json_rpc::test_util::transport::mock::MockTransport;
    use parser::Parser;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn passing_notifications_from_modue() {
        let mut test  = TestWithLocalPoolExecutor::set_up();
        test.run_test(async move {
            let fm         = file_manager_client::Handle::new(MockTransport::new());
            let loc        = controller::module::Location("test".to_string());
            let parser     = Parser::new().unwrap();
            let module_res = controller::module::Handle::new_mock(loc,"2+2",default(),fm,parser);
            let module     = module_res.unwrap();
            let controller = Handle::new_for_module(module.clone_ref());
            let mut sub    = controller.subscribe();

            module.apply_code_change(&TextChange::insert(Index::new(1),"2".to_string())).unwrap();
            assert_eq!(Some(notification::Text::Invalidate), sub.next().await);
        })
    }
}
