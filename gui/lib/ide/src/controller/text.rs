//! Text Controller module.
//!
//! Facade over filesystem API or module text API for text editor. Does discerning between Luna
//! module file and plain text file. In case of luna module idmap and metadata are hidden for the
//! user.

use crate::prelude::*;
use crate::controller::FallibleResult;

use data::text::TextChangedNotification;
use failure::_core::fmt::Formatter;
use failure::_core::fmt::Error;
use file_manager_client as fmc;
use flo_stream::MessagePublisher;
use flo_stream::Publisher;
use flo_stream::Subscriber;
use json_rpc::error::RpcError;
use shapely::shared;


// ====================
// === Notification ===
// ====================

/// A buffer size for notification publisher.
///
/// If Publisher buffer will be full, the thread sending next notification will be blocked until
/// all subscribers read message from buffer. We don't expect much traffic on file notifications,
/// therefore there is no need for setting big buffers.
const NOTIFICATION_BUFFER_SIZE : usize = 36;

/// A notification from TextController.
#[derive(Clone,Debug)]
pub enum Notification {
    /// File contents needs to be set to the following due to synchronization with external state.
    SetNewContent(String),
}



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
    pub struct Controller {
        file: FileHandle,
        /// Sink where we put events to be consumed by the view.
        notification_publisher: Publisher<Notification>,
    }

    impl {
        /// Get subscriber receiving controller's notifications.
        pub fn subscribe(&mut self) -> Subscriber<Notification> {
            self.notification_publisher.subscribe()
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
        Self::new(FileHandle::Module {controller})
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
    pub fn apply_text_change(&self, change:&TextChangedNotification) -> FallibleResult<()> {
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
            file                   : file_handle,
            notification_publisher : Publisher::new(NOTIFICATION_BUFFER_SIZE),
        };
        Self {rc:Rc::new(RefCell::new(state))}
    }

    fn file_handle(&self) -> FileHandle {
        self.with_borrowed(|state| state.file.clone())
    }
}


// === Debug implementations ===

impl Debug for Controller {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f,"Text Controller on {:?} }}",self.file)
    }
}

impl Debug for Handle {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        self.rc.borrow().fmt(f)
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
