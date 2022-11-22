//! The handlers for the files dropped on the web scene. The main object is [`Manager`]:
//! it notifies about new files, and their metadata and with methods for reading them.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]



/// Commonly used utilities.
pub mod prelude {
    pub use enso_logger::DefaultWarningLogger as Logger;
    pub use enso_logger::*;
    pub use enso_prelude::*;
}

use crate::prelude::*;

use enso_frp as frp;
use enso_web as web;
use enso_web::stream::BlobExt;
use enso_web::stream::ReadableStreamDefaultReader;
use enso_web::Closure;

#[cfg(target_arch = "wasm32")]
use enso_web::JsCast;
#[cfg(target_arch = "wasm32")]
use js_sys::Uint8Array;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen_futures::JsFuture;



// ============
// === File ===
// ============

/// The dropped file.
///
/// This structure contains some basic metadata (the public fields) and allows asynchronous way
/// to read the file content chunk by chunk.
///
/// The default structure represents a file with no name and no content.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Default, Derivative)]
#[derivative(Debug)]
pub struct File {
    pub name:      ImString,
    pub mime_type: ImString,
    pub size:      u64,
    #[derivative(Debug = "ignore")]
    reader:        Rc<Option<ReadableStreamDefaultReader>>,
}

impl File {
    /// Constructor from the [`web_sys::File`].
    pub fn from_js_file(file: &web_sys::File) -> Result<Self, web::JsValue> {
        let name = ImString::new(file.name());
        let size = file.size() as u64;
        let mime_type = ImString::new(file.type_());
        let blob = AsRef::<web_sys::Blob>::as_ref(file);
        let reader = blob.stream_reader()?;
        let reader = Rc::new(Some(reader));
        Ok(File { name, mime_type, size, reader })
    }

    #[cfg(target_arch = "wasm32")]
    /// Read the next chunk of file content.
    ///
    /// If there is no more data, it returns [`None`].
    ///
    /// The chunk size depend on the browser implementation, but it is assumed to be reasonable.
    /// See https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamDefaultReader/read and
    /// https://github.com/w3c/FileAPI/issues/144#issuecomment-570982732.
    pub async fn read_chunk(&self) -> Result<Option<Vec<u8>>, web::JsValue> {
        if let Some(reader) = &*self.reader {
            let js_result = JsFuture::from(reader.read()).await?;
            let is_done = js_sys::Reflect::get(&js_result, &"done".into())?.as_bool().unwrap();
            if is_done {
                Ok(None)
            } else {
                let chunk = js_sys::Reflect::get(&js_result, &"value".into())?;
                let data = chunk.dyn_into::<Uint8Array>()?.to_vec();
                Ok(Some(data))
            }
        } else {
            Ok(None)
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    /// Read the next chunk of file content.
    pub async fn read_chunk(&self) -> Result<Option<Vec<u8>>, web::JsValue> {
        Ok(None)
    }
}



// =======================
// === DropFileManager ===
// =======================

type DropClosure = Closure<dyn Fn(web_sys::DragEvent)>;
type DragOverClosure = Closure<dyn Fn(web_sys::DragEvent) -> bool>;

/// The Manager of dropped files.
///
/// It adds listeners for drag and drop events to the target passed during construction. It provides
/// the single frp endpoints emitting a signal when a file is dropped.
// NOTE[allow_dead] We allow dead fields here, because they keep living closures and network.
#[derive(Clone, CloneRef, Debug)]
pub struct Manager {
    #[allow(dead_code)]
    network:          frp::Network,
    files_received:   frp::Source<Vec<File>>,
    #[allow(dead_code)]
    drop_handle:      web::EventListenerHandle,
    #[allow(dead_code)]
    drag_over_handle: web::EventListenerHandle,
}

impl Manager {
    /// Constructor, adding listener to the given target.
    pub fn new(target: &enso_web::EventTarget) -> Self {
        debug!("Creating.");
        let network = frp::Network::new("DropFileManager");
        frp::extend! { network
            files_received <- source();
        }

        let drop: DropClosure = Closure::new(f!([files_received](event:web_sys::DragEvent) {
            debug!("Dropped files.");
            event.prevent_default();
            Self::handle_drop_event(event,&files_received)
        }));
        // To mark element as a valid drop target, the `dragover` event handler should return
        // `false`. See
        // https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/File_drag_and_drop#define_the_drop_zone
        let drag_over: DragOverClosure = Closure::new(|event: web_sys::DragEvent| {
            event.prevent_default();
            false
        });
        let drop_handle = web::add_event_listener(target, "drop", drop);
        let drag_over_handle = web::add_event_listener(target, "dragover", drag_over);
        Self { network, files_received, drop_handle, drag_over_handle }
    }

    /// The frp endpoint emitting signal when a file is dropped.
    pub fn files_received(&self) -> &frp::Source<Vec<File>> {
        &self.files_received
    }

    fn handle_drop_event(event: web_sys::DragEvent, files_received: &frp::Source<Vec<File>>) {
        let opt_files = event.data_transfer().and_then(|t| t.files());
        if let Some(js_files) = opt_files {
            let js_files_iter = (0..js_files.length()).filter_map(|i| js_files.get(i));
            let files_iter = js_files_iter.filter_map(|f| match File::from_js_file(&f) {
                Ok(file) => Some(file),
                Err(err) => {
                    error!("Error when processing dropped file: {err:?}.");
                    None
                }
            });
            files_received.emit(files_iter.collect_vec());
        }
    }
}
