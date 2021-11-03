//! The handlers for the files dropped on the web scene. The main object is [`Manager`]:
//! it notifies about new files, and their metadata and with methods for reading them.

use crate::prelude::*;

use crate::stream::BlobExt;
use crate::stream::ReadableStreamDefaultReader;
use crate::Error;

use enso_frp as frp;
use js_sys::Uint8Array;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
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
    pub fn from_js_file(file: &web_sys::File) -> Result<Self, Error> {
        let name = ImString::new(file.name());
        let size = file.size() as u64;
        let mime_type = ImString::new(file.type_());
        let blob = AsRef::<web_sys::Blob>::as_ref(file);
        let reader = blob.stream_reader()?;
        let reader = Rc::new(Some(reader));
        Ok(File { name, mime_type, size, reader })
    }

    /// Read the next chunk of file content.
    ///
    /// If there is no more data, it returns [`None`].
    ///
    /// The chunk size depend on the browser implementation, but it is assumed to be reasonable.
    /// See https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamDefaultReader/read and
    /// https://github.com/w3c/FileAPI/issues/144#issuecomment-570982732.
    pub async fn read_chunk(&self) -> Result<Option<Vec<u8>>, Error> {
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
    network:            frp::Network,
    files_received:     frp::Source<Vec<File>>,
    #[allow(dead_code)]
    drop_callback:      Rc<DropClosure>,
    #[allow(dead_code)]
    drag_over_callback: Rc<DragOverClosure>,
}

impl Manager {
    /// Constructor, adding listener to the given target.
    pub fn new(target: &web_sys::EventTarget) -> Self {
        let logger = Logger::new("DropFileManager");
        debug!(logger, "Creating DropFileManager");
        let network = frp::Network::new("DropFileManager");
        frp::extend! { network
            files_received <- source();
        }

        let drop: DropClosure =
            Closure::wrap(Box::new(f!([logger,files_received](event:web_sys::DragEvent) {
                debug!(logger, "Dropped files.");
                event.prevent_default();
                Self::handle_drop_event(&logger,event,&files_received)
            })));
        // To mark element as a valid drop target, the `dragover` event handler should return
        // `false`. See
        // https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/File_drag_and_drop#define_the_drop_zone
        let drag_over: DragOverClosure = Closure::wrap(Box::new(|event: web_sys::DragEvent| {
            event.prevent_default();
            false
        }));
        let drop_js = drop.as_ref().unchecked_ref();
        let drag_over_js = drag_over.as_ref().unchecked_ref();
        target.add_event_listener_with_callback("drop", drop_js).unwrap();
        target.add_event_listener_with_callback("dragover", drag_over_js).unwrap();
        let drop_callback = Rc::new(drop);
        let drag_over_callback = Rc::new(drag_over);
        Self { network, files_received, drop_callback, drag_over_callback }
    }

    /// The frp endpoint emitting signal when a file is dropped.
    pub fn files_received(&self) -> &frp::Source<Vec<File>> {
        &self.files_received
    }

    fn handle_drop_event(
        logger: &Logger,
        event: web_sys::DragEvent,
        files_received: &frp::Source<Vec<File>>,
    ) {
        let opt_files = event.data_transfer().and_then(|t| t.files());
        if let Some(js_files) = opt_files {
            let js_files_iter = (0..js_files.length()).filter_map(|i| js_files.get(i));
            let files_iter = js_files_iter.filter_map(|f| match File::from_js_file(&f) {
                Ok(file) => Some(file),
                Err(err) => {
                    error!(logger, "Error when processing dropped file: {err:?}");
                    None
                }
            });
            files_received.emit(files_iter.collect_vec());
        }
    }
}
