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
    pub use enso_prelude::*;
    pub use enso_types::*;
}

use crate::prelude::*;

use enso_frp as frp;
use enso_web as web;
use enso_web::JsCast;
use ensogl_core::display::scene::Scene;
use ensogl_core::system::web::dom::WithKnownShape;



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
    reader:        Rc<Option<web::ReadableStreamDefaultReader>>,
}

impl File {
    /// Constructor from the [`web::File`].
    pub fn from_js_file(file: &web::File) -> Result<Self, web::JsValue> {
        let name = ImString::new(file.name());
        let size = file.size() as u64;
        let mime_type = ImString::new(file.type_());
        let reader: web::ReadableStreamDefaultReader = file.stream().get_reader().dyn_into()?;
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
            let js_result = wasm_bindgen_futures::JsFuture::from(reader.read()).await?;
            let is_done = js_sys::Reflect::get(&js_result, &"done".into())?.as_bool().unwrap();
            if is_done {
                Ok(None)
            } else {
                let chunk = js_sys::Reflect::get(&js_result, &"value".into())?;
                let data = chunk.dyn_into::<js_sys::Uint8Array>()?.to_vec();
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

#[derive(Clone, Debug, Default)]
/// The data emitted by the `files_received` frp endpoint.
pub struct DropEventData {
    /// The position of the drop event in the scene coordinates.
    pub position: Vector2,
    /// The dropped files.
    pub files:    Vec<File>,
}

/// The Manager of dropped files.
///
/// It adds listeners for drag and drop events to the target passed during construction. It provides
/// the single frp endpoints emitting a signal when a file is dropped.
// NOTE[allow_dead] We allow dead fields here, because they keep living closures and network.
#[derive(Debug)]
pub struct Manager {
    #[allow(dead_code)]
    network:        frp::Network,
    files_received: frp::Source<DropEventData>,
    #[allow(dead_code)]
    handlers:       [web::CleanupHandle; 2],
}

impl Manager {
    /// Constructor, adding listener to the given target.
    pub fn new(dom: &WithKnownShape<web::EventTarget>, scene: &Scene) -> Self {
        debug!("Creating.");
        let network = frp::Network::new("DropFileManager");
        frp::extend! { network
            files_received <- source();
        }

        let target: &web::EventTarget = dom.deref();
        let received = files_received.clone_ref();
        let scene = scene.clone_ref();
        let handlers = [
            web::add_event_listener(target, "drop", move |event| {
                let event: web::DragEvent = event.dyn_into().unwrap();
                debug!("Dropped files.");
                event.prevent_default();
                Self::handle_drop_event(event, &received, &scene);
            }),
            web::add_event_listener(target, "dragover", |e| e.prevent_default()),
        ];
        Self { network, files_received, handlers }
    }

    /// The frp endpoint emitting signal when a file is dropped.
    pub fn files_received(&self) -> &frp::Source<DropEventData> {
        &self.files_received
    }

    /// Retrieve the position of the drop event in the scene coordinates.
    fn event_position(scene: &Scene, event: &web::DragEvent) -> Vector2 {
        let dom: WithKnownShape<web::EventTarget> = scene.dom.root.clone_ref().into();
        let shape = dom.shape.value();
        let base = Vector2::new(0.0, shape.height);
        let position = Vector2::new(event.client_x() as f32, -event.client_y() as f32);
        let position = base + position;
        let center = scene.frp.shape.value().center();
        let position = position - center;
        scene.screen_to_scene_coordinates(Vector3::new(position.x, position.y, 0.0)).xy()
    }

    fn handle_drop_event(
        event: web::DragEvent,
        files_received: &frp::Source<DropEventData>,
        scene: &Scene,
    ) {
        let position = Self::event_position(scene, &event);
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
            let data = DropEventData { position, files: files_iter.collect_vec() };
            files_received.emit(data);
        }
    }
}
