//! A debug scene which shows the file browser. The selected and chosen entries are logged on the
//! console.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::system::web;
use ensogl_gui_components::file_browser::*;
use ensogl_gui_components::file_browser::model::*;
use ensogl_gui_components::file_browser::model::Entry;
use ensogl_text_msdf_sys::run_once_initialized;
use ensogl_theme as theme;
use wasm_bindgen::prelude::*;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_file_browser() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
        init(&app);
        mem::forget(app);
    });
}



// ====================
// === Mock Content ===
// ====================

// === Shortcuts ===

fn folder(name:&str, content:impl FolderContent+'static) -> Entry {
    Entry {
        name: name.to_string(),
        path: name.into(),
        type_: EntryType::Folder {
            type_: FolderType::Standard,
            content: content.into(),
        }
    }
}

fn file(name:&str) -> Entry {
    Entry {
        name: name.to_string(),
        path: name.into(),
        type_: EntryType::File
    }
}


// === MockFolderContent ===

#[derive(Debug,Clone)]
struct MockFolderContent {
    entries: Rc<Vec<Entry>>
}

impl MockFolderContent {
    fn new(entries: Vec<Entry>) -> Self {
        Self { entries:Rc::new(entries) }
    }
}

impl FolderContent for MockFolderContent {
    fn request_entries
    (&self, entries_loaded: frp::Any<Rc<Vec<Entry>>>, _error_occurred: frp::Any<ImString>) {
        entries_loaded.emit(self.entries.clone());
    }
}


// === GeneratedFolderContent ===

#[derive(Debug)]
struct GeneratedFolderContent;

impl FolderContent for GeneratedFolderContent {
    fn request_entries
    (&self, entries_loaded: frp::Any<Rc<Vec<Entry>>>, _error_occurred: frp::Any<ImString>) {
        entries_loaded.emit(
            Rc::new((0..20).map(|i|
                folder(format!("Folder {}",i).as_str(),GeneratedFolderContent)
            ).collect_vec())
        );
    }
}


// === ErrorContent ===

#[derive(Debug)]
struct ErrorContent;

impl FolderContent for ErrorContent {
    fn request_entries
    (&self, _entries_loaded: frp::Any<Rc<Vec<Entry>>>, error_occurred: frp::Any<ImString>) {
        error_occurred.emit(ImString::new("Could not open folder"));
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app:&Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let file_browser = app.new_view::<FileBrowser>();
    let fs = MockFolderContent::new(vec![
        Entry {
            name: "Project's Data".to_string(),
            path: "Project's Data".into(),
            type_: EntryType::Folder {
                type_: FolderType::Project,
                content: MockFolderContent::new(vec![]).into()
            },
        },
        Entry {
            name: "Home".to_string(),
            path: "Home".into(),
            type_: EntryType::Folder {
                type_: FolderType::Home,
                content: MockFolderContent::new(vec![
                    folder("Applications", EmptyFolderContent),
                    folder("Desktop", EmptyFolderContent),
                    folder("Documents", EmptyFolderContent),
                    folder("Downloads", EmptyFolderContent),
                    folder("Enso", GeneratedFolderContent),
                    folder("Movies", EmptyFolderContent),
                    folder("Music", EmptyFolderContent),
                    folder("Pictures", EmptyFolderContent),
                    folder("Public", EmptyFolderContent),
                    folder("Error", ErrorContent),
                    file("File 1"),
                    file("File 2"),
                    file("File 3"),
                    file("File 4"),
                    file("File 5"),
                    file("File 6"),
                    file("File 7"),
                    file("File 8"),
                    file("File 9"),
                ]).into()
            },
        },
        Entry {
            name: "Root".to_string(),
            path: "Root".into(),
            type_: EntryType::Folder {
                type_: FolderType::Root,
                content: MockFolderContent::new(vec![]).into()
            },
        },
    ]);
    file_browser.set_content(AnyFolderContent::from(fs));
    app.display.add_child(&file_browser);

    let network = enso_frp::Network::new("test");
    enso_frp::extend! {network
        trace file_browser.entry_chosen;
        trace file_browser.entry_selected;
        trace file_browser.copy;
        trace file_browser.cut;
        trace file_browser.paste_into;
    }

    std::mem::forget(file_browser);
    std::mem::forget(network);
}
