//! The integration between EnsoGL File Browser and the Engine's file management API.

use crate::prelude::*;

use crate::controller::graph::NewNodeInfo;
use crate::controller::upload::pick_non_colliding_name;

use engine_protocol::language_server;
use engine_protocol::language_server::ContentRoot;
use engine_protocol::language_server::FileSystemObject;
use enso_frp as frp;
use ensogl_gui_components::file_browser::model::Entry;
use ensogl_gui_components::file_browser::model::EntryType;
use ensogl_gui_components::file_browser::model::FolderContent;
use ensogl_gui_components::file_browser::model::FolderType;
use json_rpc::error::RpcError;
use std::iter::once;


// =======================
// === Path Conversion ===
// =======================

// === Errors ===

#[derive(Clone, Debug, Fail)]
#[fail(display = "Invalid path received from File Browser Component: {}", path)]
struct InvalidPath {
    path: String,
}


// === Functions ===

/// Translate [`language_server::Path`] to path used by File Browser.
///
/// The path will be absolute, starting with "/". The first segment will be the content root id.
pub fn to_file_browser_path(path: &language_server::Path) -> std::path::PathBuf {
    let root_id_str = path.root_id.to_string();
    let segments_str = path.segments.iter().map(AsRef::<str>::as_ref);
    once("/").chain(once(root_id_str.as_ref())).chain(segments_str).collect()
}

/// Translate [`std::path::Path`] received from File Browser to a language server path.
///
/// The path should br absolute, starting with "/" and first segment should be a valid uuid of the
/// content root. The function returns `Err` if these conditions are not met.
///
/// Translating back path generated by [`to_file_browser_path`] must not fail.
pub fn from_file_browser_path(path: &std::path::Path) -> FallibleResult<language_server::Path> {
    use std::path::Component::*;
    let mut iter = path.components();
    match (iter.next(), iter.next()) {
        (Some(RootDir), Some(Normal(root_id))) => {
            let root_id = root_id.to_string_lossy().parse()?;
            Ok(language_server::Path::new(root_id, iter.map(|s| s.as_os_str().to_string_lossy())))
        }
        _ => {
            let path = path.to_string_lossy().to_string();
            Err(InvalidPath { path }.into())
        }
    }
}



// =================
// === Providers ===
// =================

// === FileProvider ===

/// The provider of the all content roots and their contents.
///
/// The content roots will be presented to the File Browser as a root directories. The paths
/// received from the Engine are translated accordingly - see [`to_file_browser_path`] and
/// [`from_file_browser_path`].
#[derive(Clone, Debug)]
pub struct FileProvider {
    connection:    Rc<language_server::Connection>,
    content_roots: Vec<Rc<ContentRoot>>,
}

impl FileProvider {
    /// Create provider of all content roots attached to given project.
    pub fn new(project: &model::Project) -> Self {
        Self { connection: project.json_rpc(), content_roots: project.content_roots() }
    }
}

impl FolderContent for FileProvider {
    fn request_entries(
        &self,
        entries_loaded: frp::Any<Rc<Vec<Entry>>>,
        _error_occurred: frp::Any<ImString>,
    ) {
        let entries = self.content_roots.iter().filter_map(|root| {
            let ls_path = language_server::Path::new_root(root.id());
            let path = to_file_browser_path(&ls_path);
            let (name, folder_type) = match &**root {
                language_server::ContentRoot::Project { .. } =>
                    Some(("Project".to_owned(), FolderType::Project)),
                language_server::ContentRoot::FileSystemRoot { path, .. } =>
                    Some((path.clone(), FolderType::Root)),
                language_server::ContentRoot::Home { .. } =>
                    Some(("Home".to_owned(), FolderType::Home)),
                language_server::ContentRoot::Library { .. } => None, /* We skip libraries, as */
                // they cannot be easily
                // inserted.
                language_server::ContentRoot::Custom { .. } => None, /* Custom content roots are
                                                                      * not used. */
            }?;
            let type_ = EntryType::Folder {
                type_:   folder_type,
                content: {
                    let connection = self.connection.clone_ref();
                    DirectoryView::new_from_root(connection, root.clone_ref()).into()
                },
            };
            Some(Entry { type_, name, path })
        });
        entries_loaded.emit(Rc::new(entries.sorted().collect_vec()));
    }
}


// === DirectoryView ===

/// A provider of the content of a specific directory.
#[derive(Clone, CloneRef, Debug)]
pub struct DirectoryView {
    connection:   Rc<language_server::Connection>,
    content_root: Rc<ContentRoot>,
    path:         Rc<language_server::Path>,
}

impl DirectoryView {
    /// Create a view of given Content Root content.
    pub fn new_from_root(
        connection: Rc<language_server::Connection>,
        content_root: Rc<ContentRoot>,
    ) -> Self {
        let path = Rc::new(language_server::Path::new_root(content_root.id()));
        Self { connection, content_root, path }
    }

    /// Returns a new view of the content of current's view subdirectory.
    pub fn sub_view(&self, sub_dir: impl Str) -> DirectoryView {
        DirectoryView {
            connection:   self.connection.clone_ref(),
            content_root: self.content_root.clone_ref(),
            path:         Rc::new(self.path.append_im(sub_dir)),
        }
    }

    /// Retrieves the directory content from the Engine.
    pub async fn get_entries_list(&self) -> Result<Vec<Entry>, RpcError> {
        let response = self.connection.file_list(&self.path).await?;
        let entries = response.paths.into_iter().map(|fs_obj| match fs_obj {
            FileSystemObject::Directory { name, path }
            | FileSystemObject::DirectoryTruncated { name, path }
            | FileSystemObject::SymlinkLoop { name, path, .. } => {
                let path = to_file_browser_path(&path).join(&name);
                let sub = self.sub_view(&name);
                let type_ =
                    EntryType::Folder { type_: FolderType::Standard, content: sub.into() };
                Entry { type_, name, path }
            }
            FileSystemObject::File { name, path } | FileSystemObject::Other { name, path } => {
                let path = to_file_browser_path(&path).join(&name);
                let type_ = EntryType::File;
                Entry { type_, name, path }
            }
        });
        Ok(entries.sorted().collect())
    }
}

impl FolderContent for DirectoryView {
    fn request_entries(
        &self,
        entries_loaded: frp::Any<Rc<Vec<Entry>>>,
        error_occurred: frp::Any<ImString>,
    ) {
        let this = self.clone_ref();
        executor::global::spawn(async move {
            match this.get_entries_list().await {
                Ok(entries) => entries_loaded.emit(Rc::new(entries)),
                Err(RpcError::RemoteError(error)) =>
                    error_occurred.emit(ImString::new(error.message)),
                Err(error) => error_occurred.emit(ImString::new(error.to_string())),
            }
        });
    }
}



// ====================
// === User Actions ===
// ====================

// === Errors ===

#[derive(Clone, Debug, Fail)]
#[fail(display = "Invalid source file for copy/move operation: {}", path)]
// the path is a String here, because there is no Display for path_buf.
struct InvalidSourceFile {
    path: String,
}


// === create_node_from_file ===

/// Create a node reading the given file.
///
/// The `path` should be a path convertible to Path from Language Server Protocol - see
/// [`from_file_browser_path`], otherwise function will return `Err`.
pub fn create_node_from_file(
    project: &model::Project,
    graph: &controller::Graph,
    path: &std::path::Path,
) -> FallibleResult {
    let ls_path = from_file_browser_path(path)?;
    let path_segments = ls_path.segments.into_iter().join("/");
    let content_root = project.content_root_by_id(ls_path.root_id)?;
    let path = match &*content_root {
        ContentRoot::Project { .. } => format!("Enso_Project.root/\"{}\"", path_segments),
        ContentRoot::FileSystemRoot { path, .. } => format!("\"{}/{}\"", path, path_segments),
        ContentRoot::Home { .. } => format!("File.home/\"{}\"", path_segments),
        ContentRoot::Library { namespace, name, .. } =>
            format!("{}.{}.Enso_Project.root / \"{}\"", namespace, name, path_segments),
        ContentRoot::Custom { .. } => "Unsupported Content Root".to_owned(),
    };
    let expression = format!("File.read {}", path);
    let node_info = NewNodeInfo::new_pushed_back(expression);
    graph.add_node(node_info)?;
    Ok(())
}


// === copy_or_move_file ===

/// A enum describing if we want to copy or move file.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug)]
pub enum FileOperation {
    Copy,
    Move,
}

impl Default for FileOperation {
    fn default() -> Self {
        Self::Copy
    }
}

impl FileOperation {
    /// Returns a verb describing the operation ("copy" or "move"), to be used in diagnostic
    /// messages.
    pub fn verb(&self) -> &'static str {
        match self {
            Self::Copy => "copy",
            Self::Move => "move",
        }
    }
}

/// Do copy or move file operation. The destination must be a directory. If the destination already
/// contains file with the same name as source's, the moved/copy file name will be altered
/// according to algorithm described in [`pick_non_colliding_name`] docs.
pub async fn do_file_operation(
    project: &model::Project,
    source: &std::path::Path,
    dest_dir: &std::path::Path,
    operation: FileOperation,
) -> FallibleResult {
    let json_rpc = project.json_rpc();
    let ls_source = from_file_browser_path(source)?;
    let ls_dest = from_file_browser_path(dest_dir)?;
    let src_name = ls_source
        .file_name()
        .ok_or_else(|| InvalidSourceFile { path: source.to_string_lossy().to_string() })?;
    let dest_name = pick_non_colliding_name(&*json_rpc, &ls_dest, src_name).await?;
    let dest_full = ls_dest.append_im(dest_name);
    match operation {
        FileOperation::Copy => json_rpc.copy_file(&ls_source, &dest_full).await?,
        FileOperation::Move => json_rpc.move_file(&ls_source, &dest_full).await?,
    }
    Ok(())
}
