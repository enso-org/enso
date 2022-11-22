//! This module describes the model structure for the [`FileBrowser`] component. The core is
//! [`AnyFolderContent`], which can be passed to the `FileBrowser` to open the folder.

use crate::prelude::*;

use enso_frp as frp;
use std::cmp::Ordering;
use std::path::PathBuf;



// =============
// === Model ===
// =============

// === Entry ===

/// The type of a folder. This is used to distinguish standard folders from the different kinds of
/// content roots.
#[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub enum FolderType {
    /// A normal sufolder in the file system.
    Standard,
    /// The projects root.
    Project,
    /// The users home folder.
    Home,
    /// The operating system's global root folder.
    Root,
    /// The root of a library.
    Library,
    /// A custom kind of content root.
    Custom,
}

/// The type of a file system entry. Distinguishes files from the different kind of folders. The
/// `EntryType` of a folder also caries the folder's content.
#[derive(Clone, Debug)]
pub enum EntryType {
    /// A file.
    File,
    /// A folder. This can also mean a content root.
    Folder {
        /// The folder type.
        type_:   FolderType,
        /// The folder's content.
        content: AnyFolderContent,
    },
}

impl Ord for EntryType {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::File, Self::File) => Ordering::Equal,
            (Self::File, Self::Folder { .. }) => Ordering::Greater,
            (Self::Folder { .. }, Self::File) => Ordering::Less,
            (Self::Folder { type_: type1, .. }, Self::Folder { type_: type2, .. }) =>
                type1.cmp(type2),
        }
    }
}

impl PartialOrd for EntryType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for EntryType {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for EntryType {}

/// A file system entry. Either a file or a folder.
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Entry {
    /// The entry type.
    pub type_: EntryType,
    /// The entrie's name.
    pub name:  String,
    /// The entrie's global path in the file system.
    pub path:  PathBuf,
}


// === FolderContent ===

/// Values implementing this trait describe the content of folders. They can be seen as a lazy,
/// asynchronous wrapper around a `Vec` of file system entries.
pub trait FolderContent: Debug {
    /// Request the list of entries inside the folder. When the list is ready, it is emitted at
    /// `entries_loaded`. If an error occurs then the error message is emitted at `error_occurred`.
    fn request_entries(
        &self,
        entries_loaded: frp::Any<Rc<Vec<Entry>>>,
        error_occurred: frp::Any<ImString>,
    );
}

/// A wrapper around `Rc<dyn FolderContent>`. Necessary to implement the `Default` trait on this
/// type, which we need to pass it through FRP networks.
#[derive(Debug, Clone)]
pub struct AnyFolderContent(Rc<dyn FolderContent>);

impl Deref for AnyFolderContent {
    type Target = dyn FolderContent;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<D: 'static + FolderContent> From<D> for AnyFolderContent {
    fn from(dir: D) -> Self {
        AnyFolderContent(Rc::new(dir))
    }
}


// === EmptyFolder ===

/// `FolderContent` that immediately provides an empty content list on request.
#[derive(Debug, Copy, Clone)]
pub struct EmptyFolderContent;

impl FolderContent for EmptyFolderContent {
    fn request_entries(
        &self,
        entries_loaded: frp::Any<Rc<Vec<Entry>>>,
        _error_occured: frp::Any<ImString>,
    ) {
        entries_loaded.emit(Rc::new(vec![]));
    }
}

impl Default for AnyFolderContent {
    fn default() -> Self {
        EmptyFolderContent.into()
    }
}
