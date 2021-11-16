//! A stub module with new searcher GUI.

use crate::prelude::*;



// =============
// === Entry ===
// =============

/// A structure describing a single entry in Searcher.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Entry {
    pub label:     ImString,
    pub is_folder: Immutable<bool>,
    pub icon:      Icon,
}

/// The typewrapper for icon name.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Icon {
    name: ImString,
}

/// Construct icon structure from its name.
#[allow(non_snake_case)]
pub fn Icon(name: impl Into<ImString>) -> Icon {
    let name = name.into();
    Icon { name }
}



// ===========
// === FRP ===
// ===========

/// A type representing path to entry in some column.
pub type EntryPath<Id> = Rc<Vec<Id>>;

ensogl::define_endpoints! { <Id:(Debug+Clone+'static)>
    Input {
        reset(),
        directory_content (EntryPath<Id>,Entry),
        set_highlight     (EntryPath<Id>),
    }

    Output {
        list_directory (EntryPath<Id>),
        highlight      (EntryPath<Id>),
        entry_chosen   (EntryPath<Id>),
    }
}

/// The Searcher View gui component.
///
/// Currently it contains only simple mechanism of requesting searcher content and printing it to
/// the console.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct View<Id: Debug + Clone + 'static> {
    pub frp: Frp<Id>,
}

impl<Id: Debug + Clone + 'static> Deref for View<Id> {
    type Target = Frp<Id>;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl<ID: ToString + Debug + Clone + 'static> View<ID> {
    /// Create new searcher view.
    pub fn new() -> Self {
        let logger = Logger::new("searcher::new::View");
        let frp = Frp::new();
        let network = &frp.network;
        enso_frp::extend! { network
            eval frp.directory_content ([logger]((crumbs,entry)) {
                let crumbs = crumbs.iter().map(ToString::to_string).join(",");
                info!(logger,"New Searcher Entry received: [{crumbs}] -> {entry:?}.");
            });

            frp.source.list_directory <+ frp.reset.constant(Rc::new(vec![]));
            frp.source.list_directory <+ frp.directory_content.filter_map(|(crumbs,entry)| {
                entry.is_folder.as_some(crumbs.clone_ref())
            });
        }

        Self { frp }
    }
}

impl<Id: ToString + Debug + Clone + 'static> Default for View<Id> {
    fn default() -> Self {
        Self::new()
    }
}
