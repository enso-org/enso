//! A module containing the hard-coded names of [`Icons`] displayed in Searcher.

use crate::prelude::*;



// =============
// === Icons ===
// =============

/// A structure serving as a type for [`ICONS`] constant, containing a set of hardcoded icon names.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Icons {
    pub search_result: ImString,
    pub libraries:     ImString,
    pub default:       ImString,
}

thread_local! {
    /// A set of hardcoded icon names, to be used when creating hardcoded categories and actions.
    pub static ICONS:Icons = Icons {
        search_result : ImString::new("search_result"),
        libraries     : ImString::new("libraries"),
        default       : ImString::new("default"),
    };
}
