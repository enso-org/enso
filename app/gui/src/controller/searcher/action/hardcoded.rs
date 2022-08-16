
use crate::prelude::*;

use crate::model::module::MethodId;

use double_representation::module;
use double_representation::tp;



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



// ===================
// === Definitions ===
// ===================

// === Suggestion ===

/// The hardcoded suggestion.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Suggestion {
    /// The name displayed in the Searcher.
    pub name:               &'static str,
    /// The code inserted when picking suggestion.
    pub code:               &'static str,
    /// The type returned by the suggestion's code.
    pub return_types:       Vec<tp::QualifiedName>,
    /// The documentation bound to the suggestion.
    pub documentation_html: Option<String>,
    /// The name of the icon bound to this entry.
    pub icon:               ImString,
}

impl Suggestion {
    /// Construct a suggestion with given name, code, and icon.
    pub(crate) fn new(name: &'static str, code: &'static str, icon: &ImString) -> Self {
        let icon = icon.clone_ref();
        Self { name, code, icon, ..default() }
    }

    /// Returns a modified suggestion with [`Suggestion::return_types`] field set. This method is
    /// only intended to be used when defining hardcoded suggestions and panics if any of the given
    /// return types fail to convert to a valid type name.
    pub(crate) fn with_return_types<'a>(
        mut self,
        return_types: impl IntoIterator<Item = &'a str>,
    ) -> Self {
        let types = return_types.into_iter().map(|rt| rt.try_into().unwrap()).collect_vec();
        self.return_types = types;
        self
    }

    /// Returns a modified suggestion with [`Suggestion::documentation_html`] field set. This
    /// method is only intended to be used when defining hardcoded suggestions and panics if a
    /// documentation parser cannot be created or the argument fails to parse as valid
    /// documentation.
    pub(crate) fn with_documentation(mut self, documentation: &str) -> Self {
        let doc_parser = parser::DocParser::new().unwrap();
        let doc_string = documentation.to_string();
        let documentation_html = doc_parser.generate_html_doc_pure(doc_string);
        self.documentation_html = Some(documentation_html.unwrap());
        self
    }
}
