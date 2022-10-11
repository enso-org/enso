//! A module containing definitions of hardcoded [`Snippet`]s displayed as virtual components in
//! the [Component Browser](crate::controller::Searcher). The module also defines names of the
//! favorites component groups where the virtual components should be displayed.
//!
//! To learn more about favorites component groups, see:
//! [`crate::controller::searcher::component::List::favorites`].

use crate::prelude::*;

use double_representation::tp;
use ide_view_component_group::icon::Id as IconId;



// =================
// === Constants ===
// =================

/// Name of the favorites component group in the `Standard.Base` library where virtual components
/// created from the [`INPUT_SNIPPETS`] should be added.
pub const INPUT_GROUP_NAME: &str = "Input";

thread_local! {
    /// Snippets describing virtual components displayed in the [`INPUT_GROUP_NAME`] favorites
    /// component group. The snippets wrap default literal values of Text and Number types. When
    /// displayed in the Component Browser as virtual components they allow the users to easily
    /// enter primitive literals in code.
    pub static INPUT_SNIPPETS: Vec<Rc<Snippet>> = vec![
        Snippet::new("text input", "\"\"", IconId::TextInput)
            .with_return_types(["Standard.Base.Data.Text.Text"])
            .with_documentation(
                "A text input node.\n\n\
                An empty text. The value can be edited and used as an input for other nodes.",
            )
            .into(),
        Snippet::new("number input", "0", IconId::NumberInput)
            .with_return_types([
                "Standard.Base.Data.Numbers.Number",
                "Standard.Base.Data.Numbers.Decimal",
                "Standard.Base.Data.Numbers.Integer",
            ])
            .with_documentation(
                "A number input node.\n\n\
                 A zero number. The value can be edited and used as an input for other nodes.",
            )
            .into(),
    ];
}


// === Filtering by Return Type ===

/// Return a filtered copy of [`INPUT_SNIPPETS`] containing only snippets which have at least one
/// of their return types on the given list of return types.
pub fn input_snippets_with_matching_return_type(
    return_types: impl IntoIterator<Item = tp::QualifiedName>,
) -> Vec<Rc<Snippet>> {
    let rt_set: HashSet<_> = return_types.into_iter().collect();
    let rt_of_snippet_is_in_set =
        |s: &&Rc<Snippet>| s.return_types.iter().any(|rt| rt_set.contains(rt));
    INPUT_SNIPPETS
        .with(|snippets| snippets.iter().filter(rt_of_snippet_is_in_set).cloned().collect_vec())
}



// ===============
// === Snippet ===
// ===============

/// A hardcoded snippet of code with a description and syntactic metadata.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Snippet {
    /// The name displayed in the [Component Browser](crate::controller::searcher).
    pub name:               &'static str,
    /// The code inserted when picking the snippet.
    pub code:               &'static str,
    /// A list of types that the return value of this snippet's code typechecks as. Used by the
    /// [Component Browser](crate::controller::searcher) to decide whether to display the
    /// snippet when filtering components by return type.
    pub return_types:       Vec<tp::QualifiedName>,
    /// The documentation bound to the snippet.
    pub documentation_html: Option<String>,
    /// The ID of the icon bound to this snippet's entry in the [Component
    /// Browser](crate::controller::searcher).
    pub icon:               IconId,
}

impl Snippet {
    /// Construct a hardcoded snippet with given name, code, and icon.
    fn new(name: &'static str, code: &'static str, icon: IconId) -> Self {
        Self { name, code, icon, ..default() }
    }

    /// Returns a modified suggestion with [`Snippet::return_types`] field set. This method is only
    /// intended to be used when defining hardcoded suggestions and panics if any of the given
    /// return types fail to convert to a valid type name.
    fn with_return_types<'a>(mut self, return_types: impl IntoIterator<Item = &'a str>) -> Self {
        let types = return_types.into_iter().map(|rt| rt.try_into().unwrap()).collect_vec();
        self.return_types = types;
        self
    }

    /// Returns a modified suggestion with [`Snippet::documentation_html`] field set. This method
    /// is only intended to be used when defining hardcoded suggestions and panics if a
    /// documentation parser cannot be created or the argument fails to parse as valid
    /// documentation.
    fn with_documentation(mut self, documentation: &str) -> Self {
        let doc_parser = parser_scala::DocParser::new().unwrap();
        let doc_string = documentation.to_string();
        let documentation_html = doc_parser.generate_html_doc_pure(doc_string);
        self.documentation_html = Some(documentation_html.unwrap());
        self
    }
}
