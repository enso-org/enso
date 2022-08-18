//! A module containing definitions of hardcoded [`Snippet`]s displayed as virtual components in
//! the [Component Browser](crate::controller::Searcher). The module also defines names of the
//! favorites component groups where the virtual components should be displayed.
//!
//! To learn more about favorites component groups, see:
//! [`crate::controller::searcher::component::List::favorites`].

use crate::prelude::*;

use ide_view_component_group::icon::Id as IconId;



// ====================
// === Type Aliases ===
// ====================

/// A hardcoded snippet of code with a description and syntactic metadata.
pub type Snippet = controller::searcher::action::hardcoded::Suggestion;



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
        snippet_with_name_and_code_and_icon("text input", "\"\"", IconId::TextInput)
            .with_return_type("Standard.Base.Data.Text.Text")
            .with_documentation(
                "A text input node.\n\n\
                An empty text. The value can be edited and used as an input for other nodes."
            )
            .into(),
        snippet_with_name_and_code_and_icon("number input", "0", IconId::NumberInput)
            .with_return_type("Standard.Base.Data.Numbers.Number")
            .with_documentation(
                 "A number input node.\n\n\
                 A zero number. The value can be edited and used as an input for other nodes."
            )
            .into(),
    ];
}


// === Constants helpers ===

fn snippet_with_name_and_code_and_icon(
    name: &'static str,
    code: &'static str,
    icon: IconId,
) -> Snippet {
    Snippet::new(name, code, &ImString::new(icon.as_str()))
}
