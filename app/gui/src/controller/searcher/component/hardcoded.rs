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

/// Name of the virtual component group in the `Standard.Base` library which contains input
/// components.
pub const INPUT_GROUP_NAME: &str = "Input";

thread_local! {
    /// Code snippets of default literal values of text and number type. The snippets are
    /// documented as code that can be used as input nodes. When converted to [`Component`]s and
    /// added to the [`component::List`] they allow the users to easily enter literals in code.
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

fn snippet_with_name_and_code_and_icon(name: &'static str, code: &'static str, icon: IconId) -> Snippet {
    Snippet::new(name, code, &ImString::new(icon.as_str()))
}
