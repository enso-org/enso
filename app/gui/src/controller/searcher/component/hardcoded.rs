use crate::prelude::*;



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
        Snippet::new("text input", "\"\"", &ImString::new("TextInput"))
            .with_return_type("Standard.Base.Data.Text.Text")
            .with_documentation(
                "A text input node.\n\n\
                An empty text. The value can be edited and used as an input for other nodes."
            )
            .into(),
        Snippet::new("number input", "0", &ImString::new("NumberInput"))
            .with_return_type("Standard.Base.Data.Numbers.Number")
            .with_documentation(
                 "A number input node.\n\n\
                 A zero number. The value can be edited and used as an input for other nodes."
            )
            .into(),
    ];
}
