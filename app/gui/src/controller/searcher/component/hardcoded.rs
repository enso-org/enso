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
pub const INPUT_COMPONENT_GROUP_NAME: &str = "Input";

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
    ];
}

/*
const INPUT_LITERAL_SNIPPETS: &[LiteralSnippet] = &[
    LiteralSnippet {
        name:               "text input",
        code:               "\"\"",
        return_type:        "Standard.Base.Data.Text.Text",
        documentation:
            "A text input node.\n\n\
            An empty text. The value can be edited and used as an input for other nodes."
        ,
        icon:               ide_view_component_group::icon::Id::TextInput,
    },
    LiteralSnippet {
        name:               "number input",
        code:               "0",
        return_type:        "Standard.Base.Data.Numbers.Number",
        documentation:
            "A number input node.\n\n\
            A zero number. The value can be edited and used as an input for other nodes."
        ,
        icon:               ide_view_component_group::icon::Id::NumberInput,
    },
];

thread_local! {
    /// Code snippets of default literal values of text and number type. The snippets are
    /// documented as code that can be used as input nodes. When converted to [`Component`]s and
    /// added to the [`component::List`] they allow the users to easily enter literals in code.
    pub static INPUT_SNIPPETS: Vec<Rc<Snippet>> = build_input_snippets().unwrap();
}


// === Constants helpers ===

fn build_input_snippets() -> FallibleResult<Vec<Rc<Snippet>>> {
    let 
    INPUT_LITERAL_SNIPPETS.into_iter().map(|c| Rc::new(c.clone().try_into()?)).collect()
}
*/



// ======================
// === LiteralSnippet ===
// ======================

/// A snippet of code with a literal value, with description and syntax metadata.
#[derive(Copy, Clone, Debug)]
struct LiteralSnippet {
    pub name:          &'static str,
    pub code:          &'static str,
    pub return_type:   &'static str,
    pub documentation: &'static str,
    pub icon:          ide_view_component_group::icon::Id,
}

impl TryFrom<LiteralSnippet> for Snippet {
    type Error = failure::Error;
    fn try_from(literal: LiteralSnippet) -> Result<Snippet, Self::Error> {
        let doc_parser = parser::DocParser::new()?;
        let doc_string = literal.documentation.to_string();
        let documentation_html = doc_parser.generate_html_doc_pure(doc_string)?;
        Ok(Snippet {
            name:               literal.name,
            code:               literal.code,
            this_arg:           None,
            argument_types:     vec![],
            return_type:        Some(literal.return_type.try_into()?),
            imports:            vec![],
            documentation_html: Some(documentation_html),
            method_id:          None,
            icon:               literal.icon.as_str().into(),
        })
    }
}
