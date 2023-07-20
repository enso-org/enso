//! A module containing definitions of hardcoded [`Snippet`]s displayed as virtual components in
//! the [Component Browser](crate::controller::Searcher). The module also defines names of the
//! favorites component groups where the virtual components should be displayed.
//!
//! To learn more about favorites component groups, see:
//! [`crate::controller::searcher::component::List::favorites`].

use crate::prelude::*;

use crate::controller::searcher::input;

use double_representation::name::QualifiedName;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::SuggestionDatabase;
use ensogl_icons::icon::Id as IconId;



// =================
// === Constants ===
// =================

/// Name of the favorites component group in the `Standard.Base` library where virtual components
/// created from the [`INPUT_SNIPPETS`] should be added.
pub const INPUT_GROUP_NAME: &str = "Standard.Base.Input";
/// Qualified name of the `Text` type.
const TEXT_ENTRY: &str = "Standard.Base.Main.Data.Text.Text";
/// Qualified name of the `Number` type.
const NUMBER_ENTRY: &str = "Standard.Base.Main.Data.Numbers.Number";

thread_local! {
    /// Snippets describing virtual components displayed in the [`INPUT_GROUP_NAME`] favorites
    /// component group. The snippets wrap default literal values of Text and Number types. When
    /// displayed in the Component Browser as virtual components they allow the users to easily
    /// enter primitive literals in code.
    pub static INPUT_SNIPPETS: Vec<Rc<Snippet>> = vec![
        Snippet::new("text input", "\"\"", IconId::TextInput)
            .with_return_types(["Standard.Base.Data.Text.Text"])
            .with_documentation_str(
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
            .with_documentation_str(
                "A number input node.\n\n\
                 A zero number. The value can be edited and used as an input for other nodes.",
            )
            .into(),
    ];
}



// ===============
// === Snippet ===
// ===============

/// A hardcoded snippet of code with a description and syntactic metadata.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Snippet {
    /// The name displayed in the [Component Browser](crate::controller::searcher).
    pub name:          ImString,
    /// The code inserted when picking the snippet.
    pub code:          ImString,
    /// A list of types that the return value of this snippet's code typechecks as. Used by the
    /// [Component Browser](crate::controller::searcher) to decide whether to display the
    /// snippet when filtering components by return type.
    pub return_types:  Vec<QualifiedName>,
    /// The documentation bound to the snippet.
    pub documentation: Option<EntryDocumentation>,
    /// The ID of the icon bound to this snippet's entry in the [Component
    /// Browser](crate::controller::searcher).
    pub icon:          IconId,
}

impl Snippet {
    /// Construct a hardcoded snippet with given name, code, and icon.
    pub fn new(name: &str, code: &str, icon: IconId) -> Self {
        Self { name: name.into(), code: code.into(), icon, ..default() }
    }

    /// Construct a hardcoded snippet for a single literal.
    pub fn from_literal(literal: &input::Literal, db: &SuggestionDatabase) -> Self {
        use input::Literal::*;
        let text_repr = literal.to_string();
        let icon = match literal {
            Text { .. } => IconId::TextInput,
            Number { .. } => IconId::NumberInput,
        };
        let snippet = Self::new(&text_repr, &text_repr, icon);
        let entry_path = match literal {
            Text { .. } => TEXT_ENTRY,
            Number(_) => NUMBER_ENTRY,
        };
        // `unwrap()` is safe here, because we test the validity of `entry_path` in the tests.
        let qualified_name = QualifiedName::from_text(entry_path).unwrap();
        let entry = db.lookup_by_qualified_name(&qualified_name);
        let docs = entry.map(|(entry_id, _)| db.documentation_for_entry(entry_id));
        snippet.with_documentation(docs.unwrap_or_default())
    }

    /// Returns a modified suggestion with [`Snippet::return_types`] field set. This method is only
    /// intended to be used when defining hardcoded suggestions and panics if any of the given
    /// return types fail to convert to a valid type name.
    fn with_return_types<'a>(mut self, return_types: impl IntoIterator<Item = &'a str>) -> Self {
        let types = return_types.into_iter().map(|rt| rt.try_into().unwrap()).collect_vec();
        self.return_types = types;
        self
    }

    /// Returns a modified suggestion with the [`Snippet::documentation`] field set. This method
    /// is only intended to be used when defining hardcoded suggestions.
    fn with_documentation_str(mut self, documentation: &str) -> Self {
        let docs = EntryDocumentation::builtin(&enso_doc_parser::parse(documentation));
        self.documentation = Some(docs);
        self
    }

    /// Returns a modified suggestion with the [`Snippet::documentation`] field set.
    fn with_documentation(mut self, documentation: EntryDocumentation) -> Self {
        self.documentation = Some(documentation);
        self
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::model::execution_context::GroupQualifiedName;

    /// Test that the qualified names used for hardcoded snippets can be constructed. We don't check
    /// if the entries are actually available in the suggestion database.
    #[test]
    fn test_qualified_names_construction() {
        QualifiedName::from_text(TEXT_ENTRY).unwrap();
        QualifiedName::from_text(NUMBER_ENTRY).unwrap();
        GroupQualifiedName::try_from(INPUT_GROUP_NAME).unwrap();
    }
}
