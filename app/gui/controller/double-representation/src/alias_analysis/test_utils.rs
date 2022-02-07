//! Utilities to facilitate testing of alias-analysis-related code.

use crate::prelude::*;

use crate::identifier::LocatedName;
use crate::identifier::NormalizedName;
use crate::test_utils::MarkdownProcessor;

use regex::Captures;
use regex::Regex;
use regex::Replacer;



// ============
// === Case ===
// ============

/// Test case for testing identifier resolution for nodes.
/// Can be expressed using markdown notation, see `from_markdown` method.
#[derive(Clone, Debug, Default)]
pub struct Case {
    /// The code: the text of the block line that is considered to be a node of a graph.
    /// Any markers are already removed.
    pub code:                String,
    /// List of spans in the code where the identifiers are introduced into the graph's scope.
    pub expected_introduced: Vec<Range<usize>>,
    /// List of spans in the code where the identifiers from the graph's scope are used.
    pub expected_used:       Vec<Range<usize>>,
}

impl Case {
    /// Constructs a test case using a markdown. Input should be text representation of the node's
    /// AST in which all identifiers introduced into the graph's scope are marked like `«foo»`, and
    /// all identifiers used from graph's scope are marked like `»sum«`.
    pub fn from_markdown(marked_code: impl Str) -> Case {
        // Regexp that matches either «sth» or »sth« into a group named `introduced` or `used`,
        // respectively. See: https://regex101.com/r/pboF8O/2 for detailed explanation.
        let regex = format!(r"«(?P<{}>[^»]*)»|»(?P<{}>[^«]*)«", INTRODUCED, USED);
        // As this is test utils, we don't try nicely handling failure nor reusing the compiled
        // regexp between calls to save some cycles.
        let regex = Regex::new(&regex).unwrap();
        let mut replacer = MarkdownReplacer::default();
        let code = regex.replace_all(marked_code.as_ref(), replacer.by_ref()).into();
        Case { code, expected_introduced: replacer.introduced, expected_used: replacer.used }
    }
}



// ========================
// === MarkdownReplacer ===
// ========================

/// We want to recognize two kinds of marked identifiers: ones introduced into the graph's scope and
/// ones used from the graph's scope.
#[derive(Clone, Copy, Debug, Display)]
pub enum Kind {
    Introduced,
    Used,
}

/// Name of the pattern group matching introduced identifier.
const INTRODUCED: &str = "introduced";

/// Name of the pattern group matching used identifier.
const USED: &str = "used";

/// Replacer that is called with each marked token. Does the following:
/// * removes the markdown, i.e. replaces `»foo«` with `foo`;
/// * counts removed markdown bytes, so it is possible to translate between indices in marked and
///   unmarked code;
/// * accumulates spans of introduced and used identifiers.
#[derive(Debug, Default)]
struct MarkdownReplacer {
    processor:  MarkdownProcessor,
    /// Spans in the unmarked code.
    introduced: Vec<Range<usize>>,
    /// Spans in the unmarked code.
    used:       Vec<Range<usize>>,
}

// Processes every single match for a marked entity.
impl Replacer for MarkdownReplacer {
    fn replace_append(&mut self, captures: &Captures, dst: &mut String) {
        let (kind, matched) = if let Some(introduced) = captures.name(INTRODUCED) {
            (Kind::Introduced, introduced)
        } else if let Some(used) = captures.name(USED) {
            (Kind::Used, used)
        } else {
            panic!("Unexpected capture: expected named capture `{}` or `{}`.", INTRODUCED, USED)
        };

        let span = self.processor.process_match(captures, &matched, dst);
        match kind {
            Kind::Introduced => self.introduced.push(span),
            Kind::Used => self.used.push(span),
        };
    }
}



// ===========================
// === IdentifierValidator ===
// ===========================

#[derive(Clone, Copy, Debug, Display, PartialEq)]
enum HasBeenValidated {
    No,
    Yes,
}

/// Helper test structure that requires that each given identifier is validated at least once.
/// Otherwise, it shall panic when dropped.
#[derive(Clone, Debug)]
pub struct IdentifierValidator<'a> {
    ast:         &'a Ast,
    name:        String,
    validations: HashMap<NormalizedName, HasBeenValidated>,
}

impl<'a> IdentifierValidator<'a> {
    /// Creates a new checker, with identifier set obtained from given node's representation
    /// spans.
    pub fn new(name: impl Str, ast: &Ast, spans: Vec<Range<usize>>) -> IdentifierValidator {
        let name = name.into();
        let repr = ast.repr();
        let mut validations = HashMap::default();
        for span in spans {
            let name = NormalizedName::new(&repr[span]);
            validations.insert(name, HasBeenValidated::No);
        }
        IdentifierValidator { ast, name, validations }
    }

    /// Marks given identifier as checked.
    pub fn validate_identifier(&mut self, name: &NormalizedName) {
        let err = iformat!("{self.name}: unexpected identifier `{name}` validated");
        let used = self.validations.get_mut(name).expect(&err);
        *used = HasBeenValidated::Yes;
    }

    /// Marks given sequence of identifiers as checked.
    pub fn validate_identifiers(&mut self, identifiers: impl IntoIterator<Item = &'a LocatedName>) {
        for identifier in identifiers {
            self.validate_identifier(&identifier.item);

            let crumbs = &identifier.crumbs;
            let ast_result = self.ast.get_traversing(crumbs);
            let ast = ast_result.expect("failed to retrieve ast from crumb");
            let name_err = || iformat!("Failed to use AST {ast.repr()} as an identifier name");
            let name = NormalizedName::try_from_ast(ast).expect(&name_err());
            assert_eq!(name, identifier.item)
        }
    }
}

/// Panics if there are remaining identifiers that were not checked.
impl<'a> Drop for IdentifierValidator<'a> {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            for elem in &self.validations {
                assert_eq!(
                    elem.1,
                    &HasBeenValidated::Yes,
                    "{}: identifier `{}` was not validated)",
                    self.name,
                    elem.0
                )
            }
        } else {
            DEBUG!("Skipping identifier validation, because thread is already in panic.");
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsing_markdown_to_test_case() {
        let code = "«sum» = »a« + »b«";
        let case = Case::from_markdown(code);
        assert_eq!(case.code, "sum = a + b");
        assert_eq!(case.expected_introduced.len(), 1);
        assert_eq!(case.expected_introduced[0], 0..3);
        assert_eq!(&case.code[case.expected_introduced[0].clone()], "sum");

        assert_eq!(case.expected_used.len(), 2);
        assert_eq!(case.expected_used[0], 6..7); // these are utf-8 byte indices
        assert_eq!(&case.code[case.expected_used[0].clone()], "a");
        assert_eq!(case.expected_used[1], 10..11);
        assert_eq!(&case.code[case.expected_used[1].clone()], "b");
    }
}
