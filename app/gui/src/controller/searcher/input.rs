//! A module containing structures keeping and interpreting the input of the searcher controller.

use crate::prelude::*;

use crate::controller::searcher::action;
use crate::controller::searcher::Filter;
use crate::controller::searcher::RequiredImport;

use ast::HasTokens;
use double_representation::name::QualifiedName;
use enso_text as text;
use parser::Parser;



// ==============
// === Errors ===
// ==============

#[derive(Clone, Debug, Fail)]
#[allow(missing_docs)]
#[fail(display = "Not a char boundary: index {} at '{}'.", index, string)]
pub struct NotACharBoundary {
    index:  usize,
    string: String,
}



// ===============
// === Literal ===
// ===============

/// A text or number literal.
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    /// A literal of a number type.
    Number(ast::known::Number),
    /// A literal of a text type.
    Text {
        /// The string representation of the finished text literal. Includes the closing
        /// delimiter even if the user didn't type it yet.
        text:              ImString,
        /// The missing closing delimiter if the user didn't type it yet.
        closing_delimiter: Option<&'static str>,
    },
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Text { text, .. } => write!(f, "{text}"),
            Self::Number(number) => write!(f, "{}", number.repr()),
        }
    }
}

impl Literal {
    /// Construct a string literal. Returns `None` if the given string is not starting with a
    /// quotation mark.
    fn try_new_text(text: &str) -> Option<Self> {
        for delimiter in ast::repr::STRING_DELIMITERS {
            if text.starts_with(delimiter) {
                let delimiter_only = text == *delimiter;
                let closing_delimiter = if text.ends_with(delimiter) && !delimiter_only {
                    None
                } else {
                    Some(*delimiter)
                };
                let text = if let Some(delimiter) = closing_delimiter {
                    let mut text = text.to_owned();
                    text.push_str(delimiter);
                    text.into()
                } else {
                    text.into()
                };
                return Some(Self::Text { text, closing_delimiter });
            }
        }
        None
    }
}



// ====================
// === AstWithRange ===
// ====================

/// A helper structure binding given AST node to the range of Component Browser input.
#[derive(Clone, Debug)]
pub struct AstWithRange<A = Ast> {
    ast:   A,
    range: text::Range<text::Byte>,
}



// =========================
// === AstPositionFinder ===
// =========================

/// A structure locating what Ast overlaps with given text offset.
#[derive(Clone, Debug, Default)]
struct AstAtPositionFinder {
    current_offset: text::Byte,
    target_offset:  text::Byte,
    result:         Vec<AstWithRange>,
}

impl AstAtPositionFinder {
    fn new(target_offset: text::Byte) -> Self {
        Self { target_offset, ..default() }
    }

    /// Find all AST nodes containing the character in the `target` offset. The most nested AST
    /// nodes will be last on the returned list.
    fn find(ast: &Ast, target: text::Byte) -> Vec<AstWithRange> {
        let mut finder = Self::new(target);
        ast.feed_to(&mut finder);
        finder.result
    }
}

impl ast::TokenConsumer for AstAtPositionFinder {
    fn feed(&mut self, token: ast::Token) {
        match token {
            ast::Token::Ast(ast) if self.current_offset <= self.target_offset => {
                let start = self.current_offset;
                ast.shape().feed_to(self);
                let end = self.current_offset;
                if end > self.target_offset {
                    self.result
                        .push(AstWithRange { ast: ast.clone_ref(), range: (start..end).into() });
                }
            }
            other => self.current_offset += other.len(),
        }
    }
}



// =================
// === EditedAst ===
// =================

/// Information about the AST node which is currently modified in the Searcher Input's expression.
#[derive(Clone, Debug, Default)]
pub struct EditedAst {
    /// The edited name. The left side of the name should be used as searching pattern, and picked
    /// suggestion should replace the entire name. `None` if no name is edited - in that case
    /// suggestion should be inserted at the cursor position.
    pub edited_name:           Option<AstWithRange>,
    /// Accessor chain (like `Foo.Bar.buz`) which is currently edited.
    pub edited_accessor_chain: Option<AstWithRange<ast::opr::Chain>>,
    /// The currently edited literal.
    pub edited_literal:        Option<Literal>,
}

impl EditedAst {
    /// Create EditedAst structure basing on the result of [`AstAtPositionFinder::find`].
    fn new(stack: Vec<AstWithRange>) -> Self {
        let mut stack = stack.into_iter();
        if let Some(leaf) = stack.next() {
            let leaf_parent = stack.next();
            let leaf_id = leaf.ast.id;
            let to_accessor_chain = |a: AstWithRange| {
                let ast = ast::opr::Chain::try_new_of(&a.ast, ast::opr::predefined::ACCESS)?;
                let target_id = ast.target.as_ref().map(|arg| arg.arg.id);
                let leaf_is_not_target = !target_id.contains(&leaf_id);
                leaf_is_not_target.then_some(AstWithRange { ast, range: a.range })
            };
            match leaf.ast.shape() {
                ast::Shape::Var(_) | ast::Shape::Cons(_) => Self {
                    edited_name:           Some(leaf),
                    edited_accessor_chain: leaf_parent.and_then(to_accessor_chain),
                    edited_literal:        None,
                },
                ast::Shape::Opr(_) => Self {
                    edited_name:           None,
                    edited_accessor_chain: leaf_parent.and_then(to_accessor_chain),
                    edited_literal:        None,
                },
                ast::Shape::Infix(_) => Self {
                    edited_name:           None,
                    edited_accessor_chain: to_accessor_chain(leaf),
                    edited_literal:        None,
                },
                ast::Shape::Number(_) => {
                    // Unwrapping is safe here, because we know that the shape is a number.
                    let ast = ast::known::Number::try_from(leaf.ast).unwrap();
                    Self { edited_literal: Some(Literal::Number(ast)), ..default() }
                }
                ast::Shape::Tree(tree) =>
                    if let Some(text) = tree.leaf_info.as_ref() {
                        let edited_literal = Literal::try_new_text(text);
                        Self { edited_literal, ..default() }
                    } else {
                        default()
                    },
                _ => default(),
            }
        } else {
            default()
        }
    }
}



// ================
// === InputAst ===
// ================

/// Searcher input content parsed to [`ast::BlockLine`] node if possible.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum InputAst {
    Line(ast::BlockLine<Ast>),
    Invalid(String),
}

impl Default for InputAst {
    fn default() -> Self {
        Self::Invalid(default())
    }
}

impl Display for InputAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InputAst::Line(ast) => write!(f, "{}", ast.repr()),
            InputAst::Invalid(string) => write!(f, "{string}"),
        }
    }
}



// =============
// === Input ===
// =============

/// The Component Browser Input.
///
/// This structure is responsible for keeping and interpreting the searcher input - it provides
/// the information:
/// * [what part of the input is a filtering pattern](Input::pattern), and
/// * [what part should be replaced when inserting a suggestion](Input::after_inserting_suggestion).
///
/// Both information are deduced from the _edited name_. The edited name is a concrete
/// identifier which we deduce the user is currently editing - we assume this is an identifier where
/// the text cursor is positioned (including the "end" of the identifier, like `foo|`).
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Input {
    /// The input in the AST form.
    pub ast:             InputAst,
    /// The current cursor position in the input.
    pub cursor_position: text::Byte,
    /// The edited part of the input: the name being edited and it's context. See [`EditedAst`] for
    /// details.
    pub edited_ast:      EditedAst,
}

impl Input {
    /// Create the structure from the code already parsed to AST.
    pub fn new(ast: ast::BlockLine<Ast>, cursor_position: text::Byte) -> Self {
        // We primarily check the character on the left of the cursor (`cursor_position_before`),
        // to properly handle the situation when the cursor is at end of the edited name.
        // But if there is no identifier on the left then we check the right side.
        let cursor_position_before =
            (cursor_position > text::Byte(0)).then(|| cursor_position - text::ByteDiff(1));
        let ast_stack_on_left =
            cursor_position_before.map(|cp| AstAtPositionFinder::find(&ast.elem, cp));
        let edited_ast_on_left = ast_stack_on_left.map_or_default(EditedAst::new);
        let edited_ast = if edited_ast_on_left.edited_name.is_none() {
            let ast_stack_on_right = AstAtPositionFinder::find(&ast.elem, cursor_position);
            let edited_ast_on_right = EditedAst::new(ast_stack_on_right);
            if edited_ast_on_right.edited_name.is_some() {
                edited_ast_on_right
            } else {
                edited_ast_on_left
            }
        } else {
            edited_ast_on_left
        };
        let ast = InputAst::Line(ast);
        Self { ast, cursor_position, edited_ast }
    }

    /// Create the structure parsing the string.
    pub fn parse(parser: &Parser, expression: impl Str, cursor_position: text::Byte) -> Self {
        match parser.parse_line(expression.as_ref()) {
            Ok(ast) => Self::new(ast, cursor_position),
            Err(_) => Self {
                ast: InputAst::Invalid(expression.into()),
                cursor_position,
                edited_ast: default(),
            },
        }
    }

    /// Return the filtering pattern for the input.
    pub fn filter(&self) -> Filter {
        let pattern = if let Some(edited) = &self.edited_ast.edited_name {
            let cursor_position_in_name = self.cursor_position - edited.range.start;
            let name = ast::identifier::name(&edited.ast);
            name.map_or_default(|name| {
                let range = ..cursor_position_in_name.value as usize;
                name.get(range).unwrap_or_default().into()
            })
        } else {
            default()
        };
        let context = self.context().map(|c| c.into_ast().repr().to_im_string());
        Filter { pattern, context }
    }

    /// Return the accessor chain being the context of the edited name, i.e. the preceding fully
    /// qualified name parts or this argument.
    pub fn context(&self) -> Option<ast::opr::Chain> {
        self.edited_ast.edited_accessor_chain.clone().map(|mut chain| {
            chain.ast.args.pop();
            chain.ast
        })
    }

    /// The range of the text which contains the currently edited name, if any.
    pub fn edited_name_range(&self) -> Option<text::Range<text::Byte>> {
        self.edited_ast.edited_name.as_ref().map(|name| name.range)
    }

    /// The range of the text which contains the currently edited accessor chain, if any.
    pub fn accessor_chain_range(&self) -> Option<text::Range<text::Byte>> {
        self.edited_ast.edited_accessor_chain.as_ref().map(|chain| chain.range)
    }

    /// The input as AST node. Returns [`None`] if the input is not a valid AST.
    pub fn ast(&self) -> Option<&Ast> {
        match &self.ast {
            InputAst::Line(ast) => Some(&ast.elem),
            InputAst::Invalid(_) => None,
        }
    }

    /// Currently edited literal, if any.
    pub fn edited_literal(&self) -> Option<&Literal> {
        self.edited_ast.edited_literal.as_ref()
    }
}



// ============================
// === Inserting Suggestion ===
// ============================

/// The description of the results of inserting suggestion.
#[derive(Clone, Debug)]
pub struct InsertedSuggestion {
    /// An entire input after inserting the suggestion.
    pub new_input:     String,
    /// The replaced range in the old input.
    pub replaced:      text::Range<text::Byte>,
    /// The range of the inserted code in the new input. It does not contain any additional spaces
    /// (in contrary to [`inserted_text`] field.
    pub inserted_code: text::Range<text::Byte>,
    /// The range of the entire inserted text in the new input. It contains code and all additional
    /// spaces.
    pub inserted_text: text::Range<text::Byte>,
    /// An import that needs to be added when applying the suggestion.
    pub import:        Option<RequiredImport>,
}


impl InsertedSuggestion {
    /// The text change resulting from this insertion.
    pub fn input_change(&self) -> text::Change<text::Byte, String> {
        let to_insert = self.new_input[self.inserted_text].to_owned();
        text::Change { range: self.replaced, text: to_insert }
    }
}


// === Insert Context ===

/// A helper abstraction responsible for generating proper code for the suggestion.
///
/// If the context contains a qualified name, we want to reuse it in the generated code sample.
/// For example, if the user types `Foo.Bar.` and accepts a method `Foo.Bar.baz` we insert
/// `Foo.Bar.baz` with `Foo` import instead of inserting `Bar.baz` with `Bar` import.
struct InsertContext<'a> {
    suggestion:    &'a action::Suggestion,
    context:       Option<ast::opr::Chain>,
    generate_this: bool,
}

impl InsertContext<'_> {
    /// Whether the context of the edited expression contains a qualified name. Qualified name is a
    /// infix chain of `ACCESS` operators with identifiers.
    fn has_qualified_name(&self) -> bool {
        if let Some(ref context) = self.context {
            let every_operand_is_name = context.enumerate_operands().flatten().all(|opr| {
                matches!(opr.item.arg.shape(), ast::Shape::Cons(_) | ast::Shape::Var(_))
            });
            let every_operator_is_access = context
                .enumerate_operators()
                .all(|opr| opr.item.ast().repr() == ast::opr::predefined::ACCESS);
            every_operand_is_name && every_operator_is_access
        } else {
            false
        }
    }

    /// All the segments of the qualified name already written by the user.
    fn qualified_name_segments(&self) -> Option<Vec<ImString>> {
        if self.has_qualified_name() {
            // Unwrap is safe here, because `has_qualified_name` would not return true in case of no
            // context.
            let context = self.context.as_ref().unwrap();
            let name_segments = context
                .enumerate_operands()
                .flatten()
                .filter_map(|opr| ast::identifier::name(&opr.item.arg).map(ImString::new))
                .collect_vec();
            Some(name_segments)
        } else {
            None
        }
    }

    /// A list of name segments to replace the user input, and also a required import for the
    /// final expression. The returned list of segments contains both the segments already entered
    /// by the user and the segments that need to be added.
    fn segments_to_replace(&self) -> Option<(Vec<ImString>, Option<RequiredImport>)> {
        if let Some(existing_segments) = self.qualified_name_segments() {
            if let action::Suggestion::FromDatabase(entry) = self.suggestion {
                let name = entry.qualified_name();
                let all_segments = name.segments().cloned().collect_vec();
                // A list of search windows is reversed, because we want to look from the end, as it
                // gives the shortest possible name.
                let window_size = existing_segments.len();
                let mut windows = all_segments.windows(window_size).rev();
                let window_position_from_end = windows.position(|w| w == existing_segments)?;
                let pos = all_segments.len().saturating_sub(window_position_from_end + window_size);
                let name_segments = all_segments.get(pos..).unwrap_or(&[]).to_vec();
                let import_segments = all_segments.get(..=pos).unwrap_or(&[]).to_vec();
                // Valid qualified name requires at least 2 segments (namespace and project name).
                // Not enough segments to build a qualified name is not a mistake here – it just
                // means we don't need any import, as the entry is already in scope.
                let minimal_count_of_segments = 2;
                let import = if import_segments.len() < minimal_count_of_segments {
                    None
                } else if let Ok(import) = QualifiedName::from_all_segments(import_segments.clone())
                {
                    Some(RequiredImport::Name(import))
                } else {
                    error!("Invalid import formed in `segments_to_replace`: {import_segments:?}.");
                    None
                };
                Some((name_segments, import))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn code_to_insert(&self) -> (Cow<str>, Option<RequiredImport>) {
        match self.suggestion {
            action::Suggestion::FromDatabase(entry) => {
                if let Some((segments, import)) = self.segments_to_replace() {
                    (Cow::from(segments.iter().join(ast::opr::predefined::ACCESS)), import)
                } else {
                    let code = entry.code_to_insert(self.generate_this);
                    let import = Some(RequiredImport::Entry(entry.clone_ref()));
                    (code, import)
                }
            }
            action::Suggestion::Hardcoded(snippet) => (Cow::from(snippet.code.as_str()), None),
        }
    }
}

// === Input ===

impl Input {
    /// Return an information about input change after inserting given suggestion.
    pub fn after_inserting_suggestion(
        &self,
        suggestion: &action::Suggestion,
        has_this: bool,
    ) -> FallibleResult<InsertedSuggestion> {
        let context = self.context();
        let generate_this = !has_this;
        let context = InsertContext { suggestion, context, generate_this };
        let default_range = (self.cursor_position..self.cursor_position).into();
        let replaced = if context.has_qualified_name() {
            self.accessor_chain_range().unwrap_or(default_range)
        } else {
            self.edited_name_range().unwrap_or(default_range)
        };
        let (code_to_insert, import) = context.code_to_insert();
        debug!("Code to insert: \"{code_to_insert}\"");
        let end_of_inserted_code = replaced.start + text::Bytes(code_to_insert.len());
        let end_of_inserted_text = end_of_inserted_code + text::Bytes(1);
        let mut new_input = self.ast.to_string();
        let raw_range = replaced.start.value..replaced.end.value;
        Self::ensure_on_char_boundary(&new_input, raw_range.start)?;
        Self::ensure_on_char_boundary(&new_input, raw_range.end)?;
        new_input.replace_range(raw_range, &code_to_insert);
        new_input.insert(end_of_inserted_code.value, ' ');
        Ok(InsertedSuggestion {
            new_input,
            replaced,
            inserted_code: (replaced.start..end_of_inserted_code).into(),
            inserted_text: (replaced.start..end_of_inserted_text).into(),
            import,
        })
    }

    /// Check if the `index` is at a char boundary inside `string` and can be used as a range
    /// boundary. Otherwise, return an error.
    fn ensure_on_char_boundary(string: &str, index: usize) -> FallibleResult<()> {
        if !string.is_char_boundary(index) {
            Err(NotACharBoundary { index, string: string.into() }.into())
        } else {
            Ok(())
        }
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn edited_literal() {
        struct Case {
            input:    &'static str,
            expected: Option<Literal>,
        }

        let cases: Vec<Case> = vec![
            Case { input: "", expected: None },
            Case {
                input:    "'",
                expected: Some(Literal::Text {
                    text:              "''".into(),
                    closing_delimiter: Some("'"),
                }),
            },
            Case {
                input:    "\"\"\"",
                expected: Some(Literal::Text {
                    text:              "\"\"\"\"\"\"".into(),
                    closing_delimiter: Some("\"\"\""),
                }),
            },
            Case {
                input:    "\"text",
                expected: Some(Literal::Text {
                    text:              "\"text\"".into(),
                    closing_delimiter: Some("\""),
                }),
            },
            Case {
                input:    "\"text\"",
                expected: Some(Literal::Text {
                    text:              "\"text\"".into(),
                    closing_delimiter: None,
                }),
            },
            Case {
                input:    "'text",
                expected: Some(Literal::Text {
                    text:              "'text'".into(),
                    closing_delimiter: Some("'"),
                }),
            },
            Case {
                input:    "'text'",
                expected: Some(Literal::Text {
                    text:              "'text'".into(),
                    closing_delimiter: None,
                }),
            },
            Case {
                input:    "x = 'text",
                expected: Some(Literal::Text {
                    text:              "'text'".into(),
                    closing_delimiter: Some("'"),
                }),
            },
            Case {
                input:    "x = \"\"\"text",
                expected: Some(Literal::Text {
                    text:              "\"\"\"text\"\"\"".into(),
                    closing_delimiter: Some("\"\"\""),
                }),
            },
        ];
        let parser = Parser::new();

        let input = Input::parse(&parser, "123", text::Byte(3));
        assert!(matches!(input.edited_literal(), Some(Literal::Number(n)) if n.repr() == "123"));
        let input = Input::parse(&parser, "x = 123", text::Byte(7));
        assert!(matches!(input.edited_literal(), Some(Literal::Number(n)) if n.repr() == "123"));

        for case in cases {
            let input = Input::parse(&parser, case.input, text::Byte(case.input.len()));
            assert_eq!(input.edited_literal(), case.expected.as_ref());
        }
    }

    #[test]
    fn parsing_input() {
        #[derive(Debug, Default)]
        struct Case {
            input: String,
            cursor_position: text::Byte,
            expected_accessor_chain_range: Option<text::Range<text::Byte>>,
            expected_name_range: Option<text::Range<text::Byte>>,
            expected_pattern: String,
        }

        impl Case {
            fn new(
                input: impl Into<String>,
                cursor_position: impl Into<text::Byte>,
                expected_accessor_chain_range: Option<impl Into<text::Range<text::Byte>>>,
                expected_name_range: Option<impl Into<text::Range<text::Byte>>>,
                expected_pattern: impl Into<String>,
            ) -> Self {
                Self {
                    input: input.into(),
                    cursor_position: cursor_position.into(),
                    expected_accessor_chain_range: expected_accessor_chain_range.map(Into::into),
                    expected_name_range: expected_name_range.map(Into::into),
                    expected_pattern: expected_pattern.into(),
                }
            }

            fn run(self, parser: &Parser) {
                debug!("Running case {} cursor position {}", self.input, self.cursor_position);
                let input = Input::parse(parser, self.input, self.cursor_position);
                let pattern = input.filter().pattern.clone_ref();
                assert_eq!(input.cursor_position, self.cursor_position);
                assert_eq!(input.edited_ast.edited_name.map(|a| a.range), self.expected_name_range);
                assert_eq!(
                    input.edited_ast.edited_accessor_chain.map(|a| a.range),
                    self.expected_accessor_chain_range
                );
                assert_eq!(pattern, self.expected_pattern);
            }
        }

        let none_range: Option<text::Range<text::Byte>> = None;
        let cases = [
            Case::new("foo", 3, none_range, Some(0..3), "foo"),
            Case::new("foo", 1, none_range, Some(0..3), "f"),
            Case::new("foo", 0, none_range, Some(0..3), ""),
            Case::new("(2 + foo) * bar", 2, none_range, none_range, ""),
            Case::new("(2 + foo)*bar", 5, none_range, Some(5..8), ""),
            Case::new("(2 + foo)*bar", 7, none_range, Some(5..8), "fo"),
            Case::new("(2 + foo)*bar", 8, none_range, Some(5..8), "foo"),
            Case::new("(2 + foo)*bar", 9, none_range, none_range, ""),
            Case::new("(2 + foo)*b", 10, none_range, Some(10..11), ""),
            Case::new("(2 + foo)*b", 11, none_range, Some(10..11), "b"),
            Case::new("Standard.Base.foo", 0, none_range, Some(0..8), ""),
            Case::new("Standard.Base.foo", 4, none_range, Some(0..8), "Stan"),
            Case::new("Standard.Base.foo", 8, none_range, Some(0..8), "Standard"),
            Case::new("Standard.Base.foo", 9, Some(0..13), Some(9..13), ""),
            Case::new("Standard.Base.foo", 11, Some(0..13), Some(9..13), "Ba"),
            Case::new("Standard.Base.foo", 13, Some(0..13), Some(9..13), "Base"),
            Case::new("Standard.Base.foo", 14, Some(0..17), Some(14..17), ""),
            Case::new("Standard.Base.foo", 15, Some(0..17), Some(14..17), "f"),
            Case::new("Standard.Base.foo", 17, Some(0..17), Some(14..17), "foo"),
            Case::new("Standard . Base . foo", 17, Some(0..21), none_range, ""),
            Case::new("Standard . Base.foo", 16, Some(11..19), Some(16..19), ""),
            Case::new("Standard.Base.", 14, Some(0..14), none_range, ""),
            Case::new("(2 + Standard.Base.foo)*b", 21, Some(5..22), Some(19..22), "fo"),
            Case::new("(2 + Standard.Base.)*b", 19, Some(5..19), none_range, ""),
        ];

        let parser = Parser::new();
        for case in cases {
            case.run(&parser);
        }
    }
}
