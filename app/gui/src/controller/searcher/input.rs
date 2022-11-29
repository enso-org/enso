//! A module containing structures keeping and interpreting the input of the searcher controller.

use crate::prelude::*;

use crate::controller::searcher::action;

use ast::HasTokens;
use enso_text as text;
use parser_scala::Parser;



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
    /// Editor accessor chain (like `Foo.Bar.buz`) which is currently edited.
    pub edited_accessor_chain: Option<AstWithRange<ast::opr::Chain>>,
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
                },
                ast::Shape::Opr(_) => Self {
                    edited_name:           None,
                    edited_accessor_chain: leaf_parent.and_then(to_accessor_chain),
                },
                ast::Shape::Infix(_) => Self {
                    edited_name:           None,
                    edited_accessor_chain: to_accessor_chain(leaf),
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
            InputAst::Invalid(string) => write!(f, "{}", string),
        }
    }
}



// =============
// === Input ===
// =============

/// The Component Browser Input.
///
/// This structure is responsible for keeping and interpreting the searcher input - it provides
/// the information [what part of the input is a filtering pattern](Input::pattern), and [what part
/// should be replaced when inserting a suggestion](Input::after_inserting_suggestion).
///
/// # How the Component Browser Input Is Interpreted
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Input {
    pub ast:             InputAst,
    pub cursor_position: text::Byte,
    pub edited_ast:      EditedAst,
}

impl Input {
    /// Create the structure from the code already parsed to AST.
    pub fn new(ast: ast::BlockLine<Ast>, cursor_position: text::Byte) -> Self {
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

    /// Return the part of the input used as filtering pattern.
    pub fn pattern(&self) -> &str {
        if let Some(edited) = &self.edited_ast.edited_name {
            let cursor_position_in_name = self.cursor_position - edited.range.start;
            let name = ast::identifier::name(&edited.ast);
            name.map_or("", |n| {
                &n[text::Range::new(text::Byte(0), text::Byte(0) + cursor_position_in_name)]
            })
        } else {
            ""
        }
    }

    /// Return the accessor chain being the context of the edited name, i.e. the preceding fully
    /// qualified name parts or this argument.
    pub fn context(&self) -> Option<ast::opr::Chain> {
        self.edited_ast.edited_accessor_chain.clone().map(|mut chain| {
            chain.ast.args.pop();
            chain.ast
        })
    }

    /// The range of the text which shall be replaced by applied suggestion.
    pub fn replaced_range(&self) -> text::Range<text::Byte> {
        match &self.edited_ast.edited_name {
            Some(name) => name.range,
            None => (self.cursor_position..self.cursor_position).into(),
        }
    }

    /// The input as AST node. Returns [`None`] if the input is not a valid AST.
    pub fn ast(&self) -> Option<&Ast> {
        match &self.ast {
            InputAst::Line(ast) => Some(&ast.elem),
            InputAst::Invalid(_) => None,
        }
    }
}


// === Inserting Suggestion ===

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
}


impl InsertedSuggestion {
    /// The text change resulting from this insertion.
    pub fn input_change(&self) -> text::Change<text::Byte, String> {
        let to_insert = self.new_input[self.inserted_text].to_owned();
        text::Change { range: self.replaced, text: to_insert }
    }
}

impl Input {
    /// Return an information about input change after inserting given suggestion.
    pub fn after_inserting_suggestion(
        &self,
        suggestion: &action::Suggestion,
    ) -> InsertedSuggestion {
        let replaced = self.replaced_range();
        let generate_this = replaced.start == text::Byte(0);
        let code_to_insert = suggestion.code_to_insert(generate_this);
        debug!("Code to insert: \"{code_to_insert}\"");
        let end_of_inserted_code = replaced.start + text::Bytes(code_to_insert.len());
        let end_of_inserted_text = end_of_inserted_code + text::Bytes(1);
        let mut new_input = self.ast.to_string();
        let raw_range = replaced.start.value..replaced.end.value;
        new_input.replace_range(raw_range, &code_to_insert);
        new_input.insert(end_of_inserted_code.value, ' ');
        InsertedSuggestion {
            new_input,
            replaced,
            inserted_code: (replaced.start..end_of_inserted_code).into(),
            inserted_text: (replaced.start..end_of_inserted_text).into(),
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
    fn parsing_input() {
        init_tracing(DEBUG);
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
                DEBUG!("Running case {self.input} cursor position {self.cursor_position}");
                let input = Input::parse(parser, self.input, self.cursor_position);
                let pattern = input.pattern().to_owned();
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

        let parser = Parser::new_or_panic();
        for case in cases {
            case.run(&parser);
        }
    }
}
