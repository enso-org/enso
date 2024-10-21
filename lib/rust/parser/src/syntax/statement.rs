//! Parses statements in module, body blocks, and type blocks.



mod function_def;
mod type_def;

use crate::empty_tree;
use crate::expression_to_pattern;
use crate::is_qualified_name;
use crate::prelude::*;
use crate::syntax::item;
use crate::syntax::maybe_with_error;
use crate::syntax::operator::Precedence;
use crate::syntax::statement::function_def::try_parse_foreign_function;
use crate::syntax::statement::function_def::FunctionBuilder;
use crate::syntax::statement::type_def::try_parse_type_def;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::tree::block;
use crate::syntax::tree::ArgumentDefinition;
use crate::syntax::tree::FunctionAnnotation;
use crate::syntax::tree::SyntaxError;
use crate::syntax::tree::TypeSignature;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::Item;
use crate::syntax::Token;
use crate::syntax::Tree;

pub use function_def::parse_args;

/// Parses normal statements.
#[derive(Debug, Default)]
pub struct BodyBlockParser<'s> {
    statement_parser: StatementParser<'s>,
}

impl<'s> BodyBlockParser<'s> {
    /// Parse the statements in a block.
    pub fn parse_body_block(
        &mut self,
        lines: &mut Vec<item::Line<'s>>,
        precedence: &mut Precedence<'s>,
    ) -> Tree<'s> {
        let lines = compound_lines_with_tail_expression(lines, |prefixes, line, is_tail| {
            if is_tail {
                self.statement_parser.parse_tail_expression(line, precedence)
            } else {
                self.statement_parser.parse_statement(prefixes, line, precedence)
            }
        });
        Tree::body_block(block::compound_lines(lines).collect())
    }

    /// Parse the declarations and statements at the top level of a module.
    pub fn parse_module(
        &mut self,
        lines: &mut Vec<item::Line<'s>>,
        precedence: &mut Precedence<'s>,
    ) -> Tree<'s> {
        let lines = compound_lines(lines, |prefixes, line| {
            self.statement_parser.parse_module_statement(prefixes, line, precedence)
        });
        Tree::body_block(block::compound_lines(lines).collect())
    }
}

fn compound_lines<'s>(
    lines: &mut Vec<item::Line<'s>>,
    mut parse_line: impl FnMut(
        &mut Vec<Line<'s, StatementPrefix<'s>>>,
        item::Line<'s>,
    ) -> Line<'s, StatementOrPrefix<'s>>,
) -> Vec<block::Line<'s>> {
    compound_lines_maybe_with_tail_expression(
        lines,
        |prefixes, line, _| parse_line(prefixes, line),
        None,
    )
}

fn compound_lines_with_tail_expression<'s>(
    lines: &mut Vec<item::Line<'s>>,
    parse_line: impl FnMut(
        &mut Vec<Line<'s, StatementPrefix<'s>>>,
        item::Line<'s>,
        bool,
    ) -> Line<'s, StatementOrPrefix<'s>>,
) -> Vec<block::Line<'s>> {
    compound_lines_maybe_with_tail_expression(
        lines,
        parse_line,
        lines.iter().enumerate().rfind(|(_, prefix)| !prefix.items.is_empty()).map(|(i, _)| i),
    )
}

fn compound_lines_maybe_with_tail_expression<'s>(
    lines: &mut Vec<item::Line<'s>>,
    mut parse_line: impl FnMut(
        &mut Vec<Line<'s, StatementPrefix<'s>>>,
        item::Line<'s>,
        bool,
    ) -> Line<'s, StatementOrPrefix<'s>>,
    tail_index: Option<usize>,
) -> Vec<block::Line<'s>> {
    let mut block_lines = Vec::new();
    let mut line_prefixes = Vec::new();
    for (i, line) in lines.drain(..).enumerate() {
        let is_tail = tail_index == Some(i);
        match parse_line(&mut line_prefixes, line, is_tail) {
            Line { newline, content: Some(StatementOrPrefix::Statement(statement)) } => {
                for Line { newline, content } in line_prefixes.drain(..) {
                    block_lines.push(block::Line { newline, expression: content.map(Tree::from) })
                }
                block_lines.push(block::Line { newline, expression: Some(statement) })
            }
            Line { newline, content: Some(StatementOrPrefix::Prefix(prefix)) } =>
                line_prefixes.push(Line { newline, content: Some(prefix) }),
            Line { newline, content: None } =>
                if line_prefixes.is_empty() {
                    block_lines.push(newline.into());
                } else {
                    line_prefixes.push(newline.into());
                },
        }
    }
    for Line { newline, content } in line_prefixes {
        block_lines.push(block::Line { newline, expression: content.map(Tree::from) })
    }
    block_lines
}

#[derive(Debug)]
struct Line<'s, T> {
    newline: token::Newline<'s>,
    content: Option<T>,
}

impl<'s, T> Line<'s, T> {
    fn map_content<U>(self, f: impl FnOnce(T) -> U) -> Line<'s, U> {
        let Line { newline, content } = self;
        Line { newline, content: content.map(f) }
    }
}

impl<'s, T> From<token::Newline<'s>> for Line<'s, T> {
    fn from(newline: token::Newline<'s>) -> Self {
        Self { newline, content: None }
    }
}

#[derive(Debug, Default)]
struct StatementParser<'s> {
    args_buffer: Vec<ArgumentDefinition<'s>>,
}

impl<'s> StatementParser<'s> {
    fn parse_statement(
        &mut self,
        prefixes: &mut Vec<Line<'s, StatementPrefix<'s>>>,
        line: item::Line<'s>,
        precedence: &mut Precedence<'s>,
    ) -> Line<'s, StatementOrPrefix<'s>> {
        parse_statement(prefixes, line, precedence, &mut self.args_buffer, StatementContext {
            evaluation_context: EvaluationContext::Eager,
            visibility_context: VisibilityContext::Private,
            tail_expression:    false,
        })
    }

    fn parse_tail_expression(
        &mut self,
        line: item::Line<'s>,
        precedence: &mut Precedence<'s>,
    ) -> Line<'s, StatementOrPrefix<'s>> {
        parse_statement(&mut vec![], line, precedence, &mut self.args_buffer, StatementContext {
            evaluation_context: EvaluationContext::Eager,
            visibility_context: VisibilityContext::Private,
            tail_expression:    true,
        })
    }

    fn parse_module_statement(
        &mut self,
        prefixes: &mut Vec<Line<'s, StatementPrefix<'s>>>,
        line: item::Line<'s>,
        precedence: &mut Precedence<'s>,
    ) -> Line<'s, StatementOrPrefix<'s>> {
        parse_statement(prefixes, line, precedence, &mut self.args_buffer, StatementContext {
            evaluation_context: EvaluationContext::Lazy,
            visibility_context: VisibilityContext::Public,
            tail_expression:    false,
        })
        .map_content(|statement_or_prefix| {
            statement_or_prefix.map_statement(|statement| {
                let error = match &statement.variant {
                    tree::Variant::Assignment(_) =>
                        SyntaxError::StmtUnexpectedAssignmentInModuleBody.into(),
                    _ => None,
                };
                maybe_with_error(statement, error)
            })
        })
    }
}

fn scan_private_keywords<'s>(items: impl IntoIterator<Item = impl AsRef<Item<'s>>>) -> usize {
    items
        .into_iter()
        .take_while(|item| {
            matches!(
                item.as_ref(),
                Item::Token(Token { variant: token::Variant::PrivateKeyword(_), .. })
            )
        })
        .count()
}

enum StatementPrefix<'s> {
    TypeSignature(TypeSignature<'s>),
    Annotation(FunctionAnnotation<'s>),
}

impl<'s> From<StatementPrefix<'s>> for Tree<'s> {
    fn from(value: StatementPrefix<'s>) -> Self {
        match value {
            StatementPrefix::TypeSignature(signature) =>
                Tree::type_signature_declaration(signature),
            StatementPrefix::Annotation(annotation) =>
                Tree::annotation(annotation).with_error(SyntaxError::AnnotationExpectedDefinition),
        }
    }
}

enum StatementOrPrefix<'s> {
    Statement(Tree<'s>),
    Prefix(StatementPrefix<'s>),
}

impl<'s> StatementOrPrefix<'s> {
    fn map_statement(self, f: impl FnOnce(Tree<'s>) -> Tree<'s>) -> Self {
        match self {
            StatementOrPrefix::Statement(statement) => StatementOrPrefix::Statement(f(statement)),
            prefix => prefix,
        }
    }
}

impl<'s> From<StatementOrPrefix<'s>> for Tree<'s> {
    fn from(value: StatementOrPrefix<'s>) -> Self {
        match value {
            StatementOrPrefix::Statement(tree) => tree,
            StatementOrPrefix::Prefix(prefix) => prefix.into(),
        }
    }
}

impl<'s> From<Tree<'s>> for StatementOrPrefix<'s> {
    fn from(value: Tree<'s>) -> Self {
        StatementOrPrefix::Statement(value)
    }
}

fn parse_statement<'s>(
    prefixes: &mut Vec<Line<'s, StatementPrefix<'s>>>,
    mut line: item::Line<'s>,
    precedence: &mut Precedence<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
    statement_context: StatementContext,
) -> Line<'s, StatementOrPrefix<'s>> {
    use token::Variant;
    let newline = line.newline;
    let private_keywords = scan_private_keywords(&line.items);
    let start = private_keywords;
    let items = &mut line.items;
    if let Some(annotation) = try_parse_annotation(items, start, precedence) {
        debug_assert_eq!(items.len(), start);
        return Line {
            newline,
            content: apply_private_keywords(
                Some(StatementOrPrefix::Prefix(StatementPrefix::Annotation(annotation))),
                items.drain(..),
                statement_context.visibility_context,
            ),
        };
    }
    if let Some(type_def) = try_parse_type_def(items, start, precedence, args_buffer) {
        debug_assert_eq!(items.len(), start);
        return Line {
            newline,
            content: apply_private_keywords(
                Some(type_def),
                items.drain(..),
                statement_context.visibility_context,
            )
            .map(StatementOrPrefix::Statement),
        };
    }
    let top_level_operator = match find_top_level_operator(&items[start..]) {
        Ok(top_level_operator) => top_level_operator.map(|(i, t)| (i + start, t)),
        Err(e) =>
            return Line {
                newline,
                content: Some(precedence.resolve_non_section(items).unwrap().with_error(e).into()),
            },
    };
    match top_level_operator {
        Some((i, Token { variant: Variant::AssignmentOperator(_), .. })) =>
            parse_assignment_like_statement(
                prefixes,
                item::Line { newline, items: mem::take(items) },
                start,
                i,
                precedence,
                args_buffer,
                statement_context,
            )
            .map_content(StatementOrPrefix::Statement),
        Some((i, Token { variant: Variant::TypeAnnotationOperator(_), .. })) => {
            let statement = parse_type_annotation_statement(
                items,
                start,
                i,
                precedence,
                statement_context.tail_expression,
            );
            Line {
                newline,
                content: apply_private_keywords(
                    Some(statement),
                    items.drain(..),
                    statement_context.visibility_context,
                ),
            }
        }
        Some(_) => unreachable!(),
        None => {
            let statement = precedence.resolve_offset(start, items);
            debug_assert!(items.len() <= start);
            Line {
                newline,
                content: apply_private_keywords(
                    statement,
                    items.drain(..),
                    statement_context.visibility_context,
                )
                .map(StatementOrPrefix::Statement),
            }
        }
    }
}

fn try_parse_annotation<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    precedence: &mut Precedence<'s>,
) -> Option<FunctionAnnotation<'s>> {
    match &items[..] {
        [Item::Token(Token { variant: token::Variant::AnnotationOperator(opr), .. }), Item::Token(Token { variant: token::Variant::Ident(ident), .. }), ..]
            if !ident.is_type =>
        {
            let ident = *ident;
            let opr = *opr;
            let argument = precedence.resolve_non_section_offset(start + 2, items);
            let annotation = items.pop().unwrap().into_token().unwrap().with_variant(ident);
            let operator = items.pop().unwrap().into_token().unwrap().with_variant(opr);
            Some(FunctionAnnotation { operator, annotation, argument })
        }
        _ => None,
    }
}

fn parse_type_annotation_statement<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    operator_index: usize,
    precedence: &mut Precedence<'s>,
    tail_expression: bool,
) -> StatementOrPrefix<'s> {
    let type_ = precedence.resolve_non_section_offset(operator_index + 1, items);
    let operator: token::TypeAnnotationOperator =
        items.pop().unwrap().into_token().unwrap().try_into().unwrap();
    let lhs = precedence.resolve_non_section_offset(start, items);
    let type_ = type_.unwrap_or_else(|| {
        empty_tree(operator.code.position_after()).with_error(SyntaxError::ExpectedType)
    });
    debug_assert!(items.len() <= start);
    if lhs.as_ref().is_some_and(is_qualified_name) && !tail_expression {
        StatementOrPrefix::Prefix(StatementPrefix::TypeSignature(TypeSignature {
            name: lhs.unwrap(),
            operator,
            type_,
        }))
    } else {
        let lhs = lhs.unwrap_or_else(|| {
            empty_tree(operator.left_offset.code.position_before())
                .with_error(SyntaxError::ExpectedExpression)
        });
        Tree::type_annotated(lhs, operator, type_).into()
    }
}

/// Apply any private keywords that were not already consumed by a statement parser that recognizes
/// them specifically (such as in a function definition).
fn apply_private_keywords<'s, U: From<Tree<'s>> + Into<Tree<'s>>>(
    mut statement: Option<U>,
    keywords: impl Iterator<Item = Item<'s>>,
    visibility_context: VisibilityContext,
) -> Option<U> {
    for item in keywords {
        let private = Tree::private(item.into_token().unwrap().try_into().unwrap());
        statement = Some(
            match statement.take() {
                Some(statement) => Tree::app(
                    private.with_error(match visibility_context {
                        VisibilityContext::Public => SyntaxError::StmtUnexpectedPrivateSubject,
                        VisibilityContext::Private => SyntaxError::StmtUnexpectedPrivateContext,
                    }),
                    statement.into(),
                ),
                None => maybe_with_error(private, match visibility_context {
                    // This is the only non-error case in this function: A `private` keyword was
                    // found not modifying any other statement, and in a context where a `private`
                    // declaration is allowed; in this case, we emit a `Private` declaration.
                    VisibilityContext::Public => None,
                    VisibilityContext::Private => Some(SyntaxError::StmtUnexpectedPrivateContext),
                }),
            }
            .into(),
        );
    }
    statement
}

fn apply_excess_private_keywords<'s>(
    mut statement: Option<Tree<'s>>,
    keywords: impl Iterator<Item = Item<'s>>,
    error: SyntaxError,
) -> Option<Tree<'s>> {
    for item in keywords {
        let private =
            Tree::private(item.into_token().unwrap().try_into().unwrap()).with_error(error.clone());
        statement = match statement.take() {
            Some(statement) => Tree::app(private, statement),
            None => private,
        }
        .into();
    }
    statement
}

#[derive(Debug, Copy, Clone)]
struct StatementContext {
    evaluation_context: EvaluationContext,
    visibility_context: VisibilityContext,
    tail_expression:    bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum EvaluationContext {
    /// A context in which variable assignments are allowed.
    Eager,
    /// A context in which variable assignments must not occur.
    Lazy,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum VisibilityContext {
    /// A context in which declared symbols are exported unless marked `private`.
    Public,
    /// A context in which declared symbols are not exported, and may not be marked `private`.
    Private,
}

fn parse_assignment_like_statement<'s>(
    prefixes: &mut Vec<Line<'s, StatementPrefix<'s>>>,
    mut line: item::Line<'s>,
    start: usize,
    operator: usize,
    precedence: &mut Precedence<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
    StatementContext { evaluation_context, visibility_context, .. }: StatementContext,
) -> Line<'s, Tree<'s>> {
    let items = &mut line.items;
    let newline = line.newline;
    if operator == start {
        let error = precedence
            .resolve_non_section_offset(start, items)
            .unwrap()
            .with_error(SyntaxError::StmtInvalidAssignmentOrMethod);
        return Line {
            newline,
            content: apply_private_keywords(Some(error), items.drain(..), visibility_context),
        };
    }

    let mut expression = precedence.resolve_offset(operator + 1, items);

    let operator = items.pop().unwrap().into_token().unwrap().try_into().unwrap();

    let qn_len = match (evaluation_context, scan_qn(&items[start..])) {
        (_, Some(Qn::Binding { len }))
        // In a context where assignments are not allowed, even a name whose last identifier is
        // capitalized can be a function definition (rather than an assignment pattern).
        | (EvaluationContext::Lazy, Some(Qn::Type { len })) => len.into(),
        _ => None,
    };

    let mut operator = Some(operator);
    if let Some(function) = try_parse_foreign_function(
        items,
        start,
        &mut operator,
        &mut expression,
        precedence,
        args_buffer,
    ) {
        return Line {
            newline,
            content: apply_private_keywords(Some(function), items.drain(..), visibility_context),
        };
    }
    let operator = operator.unwrap();

    enum Type<'s> {
        Assignment { expression: Tree<'s> },
        Function { expression: Option<Tree<'s>>, qn_len: usize },
        InvalidNoExpressionNoQn,
    }
    match match (expression, qn_len) {
        (Some(e), Some(qn_len))
            if matches!(evaluation_context, EvaluationContext::Lazy)
                || matches!(e.variant, tree::Variant::BodyBlock(_)) =>
            Type::Function { expression: Some(e), qn_len },
        (Some(expression), None) => Type::Assignment { expression },
        (Some(expression), Some(1)) if items.len() == start + 1 => Type::Assignment { expression },
        (expression, Some(qn_len)) => Type::Function { expression, qn_len },
        (None, None) => Type::InvalidNoExpressionNoQn,
    } {
        Type::Assignment { expression } => Line {
            newline,
            content: apply_private_keywords(
                Some(parse_assignment(start, items, operator, expression, precedence)),
                items.drain(..),
                visibility_context,
            ),
        },
        Type::Function { expression, qn_len } => FunctionBuilder::new(
            item::Line { newline, items: mem::take(items) },
            start,
            qn_len,
            precedence,
            args_buffer,
        )
        .build(prefixes, operator, expression, visibility_context),
        Type::InvalidNoExpressionNoQn => Line {
            newline,
            content: Some(
                Tree::opr_app(
                    precedence.resolve_non_section(items),
                    Ok(operator.with_variant(token::variant::Operator())),
                    None,
                )
                .with_error(SyntaxError::StmtInvalidAssignmentOrMethod),
            ),
        },
    }
}

fn parse_assignment<'s>(
    start: usize,
    items: &mut Vec<Item<'s>>,
    operator: token::AssignmentOperator<'s>,
    expression: Tree<'s>,
    precedence: &mut Precedence<'s>,
) -> Tree<'s> {
    let pattern =
        expression_to_pattern(precedence.resolve_non_section_offset(start, items).unwrap());
    Tree::assignment(pattern, operator, expression)
}

fn parse_pattern<'s>(
    items: &mut Vec<Item<'s>>,
    arg_start: usize,
    precedence: &mut Precedence<'s>,
) -> (Option<token::SuspensionOperator<'s>>, Option<Tree<'s>>) {
    let have_suspension = matches!(
        items.get(arg_start),
        Some(Item::Token(Token { variant: token::Variant::SuspensionOperator(_), .. }))
    );
    let pattern_start = arg_start + have_suspension as usize;
    let pattern = if items.len() - pattern_start == 1 {
        Some(match items.last().unwrap() {
            Item::Token(_) => {
                let token = items.pop().unwrap().into_token().unwrap();
                match token.variant {
                    token::Variant::Ident(variant) => Tree::ident(token.with_variant(variant)),
                    token::Variant::Wildcard(variant) =>
                        Tree::wildcard(token.with_variant(variant), None),
                    _ => tree::to_ast(token).with_error(SyntaxError::ArgDefExpectedPattern),
                }
            }
            _ => precedence
                .resolve_non_section_offset(items.len() - 1, items)
                .map(|tree| tree.with_error(SyntaxError::ArgDefExpectedPattern))
                .unwrap(),
        })
    } else {
        precedence
            .resolve_non_section_offset(pattern_start, items)
            .map(|tree| tree.with_error(SyntaxError::ArgDefExpectedPattern))
    };
    let suspension =
        have_suspension.then(|| items.pop().unwrap().into_token().unwrap().try_into().unwrap());
    (suspension, pattern)
}

fn find_top_level_operator<'a, 's>(
    items: &'a [Item<'s>],
) -> Result<Option<(usize, &'a Token<'s>)>, SyntaxError> {
    use token::Variant;
    let mut candidate: Option<(usize, &'a Token<'s>, bool)> = None;
    let mut after_first_space = false;
    for (i, item) in items.iter().enumerate() {
        let next_is_after_space =
            i != 0 && (after_first_space || matches!(Spacing::of_item(item), Spacing::Spaced));
        if let Item::Token(token) = item {
            let is_spaced = token.is_spaced();
            if !after_first_space || is_spaced {
                match &token.variant {
                    Variant::AssignmentOperator(_) => {
                        if is_spaced
                            && items
                                .get(i + 1)
                                .is_some_and(|item| Spacing::of_item(item) == Spacing::Unspaced)
                        {
                            return Err(SyntaxError::StmtLhsInvalidOperatorSpacing);
                        }
                        if is_spaced {
                            return Ok(Some((i, token)));
                        }
                        if candidate.is_none()
                            || (is_spaced && !candidate.unwrap().2)
                            || !matches!(
                                candidate.unwrap().1.variant,
                                Variant::AssignmentOperator(_)
                            )
                        {
                            candidate = Some((i, token, is_spaced));
                        }
                    }
                    Variant::TypeAnnotationOperator(_) => {
                        if is_spaced
                            && items
                                .get(i + 1)
                                .is_some_and(|item| Spacing::of_item(item) == Spacing::Unspaced)
                        {
                            return Err(SyntaxError::StmtLhsInvalidOperatorSpacing);
                        }
                        if candidate.is_none() || (is_spaced && !candidate.unwrap().2) {
                            candidate = Some((i, token, is_spaced));
                        }
                    }
                    Variant::Operator(_)
                    | Variant::DotOperator(_)
                    | Variant::ArrowOperator(_)
                    | Variant::CommaOperator(_) =>
                        if is_spaced && candidate.is_some_and(|(_, _, is_spaced)| !is_spaced) {
                            candidate = None;
                        },
                    _ => {}
                }
            }
        }
        after_first_space = next_is_after_space;
    }
    Ok(candidate.map(|(i, t, _)| (i, t)))
}

fn next_spaced(items: &[Item]) -> Option<usize> {
    for (i, item) in items.iter().enumerate().skip(1) {
        if matches!(Spacing::of_item(item), Spacing::Spaced) {
            return Some(i);
        }
    }
    None
}

#[derive(Debug)]
enum Qn {
    /// A qualified-name whose last segment is capitalized; usually a type or module.
    Type { len: usize },
    /// A qualified-name whose last segment is lowercase; usually a variable or function.
    Binding { len: usize },
}

/// Returns length of the QN.
fn scan_qn<'s>(items: impl IntoIterator<Item = impl AsRef<Item<'s>>>) -> Option<Qn> {
    #[derive(Copy, Clone)]
    enum State {
        ExpectingDot { len: usize },
        ExpectingIdent,
    }
    use token::Variant::*;
    use Item::*;
    use State::*;
    let mut state = ExpectingIdent;
    for (i, item) in items.into_iter().enumerate() {
        match item.as_ref() {
            Token(token) if i != 0 && token.is_spaced() => break,
            Token(token) => match (state, &token.variant) {
                (ExpectingDot { .. }, DotOperator(_)) => state = ExpectingIdent,
                (ExpectingIdent, Ident(ident)) if ident.is_type =>
                    state = ExpectingDot { len: i + 1 },
                (
                    ExpectingIdent,
                    Ident(_) | Operator(_) | NegationOperator(_) | UnaryOperator(_),
                ) => return Some(Qn::Binding { len: i + 1 }),
                _ => break,
            },
            Group(_) | Tree(_) => break,
            Block(_) => unreachable!(),
        }
    }
    match state {
        ExpectingDot { len } => Some(Qn::Type { len }),
        _ => None,
    }
}
