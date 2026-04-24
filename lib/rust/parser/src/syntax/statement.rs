//! Parses statements in module, body blocks, and type blocks.

mod function_def;
mod type_def;

use crate::prelude::*;
use crate::syntax::Item;
use crate::syntax::Token;
use crate::syntax::Tree;
use crate::syntax::expression::ExpressionParser;
use crate::syntax::expression::Spacing;
use crate::syntax::item;
use crate::syntax::maybe_with_error;
use crate::syntax::statement::function_def::FunctionBuilder;
use crate::syntax::statement::function_def::try_parse_foreign_function;
use crate::syntax::statement::type_def::try_parse_type_def;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::tree::AnnotationLine;
use crate::syntax::tree::ArgumentDefinition;
use crate::syntax::tree::DocComment;
use crate::syntax::tree::DocLine;
use crate::syntax::tree::FunctionAnnotation;
use crate::syntax::tree::SyntaxError;
use crate::syntax::tree::TypeSignature;
use crate::syntax::tree::TypeSignatureLine;
use crate::syntax::tree::block;
use crate::{empty_tree, to_qualified_name};
use crate::{expression_to_pattern, expression_to_type};

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
        expression_parser: &mut ExpressionParser<'s>,
    ) -> Tree<'s> {
        Tree::body_block(compound_lines_with_tail(lines, |prefixes, line, block_context| {
            self.statement_parser.parse_block_statement(
                prefixes,
                line,
                expression_parser,
                block_context,
            )
        }))
    }

    /// Parse the declarations and statements at the top level of a module.
    pub fn parse_module(
        &mut self,
        lines: &mut Vec<item::Line<'s>>,
        expression_parser: &mut ExpressionParser<'s>,
    ) -> Tree<'s> {
        let lines = compound_lines(lines, |prefixes, line| {
            self.statement_parser.parse_module_statement(prefixes, line, expression_parser)
        });
        Tree::body_block(block::compound_lines(lines).collect())
    }
}

fn compound_lines<'s>(
    lines: &mut Vec<item::Line<'s>>,
    mut parse_line: impl FnMut(
        &mut StatementPrefixes<'s>,
        item::Line<'s>,
    ) -> Line<'s, StatementOrPrefix<'s>>,
) -> Vec<block::Line<'s>> {
    compound_lines_maybe_with_tail(lines, |prefixes, line, _| parse_line(prefixes, line), None)
}

fn compound_lines_with_tail<'s>(
    lines: &mut Vec<item::Line<'s>>,
    parse_line: impl FnMut(
        &mut StatementPrefixes<'s>,
        item::Line<'s>,
        BlockContext,
    ) -> Line<'s, StatementOrPrefix<'s>>,
) -> Vec<block::Line<'s>> {
    compound_lines_maybe_with_tail(
        lines,
        parse_line,
        lines.iter().enumerate().rfind(|(_, line)| !line.items.is_empty()).map(|(i, _)| i),
    )
}

fn compound_lines_maybe_with_tail<'s>(
    lines: &mut Vec<item::Line<'s>>,
    mut parse_line: impl FnMut(
        &mut StatementPrefixes<'s>,
        item::Line<'s>,
        BlockContext,
    ) -> Line<'s, StatementOrPrefix<'s>>,
    tail_index: Option<usize>,
) -> Vec<block::Line<'s>> {
    let mut block_lines = Vec::new();
    let mut line_prefixes = StatementPrefixes::default();
    for (i, line) in lines.drain(..).enumerate() {
        let block_context =
            if tail_index == Some(i) { BlockContext::BlockTail } else { BlockContext::BlockBody };
        match parse_line(&mut line_prefixes, line, block_context) {
            Line { newline, content: Some(StatementOrPrefix::Statement(statement)) } => {
                line_prefixes.drain_unused_into(&mut block_lines);
                block_lines.push(block::Line { newline, expression: Some(statement) })
            }
            Line { newline, content: Some(StatementOrPrefix::Prefix(prefix)) } => {
                line_prefixes.push(newline, prefix)
            }
            Line { newline, content: None } => {
                if line_prefixes.prefixes.is_empty() {
                    block_lines.push(newline.into());
                } else {
                    line_prefixes.push_newline(newline);
                }
            }
        }
    }
    line_prefixes.drain_unused_into(&mut block_lines);
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
    fn parse_block_statement(
        &mut self,
        prefixes: &mut StatementPrefixes<'s>,
        line: item::Line<'s>,
        expression_parser: &mut ExpressionParser<'s>,
        block_context: BlockContext,
    ) -> Line<'s, StatementOrPrefix<'s>> {
        parse_statement(
            prefixes,
            line,
            expression_parser,
            &mut self.args_buffer,
            StatementContext {
                evaluation_context: EvaluationContext::Eager,
                visibility_context: VisibilityContext::Private,
                block_context,
            },
        )
    }

    fn parse_module_statement(
        &mut self,
        prefixes: &mut StatementPrefixes<'s>,
        line: item::Line<'s>,
        expression_parser: &mut ExpressionParser<'s>,
    ) -> Line<'s, StatementOrPrefix<'s>> {
        parse_statement(
            prefixes,
            line,
            expression_parser,
            &mut self.args_buffer,
            StatementContext {
                evaluation_context: EvaluationContext::Lazy,
                visibility_context: VisibilityContext::Public,
                block_context: BlockContext::BlockBody,
            },
        )
        .map_content(|statement_or_prefix| {
            statement_or_prefix.map_statement(|statement| {
                let error = match &statement.variant {
                    tree::Variant::Assignment(_) => {
                        SyntaxError::StmtUnexpectedAssignmentInModuleBody.into()
                    }
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
    Documentation(DocComment<'s>),
}

impl<'s> From<StatementPrefix<'s>> for Tree<'s> {
    fn from(value: StatementPrefix<'s>) -> Self {
        match value {
            StatementPrefix::TypeSignature(signature) => {
                Tree::type_signature_declaration(signature)
            }
            StatementPrefix::Annotation(annotation) => {
                Tree::annotation(annotation).with_error(SyntaxError::AnnotationExpectedDefinition)
            }
            StatementPrefix::Documentation(docs) => Tree::documentation(docs),
        }
    }
}

#[derive(From)]
#[allow(clippy::large_enum_variant)]
enum StatementOrPrefix<'s> {
    Statement(Tree<'s>),
    Prefix(StatementPrefix<'s>),
}

impl<'s> StatementOrPrefix<'s> {
    fn map_statement(self, f: impl FnOnce(Tree<'s>) -> Tree<'s>) -> Self {
        match self {
            StatementOrPrefix::Statement(statement) => f(statement).into(),
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

enum StatementPrefixLine<'s> {
    TypeSignature(TypeSignatureLine<'s>),
    Annotation(AnnotationLine<'s>),
    Documentation(DocLine<'s>),
}

impl<'s> StatementPrefixLine<'s> {
    fn new(prefix: StatementPrefix<'s>, newlines: NonEmptyVec<token::Newline<'s>>) -> Self {
        match prefix {
            StatementPrefix::TypeSignature(signature) => {
                Self::TypeSignature(TypeSignatureLine { signature, newlines })
            }
            StatementPrefix::Annotation(annotation) => {
                Self::Annotation(AnnotationLine { annotation, newlines })
            }
            StatementPrefix::Documentation(docs) => {
                Self::Documentation(DocLine { docs, newlines: newlines.into() })
            }
        }
    }
}

fn parse_statement<'s>(
    prefixes: &mut StatementPrefixes<'s>,
    mut line: item::Line<'s>,
    expression_parser: &mut ExpressionParser<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
    statement_context: StatementContext,
) -> Line<'s, StatementOrPrefix<'s>> {
    let newline = line.newline;
    let private_keywords = scan_private_keywords(&line.items);
    let start = private_keywords;
    let items = &mut line.items;
    let parsed = None
        .or_else(|| {
            try_parse_annotation(items, start, expression_parser)
                .map(StatementPrefix::Annotation)
                .map(StatementOrPrefix::Prefix)
        })
        .or_else(|| {
            try_parse_type_def(items, start, expression_parser, args_buffer)
                .map(StatementOrPrefix::Statement)
        })
        .or_else(|| {
            try_parse_doc_comment(items)
                .map(StatementPrefix::Documentation)
                .map(StatementOrPrefix::Prefix)
        });
    if let Some(parsed) = parsed {
        debug_assert_eq!(items.len(), start);
        return Line {
            newline,
            content: apply_private_keywords(
                Some(parsed),
                items.drain(..),
                statement_context.visibility_context,
            ),
        };
    }
    let top_level_operator = match find_top_level_operator(&items[start..]) {
        Ok(top_level_operator) => top_level_operator,
        Err(e) => {
            return Line {
                newline,
                content: Some(
                    expression_parser.parse_non_section(items).unwrap().with_error(e).into(),
                ),
            };
        }
    };
    match (top_level_operator, statement_context.block_context) {
        (Some(TopLevelOperator::AssignmentOperator(i)), _) => parse_assignment_like_statement(
            prefixes,
            item::Line { newline, items: mem::take(items) },
            start,
            start + i,
            expression_parser,
            args_buffer,
            statement_context,
        )
        .map_content(StatementOrPrefix::Statement),
        (Some(TopLevelOperator::TypeAnnotationOperator(i)), BlockContext::BlockBody) => {
            let statement =
                parse_type_annotation_statement(items, start, start + i, expression_parser);
            Line {
                newline,
                content: apply_private_keywords(
                    Some(statement),
                    items.drain(..),
                    statement_context.visibility_context,
                ),
            }
        }
        (None, BlockContext::BlockBody) => parse_expression_statement(
            prefixes,
            start,
            item::Line { newline, items: mem::take(items) },
            expression_parser,
            statement_context.visibility_context,
        ),
        (None | Some(TopLevelOperator::TypeAnnotationOperator(_)), BlockContext::BlockTail) => {
            let mut first_newline = newline;
            let statement = expression_parser
                .parse_offset(0, &mut line.items)
                .map(|expression| {
                    Tree::expression_statement(
                        take_doc_line(prefixes, &mut first_newline),
                        expression,
                    )
                })
                .map(StatementOrPrefix::Statement);
            Line { newline: first_newline, content: statement }
        }
    }
}

#[derive(Default)]
struct StatementPrefixes<'s> {
    prefixes: Vec<(token::Newline<'s>, StatementPrefix<'s>, usize)>,
    newlines: Vec<token::Newline<'s>>,
}

impl<'s> StatementPrefixes<'s> {
    fn push(&mut self, newline: token::Newline<'s>, prefix: StatementPrefix<'s>) {
        let newlines_start = self.newlines.len();
        self.prefixes.push((newline, prefix, newlines_start))
    }

    fn push_newline(&mut self, newline: token::Newline<'s>) {
        self.newlines.push(newline)
    }

    fn last(&self) -> Option<&StatementPrefix<'s>> {
        self.prefixes.last().map(|(_, prefix, _)| prefix)
    }

    /// `first_newline`:
    /// - Before the call, must contain the first newline after the prefix.
    /// - Upon return, will contain the newline before the prefix.
    fn pop(&mut self, first_newline: &mut token::Newline<'s>) -> StatementPrefixLine<'s> {
        let (newline_before_prefix, prefix, trailing_newlines_start) = self.prefixes.pop().unwrap();
        let original_first_newline = mem::replace(first_newline, newline_before_prefix);
        let trailing_newlines = self.newlines.drain(trailing_newlines_start..);
        let mut newlines = Vec::with_capacity(trailing_newlines.len() + 1);
        newlines.extend(trailing_newlines);
        let newlines = NonEmptyVec::from_vec_and_last(newlines, original_first_newline);
        StatementPrefixLine::new(prefix, newlines)
    }

    fn drain_unused_into(&mut self, lines: &mut Vec<block::Line<'s>>) {
        lines.reserve(self.prefixes.len() + self.newlines.len());
        let mut empty_lines = self.newlines.drain(..).map(block::Line::from);
        let mut prev_trailing_newlines_start = 0;
        for (newline_before_prefix, prefix, trailing_newlines_start) in self.prefixes.drain(..) {
            let trailing_newlines = trailing_newlines_start - prev_trailing_newlines_start;
            prev_trailing_newlines_start = trailing_newlines_start;
            lines.extend((&mut empty_lines).take(trailing_newlines));
            lines.push(block::Line {
                newline: newline_before_prefix,
                expression: Some(prefix.into()),
            });
        }
        lines.extend(empty_lines);
    }
}

fn take_doc_line<'s>(
    prefixes: &mut StatementPrefixes<'s>,
    first_newline: &mut token::Newline<'s>,
) -> Option<DocLine<'s>> {
    if let Some(StatementPrefix::Documentation(_)) = prefixes.last() {
        let StatementPrefixLine::Documentation(doc_line) = prefixes.pop(first_newline) else {
            unreachable!()
        };
        Some(doc_line)
    } else {
        None
    }
}

fn parse_expression_statement<'s>(
    prefixes: &mut StatementPrefixes<'s>,
    start: usize,
    mut line: item::Line<'s>,
    expression_parser: &mut ExpressionParser<'s>,
    visibility_context: VisibilityContext,
) -> Line<'s, StatementOrPrefix<'s>> {
    let expression = expression_parser.parse_offset(start, &mut line.items);
    debug_assert!(line.items.len() <= start);
    let expression = apply_private_keywords(expression, line.items.drain(..), visibility_context);
    let mut first_newline = line.newline;
    let expression = expression
        .map(|expression| to_statement(prefixes, &mut first_newline, expression))
        .map(StatementOrPrefix::Statement);
    Line { newline: first_newline, content: expression }
}

/// `first_newline`:
/// - Before the call, must contain the first newline before `expression_or_statement`.
/// - Upon return, will contain the newline before the returned `Tree` (which will be different from
///   the passed value if any prefixes were consumed).
fn to_statement<'s>(
    prefixes: &mut StatementPrefixes<'s>,
    first_newline: &mut token::Newline<'s>,
    expression_or_statement: Tree<'s>,
) -> Tree<'s> {
    use tree::Variant::*;
    enum ExpressionOrStatement {
        Expression,
        Statement,
    }
    use ExpressionOrStatement::*;
    match match &expression_or_statement.variant {
        // Currently could be expression or statement--treating as expression.
        Invalid(_) => Ok(Expression),
        // Currently could be expression or statement--treating as statement so prefix-line
        // annotations don't affect how documentation is attached to a type.
        AnnotatedBuiltin(_) => Ok(Statement),
        // Expression
        ArgumentBlockApplication(_)
        | OperatorBlockApplication(_)
        | Ident(_)
        | Number(_)
        | Wildcard(_)
        | SuspendedDefaultArguments(_)
        | TextLiteral(_)
        | App(_)
        | NamedApp(_)
        | UnaryOprApp(_)
        | AutoscopedIdentifier(_)
        | MultiSegmentApp(_)
        | Group(_)
        | TypeAnnotated(_)
        | CaseOf(_)
        | Array(_)
        | Tuple(_)
        | PropertyAccess(_)
        | Call(_) => Ok(Expression),
        OprApp(app) if app.lhs.is_some() && app.rhs.is_some() => Ok(Expression),
        // Expression, but since it can only occur in tail position, it never needs an
        // `ExpressionStatement` node.
        BodyBlock(_) => Ok(Statement),
        // Statement
        Private(_)
        | TypeDef(_)
        | Assignment(_)
        | Function(_)
        | ForeignFunction(_)
        | Import(_)
        | Export(_)
        | TypeSignatureDeclaration(_)
        | Annotation(_)
        | Documentation(_)
        | ConstructorDefinition(_) => Ok(Statement),
        // Operator sections (fully-applied operators are matched above)
        OprApp(_) => Err(SyntaxError::StmtUnexpectedFunctionExpressionOprSection),
        TemplateFunction(_) | Lambda(_) => Err(SyntaxError::StmtUnexpectedFunctionExpression),
        // Shouldn't be possible here, but this is not currently guaranteed by the types.
        ExpressionStatement(_) => Err(SyntaxError::Internal),
    } {
        Ok(Expression) => Tree::expression_statement(
            take_doc_line(prefixes, first_newline),
            expression_or_statement,
        ),
        Ok(Statement) => expression_or_statement,
        Err(error) => expression_or_statement.with_error(error),
    }
}

/// Parse the input as a documentation comment, if it matches the syntax.
pub fn try_parse_doc_comment<'s>(items: &mut Vec<Item<'s>>) -> Option<DocComment<'s>> {
    match items.first() {
        Some(Item::Token(token @ Token { variant: token::Variant::TextStart(_), .. }))
            if token.code.repr.0 == "##" =>
        {
            let mut items = items.drain(..);
            let Some(Item::Token(open)) = items.next() else { unreachable!() };
            let elements = items
                .filter_map(|item| {
                    let Item::Token(token) = item else { unreachable!() };
                    match token.variant {
                        token::Variant::TextSection(variant) => {
                            let token = token.with_variant(variant);
                            Some(tree::TextElement::Section { text: token })
                        }
                        token::Variant::TextEscape(variant) => {
                            let token = token.with_variant(variant);
                            Some(tree::TextElement::Escape { token })
                        }
                        token::Variant::TextNewline(_) => {
                            let token = token::newline(token.left_offset, token.code);
                            Some(tree::TextElement::Newline { newline: token })
                        }
                        token::Variant::TextEnd(_) => None,
                        _ => unreachable!(),
                    }
                })
                .collect();
            Some(DocComment { open: open.with_variant(token::variant::TextStart()), elements })
        }
        _ => None,
    }
}

fn try_parse_annotation<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    expression_parser: &mut ExpressionParser<'s>,
) -> Option<FunctionAnnotation<'s>> {
    match &items[..] {
        [
            Item::Token(Token { variant: token::Variant::AnnotationOperator(opr), .. }),
            Item::Token(Token { variant: token::Variant::Ident(ident), .. }),
            ..,
        ] if !ident.is_type => {
            let ident = *ident;
            let opr = *opr;
            let argument = expression_parser.parse_non_section_offset(start + 2, items);
            let annotation = items.pop().unwrap().try_into_token().unwrap().with_variant(ident);
            let operator = items.pop().unwrap().try_into_token().unwrap().with_variant(opr);
            Some(FunctionAnnotation { operator, annotation, argument })
        }
        _ => None,
    }
}

fn parse_type_annotation_statement<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    operator_index: usize,
    expression_parser: &mut ExpressionParser<'s>,
) -> StatementOrPrefix<'s> {
    let type_ = expression_parser.parse_non_section_offset(operator_index + 1, items);
    let operator: token::TypeAnnotationOperator =
        items.pop().unwrap().try_into_token().unwrap().try_into().unwrap();
    let lhs = expression_parser.parse_non_section_offset(start, items);
    let type_ = type_.map(expression_to_type).unwrap_or_else(|| {
        empty_tree(operator.code.position_after()).with_error(SyntaxError::ExpectedType)
    });
    debug_assert!(items.len() <= start);
    if let Some(lhs) = lhs {
        match to_qualified_name(lhs) {
            Ok(lhs) => {
                StatementPrefix::TypeSignature(TypeSignature { name: lhs, operator, type_ }).into()
            }
            Err(lhs) => Tree::type_annotated(lhs, operator, type_).into(),
        }
    } else {
        let lhs = empty_tree(operator.left_offset.code.position_before())
            .with_error(SyntaxError::ExpectedExpression);
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
        let private = Tree::private(item.try_into_token().unwrap().try_into().unwrap());
        statement = Some(
            match statement.take() {
                Some(statement) => Tree::app(
                    private.with_error(match visibility_context {
                        VisibilityContext::Public => SyntaxError::StmtUnexpectedPrivateSubject,
                        VisibilityContext::Private => SyntaxError::StmtUnexpectedPrivateContext,
                    }),
                    statement.into(),
                ),
                None => maybe_with_error(
                    private,
                    match visibility_context {
                        // This is the only non-error case in this function: A `private` keyword was
                        // found not modifying any other statement, and in a context where a `private`
                        // declaration is allowed; in this case, we emit a `Private` declaration.
                        VisibilityContext::Public => None,
                        VisibilityContext::Private => {
                            Some(SyntaxError::StmtUnexpectedPrivateContext)
                        }
                    },
                ),
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
        let private = Tree::private(item.try_into_token().unwrap().try_into().unwrap())
            .with_error(error.clone());
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
    block_context: BlockContext,
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum BlockContext {
    /// A line in non-final position, which must be a statement (including expression statements).
    BlockBody,
    /// A line in final position, which may be a statement or a tail expression.
    BlockTail,
}

fn parse_assignment_like_statement<'s>(
    prefixes: &mut StatementPrefixes<'s>,
    mut line: item::Line<'s>,
    start: usize,
    operator: usize,
    expression_parser: &mut ExpressionParser<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
    StatementContext { evaluation_context, visibility_context, .. }: StatementContext,
) -> Line<'s, Tree<'s>> {
    let items = &mut line.items;
    let newline = line.newline;
    if operator == start {
        let error = expression_parser
            .parse_non_section_offset(start, items)
            .unwrap()
            .with_error(SyntaxError::StmtInvalidAssignmentOrMethod);
        return Line {
            newline,
            content: apply_private_keywords(Some(error), items.drain(..), visibility_context),
        };
    }

    let mut expression = expression_parser.parse_offset(operator + 1, items);

    let operator = items.pop().unwrap().try_into_token().unwrap().try_into().unwrap();

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
        expression_parser,
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
            if evaluation_context == EvaluationContext::Lazy
                || matches!(e.variant, tree::Variant::BodyBlock(_)) =>
        {
            Type::Function { expression: Some(e), qn_len }
        }
        (Some(expression), None) => Type::Assignment { expression },
        (Some(expression), Some(1)) if items.len() == start + 1 => Type::Assignment { expression },
        (expression, Some(qn_len)) => Type::Function { expression, qn_len },
        (None, None) => Type::InvalidNoExpressionNoQn,
    } {
        Type::Assignment { expression } => AssignmentBuilder::new(
            start,
            item::Line { newline, items: mem::take(items) },
            operator,
            expression,
            expression_parser,
        )
        .build(prefixes, visibility_context),
        Type::Function { expression, qn_len } => FunctionBuilder::new(
            item::Line { newline, items: mem::take(items) },
            start,
            qn_len,
            expression_parser,
            args_buffer,
        )
        .build(prefixes, operator, expression, visibility_context),
        Type::InvalidNoExpressionNoQn => Line {
            newline,
            content: Some(
                Tree::opr_app(
                    expression_parser.parse_non_section(items),
                    Ok(operator.with_variant(token::variant::Operator())),
                    None,
                )
                .with_error(SyntaxError::StmtInvalidAssignmentOrMethod),
            ),
        },
    }
}

struct AssignmentBuilder<'s> {
    newline: token::Newline<'s>,
    pattern: Tree<'s>,
    operator: token::AssignmentOperator<'s>,
    expression: Tree<'s>,
    excess_items: Vec<Item<'s>>,
}

impl<'s> AssignmentBuilder<'s> {
    fn new(
        start: usize,
        mut line: item::Line<'s>,
        operator: token::AssignmentOperator<'s>,
        expression: Tree<'s>,
        expression_parser: &mut ExpressionParser<'s>,
    ) -> Self {
        let pattern = expression_to_pattern(
            expression_parser.parse_non_section_offset(start, &mut line.items).unwrap(),
        );
        Self { newline: line.newline, pattern, operator, expression, excess_items: line.items }
    }

    fn build(
        self,
        prefixes: &mut StatementPrefixes<'s>,
        visibility_context: VisibilityContext,
    ) -> Line<'s, Tree<'s>> {
        let Self { newline, pattern, operator, expression, excess_items } = self;
        let mut first_newline = newline;
        let doc_line = take_doc_line(prefixes, &mut first_newline);
        Line {
            newline: first_newline,
            content: apply_private_keywords(
                Some(Tree::assignment(doc_line, pattern, operator, expression)),
                excess_items.into_iter(),
                visibility_context,
            ),
        }
    }
}

fn parse_pattern<'s>(
    items: &mut Vec<Item<'s>>,
    arg_start: usize,
    expression_parser: &mut ExpressionParser<'s>,
) -> (Option<token::SuspensionOperator<'s>>, Option<Tree<'s>>) {
    let have_suspension = matches!(
        items.get(arg_start),
        Some(Item::Token(Token { variant: token::Variant::SuspensionOperator(_), .. }))
    );
    let pattern_start = arg_start + have_suspension as usize;
    let pattern = if items.len() - pattern_start == 1 {
        Some(match items.last().unwrap() {
            Item::Token(_) => {
                let token = items.pop().unwrap().try_into_token().unwrap();
                match token.variant {
                    token::Variant::Ident(variant) => Tree::ident(token.with_variant(variant)),
                    token::Variant::Wildcard(variant) => {
                        Tree::wildcard(token.with_variant(variant))
                    }
                    _ => tree::to_ast(token).with_error(SyntaxError::ArgDefExpectedPattern),
                }
            }
            _ => expression_parser
                .parse_non_section_offset(items.len() - 1, items)
                .map(|tree| tree.with_error(SyntaxError::ArgDefExpectedPattern))
                .unwrap(),
        })
    } else {
        expression_parser
            .parse_non_section_offset(pattern_start, items)
            .map(|tree| tree.with_error(SyntaxError::ArgDefExpectedPattern))
    };
    let suspension =
        have_suspension.then(|| items.pop().unwrap().try_into_token().unwrap().try_into().unwrap());
    (suspension, pattern)
}

#[derive(Debug, Copy, Clone)]
enum TopLevelOperator {
    AssignmentOperator(usize),
    TypeAnnotationOperator(usize),
}

/// Performs a single step of top-down parsing by scanning ahead for the highest-precedence
/// operators.
fn find_top_level_operator(items: &[Item]) -> Result<Option<TopLevelOperator>, SyntaxError> {
    use token::Variant;
    let mut candidate: Option<(TopLevelOperator, Spacing)> = None;
    let mut after_first_space = false;
    for (i, item) in items.iter().enumerate() {
        let spacing = Spacing::of_item(item);
        let next_is_after_space = i != 0 && (after_first_space || spacing == Spacing::Spaced);
        if let Item::Token(token) = item {
            if !after_first_space || spacing == Spacing::Spaced {
                match (&token.variant, spacing, candidate) {
                    (
                        Variant::AssignmentOperator(_) | Variant::TypeAnnotationOperator(_),
                        Spacing::Spaced,
                        _,
                    ) if items
                        .get(i + 1)
                        .is_some_and(|item| Spacing::of_item(item) == Spacing::Unspaced) =>
                    {
                        return Err(SyntaxError::StmtLhsInvalidOperatorSpacing);
                    }
                    (Variant::AssignmentOperator(_), Spacing::Spaced, _) => {
                        return Ok(Some(TopLevelOperator::AssignmentOperator(i)));
                    }
                    (
                        Variant::AssignmentOperator(_),
                        Spacing::Unspaced,
                        None
                        | Some((TopLevelOperator::TypeAnnotationOperator(_), Spacing::Unspaced)),
                    ) => {
                        candidate =
                            Some((TopLevelOperator::AssignmentOperator(i), Spacing::Unspaced));
                    }
                    (Variant::TypeAnnotationOperator(_), spacing, None)
                    | (
                        Variant::TypeAnnotationOperator(_),
                        spacing @ Spacing::Spaced,
                        Some((_, Spacing::Unspaced)),
                    ) => {
                        candidate = Some((TopLevelOperator::TypeAnnotationOperator(i), spacing));
                    }
                    (
                        Variant::Operator(_)
                        | Variant::DotOperator(_)
                        | Variant::ArrowOperator(_)
                        | Variant::CommaOperator(_),
                        Spacing::Spaced,
                        Some((_, Spacing::Unspaced)),
                    ) => {
                        candidate = None;
                    }
                    _ => {}
                }
            }
        }
        after_first_space = next_is_after_space;
    }
    Ok(candidate.map(|(t, _)| t))
}

fn next_spaced(items: &[Item]) -> Option<usize> {
    for (i, item) in items.iter().enumerate().skip(1) {
        if Spacing::of_item(item) == Spacing::Spaced {
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
    use Item::*;
    use State::*;
    use token::Variant::*;
    let mut state = ExpectingIdent;
    for (i, item) in items.into_iter().enumerate() {
        match item.as_ref() {
            Token(token) if i != 0 && token.is_spaced() => break,
            Token(token) => match (state, &token.variant) {
                (ExpectingDot { .. }, DotOperator(_)) => state = ExpectingIdent,
                (ExpectingIdent, Ident(ident)) if ident.is_type => {
                    state = ExpectingDot { len: i + 1 }
                }
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
