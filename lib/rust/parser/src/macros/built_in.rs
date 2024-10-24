//! Built-in macro definitions.

use crate::macros::pattern::*;
use crate::macros::*;

use crate::empty_tree;
use crate::expect_qualified_name;
use crate::expression_to_pattern;
use crate::source::Code;
use crate::syntax::maybe_with_error;
use crate::syntax::operator;
use crate::syntax::token;
use crate::syntax::tree::SyntaxError;
use crate::syntax::Item;
use crate::syntax::Token;



// =======================
// === Built-in macros ===
// =======================

/// All built-in macro definitions.
pub fn all() -> resolver::MacroMap {
    resolver::MacroMap { expression: expression(), statement: statement() }
}

/// Built-in macro definitions that match anywhere in an expression.
fn expression() -> resolver::SegmentMap<'static> {
    let mut macro_map = resolver::SegmentMap::default();
    macro_map.register(if_then());
    macro_map.register(if_then_else());
    macro_map.register(lambda());
    macro_map.register(case());
    macro_map.register(array());
    macro_map.register(tuple());
    macro_map.register(splice());
    macro_map.register(skip());
    macro_map.register(freeze());
    macro_map
}

/// Built-in macro definitions that match only from the first token in a line.
fn statement() -> resolver::SegmentMap<'static> {
    let mut macro_map = resolver::SegmentMap::default();
    register_import_macros(&mut macro_map);
    register_export_macros(&mut macro_map);
    macro_map
}

fn register_import_macros(macros: &mut resolver::SegmentMap<'_>) {
    use crate::macro_definition;
    let defs = [
        macro_definition! {("import", everything()) import_body},
        macro_definition! {("import", everything(), "as", everything()) import_body},
        macro_definition! {("polyglot", everything(), "import", everything()) import_body},
        macro_definition! {
        ("polyglot", everything(), "import", everything(), "as", everything()) import_body},
        macro_definition! {
        ("from", everything(), "import", nothing(), "all", nothing()) import_body},
        macro_definition! {
        ("from", everything(), "import", nothing(), "all", nothing(), "hiding", everything())
        import_body},
        macro_definition! {("from", everything(), "import", everything()) import_body},
    ];
    for def in defs {
        macros.register(def);
    }
}

fn import_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    let mut polyglot = None;
    let mut from = None;
    let mut import = None;
    let mut all = None;
    let mut as_ = None;
    let mut hiding = None;
    let mut incomplete_import = false;
    for segment in segments {
        let header = segment.header;
        let mut tokens = segment.result.tokens();
        let body;
        let field = match header.code.as_ref() {
            "polyglot" => {
                body = Some(
                    precedence
                        .resolve(&mut tokens)
                        .map(expect_ident)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut polyglot
            }
            "from" => {
                body = Some(
                    precedence
                        .resolve(&mut tokens)
                        .map(expect_qualified_name)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut from
            }
            "import" => {
                let expect = match from {
                    Some(_) => expect_ident,
                    None => expect_qualified_name,
                };
                body = sequence_tree(precedence, &mut tokens, expect);
                incomplete_import = body.is_none();
                &mut import
            }
            "all" => {
                debug_assert!(tokens.is_empty());
                all = Some(header.with_variant(token::variant::AllKeyword()));
                incomplete_import = false;
                continue;
            }
            "as" => {
                body = Some(
                    precedence
                        .resolve(&mut tokens)
                        .map(expect_ident)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut as_
            }
            "hiding" => {
                body = Some(
                    sequence_tree(precedence, &mut tokens, expect_ident)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut hiding
            }
            _ => unreachable!(),
        };
        *field = Some(syntax::tree::MultiSegmentAppSegment { header, body });
    }
    let import = syntax::Tree::import(polyglot, from, import.unwrap(), all, as_, hiding);
    if incomplete_import {
        return import.with_error("Expected name or `all` keyword following `import` keyword.");
    }
    import
}

fn register_export_macros(macros: &mut resolver::SegmentMap<'_>) {
    use crate::macro_definition;
    let defs = [
        macro_definition! {("export", everything()) export_body},
        macro_definition! {("export", everything(), "as", everything()) export_body},
        macro_definition! {("from", everything(), "export", everything()) export_body},
        macro_definition! {
        ("from", everything(), "export", nothing(), "all", nothing()) export_body},
        macro_definition! {
        ("from", everything(), "export", everything(), "hiding", everything()) export_body},
        macro_definition! {
        ("from", everything(), "export", nothing(), "all", nothing(), "hiding", everything())
        export_body},
        macro_definition! {
        ("from", everything(), "as", everything(), "export", everything()) export_body},
    ];
    for def in defs {
        macros.register(def);
    }
}

fn export_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    let mut from = None;
    let mut export = None;
    let mut all = None;
    let mut as_ = None;
    let mut hiding = None;
    let mut incomplete_export = false;
    for segment in segments {
        let header = segment.header;
        let mut tokens = segment.result.tokens();
        let body;
        let field = match header.code.as_ref() {
            "from" => {
                body = Some(
                    precedence
                        .resolve(&mut tokens)
                        .map(expect_qualified_name)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut from
            }
            "export" => {
                let expect = match from {
                    Some(_) => expect_ident,
                    None => expect_qualified_name,
                };
                body = sequence_tree(precedence, &mut tokens, expect);
                incomplete_export = body.is_none();
                &mut export
            }
            "all" => {
                debug_assert!(tokens.is_empty());
                body = None;
                incomplete_export = false;
                &mut all
            }
            "as" => {
                body = Some(
                    precedence
                        .resolve(&mut tokens)
                        .map(expect_ident)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut as_
            }
            "hiding" => {
                body = Some(
                    sequence_tree(precedence, &mut tokens, expect_ident)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut hiding
            }
            _ => unreachable!(),
        };
        *field = Some(syntax::tree::MultiSegmentAppSegment { header, body });
    }
    let export = export.unwrap();
    let error = if all.is_some() {
        SyntaxError::ImportsNoAllInExport
    } else if hiding.is_some() {
        SyntaxError::ImportsNoHidingInExport
    } else if incomplete_export {
        SyntaxError::ImportsExpectedNameInExport
    } else {
        return syntax::Tree::export(from, export, as_);
    };
    let mut segments = vec![];
    segments.extend(from);
    segments.push(export);
    segments.extend(all);
    segments.extend(as_);
    segments.extend(hiding);
    syntax::Tree::multi_segment_app(segments.try_into().unwrap()).with_error(error)
}

/// If-then-else macro definition.
pub fn if_then_else<'s>() -> Definition<'s> {
    crate::macro_definition! {
    ("if", everything(), "then", everything(), "else", or(block(), many(not_block()))) if_body}
}

/// If-then macro definition.
pub fn if_then<'s>() -> Definition<'s> {
    crate::macro_definition! {("if", everything(), "then", everything()) if_body}
}

fn if_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    capture_expressions(segments, precedence)
}

/// Lambda expression.
///
/// The lambda operator `\` is similar to a unary operator, but is implemented as a macro because it
/// doesn't follow the whitespace precedence rules.
pub fn lambda<'s>() -> Definition<'s> {
    crate::macro_definition! {("\\", everything(), "->", everything()) lambda_body}
}

fn lambda_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    let (body, mut rest) = segments.pop();
    let arguments = rest.pop().unwrap();
    let backslash = arguments.header.with_variant(token::variant::LambdaOperator());
    let arguments = syntax::parse_args(&mut arguments.result.tokens(), 0, precedence);
    let arrow = body.header.with_variant(token::variant::ArrowOperator());
    let body_expression = precedence.resolve(&mut body.result.tokens());
    let body_expression =
        body_expression.unwrap_or_else(|| expected_nonempty(arrow.code.position_after()));
    syntax::Tree::lambda(backslash, arguments, arrow, body_expression)
}

/// Case expression.
pub fn case<'s>() -> Definition<'s> {
    crate::macro_definition! {("case", everything(), "of", everything()) case_body}
}

fn case_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    use syntax::tree::*;
    let (of, mut rest) = segments.pop();
    let case = rest.pop().unwrap();
    let case_ = case.header.with_variant(token::variant::CaseKeyword());
    let mut expression = case.result.tokens();
    let expression = precedence.resolve(&mut expression);
    let of_ = of.header.with_variant(token::variant::OfKeyword());
    let mut initial_case = vec![];
    let mut block = default();
    for item in of.result.tokens() {
        match item {
            Item::Block(lines) => block = lines,
            _ => initial_case.push(item),
        }
    }
    let mut case_lines = vec![];
    let mut error = None;
    if !initial_case.is_empty() {
        let location = of_.code.position_after();
        let newline = token::newline(location.clone(), location);
        let line = syntax::item::Line { newline, items: initial_case };
        let (case, initial_case_error) = parse_case_line(line, precedence);
        case_lines.push(case);
        error = initial_case_error;
    }
    case_lines.reserve(case_lines.len() + block.len());
    for line in block.into_vec() {
        let (case, case_error) = parse_case_line(line, precedence);
        case_lines.push(case);
        error = error.or(case_error);
    }
    let tree = Tree::case_of(case_, expression, of_, case_lines);
    maybe_with_error(tree, error)
}

fn parse_case_line<'s>(
    line: syntax::item::Line<'s>,
    precedence: &mut operator::Precedence<'s>,
) -> (syntax::tree::CaseLine<'s>, Option<SyntaxError>) {
    let syntax::item::Line { newline, mut items } = line;
    if let documentation @ Some(_) = try_parse_doc_comment(&mut items, precedence) {
        return (
            syntax::tree::CaseLine {
                newline: newline.into(),
                case:    Some(syntax::tree::Case { documentation, ..default() }),
            },
            default(),
        );
    }
    let (case, error) = parse_case(&mut items, precedence);
    (syntax::tree::CaseLine { newline: newline.into(), case: Some(case) }, error)
}

fn parse_case<'s>(
    items: &mut Vec<Item<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> (syntax::tree::Case<'s>, Option<SyntaxError>) {
    let mut arrow = None;
    let mut pattern = None;
    let expression;
    if let Some(arrow_i) = find_top_level_arrow(items) {
        expression = if arrow_i + 1 < items.len() {
            precedence.resolve_offset(arrow_i + 1, items)
        } else {
            None
        };
        let op = items.pop().unwrap().into_token().unwrap();
        arrow = Some(op.with_variant(token::variant::ArrowOperator()));
        pattern = precedence.resolve(items).map(expression_to_pattern);
    } else {
        expression = precedence.resolve(items);
    }
    let error = match (&pattern, &arrow, &expression) {
        (Some(_), Some(_), Some(_)) | (None, None, None) => None,
        _ => Some(SyntaxError::CaseOfInvalidCase),
    };
    (syntax::tree::Case { pattern, arrow, expression, ..default() }, error)
}

fn find_top_level_arrow(items: &[Item]) -> Option<usize> {
    let mut spaced = false;
    let mut arrow_i = None;
    for (i, item) in items.iter().enumerate() {
        if let Item::Token(token) = item {
            if token.is_spaced() {
                if let Token { variant: token::Variant::ArrowOperator(_), .. } = token {
                    arrow_i = Some(i);
                    break;
                }
                if i != 0 {
                    spaced = true;
                }
            }
        }
    }
    if arrow_i.is_none() && !spaced {
        items.iter().enumerate().find_map(|(i, item)| match item {
            Item::Token(Token { variant: token::Variant::ArrowOperator(_), .. }) => Some(i),
            _ => None,
        })
    } else {
        arrow_i
    }
}

fn try_parse_doc_comment<'s>(
    items: &mut Vec<Item<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> Option<syntax::tree::DocComment<'s>> {
    if matches!(
        items.first(),
        Some(Item::Token(token @ Token { variant: token::Variant::TextStart(_), .. })) if token.code == "##"
    ) {
        let Some(syntax::Tree {
            variant: syntax::tree::Variant::Documented(mut documented),
            span,
            ..
        }) = precedence.resolve(items)
        else {
            unreachable!()
        };
        debug_assert_eq!(documented.expression, None);
        documented.documentation.open.left_offset += span.left_offset;
        Some(documented.documentation)
    } else {
        None
    }
}

/// Array literal.
pub fn array<'s>() -> Definition<'s> {
    crate::macro_definition! {("[", everything(), "]", nothing()) array_body}
}

fn array_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    let GroupedSequence { left, first, rest, right } = grouped_sequence(segments, precedence);
    syntax::tree::Tree::array(left, first, rest, right)
}

/// Tuple literal.
pub fn tuple<'s>() -> Definition<'s> {
    crate::macro_definition! {("{", everything(), "}", nothing()) tuple_body}
}

fn tuple_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    let GroupedSequence { left, first, rest, right } = grouped_sequence(segments, precedence);
    syntax::tree::Tree::tuple(left, first, rest, right)
}

struct GroupedSequence<'s> {
    left:  token::OpenSymbol<'s>,
    first: Option<syntax::Tree<'s>>,
    rest:  Vec<syntax::tree::OperatorDelimitedTree<'s>>,
    right: token::CloseSymbol<'s>,
}

fn grouped_sequence<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> GroupedSequence<'s> {
    let (right, mut rest) = segments.pop();
    let right = right.header.with_variant(token::variant::CloseSymbol());
    let left = rest.pop().unwrap();
    let left_ = left.header.with_variant(token::variant::OpenSymbol());
    let (first, rest) = sequence(precedence, &mut left.result.tokens());
    GroupedSequence { left: left_, first, rest, right }
}

fn sequence<'s>(
    precedence: &mut operator::Precedence<'s>,
    tokens: &mut Vec<Item<'s>>,
) -> (Option<syntax::Tree<'s>>, Vec<syntax::tree::OperatorDelimitedTree<'s>>) {
    use syntax::tree::*;
    let mut rest = vec![];
    if !tokens.is_empty() {
        let mut i = tokens.len() - 1;
        loop {
            if let Item::Token(Token { variant: token::Variant::CommaOperator(_), .. }) =
                tokens.get(i).unwrap()
            {
                let body =
                    if i < tokens.len() { precedence.resolve_offset(i + 1, tokens) } else { None };
                let operator = tokens
                    .pop()
                    .unwrap()
                    .into_token()
                    .unwrap()
                    .with_variant(token::variant::Operator());
                rest.push(OperatorDelimitedTree { operator, body });
            }
            if i == 0 {
                break;
            }
            i -= 1;
        }
    }
    rest.reverse();
    let first = precedence.resolve(tokens);
    (first, rest)
}

fn sequence_tree<'s>(
    precedence: &mut operator::Precedence<'s>,
    tokens: &mut Vec<Item<'s>>,
    mut f: impl FnMut(syntax::Tree<'s>) -> syntax::Tree<'s>,
) -> Option<syntax::Tree<'s>> {
    use syntax::tree::*;
    let (first, rest) = sequence(precedence, tokens);
    let mut invalid = first.is_none();
    let mut tree = first.map(&mut f);
    for OperatorDelimitedTree { operator, body } in rest {
        invalid = invalid || body.is_none();
        tree = Tree::opr_app(tree, Ok(operator), body.map(&mut f)).into();
    }
    if invalid {
        tree = tree.map(|tree| tree.with_error("Malformed comma-delimited sequence."));
    }
    tree
}

fn splice<'s>() -> Definition<'s> {
    crate::macro_definition! {("`", everything(), "`", nothing()) splice_body}
}

fn splice_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    let (close, mut segments) = segments.pop();
    let close = close.header.with_variant(token::variant::CloseSymbol());
    let segment = segments.pop().unwrap();
    let open = segment.header.with_variant(token::variant::OpenSymbol());
    let mut expression = segment.result.tokens();
    let expression = precedence.resolve(&mut expression);
    let splice = syntax::tree::TextElement::Splice { open, expression, close };
    syntax::Tree::text_literal(default(), default(), vec![splice], default())
}

fn skip<'s>() -> Definition<'s> {
    crate::macro_definition! {("SKIP", everything()) capture_expressions}
}

fn freeze<'s>() -> Definition<'s> {
    crate::macro_definition! {("FREEZE", everything()) capture_expressions}
}

/// Macro body builder that just parses the tokens of each segment as expressions, and places them
/// in a [`MultiSegmentApp`].
fn capture_expressions<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    use syntax::tree::*;
    Tree::multi_segment_app(segments.mapped(|s| {
        let header = s.header;
        let body = precedence.resolve(&mut s.result.tokens());
        MultiSegmentAppSegment { header, body }
    }))
}

// === Validators ===

fn expect_ident(tree: syntax::Tree) -> syntax::Tree {
    let error = match &tree.variant {
        syntax::tree::Variant::Ident(_) => None,
        _ => Some("Expected identifier."),
    };
    maybe_with_error(tree, error)
}

fn expected_nonempty(location: Code) -> syntax::Tree {
    empty_tree(location).with_error("Expected tokens.")
}
