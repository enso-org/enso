//! Built-in macro definitions.

use crate::macros::pattern::*;
use crate::macros::*;

use crate::empty_tree;
use crate::expect_qualified_name;
use crate::source::Code;
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
        let tokens = segment.result.tokens();
        let body;
        let field = match header.code.as_ref() {
            "polyglot" => {
                body = Some(
                    precedence
                        .resolve(tokens)
                        .map(expect_ident)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut polyglot
            }
            "from" => {
                body = Some(
                    precedence
                        .resolve(tokens)
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
                body = sequence_tree(precedence, tokens, expect);
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
                        .resolve(tokens)
                        .map(expect_ident)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut as_
            }
            "hiding" => {
                body = Some(
                    sequence_tree(precedence, tokens, expect_ident)
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
        let tokens = segment.result.tokens();
        let body;
        let field = match header.code.as_ref() {
            "from" => {
                body = Some(
                    precedence
                        .resolve(tokens)
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
                body = sequence_tree(precedence, tokens, expect);
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
                        .resolve(tokens)
                        .map(expect_ident)
                        .unwrap_or_else(|| expected_nonempty(header.code.position_after())),
                );
                &mut as_
            }
            "hiding" => {
                body = Some(
                    sequence_tree(precedence, tokens, expect_ident)
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
    return syntax::Tree::multi_segment_app(segments.try_into().unwrap()).with_error(error);
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
    crate::macro_definition! {("\\", everything()) lambda_body}
}

fn lambda_body<'s>(
    segments: NonEmptyVec<MatchedSegment<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> syntax::Tree<'s> {
    let (segment, _) = segments.pop();
    let operator = segment.header;
    let Token { left_offset, code, .. } = operator;
    let operator = token::lambda_operator(left_offset, code);
    let arrow = segment.result.tokens();
    let arrow = precedence.resolve(arrow);
    syntax::Tree::lambda(operator, arrow)
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
    let expression = case.result.tokens();
    let expression = precedence.resolve(expression);
    let of_ = of.header.with_variant(token::variant::OfKeyword());
    let mut case_builder = CaseBuilder::default();
    let mut initial_case = vec![];
    let mut block = default();
    for item in of.result.tokens() {
        match item {
            Item::Block(lines) => block = lines,
            _ => initial_case.push(item),
        }
    }
    if !initial_case.is_empty() {
        let location = of_.code.position_after();
        let newline = token::newline(location.clone(), location);
        case_builder.push(syntax::item::Line { newline, items: initial_case });
    }
    block.into_vec().into_iter().for_each(|line| case_builder.push(line));
    let (case_lines, any_invalid) = case_builder.finish();
    let tree = Tree::case_of(case_, expression, of_, case_lines);
    if any_invalid {
        return tree.with_error("Invalid case expression.");
    }
    tree
}

#[derive(Default)]
struct CaseBuilder<'s> {
    // Case components
    documentation: Option<syntax::tree::DocComment<'s>>,
    pattern:       Option<syntax::Tree<'s>>,
    arrow:         Option<token::ArrowOperator<'s>>,
    // Within-case state
    spaces:        bool,
    tokens:        Vec<Item<'s>>,
    resolver:      operator::Precedence<'s>,
    // Output
    case_lines:    Vec<syntax::tree::CaseLine<'s>>,
    any_invalid:   bool,
}

impl<'s> CaseBuilder<'s> {
    fn push(&mut self, line: syntax::item::Line<'s>) {
        let syntax::item::Line { newline, items } = line;
        self.case_lines.push(syntax::tree::CaseLine { newline: newline.into(), ..default() });
        for token in items {
            if let Item::Token(token @ Token { left_offset, variant, .. }) = &token {
                if self.arrow.is_none()
                    && let token::Variant::ArrowOperator(arrow_op) = variant
                    && !left_offset.is_empty()
                {
                    self.resolver.extend(self.tokens.drain(..));
                    self.arrow = Some(token.clone().with_variant(*arrow_op));
                    self.pattern = self.resolver.finish().map(crate::expression_to_pattern);
                    continue;
                }
                self.spaces = self.spaces || (!left_offset.is_empty() && !self.tokens.is_empty());
            }
            self.tokens.push(token);
        }
        self.finish_line();
    }

    fn finish_line(&mut self) {
        if self.arrow.is_none() && !self.spaces {
            for (i, token) in self.tokens.iter().enumerate() {
                if let Item::Token(
                    token @ Token { variant: token::Variant::ArrowOperator(arrow_op), .. },
                ) = token
                {
                    self.arrow = Some(token.clone().with_variant(*arrow_op));
                    let including_arrow = self.tokens.drain(..=i);
                    self.resolver.extend(including_arrow.take(i));
                    self.pattern = self.resolver.finish().map(crate::expression_to_pattern);
                    break;
                }
            }
        }
        self.spaces = false;
        self.resolver.extend(self.tokens.drain(..));
        let pattern = self.pattern.take();
        let arrow = self.arrow.take();
        let expression = match self.resolver.finish() {
            Some(syntax::Tree {
                span,
                variant:
                    syntax::tree::Variant::Documented(box syntax::tree::Documented {
                        mut documentation,
                        expression: None,
                    }),
                ..
            }) if self.documentation.is_none() => {
                documentation.open.left_offset += span.left_offset;
                if self.case_lines.is_empty() {
                    self.case_lines.push(default());
                }
                let case = self.case_lines.last_mut().unwrap().case.get_or_insert_default();
                case.documentation = documentation.into();
                return;
            }
            e => e,
        };
        if pattern.is_none() && arrow.is_none() && expression.is_none() {
            return;
        }
        self.any_invalid =
            self.any_invalid || pattern.is_none() || arrow.is_none() || expression.is_none();
        if self.case_lines.is_empty() {
            self.case_lines.push(default());
        }
        let case = &mut self.case_lines.last_mut().unwrap().case.get_or_insert_default();
        case.pattern = pattern;
        case.arrow = arrow;
        case.expression = expression;
    }

    fn finish(mut self) -> (Vec<syntax::tree::CaseLine<'s>>, bool) {
        self.finish_line();
        (self.case_lines, self.any_invalid)
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
    let right = into_close_symbol(right.header);
    let left = rest.pop().unwrap();
    let left_ = into_open_symbol(left.header);
    let (first, rest) = sequence(precedence, left.result.tokens());
    GroupedSequence { left: left_, first, rest, right }
}

fn sequence<'s>(
    precedence: &mut operator::Precedence<'s>,
    tokens: impl IntoIterator<Item = Item<'s>>,
) -> (Option<syntax::Tree<'s>>, Vec<syntax::tree::OperatorDelimitedTree<'s>>) {
    use syntax::tree::*;
    let mut first = None;
    let mut rest: Vec<OperatorDelimitedTree<'s>> = default();
    for token in tokens {
        match token {
            Item::Token(Token { left_offset, code, variant: token::Variant::CommaOperator(_) }) => {
                *(match rest.last_mut() {
                    Some(rest) => &mut rest.body,
                    None => &mut first,
                }) = precedence.finish();
                let operator = Token(left_offset, code, token::variant::Operator());
                rest.push(OperatorDelimitedTree { operator, body: default() });
            }
            _ => {
                precedence.push(token);
            }
        }
    }
    *(match rest.last_mut() {
        Some(rest) => &mut rest.body,
        None => &mut first,
    }) = precedence.finish();
    (first, rest)
}

fn sequence_tree<'s>(
    precedence: &mut operator::Precedence<'s>,
    tokens: impl IntoIterator<Item = Item<'s>>,
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
    let close = into_close_symbol(close.header);
    let segment = segments.pop().unwrap();
    let open = into_open_symbol(segment.header);
    let expression = segment.result.tokens();
    let expression = precedence.resolve(expression);
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
        let body = s.result.tokens();
        let body = precedence.resolve(body);
        MultiSegmentAppSegment { header, body }
    }))
}

// === Token conversions ===

fn try_into_token(item: Item) -> Option<Token> {
    match item {
        Item::Token(token) => Some(token),
        _ => None,
    }
}

fn try_token_into_ident(token: Token) -> Option<token::Ident> {
    match token.variant {
        token::Variant::Ident(ident) => {
            let Token { left_offset, code, .. } = token;
            Some(Token(left_offset, code, ident))
        }
        _ => None,
    }
}

fn try_tree_into_ident(tree: syntax::Tree) -> Option<token::Ident> {
    match tree.variant {
        syntax::tree::Variant::Ident(box syntax::tree::Ident { token }) => Some(token),
        _ => None,
    }
}

fn into_open_symbol(token: Token) -> token::OpenSymbol {
    let Token { left_offset, code, .. } = token;
    token::open_symbol(left_offset, code)
}

fn into_close_symbol(token: Token) -> token::CloseSymbol {
    let Token { left_offset, code, .. } = token;
    token::close_symbol(left_offset, code)
}

fn into_ident<T>(token: Token<T>) -> token::Ident {
    token.with_variant(token::variant::Ident(false, 0, false, false, false))
}


// === Validators ===

fn expect_ident(tree: syntax::Tree) -> syntax::Tree {
    if matches!(tree.variant, syntax::tree::Variant::Ident(_)) {
        tree
    } else {
        tree.with_error("Expected identifier.")
    }
}

fn expected_nonempty(location: Code) -> syntax::Tree {
    empty_tree(location).with_error("Expected tokens.")
}
