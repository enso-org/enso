//! Built-in macro definitions.

use crate::macros::pattern::*;
use crate::macros::*;

use crate::syntax::operator;



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
    macro_map.register(group());
    macro_map.register(lambda());
    macro_map.register(case());
    macro_map.register(array());
    macro_map.register(tuple());
    macro_map.register(splice());
    macro_map
}

/// Built-in macro definitions that match only from the first token in a line.
fn statement() -> resolver::SegmentMap<'static> {
    let mut macro_map = resolver::SegmentMap::default();
    register_import_macros(&mut macro_map);
    register_export_macros(&mut macro_map);
    macro_map.register(type_def());
    macro_map.register(foreign());
    macro_map
}

fn register_import_macros(macros: &mut resolver::SegmentMap<'_>) {
    use crate::macro_definition;
    let defs = [
        macro_definition! {("import", everything()) import_body},
        macro_definition! {("import", everything(), "as", identifier()) import_body},
        macro_definition! {("import", everything(), "hiding", everything()) import_body},
        macro_definition! {("polyglot", everything(), "import", everything()) import_body},
        macro_definition! {
        ("polyglot", everything(), "import", everything(), "as", identifier()) import_body},
        macro_definition! {
        ("polyglot", everything(), "import", everything(), "hiding", everything()) import_body},
        macro_definition! {
        ("from", everything(), "import", everything(), "hiding", everything()) import_body},
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

fn import_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let mut polyglot = None;
    let mut from = None;
    let mut import = None;
    let mut all = None;
    let mut as_ = None;
    let mut hiding = None;
    let mut parser = operator::Precedence::new();
    for segment in segments {
        let header = segment.header;
        let tokens = segment.result.tokens();
        let body;
        let field = match header.code.as_ref() {
            "polyglot" => {
                body = parser.resolve(tokens).map(expect_ident);
                &mut polyglot
            }
            "from" => {
                body = parser.resolve(tokens).map(expect_qualified);
                &mut from
            }
            "import" => {
                let expect = match from {
                    Some(_) => expect_ident,
                    None => expect_qualified,
                };
                body = sequence_tree(&mut parser, tokens, expect);
                &mut import
            }
            "all" => {
                debug_assert!(tokens.is_empty());
                all = Some(into_ident(header));
                continue;
            }
            "as" => {
                body = parser.resolve(tokens).map(expect_ident);
                &mut as_
            }
            "hiding" => {
                body = sequence_tree(&mut parser, tokens, expect_ident);
                &mut hiding
            }
            _ => unreachable!(),
        };
        *field = Some(syntax::tree::MultiSegmentAppSegment { header, body });
    }
    let import = import.unwrap();
    syntax::Tree::import(polyglot, from, import, all, as_, hiding)
}

fn register_export_macros(macros: &mut resolver::SegmentMap<'_>) {
    use crate::macro_definition;
    let defs = [
        macro_definition! {("export", everything()) export_body},
        macro_definition! {("export", everything(), "as", identifier()) export_body},
        macro_definition! {("from", everything(), "export", everything()) export_body},
        macro_definition! {
        ("from", everything(), "export", nothing(), "all", nothing()) export_body},
        macro_definition! {
        ("from", everything(), "export", everything(), "hiding", everything()) export_body},
        macro_definition! {
        ("from", everything(), "export", nothing(), "all", nothing(), "hiding", everything())
        export_body},
        macro_definition! {
        ("from", everything(), "as", identifier(), "export", everything()) export_body},
    ];
    for def in defs {
        macros.register(def);
    }
}

fn export_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let mut from = None;
    let mut export = None;
    let mut all = None;
    let mut as_ = None;
    let mut hiding = None;
    let mut parser = operator::Precedence::new();
    for segment in segments {
        let header = segment.header;
        let tokens = segment.result.tokens();
        let body;
        let field = match header.code.as_ref() {
            "from" => {
                body = parser.resolve(tokens).map(expect_qualified);
                &mut from
            }
            "export" => {
                let expect = match from {
                    Some(_) => expect_ident,
                    None => expect_qualified,
                };
                body = sequence_tree(&mut parser, tokens, expect);
                &mut export
            }
            "all" => {
                debug_assert!(tokens.is_empty());
                all = Some(into_ident(header));
                continue;
            }
            "as" => {
                body = parser.resolve(tokens).map(expect_ident);
                &mut as_
            }
            "hiding" => {
                body = sequence_tree(&mut parser, tokens, expect_ident);
                &mut hiding
            }
            _ => unreachable!(),
        };
        *field = Some(syntax::tree::MultiSegmentAppSegment { header, body });
    }
    let export = export.unwrap();
    syntax::Tree::export(from, export, all, as_, hiding)
}

/// If-then-else macro definition.
pub fn if_then_else<'s>() -> Definition<'s> {
    crate::macro_definition! {
    ("if", everything(), "then", everything(), "else", everything()) if_body}
}

/// If-then macro definition.
pub fn if_then<'s>() -> Definition<'s> {
    crate::macro_definition! {("if", everything(), "then", everything()) if_body}
}

fn if_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    use syntax::tree::*;
    let segments = segments.mapped(|s| {
        let header = s.header;
        let body = s.result.tokens();
        let body = match operator::resolve_operator_precedence_if_non_empty(body) {
            Some(Tree {
                variant:
                    box Variant::ArgumentBlockApplication(ArgumentBlockApplication {
                        lhs: None,
                        arguments,
                    }),
                span,
            }) => {
                let mut block = block::body_from_lines(arguments);
                block.span.left_offset += span.left_offset;
                Some(block)
            }
            e => e,
        };
        MultiSegmentAppSegment { header, body }
    });
    Tree::multi_segment_app(segments)
}

/// Group macro definition.
pub fn group<'s>() -> Definition<'s> {
    crate::macro_definition! {("(", everything(), ")", nothing()) group_body}
}

fn group_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let (close, mut segments) = segments.pop();
    let close = into_close_symbol(close.header);
    let segment = segments.pop().unwrap();
    let open = into_open_symbol(segment.header);
    let body = segment.result.tokens();
    let body = operator::resolve_operator_precedence_if_non_empty(body);
    syntax::Tree::group(Some(open), body, Some(close))
}

/// Type definitions.
fn type_def<'s>() -> Definition<'s> {
    crate::macro_definition! {("type", everything()) type_def_body}
}

fn type_def_body(matched_segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    use syntax::tree::*;
    let segment = matched_segments.pop().0;
    let header = into_ident(segment.header);
    let mut tokens = segment.result.tokens();
    let mut block = vec![];
    if let Some(syntax::Item::Block(lines)) = tokens.last_mut() {
        block = mem::take(lines);
        tokens.pop();
    }
    let mut tokens = tokens.into_iter();
    let name = match tokens.next() {
        Some(syntax::Item::Token(syntax::Token {
            left_offset,
            code,
            variant: syntax::token::Variant::Ident(ident),
        })) => syntax::Token(left_offset, code, ident),
        _ => {
            let placeholder =
                Tree::ident(syntax::token::ident("", "", false, 0, false, false, false));
            return placeholder.with_error("Expected identifier after `type` keyword.");
        }
    };
    let params = operator::Precedence::new()
        .resolve_non_section(tokens)
        .map(crate::collect_arguments_inclusive)
        .unwrap_or_default();
    let mut builder = TypeDefBodyBuilder::default();
    let mut beginning_of_line = true;
    for item in &mut block {
        match item {
            syntax::Item::Token(syntax::Token {
                variant: syntax::token::Variant::Newline(_),
                ..
            }) => {
                beginning_of_line = true;
                continue;
            }
            syntax::Item::Token(syntax::Token { variant, .. })
                if beginning_of_line && matches!(variant, syntax::token::Variant::Operator(_)) =>
            {
                let opr_ident =
                    syntax::token::variant::Ident { is_operator_lexically: true, ..default() };
                *variant = syntax::token::Variant::Ident(opr_ident);
            }
            _ => (),
        }
        beginning_of_line = false;
    }
    for block::Line { newline, expression } in block::lines(block) {
        builder.line(newline, expression);
    }
    let (constructors, body) = builder.finish();
    Tree::type_def(header, name, params, constructors, body)
}

#[derive(Default)]
struct TypeDefBodyBuilder<'s> {
    constructors:  Vec<syntax::tree::TypeConstructorLine<'s>>,
    body:          Vec<syntax::tree::block::Line<'s>>,
    documentation: Option<(syntax::token::Newline<'s>, syntax::tree::DocComment<'s>)>,
}

impl<'s> TypeDefBodyBuilder<'s> {
    /// Apply the line to the state.
    pub fn line(
        &mut self,
        mut newline: syntax::token::Newline<'s>,
        expression: Option<syntax::Tree<'s>>,
    ) {
        if self.documentation.is_none() &&
                let Some(syntax::Tree { span, variant: box syntax::tree::Variant::Documented(
                    syntax::tree::Documented { mut documentation, expression: None }) }) = expression {
            documentation.open.left_offset += span.left_offset;
            self.documentation = (newline, documentation).into();
            return;
        }
        if let Some((_, doc)) = &mut self.documentation && expression.is_none() {
            doc.newlines.push(newline);
            return;
        }
        if self.body.is_empty() {
            if let Some(expression) = expression {
                match Self::to_constructor_line(expression) {
                    Ok(mut expression) => {
                        if let Some((nl, mut doc)) = self.documentation.take() {
                            let nl = mem::replace(&mut newline, nl);
                            doc.newlines.push(nl);
                            expression.documentation = doc.into();
                        }
                        let expression = Some(expression);
                        let line = syntax::tree::TypeConstructorLine { newline, expression };
                        self.constructors.push(line);
                    }
                    Err(expression) => self.push_body(newline, expression.into()),
                }
            } else {
                self.constructors.push(newline.into());
            }
        } else {
            self.push_body(newline, expression);
        }
    }

    fn push_body(
        &mut self,
        mut newline: syntax::token::Newline<'s>,
        expression: Option<syntax::Tree<'s>>,
    ) {
        let mut expression = expression.map(crate::expression_to_statement);
        if let Some((nl, mut doc)) = self.documentation.take() {
            let nl = mem::replace(&mut newline, nl);
            doc.newlines.push(nl);
            expression = syntax::Tree::documented(doc, expression.take()).into();
        }
        self.body.push(syntax::tree::block::Line { newline, expression });
    }

    /// Return the constructor/body sequences.
    pub fn finish(
        mut self,
    ) -> (Vec<syntax::tree::TypeConstructorLine<'s>>, Vec<syntax::tree::block::Line<'s>>) {
        if let Some((newline, doc)) = self.documentation {
            let expression = syntax::Tree::documented(doc, default()).into();
            self.body.push(syntax::tree::block::Line { newline, expression });
        }
        (self.constructors, self.body)
    }

    /// Interpret the given expression as a `TypeConstructorDef`, if its syntax is compatible.
    fn to_constructor_line(
        expression: syntax::Tree<'_>,
    ) -> Result<syntax::tree::TypeConstructorDef<'_>, syntax::Tree<'_>> {
        use syntax::tree::*;
        let mut last_argument_default = default();
        let mut left_offset = crate::source::Offset::default();
        let documentation = default();
        let lhs = match &expression {
            Tree {
                variant:
                    box Variant::OprApp(OprApp { lhs: Some(lhs), opr: Ok(opr), rhs: Some(rhs) }),
                span,
            } if opr.properties.is_assignment() => {
                left_offset = span.left_offset.clone();
                last_argument_default = Some((opr.clone(), rhs.clone()));
                lhs
            }
            Tree {
                variant:
                    box Variant::ArgumentBlockApplication(ArgumentBlockApplication {
                        lhs: Some(Tree { variant: box Variant::Ident(ident), span: span_ }),
                        arguments,
                    }),
                span,
            } => {
                let mut constructor = ident.token.clone();
                constructor.left_offset += &span.left_offset;
                constructor.left_offset += &span_.left_offset;
                let block = arguments
                    .iter()
                    .cloned()
                    .map(|block::Line { newline, expression }| ArgumentDefinitionLine {
                        newline,
                        argument: expression.map(crate::parse_argument_definition),
                    })
                    .collect();
                let arguments = default();
                return Ok(TypeConstructorDef { documentation, constructor, arguments, block });
            }
            _ => &expression,
        };
        let (constructor, mut arguments) = crate::collect_arguments(lhs.clone());
        if let Tree { variant: box Variant::Ident(Ident { token }), span } = constructor && token.is_type {
            let mut constructor = token;
            constructor.left_offset += left_offset;
            constructor.left_offset += span.left_offset;
            if let Some((equals, expression)) = last_argument_default
                    && let Some(ArgumentDefinition { open: None, default, close: None, .. })
                    = arguments.last_mut() && default.is_none() {
                *default = Some(ArgumentDefault { equals, expression });
            }
            let block = default();
            return Ok(TypeConstructorDef{ documentation, constructor, arguments, block });
        }
        Err(expression)
    }
}

/// Lambda expression.
///
/// The lambda operator `\` is similar to a unary operator, but is implemented as a macro because it
/// doesn't follow the whitespace precedence rules.
pub fn lambda<'s>() -> Definition<'s> {
    crate::macro_definition! {("\\", everything()) lambda_body}
}

fn lambda_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let (segment, _) = segments.pop();
    let operator = segment.header;
    let syntax::token::Token { left_offset, code, .. } = operator;
    let properties = syntax::token::OperatorProperties::default();
    let operator = syntax::token::operator(left_offset, code, properties);
    let arrow = segment.result.tokens();
    let arrow = operator::resolve_operator_precedence_if_non_empty(arrow);
    syntax::Tree::lambda(operator, arrow)
}

/// Case expression.
pub fn case<'s>() -> Definition<'s> {
    crate::macro_definition! {("case", everything(), "of", everything()) case_body}
}

fn case_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    use operator::resolve_operator_precedence_if_non_empty;
    use syntax::tree::*;
    let (of, mut rest) = segments.pop();
    let case = rest.pop().unwrap();
    let case_ = into_ident(case.header);
    let expression = case.result.tokens();
    let expression = resolve_operator_precedence_if_non_empty(expression);
    let of_ = into_ident(of.header);
    let mut case_builder = CaseBuilder::default();
    for item in of.result.tokens() {
        if let syntax::Item::Block(items) = item {
            items.into_iter().for_each(|item| case_builder.push(item));
        } else {
            case_builder.push(item);
        }
    }
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
    arrow:         Option<syntax::token::Operator<'s>>,
    // Within-case state
    spaces:        bool,
    tokens:        Vec<syntax::Item<'s>>,
    resolver:      operator::Precedence<'s>,
    // Output
    case_lines:    Vec<syntax::tree::CaseLine<'s>>,
    any_invalid:   bool,
}

impl<'s> CaseBuilder<'s> {
    fn push(&mut self, token: syntax::Item<'s>) {
        if let syntax::Item::Token(syntax::Token {
            left_offset,
            code,
            variant: syntax::token::Variant::Newline(_),
        }) = token
        {
            self.finish_line();
            let newline = syntax::token::newline(left_offset, code).into();
            self.case_lines.push(syntax::tree::CaseLine { newline, ..default() });
            return;
        }
        if self.arrow.is_none() &&
                let syntax::Item::Token(syntax::Token { left_offset, code, variant: syntax::token::Variant::Operator(op) }) = &token
                && op.properties.is_arrow()
                && !left_offset.is_empty() {
            self.resolver.extend(self.tokens.drain(..));
            self.arrow = Some(syntax::token::operator(left_offset.clone(), code.clone(), op.properties));
            self.pattern = self.resolver.finish().map(crate::expression_to_pattern);
            return;
        }
        if let syntax::Item::Token(syntax::Token { left_offset, .. }) = &token {
            self.spaces = self.spaces || (!left_offset.is_empty() && !self.tokens.is_empty());
        }
        self.tokens.push(token);
    }

    fn finish_line(&mut self) {
        if self.arrow.is_none() && !self.spaces {
            for (i, token) in self.tokens.iter().enumerate() {
                if let syntax::Item::Token(syntax::Token { left_offset, code, variant: syntax::token::Variant::Operator(op) }) = &token
                    && op.properties.is_arrow() {
                    self.arrow = Some(syntax::token::operator(left_offset.clone(), code.clone(), op.properties));
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
                    box syntax::tree::Variant::Documented(syntax::tree::Documented {
                        mut documentation,
                        expression: None,
                    }),
            }) if self.documentation.is_none() => {
                documentation.open.left_offset += span.left_offset;
                if self.case_lines.is_empty() {
                    self.case_lines.push(default());
                }
                let mut case = self.case_lines.last_mut().unwrap().case.get_or_insert_default();
                case.documentation = documentation.into();
                return;
            }
            Some(syntax::Tree {
                span,
                variant:
                    box syntax::tree::Variant::ArgumentBlockApplication(
                        syntax::tree::ArgumentBlockApplication { lhs: None, arguments },
                    ),
            }) => {
                let mut block = syntax::tree::block::body_from_lines(arguments);
                block.span.left_offset += span.left_offset;
                Some(block)
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
        let mut case = &mut self.case_lines.last_mut().unwrap().case.get_or_insert_default();
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

fn array_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let GroupedSequence { left, first, rest, right } = grouped_sequence(segments);
    syntax::tree::Tree::array(left, first, rest, right)
}

/// Tuple literal.
pub fn tuple<'s>() -> Definition<'s> {
    crate::macro_definition! {("{", everything(), "}", nothing()) tuple_body}
}

fn tuple_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let GroupedSequence { left, first, rest, right } = grouped_sequence(segments);
    syntax::tree::Tree::tuple(left, first, rest, right)
}

struct GroupedSequence<'s> {
    left:  syntax::token::OpenSymbol<'s>,
    first: Option<syntax::Tree<'s>>,
    rest:  Vec<syntax::tree::OperatorDelimitedTree<'s>>,
    right: syntax::token::CloseSymbol<'s>,
}

fn grouped_sequence(segments: NonEmptyVec<MatchedSegment>) -> GroupedSequence {
    let (right, mut rest) = segments.pop();
    let right = into_close_symbol(right.header);
    let left = rest.pop().unwrap();
    let left_ = into_open_symbol(left.header);
    let mut parser = operator::Precedence::new();
    let (first, rest) = sequence(&mut parser, left.result.tokens());
    GroupedSequence { left: left_, first, rest, right }
}

fn sequence<'s>(
    parser: &mut operator::Precedence<'s>,
    tokens: impl IntoIterator<Item = syntax::Item<'s>>,
) -> (Option<syntax::Tree<'s>>, Vec<syntax::tree::OperatorDelimitedTree<'s>>) {
    use syntax::tree::*;
    let mut first = None;
    let mut rest: Vec<OperatorDelimitedTree<'s>> = default();
    for token in tokens {
        match token {
            syntax::Item::Token(syntax::Token {
                left_offset,
                code,
                variant: syntax::token::Variant::Operator(op),
            }) if op.properties.is_sequence() => {
                *(match rest.last_mut() {
                    Some(rest) => &mut rest.body,
                    None => &mut first,
                }) = parser.finish();
                let operator = syntax::Token(left_offset, code, op);
                rest.push(OperatorDelimitedTree { operator, body: default() });
            }
            _ => {
                parser.push(token);
            }
        }
    }
    *(match rest.last_mut() {
        Some(rest) => &mut rest.body,
        None => &mut first,
    }) = parser.finish();
    (first, rest)
}

fn sequence_tree<'s>(
    parser: &mut operator::Precedence<'s>,
    tokens: impl IntoIterator<Item = syntax::Item<'s>>,
    mut f: impl FnMut(syntax::Tree<'s>) -> syntax::Tree<'s>,
) -> Option<syntax::Tree<'s>> {
    use syntax::tree::*;
    let (first, rest) = sequence(parser, tokens);
    let mut invalid = first.is_none();
    let mut tree = first.map(&mut f);
    for OperatorDelimitedTree { operator, body } in rest {
        invalid = invalid || body.is_none();
        tree = Tree::opr_app(tree, Ok(operator), body).into();
    }
    if invalid {
        tree = tree.map(|tree| tree.with_error("Malformed comma-delimited sequence."));
    }
    tree
}

fn splice<'s>() -> Definition<'s> {
    crate::macro_definition! {("`", everything(), "`", nothing()) splice_body}
}

fn splice_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let (close, mut segments) = segments.pop();
    let close = into_close_symbol(close.header);
    let segment = segments.pop().unwrap();
    let open = into_open_symbol(segment.header);
    let expression = segment.result.tokens();
    let expression = operator::resolve_operator_precedence_if_non_empty(expression);
    let splice = syntax::tree::TextElement::Splice { open, expression, close };
    syntax::Tree::text_literal(default(), default(), vec![splice], default(), default())
}

fn foreign<'s>() -> Definition<'s> {
    crate::macro_definition! {("foreign", everything()) foreign_body}
}

fn foreign_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let segment = segments.pop().0;
    let keyword = into_ident(segment.header);
    let tokens = segment.result.tokens().into_iter();
    match try_foreign_body(keyword.clone(), tokens.clone()) {
        Ok(foreign) => foreign,
        Err(error) => (match operator::resolve_operator_precedence_if_non_empty(tokens) {
            Some(rhs) => syntax::Tree::app(keyword.into(), rhs),
            None => keyword.into(),
        })
        .with_error(error),
    }
}

fn try_foreign_body<'s>(
    keyword: syntax::token::Ident<'s>,
    tokens: impl IntoIterator<Item = syntax::Item<'s>>,
) -> Result<syntax::Tree, &'static str> {
    let mut tokens = tokens.into_iter();
    let language = tokens
        .next()
        .and_then(try_into_token)
        .and_then(try_token_into_ident)
        .ok_or("Expected an identifier specifying foreign method's language.")?;
    let expected_name = "Expected an identifier specifying foreign function's name.";
    let function =
        operator::resolve_operator_precedence_if_non_empty(tokens).ok_or(expected_name)?;
    let expected_function = "Expected a function definition after foreign declaration.";
    let box syntax::tree::Variant::OprApp(
            syntax::tree::OprApp { lhs: Some(lhs), opr: Ok(equals), rhs: Some(body) }) = function.variant else {
        return Err(expected_function)
    };
    if !equals.properties.is_assignment() {
        return Err(expected_function);
    };
    let (name, args) = crate::collect_arguments(lhs);
    let mut name = try_tree_into_ident(name).ok_or(expected_name)?;
    name.left_offset += function.span.left_offset;
    Ok(syntax::Tree::foreign_function(keyword, language, name, args, equals, body))
}

// === Token conversions ===

fn try_into_token(item: syntax::Item) -> Option<syntax::Token> {
    match item {
        syntax::Item::Token(token) => Some(token),
        _ => None,
    }
}

fn try_token_into_ident(token: syntax::Token) -> Option<syntax::token::Ident> {
    match token.variant {
        syntax::token::Variant::Ident(ident) => {
            let syntax::token::Token { left_offset, code, .. } = token;
            Some(syntax::Token(left_offset, code, ident))
        }
        _ => None,
    }
}

fn try_tree_into_ident(tree: syntax::Tree) -> Option<syntax::token::Ident> {
    match tree.variant {
        box syntax::tree::Variant::Ident(syntax::tree::Ident { token }) => Some(token),
        _ => None,
    }
}

fn into_open_symbol(token: syntax::token::Token) -> syntax::token::OpenSymbol {
    let syntax::token::Token { left_offset, code, .. } = token;
    syntax::token::open_symbol(left_offset, code)
}

fn into_close_symbol(token: syntax::token::Token) -> syntax::token::CloseSymbol {
    let syntax::token::Token { left_offset, code, .. } = token;
    syntax::token::close_symbol(left_offset, code)
}

fn into_ident(token: syntax::token::Token) -> syntax::token::Ident {
    let syntax::token::Token { left_offset, code, .. } = token;
    syntax::token::ident(left_offset, code, false, 0, false, false, false)
}


// === Validators ===

fn expect_ident(tree: syntax::Tree) -> syntax::Tree {
    if matches!(&*tree.variant, syntax::tree::Variant::Ident(_)) {
        tree
    } else {
        tree.with_error("Expected identifier.")
    }
}

fn expect_qualified(tree: syntax::Tree) -> syntax::Tree {
    if crate::is_qualified_name(&tree) {
        tree
    } else {
        tree.with_error("Expected qualified name.")
    }
}
