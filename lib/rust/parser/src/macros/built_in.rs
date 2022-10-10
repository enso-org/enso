//! Built-in macro definitions.

use crate::macros::pattern::*;
use crate::macros::*;

use crate::syntax::operator;



// =======================
// === Built-in macros ===
// =======================

/// All built-in macro definitions.
pub fn all() -> resolver::SegmentMap<'static> {
    let mut macro_map = resolver::SegmentMap::default();
    macro_map.register(if_then());
    macro_map.register(if_then_else());
    register_import_macros(&mut macro_map);
    register_export_macros(&mut macro_map);
    macro_map.register(group());
    macro_map.register(type_def());
    macro_map.register(lambda());
    macro_map.register(case());
    macro_map.register(array());
    macro_map.register(tuple());
    macro_map.register(splice());

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
    for segment in segments {
        let header = segment.header;
        let body = operator::resolve_operator_precedence_if_non_empty(segment.result.tokens());
        let field = match header.code.as_ref() {
            "polyglot" => &mut polyglot,
            "from" => &mut from,
            "import" => &mut import,
            "all" => {
                all = Some(into_ident(header));
                continue;
            }
            "as" => &mut as_,
            "hiding" => &mut hiding,
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
    for segment in segments {
        let header = segment.header;
        let body = operator::resolve_operator_precedence_if_non_empty(segment.result.tokens());
        let field = match header.code.as_ref() {
            "from" => &mut from,
            "export" => &mut export,
            "all" => {
                all = Some(into_ident(header));
                continue;
            }
            "as" => &mut as_,
            "hiding" => &mut hiding,
            _ => unreachable!(),
        };
        *field = Some(syntax::tree::MultiSegmentAppSegment { header, body });
    }
    let export = export.unwrap();
    syntax::Tree::export(from, export, all, as_, hiding)
}

/// If-then-else macro definition.
pub fn if_then_else<'s>() -> Definition<'s> {
    crate::macro_definition! {("if", everything(), "then", everything(), "else", everything())}
}

/// If-then macro definition.
pub fn if_then<'s>() -> Definition<'s> {
    crate::macro_definition! {("if", everything(), "then", everything())}
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
    let mut tokens = segment.result.tokens().into_iter();
    let mut i = 0;
    for (i_, item) in tokens.as_slice().iter().enumerate() {
        if matches!(item, syntax::Item::Block(_)) {
            break;
        }
        i = i_ + 1;
    }
    let Some(first_line) = operator::Precedence::new().resolve_non_section(tokens.by_ref().take(i)) else {
        let placeholder = Tree::ident(syntax::token::ident("", "", false, 0, false, false));
        return placeholder.with_error("Expected identifier after `type` keyword.");
    };
    let (name, params) = crate::collect_arguments(first_line);
    let name = match name {
        Tree { variant: box Variant::Ident(Ident { mut token }), span } => {
            token.left_offset += span.left_offset;
            token
        }
        other => {
            return other.with_error("Expected identifier after `type` keyword.");
        }
    };
    let mut builder = TypeDefBodyBuilder::default();
    if let Some(syntax::Item::Block(block)) = tokens.next() {
        for block::Line { newline, expression } in block::lines(block) {
            builder.line(newline, expression);
        }
    }
    debug_assert_eq!(tokens.next(), None);
    let (constructors, body) = builder.finish();
    Tree::type_def(header, name, params, constructors, body)
}

#[derive(Default)]
struct TypeDefBodyBuilder<'s> {
    constructors: Vec<syntax::tree::TypeConstructorLine<'s>>,
    body:         Vec<syntax::tree::block::Line<'s>>,
}

impl<'s> TypeDefBodyBuilder<'s> {
    /// Apply the line to the state.
    pub fn line(
        &mut self,
        newline: syntax::token::Newline<'s>,
        expression: Option<syntax::Tree<'s>>,
    ) {
        if self.body.is_empty() {
            if let Some(expression) = expression {
                match Self::to_constructor_line(expression) {
                    Ok(expression) => {
                        let expression = Some(expression);
                        let line = syntax::tree::TypeConstructorLine { newline, expression };
                        self.constructors.push(line);
                    }
                    Err(expression) => {
                        let expression = crate::expression_to_statement(expression);
                        let expression = Some(expression);
                        self.body.push(syntax::tree::block::Line { newline, expression });
                    }
                }
            } else {
                self.constructors.push(newline.into());
            }
        } else {
            let expression = expression.map(crate::expression_to_statement);
            self.body.push(syntax::tree::block::Line { newline, expression });
        }
    }

    /// Return the constructor/body sequences.
    pub fn finish(
        self,
    ) -> (Vec<syntax::tree::TypeConstructorLine<'s>>, Vec<syntax::tree::block::Line<'s>>) {
        (self.constructors, self.body)
    }

    /// Interpret the given expression as a `TypeConstructorDef`, if its syntax is compatible.
    fn to_constructor_line(
        expression: syntax::Tree<'_>,
    ) -> Result<syntax::tree::TypeConstructorDef<'_>, syntax::Tree<'_>> {
        use syntax::tree::*;
        let mut left_offset = crate::source::Offset::default();
        let mut last_argument_default = default();
        let lhs = match &expression {
            Tree {
                variant:
                    box Variant::OprApp(OprApp { lhs: Some(lhs), opr: Ok(opr), rhs: Some(rhs) }),
                span,
            } if opr.properties.is_assignment() => {
                left_offset += span.left_offset.clone();
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
                let mut left_offset = span.left_offset.clone();
                left_offset += &span_.left_offset;
                left_offset += constructor.left_offset;
                constructor.left_offset = left_offset;
                let block = arguments
                    .iter()
                    .cloned()
                    .map(|block::Line { newline, expression }| ArgumentDefinitionLine {
                        newline,
                        argument: expression.map(crate::parse_argument_definition),
                    })
                    .collect();
                let arguments = default();
                return Ok(TypeConstructorDef { constructor, arguments, block });
            }
            _ => &expression,
        };
        let (constructor, mut arguments) = crate::collect_arguments(lhs.clone());
        if let Tree { variant: box Variant::Ident(Ident { token }), span } = constructor && token.is_type {
            let mut constructor = token;
            left_offset += span.left_offset;
            constructor.left_offset += left_offset;
            if let Some((equals, expression)) = last_argument_default
                    && let Some(ArgumentDefinition { open: None, default, close: None, .. })
                    = arguments.last_mut() && default.is_none() {
                *default = Some(ArgumentDefault { equals, expression });
            }
            let block = default();
            return Ok(TypeConstructorDef{ constructor, arguments, block });
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
    let body = of.result.tokens();
    let body = resolve_operator_precedence_if_non_empty(body);
    let mut case_lines = vec![];
    if let Some(body) = body {
        match body.variant {
            box Variant::ArgumentBlockApplication(ArgumentBlockApplication { lhs, arguments }) => {
                if let Some(lhs) = lhs {
                    case_lines.push(CaseLine { case: Some(lhs.into()), ..default() });
                }
                case_lines.extend(arguments.into_iter().map(
                    |block::Line { newline, expression }| CaseLine {
                        newline: newline.into(),
                        case:    expression.map(Case::from),
                    },
                ));
                if let Some(left_offset) =
                    case_lines.first_mut().and_then(CaseLine::left_offset_mut)
                {
                    *left_offset += body.span.left_offset;
                }
            }
            _ => case_lines.push(CaseLine { case: Some(body.into()), ..default() }),
        }
    }
    Tree::case_of(case_, expression, of_, case_lines)
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
    use syntax::tree::*;
    let (right, mut rest) = segments.pop();
    let right_ = into_close_symbol(right.header);
    let left = rest.pop().unwrap();
    let left_ = into_open_symbol(left.header);
    let expression = left.result.tokens();
    let expression = operator::resolve_operator_precedence_if_non_empty(expression);
    let mut rest = vec![];
    let mut lhs_ = &expression;
    let mut left_offset = crate::source::span::Offset::default();
    while let Some(Tree {
                       variant: box Variant::OprApp(OprApp { lhs, opr: Ok(opr), rhs: Some(rhs) }), span
                   }) = lhs_ && opr.properties.is_sequence() {
        lhs_ = lhs;
        let operator = opr.clone();
        let body = rhs.clone();
        rest.push(OperatorDelimitedTree { operator, body });
        left_offset += span.left_offset.clone();
    }
    rest.reverse();
    let mut first = lhs_.clone();
    if let Some(first) = &mut first {
        first.span.left_offset += left_offset;
    }
    GroupedSequence { left: left_, first, rest, right: right_ }
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
    syntax::Tree::text_literal(default(), vec![splice], default(), default())
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
    syntax::token::ident(left_offset, code, false, 0, false, false)
}
