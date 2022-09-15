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
        macro_definition! {("import", everything(), "as", everything()) import_body},
        macro_definition! {("import", everything(), "hiding", everything()) import_body},
        macro_definition! {("polyglot", everything(), "import", everything()) import_body},
        macro_definition! {
        ("polyglot", everything(), "import", everything(), "as", everything()) import_body},
        macro_definition! {
        ("polyglot", everything(), "import", everything(), "hiding", everything()) import_body},
        macro_definition! {
        ("from", everything(), "import", everything(), "hiding", everything()) import_body},
        macro_definition! {
        ("from", everything(), "as", everything(), "import", everything()) import_body},
        macro_definition! {("from", everything(), "import", everything()) import_body},
    ];
    for def in defs {
        macros.register(def);
    }
}

fn import_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    use operator::resolve_operator_precedence_if_non_empty;
    let mut polyglot = None;
    let mut from = None;
    let mut from_as = None;
    let mut import = None;
    let mut import_as = None;
    let mut hiding = None;
    for segment in segments {
        let header = segment.header;
        let body = resolve_operator_precedence_if_non_empty(segment.result.tokens());
        let field = match header.code.as_ref() {
            "polyglot" => &mut polyglot,
            "from" => &mut from,
            "as" if import.is_none() => &mut from_as,
            "import" => &mut import,
            "as" => &mut import_as,
            "hiding" => &mut hiding,
            _ => unreachable!(),
        };
        *field = Some(syntax::tree::MultiSegmentAppSegment { header, body });
    }
    let import = import.unwrap();
    syntax::Tree::import(polyglot, from, from_as, import, import_as, hiding)
}

fn register_export_macros(macros: &mut resolver::SegmentMap<'_>) {
    use crate::macro_definition;
    let defs = [
        macro_definition! {("export", everything()) export_body},
        macro_definition! {("export", everything(), "as", everything()) export_body},
        macro_definition! {("from", everything(), "export", everything()) export_body},
        macro_definition! {
        ("from", everything(), "export", everything(), "hiding", everything()) export_body},
        macro_definition! {
        ("from", everything(), "as", everything(), "export", everything()) export_body},
    ];
    for def in defs {
        macros.register(def);
    }
}

fn export_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    use operator::resolve_operator_precedence_if_non_empty;
    let mut from = None;
    let mut from_as = None;
    let mut export = None;
    let mut export_as = None;
    let mut hiding = None;
    for segment in segments {
        let header = segment.header;
        let body = resolve_operator_precedence_if_non_empty(segment.result.tokens());
        let field = match header.code.as_ref() {
            "from" => &mut from,
            "as" if export.is_none() => &mut from_as,
            "export" => &mut export,
            "as" => &mut export_as,
            "hiding" => &mut hiding,
            _ => unreachable!(),
        };
        *field = Some(syntax::tree::MultiSegmentAppSegment { header, body });
    }
    let export = export.unwrap();
    syntax::Tree::export(from, from_as, export, export_as, hiding)
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
    use operator::resolve_operator_precedence_if_non_empty;
    let (close, mut segments) = segments.pop();
    let close = into_symbol(close.header);
    let segment = segments.pop().unwrap();
    let open = into_symbol(segment.header);
    let body = segment.result.tokens();
    let body = resolve_operator_precedence_if_non_empty(body);
    syntax::Tree::group(open, body, close)
}

/// Type definitions.
fn type_def<'s>() -> Definition<'s> {
    crate::macro_definition! {("type", everything()) type_def_body}
}

fn type_def_body(matched_segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    use syntax::tree::*;
    let segment = matched_segments.pop().0;
    let header = segment.header;
    let mut rest = segment.result.tokens();
    let name = match rest.get(0).map(|item| item.clone().to_ast()) {
        Some(Tree { variant: box Variant::Ident(Ident { mut token }), span }) => {
            token.left_offset += span.left_offset;
            token
        }
        _ => {
            let placeholder = Tree::ident(syntax::token::ident("", "", false, 0, false, false));
            return placeholder.with_error("Expected identifier after `type` keyword.");
        }
    };
    let rest = operator::resolve_operator_precedence_if_non_empty(rest.drain(1..));
    let mut params = vec![];
    let mut lines = None;
    if let Some(mut rest) = rest {
        loop {
            match &mut rest.variant {
                box Variant::ArgumentBlockApplication(ArgumentBlockApplication {
                    lhs,
                    arguments,
                }) => {
                    if let Some(lhs) = lhs {
                        lhs.span.left_offset += rest.span.left_offset.clone();
                        lines = Some(mem::take(arguments));
                        rest = lhs.clone();
                        continue;
                    } else if let Some(first) = arguments.first_mut() {
                        first.newline.left_offset += rest.span.left_offset.clone();
                    }
                    lines = Some(mem::take(arguments));
                    break;
                }
                box Variant::Ident(..) => {
                    params.push(crate::parse_argument_definition(rest.clone()));
                    break;
                }
                _ => (),
            };
            if let Some(arg) = crate::parse_argument_application(&mut rest) {
                params.push(arg);
            } else {
                break;
            }
        }
    }
    params.reverse();
    let mut builder = TypeDefBodyBuilder::default();
    if let Some(lines) = lines {
        for block::Line { newline, expression } in lines {
            builder.line(newline, expression);
        }
    }
    let (constructors, body) = builder.finish();
    let header = into_ident(header);
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

    /// Interpret the given expression as an `TypeConstructorDef`, if its syntax is compatible.
    fn to_constructor_line(
        expression: syntax::Tree<'_>,
    ) -> Result<syntax::tree::TypeConstructorDef<'_>, syntax::Tree<'_>> {
        use syntax::tree::*;
        let mut left_offset = crate::source::Offset::default();
        let mut last_argument_default = default();
        let mut lhs = match &expression {
            Tree {
                variant:
                    box Variant::OprApp(OprApp { lhs: Some(lhs), opr: Ok(opr), rhs: Some(rhs) }),
                span,
            } if opr.properties.is_assignment() => {
                left_offset += span.left_offset.clone();
                last_argument_default = Some((opr, rhs));
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
                let block = arguments.clone();
                let arguments = default();
                return Ok(TypeConstructorDef { constructor, arguments, block });
            }
            _ => &expression,
        };
        let mut arguments = vec![];
        while let Tree { variant: box Variant::App(App { func, arg }), span } = lhs {
            left_offset += &span.left_offset;
            lhs = func;
            arguments.push(arg.clone());
        }
        if let Tree { variant: box Variant::Ident(Ident { token }), span } = lhs && token.is_type {
            let mut constructor = token.clone();
            left_offset += &span.left_offset;
            left_offset += constructor.left_offset;
            constructor.left_offset = left_offset;
            arguments.reverse();
            if let Some((opr, rhs)) = last_argument_default && let Some(lhs) = arguments.pop() {
                arguments.push(Tree::opr_app(Some(lhs), Ok(opr.clone()), Some(rhs.clone())));
            }
            let block = default();
            return Ok(TypeConstructorDef { constructor, arguments, block });
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
    use operator::resolve_operator_precedence_if_non_empty;
    let (segment, _) = segments.pop();
    let operator = segment.header;
    let syntax::token::Token { left_offset, code, .. } = operator;
    let properties = syntax::token::OperatorProperties::default();
    let operator = syntax::token::operator(left_offset, code, properties);
    let arrow = segment.result.tokens();
    let arrow = resolve_operator_precedence_if_non_empty(arrow);
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
    let mut initial = None;
    let mut lines = vec![];
    if let Some(body) = body {
        match body.variant {
            box Variant::ArgumentBlockApplication(ArgumentBlockApplication { lhs, arguments }) => {
                initial = lhs;
                lines = arguments;
                let mut left_offset = body.span.left_offset;
                if let Some(initial) = initial.as_mut() {
                    left_offset += mem::take(&mut initial.span.left_offset);
                    initial.span.left_offset = left_offset;
                } else if let Some(first) = lines.first_mut() {
                    left_offset += mem::take(&mut first.newline.left_offset);
                    first.newline.left_offset = left_offset;
                }
            }
            _ => initial = Some(body),
        }
    }
    Tree::case(case_, expression, of_, initial, lines)
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
    left:  syntax::token::Symbol<'s>,
    first: Option<syntax::Tree<'s>>,
    rest:  Vec<syntax::tree::OperatorDelimitedTree<'s>>,
    right: syntax::token::Symbol<'s>,
}

fn grouped_sequence(segments: NonEmptyVec<MatchedSegment>) -> GroupedSequence {
    use operator::resolve_operator_precedence_if_non_empty;
    use syntax::tree::*;
    let (right, mut rest) = segments.pop();
    let right_ = into_symbol(right.header);
    let left = rest.pop().unwrap();
    let left_ = into_symbol(left.header);
    let expression = left.result.tokens();
    let expression = resolve_operator_precedence_if_non_empty(expression);
    let mut rest = vec![];
    let mut lhs_ = &expression;
    while let Some(Tree {
                       variant: box Variant::OprApp(OprApp { lhs, opr: Ok(opr), rhs: Some(rhs) }), ..
                   }) = lhs_ && opr.properties.is_sequence() {
        lhs_ = lhs;
        let operator = opr.clone();
        let body = rhs.clone();
        rest.push(OperatorDelimitedTree { operator, body });
    }
    let first = lhs_.clone();
    GroupedSequence { left: left_, first, rest, right: right_ }
}

fn splice<'s>() -> Definition<'s> {
    crate::macro_definition! {("`", everything(), "`", nothing()) splice_body}
}

fn splice_body(segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    use operator::resolve_operator_precedence_if_non_empty;
    let (close, mut segments) = segments.pop();
    let close = into_symbol(close.header);
    let segment = segments.pop().unwrap();
    let open = into_symbol(segment.header);
    let expression = segment.result.tokens();
    let expression = resolve_operator_precedence_if_non_empty(expression);
    let splice = syntax::tree::TextElement::Splice { open, expression, close };
    syntax::Tree::text_literal(default(), vec![splice], default(), default())
}

fn into_symbol(token: syntax::token::Token) -> syntax::token::Symbol {
    let syntax::token::Token { left_offset, code, .. } = token;
    syntax::token::symbol(left_offset, code)
}

fn into_ident(token: syntax::token::Token) -> syntax::token::Ident {
    let syntax::token::Token { left_offset, code, .. } = token;
    syntax::token::ident(left_offset, code, false, 0, false, false)
}
