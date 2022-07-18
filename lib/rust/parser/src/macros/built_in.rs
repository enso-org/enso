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
    // macro_map.register(if_then());
    // macro_map.register(if_then_else());
    macro_map.register(group());
    macro_map.register(type_def());
    macro_map
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
    crate::macro_definition! {("(", everything(), ")", nothing())}
}

/// New type definition macro definition.
pub fn type_def<'s>() -> Definition<'s> {
    use pattern::*;
    #[rustfmt::skip]
    let pattern = 
        identifier() / "name" % "type name" >>
        many(identifier() % "type parameter" / "param") % "type parameters" >>
        block(
            everything() / "statements" % "type methods definitions"
        ) % "type definition body";
    crate::macro_definition! {
        ("type", pattern)
        type_def_body
    }
}

fn type_def_body(matched_segments: NonEmptyVec<MatchedSegment>) -> syntax::Tree {
    let segment = matched_segments.pop().0;
    let match_tree = segment.result.into_var_map();
    let mut v = match_tree.view();
    let name = &v.query("name").unwrap()[0];
    let name = operator::resolve_operator_precedence_if_non_empty(name.clone()).unwrap();
    let no_params = [];
    let params = v.nested().query("param").unwrap_or(&no_params);
    let params = params
        .iter()
        .map(|tokens| operator::resolve_operator_precedence_if_non_empty(tokens.clone()).unwrap())
        .collect_vec();
    let mut constructors = default();
    let mut body = default();
    if let Some(statements) = v.query("statements") {
        let statements = &statements[0];
        let mut builder = TypeDefBodyBuilder::default();
        for (i, item) in statements.iter().enumerate() {
            if let syntax::Item::Token(syntax::Token {
                variant: syntax::token::Variant::Newline(newline),
                left_offset,
                code,
            }) = item
            {
                let newline = syntax::Token(left_offset.clone(), code.clone(), *newline);
                builder.line(i, newline, statements);
            }
        }
        let (constructors_, body_) = builder.finish(statements);
        constructors = constructors_;
        body = body_;
    }
    syntax::Tree::type_def(segment.header, name, params, constructors, body)
}

#[derive(Default)]
struct TypeDefBodyBuilder<'s> {
    constructors: Vec<syntax::tree::TypeConstructorLine<'s>>,
    body:         Vec<syntax::tree::block::Line<'s>>,
    line_items:   Vec<syntax::Item<'s>>,
    newline:      Option<(usize, syntax::token::Newline<'s>)>,
}

impl<'s> TypeDefBodyBuilder<'s> {
    /// Add the line beginning at the given newline token to the state.
    pub fn line(
        &mut self,
        i: usize,
        newline: syntax::token::Newline<'s>,
        body: &[syntax::Item<'s>],
    ) {
        let prev_newline = self.newline.replace((i + 1, newline));
        if i != 0 {
            let (start, newline) =
                prev_newline.unwrap_or_else(|| (0, syntax::token::newline("", "")));
            self.line_(newline, &body[start..i]);
        }
    }

    /// Finish building the constructor/body sequences, and return them.
    pub fn finish(
        mut self,
        body: &[syntax::Item<'s>],
    ) -> (Vec<syntax::tree::TypeConstructorLine<'s>>, Vec<syntax::tree::block::Line<'s>>) {
        if let Some((start, newline)) = self.newline.clone() {
            self.line_(newline, &body[start..]);
        }
        (self.constructors, self.body)
    }

    fn line_(&mut self, newline: syntax::token::Newline<'s>, items: &[syntax::Item<'s>]) {
        self.line_items.extend(items.iter().cloned());
        let expression = (!self.line_items.is_empty())
            .as_some_from(|| operator::resolve_operator_precedence(self.line_items.drain(..)));
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

    /// Interpret the given expression as an `TypeConstructorDef`, if its syntax is compatible.
    fn to_constructor_line(
        expression: syntax::Tree<'_>,
    ) -> Result<syntax::tree::TypeConstructorDef<'_>, syntax::Tree<'_>> {
        use syntax::tree::*;
        if let Tree {
            variant:
                box Variant::ArgumentBlockApplication(ArgumentBlockApplication {
                    lhs: Some(Tree { variant: box Variant::Ident(ident), span: span_ }),
                    arguments,
                }),
            span,
        } = expression
        {
            let mut left_offset = span.left_offset;
            left_offset += span_.left_offset;
            let mut constructor = ident.token;
            left_offset += constructor.left_offset;
            constructor.left_offset = left_offset;
            let block = arguments;
            let arguments = default();
            return Ok(TypeConstructorDef { constructor, arguments, block });
        }
        let mut arguments = vec![];
        let mut lhs = &expression;
        let mut left_offset = crate::source::span::Offset::default();
        while let Tree { variant: box Variant::App(App { func, arg }), span } = lhs {
            left_offset += &span.left_offset;
            lhs = func;
            arguments.push(arg.clone());
        }
        if let Tree { variant: box Variant::Ident(Ident { token }), span } = lhs {
            left_offset += &span.left_offset;
            let mut constructor = token.clone();
            left_offset += constructor.left_offset;
            constructor.left_offset = left_offset;
            arguments.reverse();
            let block = default();
            return Ok(TypeConstructorDef { constructor, arguments, block });
        }
        Err(expression)
    }
}
