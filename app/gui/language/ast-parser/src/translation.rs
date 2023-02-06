use enso_prelude::*;
use enso_profiler::prelude::*;

use ast::Ast;
use enso_parser::syntax;
use enso_parser::syntax::tree;
use enso_parser::syntax::Tree;
use enso_profiler as profiler;
use std::collections::BTreeMap;



/// Enable extra log messages and assertions.
const DEBUG: bool = true;



// =======================
// === Translation API ===
// =======================

#[profile(Detail)]
pub fn tree_to_ast(tree: &Tree, ids: BTreeMap<(usize, usize), uuid::Uuid>) -> Ast {
    use ast::HasRepr;
    let mut context = Translate { ids, ..Default::default() };
    let ast = match &*tree.variant {
        tree::Variant::BodyBlock(block) => context.translate_module(block),
        _ => panic!(),
    };
    if DEBUG {
        debug_assert_eq!(ast.repr(), tree.code());
        if !context.ids.is_empty() || !context.ids_missed.is_empty() {
            warn!(
                "ids not matched: {:?}\nids missed: {:?}\nids assigned: {:?}",
                &context.ids, &context.ids_missed, &context.ids_assigned
            );
        }
    }
    ast
}


// === Implementation ===

#[derive(Debug, Default)]
struct Translate {
    offset:       usize,
    ids:          BTreeMap<(usize, usize), uuid::Uuid>,
    space_after:  BTreeMap<usize, usize>,
    // Debugging
    ids_assigned: Vec<((usize, usize), uuid::Uuid)>,
    ids_missed:   Vec<(usize, usize)>,
}

impl Translate {
    fn visit_space(&mut self, span: &enso_parser::source::Span) -> usize {
        let space = span.left_offset.code.repr.len();
        self.space_after.insert(self.offset, space);
        self.offset += space;
        span.left_offset.visible.width_in_spaces
    }

    fn visit_token<T>(&mut self, token: &syntax::Token<T>) -> WithInitialSpace<String> {
        let space = token.left_offset.visible.width_in_spaces;
        let body = token.code.to_string();
        self.space_after.insert(self.offset, space);
        self.offset += token.left_offset.code.repr.len();
        self.offset += token.code.repr.len();
        WithInitialSpace { space, body }
    }

    fn visit_token_ref<T>(&mut self, token: syntax::token::Ref<T>) -> WithInitialSpace<String> {
        let space = token.left_offset.visible.width_in_spaces;
        let body = token.code.to_string();
        self.space_after.insert(self.offset, space);
        self.offset += token.left_offset.code.repr.len();
        self.offset += token.code.repr.len();
        WithInitialSpace { space, body }
    }
}

struct AstBuilder {
    start: usize,
}

impl Translate {
    fn start_ast(&mut self) -> AstBuilder {
        AstBuilder { start: self.offset }
    }

    fn finish_ast<S: Into<ast::Shape<Ast>>>(&mut self, shape: S, builder: AstBuilder) -> Ast {
        let AstBuilder { start } = builder;
        let start = start + self.space_after.get(&start).copied().unwrap_or_default();
        let end = self.offset;
        let id = self.ids.remove(&(start, end));
        if DEBUG {
            match id {
                Some(id) => self.ids_assigned.push(((start, end), id)),
                None => self.ids_missed.push((start, end)),
            }
        }
        Ast::new(shape, id)
    }
}

impl Translate {
    fn translate(&mut self, tree: &Tree) -> WithInitialSpace<Ast> {
        let space = self.visit_space(&tree.span);
        let builder = self.start_ast();
        let body = match &*tree.variant {
            tree::Variant::BodyBlock(block) => {
                let block = self.translate_block(&block.statements).unwrap().expect_unspaced();
                self.finish_ast(block, builder)
            }
            tree::Variant::Ident(tree::Ident { token }) => {
                let name = self.visit_token(token).expect_unspaced();
                match token.is_type {
                    true => self.finish_ast(ast::Cons { name }, builder),
                    false => self.finish_ast(ast::Var { name }, builder),
                }
            }
            tree::Variant::Number(tree::Number { base, integer, .. }) => {
                let base = base.as_ref().map(|base| self.visit_token(base).expect_unspaced());
                let int = integer
                    .as_ref()
                    .map(|integer| self.visit_token(integer).expect_unspaced())
                    .unwrap_or_default();
                self.finish_ast(ast::Number { base, int }, builder)
            }
            tree::Variant::App(tree::App { func, arg }) => {
                let func = self.translate(func);
                let arg = self.translate(arg);
                let app = prefix(func, arg).expect_unspaced();
                self.finish_ast(app, builder)
            }
            tree::Variant::OprApp(tree::OprApp { lhs: Some(_), opr: Ok(opr), rhs: Some(_) })
                if opr.properties.is_arrow() =>
            {
                let span_info = self.translate_items(tree);
                let type_info = ast::TreeType::Lambda;
                self.finish_ast(ast::Tree { span_info, type_info, leaf_info: None }, builder)
            }
            tree::Variant::OprApp(tree::OprApp { lhs, opr, rhs }) => {
                let opr_app = self.translate_opr_app(lhs.as_ref(), opr, rhs.as_ref());
                self.finish_ast(opr_app, builder)
            }
            tree::Variant::OprSectionBoundary(tree::OprSectionBoundary { ast, .. }) =>
                self.translate(ast).expect_unspaced(),
            tree::Variant::Function(func) => {
                let func = self.translate_function(func);
                self.finish_ast(func, builder)
            }
            tree::Variant::ForeignFunction(func) => {
                let func = self.translate_foreign_function(func);
                self.finish_ast(func, builder)
            }
            tree::Variant::UnaryOprApp(tree::UnaryOprApp { opr, rhs }) => {
                let opr_builder = self.start_ast();
                let name = self.visit_token(opr);
                let opr = name.map(|_| self.finish_ast(opr_from_token(opr), opr_builder));
                if let Some(arg) = rhs {
                    let arg = self.translate(arg);
                    let section = section_right(opr, arg).expect_unspaced();
                    self.finish_ast(section, builder)
                } else {
                    let opr = opr.expect_unspaced();
                    self.finish_ast(ast::SectionSides { opr }, builder)
                }
            }
            tree::Variant::Assignment(tree::Assignment { pattern, equals, expr }) =>
                self.opr_app(pattern, equals, expr).expect_unspaced(),
            tree::Variant::OperatorBlockApplication(app) => {
                let tree::OperatorBlockApplication { lhs, expressions, excess } = app;
                let func = self.translate(lhs.as_ref().unwrap());
                let block = self.translate_operator_block(expressions, excess);
                let app = prefix(func, block).expect_unspaced();
                self.finish_ast(app, builder)
            }
            tree::Variant::TemplateFunction(tree::TemplateFunction { ast, .. }) =>
                self.translate(ast).expect_unspaced(),
            tree::Variant::Wildcard(_) => self.finish_ast(ast::Blank {}, builder),
            tree::Variant::ArgumentBlockApplication(app) => {
                let tree::ArgumentBlockApplication { lhs, arguments } = app;
                let func = self.translate(lhs.as_ref().unwrap());
                let arg = self.translate_block(arguments).unwrap().map(Ast::from);
                let app = prefix(func, arg).expect_unspaced();
                self.finish_ast(app, builder)
            }
            tree::Variant::DefaultApp(tree::DefaultApp { func, default }) => {
                let func = self.translate(func);
                let arg_builder = self.start_ast();
                let default = self.visit_token(default);
                let arg = default.map(|name| self.finish_ast(ast::Var { name }, arg_builder));
                let app = prefix(func, arg).expect_unspaced();
                self.finish_ast(app, builder)
            }
            tree::Variant::NamedApp(tree::NamedApp { func, open, name, equals, arg, close }) => {
                let func = self.translate(func);
                let open = open.as_ref().map(|token| self.visit_token(token));
                let name = self.visit_token(name);
                let larg = name.map(|name| Ast::from(ast::Var { name }));
                let opr = self.visit_token(equals).map(|_| Ast::from(opr_from_token(equals)));
                let rarg = self.translate(arg);
                let mut arg = infix(larg, opr, rarg).map(Ast::from);
                let close = close.as_ref().map(|token| self.visit_token(token));
                if let Some(open) = open && let Some(close) = close {
                    arg = open.map(|open| group(open, arg, close));
                }
                let app = prefix(func, arg).expect_unspaced();
                self.finish_ast(app, builder)
            }
            tree::Variant::TypeSignature(tree::TypeSignature { variable, operator, type_ }) =>
                self.opr_app(variable, operator, type_).expect_unspaced(),
            tree::Variant::TypeAnnotated(tree::TypeAnnotated { expression, operator, type_ }) =>
                self.opr_app(expression, operator, type_).expect_unspaced(),
            tree::Variant::AnnotatedBuiltin(tree::AnnotatedBuiltin {
                token,
                annotation,
                newlines,
                expression,
            }) => {
                let at = self.visit_token(token).expect_unspaced();
                let func = self.visit_token(annotation);
                let func =
                    func.map(|func| Ast::from(ast::Annotation { name: format!("{at}{func}") }));
                let arg = self.translate(expression.as_ref().unwrap());
                debug_assert!(
                    newlines.is_empty(),
                    "Multiline expression must be handled in translate_lines."
                );
                let app = prefix(func, arg).expect_unspaced();
                self.finish_ast(app, builder)
            }
            tree::Variant::Documented(tree::Documented { documentation: _, expression }) => {
                warn!("Multiline expression should have been handled in translate_lines.");
                self.translate(expression.as_ref().unwrap()).without_space()
            }
            tree::Variant::Import(import) => {
                let span_info = self.translate_items(tree);
                let type_info = analyze_import(import).unwrap_or_default();
                self.finish_ast(ast::Tree { span_info, type_info, leaf_info: None }, builder)
            }
            tree::Variant::CaseOf(_) => {
                // TODO: Analyzed-representation to support alias analysis.
                let span_info = self.translate_items(tree);
                let type_info = ast::TreeType::Expression;
                self.finish_ast(ast::Tree { span_info, type_info, leaf_info: None }, builder)
            }
            tree::Variant::TextLiteral(_) => {
                self.translate_items(tree);
                let type_info = ast::TreeType::Expression;
                let span_info = vec![];
                let leaf_info = Some(tree.trimmed_code());
                self.finish_ast(ast::Tree { span_info, type_info, leaf_info }, builder)
            }
            _ => {
                let span_info = self.translate_items(tree);
                let type_info = ast::TreeType::Expression;
                self.finish_ast(ast::Tree { span_info, type_info, leaf_info: None }, builder)
            }
        };
        WithInitialSpace { space, body }
    }

    fn translate_lines(
        &mut self,
        tree: &Tree,
        out: &mut impl Extend<WithInitialSpace<Option<Ast>>>,
    ) {
        match &*tree.variant {
            tree::Variant::AnnotatedBuiltin(tree::AnnotatedBuiltin {
                token,
                annotation,
                newlines,
                expression,
            }) if !newlines.is_empty() => {
                let space = self.visit_space(&tree.span);
                let at = self.visit_token(token).expect_unspaced();
                let annotation = self.visit_token(annotation).expect_unspaced();
                let body = Some(Ast::from(ast::Annotation { name: format!("{at}{annotation}") }));
                out.extend_one(WithInitialSpace { space, body });
                out.extend(newlines.iter().map(|token| {
                    let (space, _) = self.visit_token(token).split();
                    let body = None;
                    WithInitialSpace { space, body }
                }));
                out.extend(
                    expression.as_ref().map(|expression| self.translate(expression).map(Some)),
                );
            }
            tree::Variant::Annotated(_) => todo!(),
            tree::Variant::Documented(tree::Documented { documentation, expression }) => {
                let space = self.visit_space(&tree.span);
                let body = Some(self.translate_doc(documentation));
                out.extend_one(WithInitialSpace { space, body });
                if let Some(expression) = expression {
                    self.translate_lines(expression, out);
                }
            }
            _ => out.extend_one(self.translate(tree).map(Some)),
        }
    }

    fn translate_doc(&mut self, documentation: &tree::DocComment) -> Ast {
        let open = self.visit_token(&documentation.open);
        let mut span_info = RawSpanTreeBuilder::new();
        span_info.token(open);
        for element in &documentation.elements {
            span_info.token(match element {
                tree::TextElement::Section { text } => self.visit_token(text),
                tree::TextElement::Escape { token } => self.visit_token(token),
                tree::TextElement::Newline { newline } => self.visit_token(newline),
                tree::TextElement::Splice { .. } => {
                    let error = "Lexer must not emit splices in documentation comments.";
                    debug_assert!(false, "{error}");
                    error!("{error}");
                    continue;
                }
            })
        }
        // One newline is implicit.
        let mut token = None;
        for newline in &documentation.newlines {
            if let Some(prev) = token.replace(self.visit_token(newline)) {
                span_info.token(prev);
            }
        }
        let rendered = documentation.content().into();
        let type_info = ast::TreeType::Documentation { rendered };
        let span_info = span_info.build().expect_unspaced();
        Ast::from(ast::Tree { span_info, type_info, leaf_info: None })
    }

    fn translate_function(&mut self, function: &tree::Function) -> ast::Shape<Ast> {
        let tree::Function { name, args, equals, body } = function;
        let name = self.translate(name);
        let mut lhs_terms = vec![name];
        lhs_terms.extend(args.iter().map(|a| self.translate_argument_definition(a)));
        let larg =
            lhs_terms.into_iter().reduce(|func, arg| prefix(func, arg).map(Ast::from)).unwrap();
        let opr = self.visit_token(equals).map(|_| Ast::from(opr_from_token(equals)));
        match body {
            Some(body) => {
                let rarg = self.translate(body);
                infix(larg, opr, rarg).expect_unspaced()
            }
            None => section_left(larg, opr).expect_unspaced(),
        }
    }

    fn translate_foreign_function(&mut self, func: &tree::ForeignFunction) -> ast::Shape<Ast> {
        let tree::ForeignFunction { foreign, language, name, args, equals, body } = func;
        let mut lhs_terms: Vec<_> = [foreign, language, name]
            .into_iter()
            .map(|ident| self.visit_token(ident).map(|name| Ast::from(ast::Var { name })))
            .collect();
        lhs_terms.extend(args.iter().map(|a| self.translate_argument_definition(a)));
        let lhs =
            lhs_terms.into_iter().reduce(|func, arg| prefix(func, arg).map(Ast::from)).unwrap();
        let equals = self.visit_token(equals).map(|_| Ast::from(opr_from_token(equals)));
        let body = self.translate(body);
        infix(lhs, equals, body).expect_unspaced()
    }

    fn opr_app(
        &mut self,
        lhs: &Tree,
        opr: &syntax::token::Operator,
        rhs: &Tree,
    ) -> WithInitialSpace<Ast> {
        let builder = self.start_ast();
        let lhs = self.translate(lhs);
        let opr_builder = self.start_ast();
        let opr = self.visit_token(opr).map(|_| opr_from_token(opr));
        let opr = opr.map(|opr| self.finish_ast(opr, opr_builder));
        let rhs = self.translate(rhs);
        infix(lhs, opr, rhs).map(|opr_app| self.finish_ast(opr_app, builder))
    }

    fn translate_opr(&mut self, opr: &tree::OperatorOrError) -> WithInitialSpace<Ast> {
        let opr_builder = self.start_ast();
        let opr = match opr {
            Ok(name) => {
                let token = self.visit_token(name);
                match name.code.repr.strip_suffix('=') {
                    Some(mod_name) if mod_name.contains(|c| c != '=') => token.map(|_| {
                        let name = mod_name.to_string();
                        ast::Shape::from(ast::Mod { name })
                    }),
                    _ => token.map(|_| opr_from_token(name)),
                }
            }
            Err(names) => {
                let mut span_info = RawSpanTreeBuilder::new();
                for token in &names.operators {
                    span_info.token(self.visit_token(token));
                }
                let type_info = ast::TreeType::Expression;
                span_info.build().map(|span_info| {
                    ast::Shape::from(ast::Tree { span_info, type_info, leaf_info: None })
                })
            }
        };
        opr.map(|opr| self.finish_ast(opr, opr_builder))
    }

    fn translate_opr_app(
        &mut self,
        lhs: Option<&Tree>,
        opr: &tree::OperatorOrError,
        rhs: Option<&Tree>,
    ) -> ast::Shape<Ast> {
        let larg = lhs.map(|a| self.translate(a));
        let opr = self.translate_opr(opr);
        let rarg = rhs.map(|a| self.translate(a));
        (match (larg, rarg) {
            (Some(larg), Some(rarg)) => infix(larg, opr, rarg),
            (Some(larg), None) => section_left(larg, opr),
            (None, Some(rarg)) => section_right(opr, rarg),
            (None, None) => section_sides(opr),
        })
        .expect_unspaced()
    }

    fn translate_module(&mut self, block: &tree::BodyBlock) -> Ast {
        let builder = self.start_ast();
        let (lines, _) =
            self.translate_block_raw(&block.statements).unwrap_or_default().expect_unspaced();
        self.finish_ast(ast::Module { lines }, builder)
    }

    fn translate_block<'a, 's: 'a>(
        &mut self,
        tree_lines: impl IntoIterator<Item = &'a tree::block::Line<'s>>,
    ) -> Option<WithInitialSpace<ast::Block<Ast>>> {
        let (space, (ast_lines, indent)) = self.translate_block_raw(tree_lines)?.split();
        let mut empty_lines = vec![];
        let mut first_line = None;
        let mut lines = vec![];
        for line in ast_lines {
            if first_line.is_none() {
                if let Some(elem) = line.elem {
                    first_line = Some(ast::BlockLine { elem, off: line.off });
                } else {
                    empty_lines.push(line.off);
                }
            } else {
                lines.push(line);
            }
        }
        let first_line = first_line?;
        let body = ast::Block { indent, empty_lines, first_line, lines };
        Some(WithInitialSpace { space, body })
    }

    fn translate_block_raw<'a, 's: 'a>(
        &mut self,
        tree_lines: impl IntoIterator<Item = &'a tree::block::Line<'s>>,
    ) -> Option<WithInitialSpace<(Vec<ast::BlockLine<Option<Ast>>>, usize)>> {
        let tree_lines = tree_lines.into_iter();
        let mut ast_lines: Vec<ast::BlockLine<Option<Ast>>> =
            Vec::with_capacity(tree_lines.size_hint().0);
        let mut statement_lines = vec![];
        let mut initial_indent = None;
        let mut space = 0;
        for tree::block::Line { newline, expression } in tree_lines {
            // Mapping from [`Tree`]'s leading offsets to [`Ast`]'s trailing offsets:
            // Initially, we create each line with no trailing offset.
            let off = 0;
            // We write each line's leading offset into the trailing offset of the previous line
            // (or, for the first line, the initial offset).
            let trailing_space = self.visit_token(newline).space;
            *ast_lines.last_mut().map(|line| &mut line.off).unwrap_or(&mut space) = trailing_space;
            match &expression {
                Some(statement) => {
                    self.translate_lines(statement, &mut statement_lines);
                    if initial_indent.is_none() && let Some(first) = statement_lines.first() {
                        initial_indent = Some(first.space);
                    }
                    let new_lines = statement_lines
                        .drain(..)
                        .map(|elem| ast::BlockLine { elem: elem.without_space(), off });
                    ast_lines.extend(new_lines);
                }
                None => ast_lines.push(ast::BlockLine { elem: None, off }),
            }
        }
        let body = (ast_lines, initial_indent?);
        Some(WithInitialSpace { space, body })
    }

    fn translate_operator_block<'a, 's: 'a>(
        &mut self,
        operator_lines: impl IntoIterator<Item = &'a tree::block::OperatorLine<'s>>,
        excess: impl IntoIterator<Item = &'a tree::block::Line<'s>>,
    ) -> WithInitialSpace<Ast> {
        let mut ast_lines: Vec<ast::BlockLine<_>> = vec![];
        let mut indent = None;
        let mut space = 0;
        let off = 0;
        for tree::block::OperatorLine { newline, expression } in operator_lines {
            let trailing_space = self.visit_token(newline).space;
            *ast_lines.last_mut().map(|line| &mut line.off).unwrap_or(&mut space) = trailing_space;
            let elem = expression.as_ref().map(|expression| {
                let opr = self.translate_opr(&expression.operator);
                let arg = self.translate(&expression.expression);
                let (space, elem) = section_right(opr, arg).split();
                if indent.is_none() {
                    indent = Some(space);
                }
                Ast::from(elem)
            });
            ast_lines.push(ast::BlockLine { elem, off });
        }
        let indent = indent.unwrap();
        let mut statement_lines = vec![];
        for tree::block::Line { newline, expression } in excess {
            let trailing_space = self.visit_token(newline).space;
            *ast_lines.last_mut().map(|line| &mut line.off).unwrap_or(&mut space) = trailing_space;
            match &expression {
                Some(statement) => {
                    self.translate_lines(statement, &mut statement_lines);
                    let new_lines = statement_lines
                        .drain(..)
                        .map(|elem| ast::BlockLine { elem: elem.without_space(), off });
                    ast_lines.extend(new_lines);
                }
                None => ast_lines.push(ast::BlockLine { elem: None, off }),
            }
        }
        let mut first_line = None;
        let mut empty_lines = vec![];
        let mut lines = vec![];
        for ast::BlockLine { elem, off } in ast_lines {
            match (elem, &mut first_line) {
                (None, None) => empty_lines.push(off),
                (Some(elem), None) => first_line = Some(ast::BlockLine { elem, off }),
                (elem, Some(_)) => lines.push(ast::BlockLine { elem, off }),
            }
        }
        let first_line = first_line.unwrap();
        let body = Ast::from(ast::Block { indent, empty_lines, first_line, lines });
        WithInitialSpace { space, body }
    }

    fn translate_argument_definition(
        &mut self,
        arg: &tree::ArgumentDefinition,
    ) -> WithInitialSpace<Ast> {
        let tree::ArgumentDefinition {
            open,
            open2,
            suspension,
            pattern,
            type_,
            close2,
            default,
            close,
        } = arg;
        let open = open.as_ref().map(|token| self.visit_token(token));
        let open2 = open2.as_ref().map(|token| self.visit_token(token));
        let suspension = suspension
            .as_ref()
            .map(|token| self.visit_token(token).map(|_| Ast::from(opr_from_token(token))));
        let mut term = self.translate(pattern);
        if let Some(opr) = suspension {
            term = section_right(opr, term).map(Ast::from);
        }
        if let Some(tree::ArgumentType { operator, type_ }) = type_ {
            let opr = self.visit_token(operator).map(|_| Ast::from(opr_from_token(operator)));
            let rarg = self.translate(type_);
            term = infix(term, opr, rarg).map(Ast::from);
        }
        let close2 = close2.as_ref().map(|token| self.visit_token(token));
        if let Some(open) = open2 && let Some(close) = close2 {
            term = open.map(|open| group(open, term, close));
        }
        if let Some(tree::ArgumentDefault { equals, expression }) = default {
            let opr = self.visit_token(equals).map(|_| Ast::from(opr_from_token(equals)));
            let rarg = self.translate(expression);
            term = infix(term, opr, rarg).map(Ast::from);
        }
        let close = close.as_ref().map(|token| self.visit_token(token));
        if let Some(open) = open && let Some(close) = close {
            term = open.map(|open| group(open, term, close));
        }
        term
    }

    /// Analyze a [`Tree`] and produce a representation used by the graph editor.
    fn translate_items(&mut self, tree: &syntax::tree::Tree<'_>) -> Vec<ast::RawSpanTree<Ast>> {
        let mut span_info = RawSpanTreeBuilder::new();
        tree.visit_items(|item| match item {
            syntax::item::Ref::Token(token) => span_info.token(self.visit_token_ref(token)),
            syntax::item::Ref::Tree(tree) => span_info.child(self.translate(tree)),
        });
        span_info.build().expect_unspaced()
    }
}

fn opr_from_token(token: &syntax::token::Operator) -> ast::Shape<Ast> {
    let name = token.code.repr.to_string();
    let right_assoc = token.properties.associativity() == syntax::token::Associativity::Right;
    ast::Shape::from(ast::Opr { name, right_assoc })
}

fn group(open: String, body: WithInitialSpace<Ast>, close: WithInitialSpace<String>) -> Ast {
    let (body_space, body) = body.split();
    let (close_space, close) = close.split();
    let min_elements = 3; // There are always at least 3 elements: open, close, and body
    let mut span_info = Vec::with_capacity(min_elements);
    span_info.push(ast::RawSpanTree::Token(ast::RawSpanTreeToken { token: open }));
    span_info.extend(ast::RawSpanTree::space(body_space));
    span_info.push(ast::RawSpanTree::Child(ast::RawSpanTreeChild { node: body }));
    span_info.extend(ast::RawSpanTree::space(close_space));
    span_info.push(ast::RawSpanTree::Token(ast::RawSpanTreeToken { token: close }));
    let type_info = ast::TreeType::Expression;
    Ast::from(ast::Tree { span_info, type_info, leaf_info: None })
}



// === Semantic Analysis ===


// TODO: In place of this analysis (and a similar analysis in Java [`TreeToIr`]),
//  refactor [`tree::Import`] to a higher-level representation resembling
//  [`ast::ImportedNames`] (but with concrete tokens).
fn analyze_import(import: &tree::Import) -> Option<ast::TreeType> {
    let tree::Import { polyglot, from, import, all, as_, hiding } = import;
    fn parse_module(tree: &Tree) -> Option<Vec<ImString>> {
        let mut segments = vec![];
        for tree in tree.left_assoc_rev(".") {
            match &*tree.variant {
                tree::Variant::Ident(tree::Ident { token }) =>
                    segments.push(token.code.to_string().into()),
                _ => return None,
            }
        }
        segments.reverse();
        Some(segments)
    }
    fn parse_ident(tree: &Tree) -> Option<String> {
        match &*tree.variant {
            tree::Variant::Ident(tree::Ident { token }) => Some(token.code.to_string()),
            _ => None,
        }
    }
    fn parse_idents(tree: &Tree) -> Option<std::collections::BTreeSet<String>> {
        let mut names = std::collections::BTreeSet::new();
        for tree in tree.left_assoc_rev(",") {
            match &*tree.variant {
                tree::Variant::Ident(tree::Ident { token }) => {
                    names.insert(token.code.to_string());
                }
                _ => return None,
            }
        }
        Some(names)
    }
    let module;
    let imported;
    match (polyglot, from, all, as_, hiding) {
        (None, None, None, None, None) => {
            module = import.body.as_ref().and_then(parse_module)?;
            imported = ast::ImportedNames::Module { alias: None };
        }
        (None, None, None, Some(as_), None) => {
            module = import.body.as_ref().and_then(parse_module)?;
            let alias = as_.body.as_ref().and_then(parse_ident);
            imported = ast::ImportedNames::Module { alias };
        }
        (None, Some(from), None, None, None) => {
            module = from.body.as_ref().and_then(parse_module)?;
            let names = import.body.as_ref().and_then(parse_idents)?;
            imported = ast::ImportedNames::List { names };
        }
        (None, Some(from), Some(_), None, None) => {
            module = from.body.as_ref().and_then(parse_module)?;
            imported = ast::ImportedNames::All { except: Default::default() };
        }
        (None, Some(from), Some(_), None, Some(hiding)) => {
            module = from.body.as_ref().and_then(parse_module)?;
            let except = hiding.body.as_ref().and_then(parse_idents)?;
            imported = ast::ImportedNames::All { except };
        }
        _ => return None,
    }
    Some(ast::TreeType::Import { module, imported })
}


// === WithInitialSpace ===

/// Helper for propagating spacing from children (Tree-style left offsets) to parents (Ast-style
/// top-down spacing).
#[derive(Debug, Default)]
struct WithInitialSpace<T> {
    space: usize,
    body:  T,
}

impl<T: Debug> WithInitialSpace<T> {
    /// Return the value, ignoring any initial space.
    fn without_space(self) -> T {
        self.body
    }

    /// If any initial space is present, emit a warning; forget the space and return the value.
    fn expect_unspaced(self) -> T {
        if DEBUG {
            debug_assert_eq!(self.space, 0, "Expected no space before term: {:?}", &self.body);
        } else if self.space != 0 {
            warn!("Expected no space before term: {:?}", &self.body);
        }
        self.body
    }

    /// Return the initial space and the value.
    fn split(self) -> (usize, T) {
        (self.space, self.body)
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> WithInitialSpace<U> {
        let WithInitialSpace { space, body } = self;
        let body = f(body);
        WithInitialSpace { space, body }
    }
}


// === Shape-building helpers

fn prefix(
    func: WithInitialSpace<Ast>,
    arg: WithInitialSpace<Ast>,
) -> WithInitialSpace<ast::Shape<Ast>> {
    func.map(|func| {
        let (off, arg) = arg.split();
        (ast::Prefix { func, off, arg }).into()
    })
}

fn section_sides(opr: WithInitialSpace<Ast>) -> WithInitialSpace<ast::Shape<Ast>> {
    opr.map(|opr| (ast::SectionSides { opr }).into())
}

fn section_left(
    arg: WithInitialSpace<Ast>,
    opr: WithInitialSpace<Ast>,
) -> WithInitialSpace<ast::Shape<Ast>> {
    arg.map(|arg| {
        let (off, opr) = opr.split();
        (ast::SectionLeft { arg, off, opr }).into()
    })
}

fn section_right(
    opr: WithInitialSpace<Ast>,
    arg: WithInitialSpace<Ast>,
) -> WithInitialSpace<ast::Shape<Ast>> {
    opr.map(|opr| {
        let (off, arg) = arg.split();
        (ast::SectionRight { opr, off, arg }).into()
    })
}

fn infix(
    larg: WithInitialSpace<Ast>,
    opr: WithInitialSpace<Ast>,
    rarg: WithInitialSpace<Ast>,
) -> WithInitialSpace<ast::Shape<Ast>> {
    larg.map(|larg| {
        let (loff, opr) = opr.split();
        let (roff, rarg) = rarg.split();
        (ast::Infix { larg, loff, opr, roff, rarg }).into()
    })
}


// === RawSpanTreeBuilder ===

#[derive(Debug, Default)]
pub struct RawSpanTreeBuilder {
    space: Option<usize>,
    spans: Vec<ast::RawSpanTree<Ast>>,
}

impl RawSpanTreeBuilder {
    fn new() -> Self {
        Self::default()
    }

    fn token(&mut self, value: WithInitialSpace<String>) {
        let (space, value) = value.split();
        if self.space.is_none() {
            self.space = Some(space);
        } else {
            self.spans.extend(ast::RawSpanTree::space(space));
        }
        self.spans.push(ast::RawSpanTree::Token(ast::RawSpanTreeToken { token: value }));
    }

    fn child(&mut self, node: WithInitialSpace<Ast>) {
        let (space, node) = node.split();
        if self.space.is_none() {
            self.space = Some(space);
        } else {
            self.spans.extend(ast::RawSpanTree::space(space));
        }
        self.spans.push(ast::RawSpanTree::Child(ast::RawSpanTreeChild { node }));
    }

    fn build(self) -> WithInitialSpace<Vec<ast::RawSpanTree<Ast>>> {
        let space = self.space.unwrap_or_default();
        let body = self.spans;
        WithInitialSpace { space, body }
    }
}
