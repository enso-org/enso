use ast::Ast;
use enso_parser::syntax;
use enso_parser::syntax::tree;
use enso_parser::syntax::tree::NonEmptyOperatorSequence;
use enso_parser::syntax::Tree;
use enso_prelude::ImString;
use std::collections::BTreeMap;



// =======================
// === Translation API ===
// =======================

fn to_legacy_ast_module(tree: &Tree, ids: BTreeMap<(usize, usize), uuid::Uuid>) -> Result<Ast, ()> {
    let mut context = Translate { offset: Default::default(), ids };
    match &*tree.variant {
        tree::Variant::BodyBlock(block) => Ok(context.translate_module(&block).into()),
        _ => Err(()),
    }
}

pub fn to_legacy_ast(tree: &Tree, ids: BTreeMap<(usize, usize), uuid::Uuid>) -> Ast {
    use ast::HasRepr;
    let ast = to_legacy_ast_module(tree, ids).unwrap();
    debug_assert_eq!(ast.repr(), tree.code());
    ast
}


// === Implementation ===

#[derive(Debug)]
struct Translate {
    offset: usize,
    ids:    BTreeMap<(usize, usize), uuid::Uuid>,
}

impl Translate {
    fn visit_space(&mut self, span: &enso_parser::source::Span) -> usize {
        self.offset += span.left_offset.code.repr.len();
        span.left_offset.visible.width_in_spaces
    }

    fn visit_token<T>(&mut self, token: &syntax::Token<T>) -> WithInitialSpace<String> {
        self.offset += token.left_offset.code.repr.len();
        self.offset += token.code.repr.len();
        let space = token.left_offset.visible.width_in_spaces;
        let body = token.code.to_string();
        WithInitialSpace { space, body }
    }

    fn visit_token_ref<T>(&mut self, token: syntax::token::Ref<T>) -> WithInitialSpace<String> {
        self.offset += token.left_offset.code.repr.len();
        self.offset += token.code.repr.len();
        let space = token.left_offset.visible.width_in_spaces;
        let body = token.code.to_string();
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
        let end = self.offset;
        let id = self.ids.get(&(start, end)).cloned();
        Ast::new(shape, id)
    }
}

impl Translate {
    fn translate(&mut self, tree: &Tree) -> WithInitialSpace<Ast> {
        let space = self.visit_space(&tree.span);
        let builder = self.start_ast();
        let body = match &*tree.variant {
            tree::Variant::BodyBlock(block) => {
                let block = self.translate_block(&block.statements).unwrap();
                self.finish_ast(block, builder)
            }
            tree::Variant::Ident(tree::Ident { token }) if token.is_type => {
                let name = self.visit_token(token).expect_unspaced();
                self.finish_ast(ast::Cons { name }, builder)
            }
            tree::Variant::Ident(ident) => {
                let name = self.visit_token(&ident.token).expect_unspaced();
                self.finish_ast(ast::Var { name }, builder)
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
                let func = self.translate(func).expect_unspaced();
                let (off, arg) = self.translate(arg).split();
                self.finish_ast(ast::Prefix { func, off, arg }, builder)
            }
            tree::Variant::OprApp(tree::OprApp {
                lhs: Some(lhs),
                opr: Ok(opr),
                rhs: Some(rhs),
            }) if opr.properties.is_arrow() => {
                let span_info = self.translate_items(tree);
                let type_info = ast::TreeType::Lambda;
                self.finish_ast(ast::Tree { span_info, type_info }, builder)
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
                let name = self.visit_token(opr).expect_unspaced();
                let opr = self.finish_ast(ast::Opr { name }, opr_builder);
                if let Some(arg) = rhs {
                    let (off, arg) = self.translate(arg).split();
                    self.finish_ast(ast::SectionRight { opr, off, arg }, builder)
                } else {
                    self.finish_ast(ast::SectionSides { opr }, builder)
                }
            }
            tree::Variant::Assignment(tree::Assignment { pattern, equals, expr }) =>
                self.opr_app(pattern, equals, expr),
            tree::Variant::OperatorBlockApplication(app) => {
                let tree::OperatorBlockApplication { lhs, expressions, excess } = app;
                let func = self.translate(lhs.as_ref().unwrap()).expect_unspaced();
                let arg = self.translate_operator_block(expressions);
                let off = 0; // TODO
                self.finish_ast(ast::Prefix { func, off, arg }, builder)
            }
            tree::Variant::TemplateFunction(tree::TemplateFunction { ast, .. }) =>
                self.translate(ast).expect_unspaced(),
            tree::Variant::Wildcard(_) => self.finish_ast(ast::Blank {}, builder),
            tree::Variant::ArgumentBlockApplication(app) => {
                let tree::ArgumentBlockApplication { lhs, arguments } = app;
                let func = self.translate(lhs.as_ref().unwrap()).expect_unspaced();
                let arg = self.translate_block(arguments).unwrap().into();
                let off = 0; // TODO
                self.finish_ast(ast::Prefix { func, off, arg }, builder)
            }
            tree::Variant::DefaultApp(tree::DefaultApp { func, default }) => {
                let func = self.translate(func).expect_unspaced();
                let arg_builder = self.start_ast();
                let (off, name) = self.visit_token(default).split();
                let arg = self.finish_ast(ast::Var { name }, arg_builder);
                self.finish_ast(ast::Prefix { func, off, arg }, builder)
            }
            tree::Variant::NamedApp(tree::NamedApp { func, open, name, equals, arg, close }) => {
                let func = self.translate(func).expect_unspaced();
                let (off, name) = self.visit_token(name).split();
                let larg = Ast::from(ast::Var { name });
                let (loff, name) = self.visit_token(equals).split();
                let opr = Ast::from(ast::Opr { name });
                let (roff, rarg) = self.translate(arg).split();
                let arg = Ast::from(ast::Infix { larg, loff, opr, roff, rarg });
                self.finish_ast(ast::Prefix { func, off, arg }, builder)
            }
            tree::Variant::TypeSignature(tree::TypeSignature { variable, operator, type_ }) =>
                self.opr_app(variable, operator, type_),
            tree::Variant::TypeAnnotated(tree::TypeAnnotated { expression, operator, type_ }) =>
                self.opr_app(expression, operator, type_),
            tree::Variant::AnnotatedBuiltin(tree::AnnotatedBuiltin {
                token,
                annotation,
                newlines,
                expression,
            }) => {
                let func = self.visit_token(annotation).expect_unspaced();
                let func = Ast::from(ast::Annotation { name: format!("@{}", func) });
                let (off, arg) = self.translate(expression.as_ref().unwrap()).split();
                self.finish_ast(ast::Prefix { func, off, arg }, builder)
            }
            tree::Variant::Documented(tree::Documented { documentation, expression }) =>
                self.translate(expression.as_ref().unwrap()).without_space(),
            tree::Variant::Import(import) => {
                let span_info = self.translate_items(tree);
                let type_info = analyze_import(import).unwrap_or_default();
                self.finish_ast(ast::Tree { span_info, type_info }, builder)
            }
            tree::Variant::CaseOf(_) => {
                // TODO: Analyzed-representation to support alias analysis.
                let span_info = self.translate_items(tree);
                let type_info = ast::TreeType::Expression;
                self.finish_ast(ast::Tree { span_info, type_info }, builder)
            }
            _ => {
                let span_info = self.translate_items(tree);
                let type_info = ast::TreeType::Expression;
                self.finish_ast(ast::Tree { span_info, type_info }, builder)
            }
        };
        WithInitialSpace { space, body }
    }

    fn translate_lines(&mut self, tree: &Tree, out: &mut impl Extend<WithInitialSpace<Ast>>) {
        match &*tree.variant {
            tree::Variant::AnnotatedBuiltin(tree::AnnotatedBuiltin {
                token,
                annotation,
                newlines,
                expression,
            }) if !newlines.is_empty() => todo!(),
            tree::Variant::Annotated(_) => todo!(),
            tree::Variant::Documented(tree::Documented { documentation, expression }) => {
                let space = self.visit_space(&tree.span);
                let body = self.translate_doc(documentation);
                out.extend_one(WithInitialSpace { space, body });
                if let Some(expression) = expression {
                    self.translate_lines(expression, out);
                }
            }
            _ => out.extend_one(self.translate(tree)),
        }
    }

    fn translate_doc(&mut self, documentation: &tree::DocComment) -> Ast {
        // TODO: advance `offset`
        let token = ast::RawSpanTree::Token(documentation.code());
        let rendered = documentation.content().into();
        let type_info = ast::TreeType::Documentation { rendered };
        let span_info = vec![token];
        Ast::from(ast::Tree { span_info, type_info })
    }

    fn translate_function(&mut self, function: &tree::Function) -> ast::Shape<Ast> {
        let tree::Function { name, args, equals, body } = function;
        let name = self.translate(name);
        let mut lhs_terms = vec![name];
        lhs_terms.extend(args.into_iter().map(|a| self.translate_argument_definition(a)));
        let larg = lhs_terms
            .into_iter()
            .reduce(|func, arg| {
                let (off, arg) = arg.split();
                func.map(|func| Ast::from(ast::Prefix { func, off, arg }))
            })
            .unwrap()
            .expect_unspaced();
        let (loff, name) = self.visit_token(equals).split();
        let opr = Ast::from(ast::Opr { name });
        match body {
            Some(body) => {
                let (roff, rarg) = self.translate(body).split();
                ast::Shape::from(ast::Infix { larg, loff, opr, roff, rarg })
            }
            None => ast::Shape::from(ast::SectionLeft { arg: larg, off: loff, opr }),
        }
    }

    fn translate_foreign_function(&mut self, func: &tree::ForeignFunction) -> ast::Shape<Ast> {
        let tree::ForeignFunction { foreign, language, name, args, equals, body } = func;
        let mut lhs_terms: Vec<_> = [foreign, language, name]
            .into_iter()
            .map(|ident| {
                let (space, name) = self.visit_token(ident).split();
                let body = Ast::from(ast::Var { name });
                WithInitialSpace { space, body }
            })
            .collect();
        lhs_terms.extend(args.into_iter().map(|a| self.translate_argument_definition(a)));
        let larg = lhs_terms
            .into_iter()
            .reduce(|func, arg| {
                let (off, arg) = arg.split();
                func.map(|func| Ast::from(ast::Prefix { func, off, arg }))
            })
            .unwrap()
            .expect_unspaced();
        let (loff, name) = self.visit_token(equals).split();
        let opr = Ast::from(ast::Opr { name });
        let (roff, rarg) = self.translate(body).split();
        ast::Shape::from(ast::Infix { larg, loff, opr, roff, rarg })
    }

    fn opr_app(&mut self, lhs: &Tree, opr: &syntax::token::Operator, rhs: &Tree) -> Ast {
        let builder = self.start_ast();
        let larg = self.translate(lhs).expect_unspaced();
        let (loff, name) = self.visit_token(opr).split();
        let opr = Ast::from(ast::Opr { name });
        let (roff, rarg) = self.translate(rhs).split();
        let opr_app = ast::Infix { larg, loff, opr, roff, rarg };
        self.finish_ast(opr_app, builder)
    }

    fn translate_opr(&mut self, opr: &tree::OperatorOrError) -> WithInitialSpace<Ast> {
        match opr {
            Ok(name) => self.visit_token(name).map(|name| Ast::from(ast::Opr { name })),
            Err(names) => todo!(),
        }
    }

    fn translate_opr_app(
        &mut self,
        lhs: Option<&Tree>,
        opr: &tree::OperatorOrError,
        rhs: Option<&Tree>,
    ) -> ast::Shape<Ast> {
        let larg = lhs.map(|a| self.translate(a)).map(|e| e.expect_unspaced());
        let (loff, opr) = self.translate_opr(opr).split();
        let rarg = rhs.map(|a| self.translate(a)).map(|e| e.split());
        match (larg, rarg) {
            (Some(larg), Some((roff, rarg))) =>
                ast::Shape::from(ast::Infix { larg, loff, opr, roff, rarg }),
            (Some(arg), None) => ast::Shape::from(ast::SectionLeft { arg, off: loff, opr }),
            (None, Some((off, arg))) => ast::Shape::from(ast::SectionRight { opr, off, arg }),
            (None, None) => ast::Shape::from(ast::SectionSides { opr }),
        }
    }

    fn translate_module(&mut self, block: &tree::BodyBlock) -> Ast {
        let (lines, _) = self.translate_block_raw(&block.statements);
        Ast::from(ast::Module { lines })
    }

    fn translate_block<'a, 's: 'a>(
        &mut self,
        tree_lines: impl IntoIterator<Item = &'a tree::block::Line<'s>>,
    ) -> Option<ast::Block<Ast>> {
        let (ast_lines, indent) = self.translate_block_raw(tree_lines);
        let indent = indent?;
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
        Some(ast::Block { indent, empty_lines, first_line, lines })
    }

    fn translate_block_raw<'a, 's: 'a>(
        &mut self,
        tree_lines: impl IntoIterator<Item = &'a tree::block::Line<'s>>,
    ) -> (Vec<ast::BlockLine<Option<Ast>>>, Option<usize>) {
        let tree_lines = tree_lines.into_iter();
        let mut ast_lines: Vec<ast::BlockLine<Option<Ast>>> =
            Vec::with_capacity(tree_lines.size_hint().0);
        let mut statement_lines = vec![];
        let mut initial_indent = None;
        for tree::block::Line { newline, expression } in tree_lines {
            // Mapping from [`Tree`]'s leading offsets to [`Ast`]'s trailing offsets:
            // Initially, we create each line with no trailing offset.
            let off = 0;
            // If we encounter a leading offset, we represent it by modifying the trailing offset of
            // the previous line.
            let trailing_space = self.visit_token(&newline).space;
            if trailing_space != 0 {
                if let Some(last) = ast_lines.last_mut() {
                    last.off = trailing_space;
                } else {
                    // TODO: This should be leading whitespace for the whole block.
                    todo!()
                }
            }
            match &expression {
                Some(statement) => {
                    self.translate_lines(statement, &mut statement_lines);
                    if initial_indent.is_none() && let Some(first) = statement_lines.first() {
                        initial_indent = Some(first.space);
                    }
                    let new_lines = statement_lines
                        .drain(..)
                        .map(|elem| ast::BlockLine { elem: Some(elem.without_space()), off });
                    ast_lines.extend(new_lines);
                }
                None => ast_lines.push(ast::BlockLine { elem: None, off }),
            }
        }
        (ast_lines, initial_indent)
    }

    fn translate_operator_block<'a, 's: 'a>(
        &mut self,
        operator_lines: impl IntoIterator<Item = &'a tree::block::OperatorLine<'s>>,
    ) -> Ast {
        // TODO: support doc comments
        let mut indent = 0;
        let mut empty_lines = vec![];
        let mut first_line = None;
        let mut lines = vec![];
        for line in operator_lines {
            let elem = line.expression.as_ref().map(|expression| {
                let opr = self.translate_opr(&expression.operator).expect_unspaced();
                let (off, arg) = self.translate(&expression.expression).split();
                Ast::from(ast::SectionRight { opr, off, arg })
            });
            let off = 0; // TODO
            if first_line.is_none() {
                // TODO: handle multiple-operator error; visit_token
                if let Some(elem) = elem {
                    indent = line
                        .expression
                        .as_ref()
                        .unwrap()
                        .operator
                        .first_operator()
                        .left_offset
                        .visible
                        .width_in_spaces;
                    first_line = Some(ast::BlockLine { elem, off });
                } else {
                    empty_lines.push(off);
                }
            } else {
                lines.push(ast::BlockLine { elem, off });
            }
        }
        let first_line = first_line.unwrap();
        Ast::from(ast::Block { indent, empty_lines, first_line, lines })
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
        let suspension = suspension.as_ref().map(|token| self.visit_token(token));
        let mut term = self.translate(pattern);
        if let Some(opr) = suspension {
            let (space, name) = opr.split();
            let opr = Ast::from(ast::Opr { name });
            let (off, arg) = term.split();
            let body = Ast::from(ast::SectionRight { opr, off, arg });
            term = WithInitialSpace { space, body };
        }
        // TODO: type_
        let close2 = close2.as_ref().map(|token| self.visit_token(token));
        if let Some(open) = open2 && let Some(close) = close2 {
            term = open.map(|open| group(open, term, close));
        }
        if let Some(tree::ArgumentDefault { equals, expression }) = default {
            let (space, larg) = term.split();
            let (loff, name) = self.visit_token(equals).split();
            let opr = Ast::from(ast::Opr { name });
            let (roff, rarg) = self.translate(&expression).split();
            let body = Ast::from(ast::Infix { larg, loff, opr, roff, rarg });
            term = WithInitialSpace { space, body };
        }
        let close = close.as_ref().map(|token| self.visit_token(token));
        if let Some(open) = open && let Some(close) = close {
            term = open.map(|open| group(open, term, close));
        }
        term
    }

    /// Analyze a [`Tree`] and produce a representation used by the graph editor.
    fn translate_items(&mut self, tree: &syntax::tree::Tree<'_>) -> Vec<ast::RawSpanTree> {
        let mut span_info = vec![];
        tree.visit_items(|item| match item {
            syntax::item::Ref::Token(token) => {
                let (space, token) = self.visit_token_ref(token).split();
                span_info.extend(ast::RawSpanTree::space(space));
                span_info.push(ast::RawSpanTree::Token(token));
            }
            syntax::item::Ref::Tree(tree) => {
                let (space, ast) = self.translate(tree).split();
                span_info.extend(ast::RawSpanTree::space(space));
                span_info.push(ast::RawSpanTree::Child(ast));
            }
        });
        span_info
    }
}

fn group(open: String, body: WithInitialSpace<Ast>, close: WithInitialSpace<String>) -> Ast {
    let (body_space, body) = body.split();
    let (close_space, close) = close.split();
    let min_elements = 3; // There are always at least 3 elements: open, close, and body
    let mut span_info = Vec::with_capacity(min_elements);
    span_info.push(ast::RawSpanTree::Token(open));
    span_info.extend(ast::RawSpanTree::space(body_space));
    span_info.push(ast::RawSpanTree::Child(body));
    span_info.extend(ast::RawSpanTree::space(close_space));
    span_info.push(ast::RawSpanTree::Token(close));
    let type_info = ast::TreeType::Expression;
    Ast::from(ast::Tree { span_info, type_info })
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
struct WithInitialSpace<T> {
    space: usize,
    body:  T,
}

impl<T: core::fmt::Debug> WithInitialSpace<T> {
    /// Return the value, ignoring any initial space.
    fn without_space(self) -> T {
        self.body
    }

    /// Return the value if there is no initial space.
    fn into_unspaced(self) -> Option<T> {
        (self.space == 0).then_some(self.body)
    }

    /// If any initial space is present, emit a warning; forget the space and return the value.
    fn expect_unspaced(self) -> T {
        // TODO: This should be a warning.
        debug_assert_eq!(self.space, 0, "Expected no space before {:?}", &self.body);
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
