use enso_prelude::*;
use enso_profiler::prelude::*;

use ast::Ast;
use enso_parser::syntax;
use enso_parser::syntax::tree;
use enso_parser::syntax::Tree;
use enso_profiler as profiler;
use std::collections::BTreeMap;



/// Enable extra log messages and assertions.
const DEBUG: bool = false;



// =======================
// === Translation API ===
// =======================

/// Translates an [`AST`] from the [`Tree`] representation produced by [`enso_parser`] to the
/// [`Ast`] representation used by the GUI (see [`crate`] documentation for high-level overview).
/// The returned tree will contain IDs from the given map.
#[profile(Detail)]
pub fn tree_to_ast(mut tree: &Tree, ids: BTreeMap<(usize, usize), uuid::Uuid>) -> Ast {
    use ast::HasRepr;
    let mut context = Translate { ids, ..Default::default() };
    let ast = loop {
        match &*tree.variant {
            tree::Variant::BodyBlock(block) => break context.translate_module(block),
            tree::Variant::Invalid(tree::Invalid { ast, error }) => {
                warn!("Parser reports invalid module: {}", error.message);
                tree = ast
            }
            _ => unreachable!("enso_parser always returns a tree with a BodyBlock as root."),
        }
    };
    if DEBUG {
        debug_assert_eq!(ast.repr(), tree.code(), "Ast should represent same code as Tree.");
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
    /// The offset, in bytes, from the beginning of the input source code. This must be tracked
    /// during [`Tree`] traversal because [`Tree`] doesn't have absolute source references, only
    /// token lengths.
    offset:      usize,
    /// IDs to associate with byte ranges of the source code.
    ids:         BTreeMap<(usize, usize), uuid::Uuid>,
    /// The [`AstBuilder`] interface supports associating IDs with byte ranges; however, it's
    /// important to ensure that the byte ranges don't include any leading whitespace. This would
    /// be a difficult invariant to maintain through careful usage of [`AstBuilder`]; instead,
    /// we record where whitespace occurs in [`space_after`], and use that to adjust the byte
    /// ranges.
    space_after: BTreeMap<usize, usize>,

    // === Diagnostic information used when [`DEBUG`] is enabled ===
    /// IDs that were in [`ids`], and now have been successfully attached to [`Ast`] nodes.
    ids_assigned: Vec<((usize, usize), uuid::Uuid)>,
    /// Byte ranges of [`Ast`] nodes that we didn't find any IDs for.
    ids_missed:   Vec<(usize, usize)>,
}

impl Translate {
    /// This must be called at the beginning of each [`Tree`], as they are processed in depth-first
    /// order. It updates the internal counter to include the leading whitespace bytes, and returns
    /// the visible width (indent) of the leading space.
    fn visit_space(&mut self, span: &enso_parser::source::Span) -> usize {
        let space = span.left_offset.code.repr.len();
        self.space_after.insert(self.offset, space);
        self.offset += space;
        span.left_offset.visible.width_in_spaces
    }

    /// This must be called at the beginning of each [`Token`], as they are processed in depth-first
    /// order. It updates the internal counter for the token's bytes, and returns its contents.
    fn visit_token<T: Copy>(&mut self, token: &syntax::Token<T>) -> WithInitialSpace<String> {
        self.visit_token_ref(syntax::token::Ref::<T>::from(token))
    }

    /// This must be called at the beginning of each [`Token`], as they are processed in depth-first
    /// order. It updates the internal counter for the token's bytes, and returns its contents.
    fn visit_token_ref<T>(&mut self, token: syntax::token::Ref<T>) -> WithInitialSpace<String> {
        let space = token.left_offset.visible.width_in_spaces;
        let body = token.code.to_string();
        self.space_after.insert(self.offset, space);
        self.offset += token.left_offset.code.repr.len();
        self.offset += token.code.repr.len();
        WithInitialSpace { space, body }
    }
}

impl Translate {
    /// Translate a [`Tree`].
    /// The returned [`Ast`] can be [`None`] if an empty block is encountered.
    fn translate(&mut self, tree: &Tree) -> WithInitialSpace<Option<Ast>> {
        let space = self.visit_space(&tree.span);
        let body = self.translate_expression_body(tree);
        WithInitialSpace { space, body }
    }

    /// Translate a [`Tree`], except for the leading space.
    /// This can return [`None`] if an empty block is encountered.
    fn translate_expression_body(&mut self, tree: &Tree) -> Option<Ast> {
        let builder = self.start_ast();
        Some(match &*tree.variant {
            tree::Variant::BodyBlock(block) => {
                let block = self.translate_block(&block.statements)?.expect_unspaced();
                self.finish_ast(block, builder)
            }
            tree::Variant::Ident(tree::Ident { token }) => {
                let name = self.visit_token(token).expect_unspaced();
                match token.is_type {
                    true => self.finish_ast(ast::Cons { name }, builder),
                    false => self.finish_ast(ast::Var { name }, builder),
                }
            }
            tree::Variant::Number(tree::Number { base, integer, fractional_digits }) => {
                let base = base.as_ref().map(|base| self.visit_token(base).expect_unspaced());
                let mut int = integer
                    .as_ref()
                    .map(|integer| self.visit_token(integer).expect_unspaced())
                    .unwrap_or_default();
                if let Some(tree::FractionalDigits { dot, digits }) = fractional_digits {
                    let dot = self.visit_token(dot).expect_unspaced();
                    let digits = self.visit_token(digits).expect_unspaced();
                    int = format!("{int}{dot}{digits}");
                }
                self.finish_ast(ast::Number { base, int }, builder)
            }
            tree::Variant::App(tree::App { func, arg }) => {
                let func = self.translate(func);
                let arg = self.translate(arg);
                let app = maybe_prefix(func, arg).expect_unspaced()?;
                self.finish_ast(app, builder)
            }
            tree::Variant::OprApp(tree::OprApp { lhs: Some(_), opr: Ok(opr), rhs: Some(_) })
                if opr.properties.is_arrow() =>
            {
                let ast = ast::Tree::lambda(self.translate_items(tree));
                self.finish_ast(ast, builder)
            }
            tree::Variant::OprApp(tree::OprApp { lhs, opr, rhs }) => {
                let larg = lhs.as_ref().map(|a| self.translate(a)).unwrap_or_default();
                let opr = self.translate_operators(opr);
                let rarg = rhs.as_ref().map(|a| self.translate(a)).unwrap_or_default();
                let opr_app = infix(larg, opr, rarg).expect_unspaced();
                self.finish_ast(opr_app, builder)
            }
            tree::Variant::OprSectionBoundary(tree::OprSectionBoundary { ast, .. }) =>
                self.translate(ast).expect_unspaced()?,
            tree::Variant::Function(func) => {
                let func = self.translate_function(func);
                self.finish_ast(func, builder)
            }
            tree::Variant::ForeignFunction(func) => {
                let func = self.translate_foreign_function(func);
                self.finish_ast(func, builder)
            }
            tree::Variant::UnaryOprApp(tree::UnaryOprApp { opr, rhs }) => {
                let opr = self.translate_operator(opr);
                if let Some(arg) = rhs {
                    let non_block_operand = "Unary operator cannot be applied to an (empty) block.";
                    let arg = self.translate(arg).expect(non_block_operand);
                    let value = match arg.body.shape() {
                        ast::Shape::Number(ast::Number { base, int }) =>
                            (ast::Number { base: base.clone(), int: format!("-{int}") }).into(),
                        _ => prefix(opr, arg).expect_unspaced(),
                    };
                    self.finish_ast(value, builder)
                } else {
                    let opr = opr.expect_unspaced();
                    self.finish_ast(ast::SectionSides { opr }, builder)
                }
            }
            tree::Variant::Assignment(tree::Assignment { pattern, equals, expr }) =>
                self.opr_app(pattern, equals, expr).expect_unspaced(),
            tree::Variant::OperatorBlockApplication(app) => {
                let tree::OperatorBlockApplication { lhs, expressions, excess } = app;
                let func = lhs.as_ref().map(|lhs| self.translate(lhs)).unwrap_or_default();
                let block = self.translate_operator_block(expressions, excess);
                let app = maybe_prefix(func, block).expect_unspaced()?;
                self.finish_ast(app, builder)
            }
            tree::Variant::TemplateFunction(tree::TemplateFunction { ast, .. }) =>
                self.translate(ast).expect_unspaced()?,
            tree::Variant::Wildcard(tree::Wildcard { token, .. }) => {
                self.visit_token(token).expect_unspaced();
                self.finish_ast(ast::Blank {}, builder)
            }
            tree::Variant::ArgumentBlockApplication(app) => {
                let tree::ArgumentBlockApplication { lhs, arguments } = app;
                let func = lhs.as_ref().map(|lhs| self.translate(lhs)).unwrap_or_default();
                let arg = self
                    .translate_block(arguments)
                    .map(|arg| arg.map(|arg| Some(Ast::from(arg))))
                    .unwrap_or_default();
                let app = maybe_prefix(func, arg).expect_unspaced()?;
                self.finish_ast(app, builder)
            }
            tree::Variant::DefaultApp(tree::DefaultApp { func, default }) => {
                let func = self.translate(func);
                let arg_builder = self.start_ast();
                let default = self.visit_token(default);
                let arg = default.map(|name| self.finish_ast(ast::Var { name }, arg_builder));
                let app = maybe_prefix(func, arg).expect_unspaced()?;
                self.finish_ast(app, builder)
            }
            tree::Variant::NamedApp(tree::NamedApp { func, open, name, equals, arg, close }) => {
                let func = self.translate(func);
                let open = open.as_ref().map(|token| self.visit_token(token));
                let name = self.visit_token(name);
                let larg = name.map(|name| Ast::from(ast::Var { name }));
                let opr = self.translate_operator(equals);
                let non_block_operand = "Named-application operand cannot be an (empty) block.";
                let rarg = self.translate(arg).expect(non_block_operand);
                let mut arg = infix(larg, opr, rarg).map(Ast::from);
                let close = close.as_ref().map(|token| self.visit_token(token));
                if let Some(open) = open && let Some(close) = close {
                    arg = open.map(|open| group(open, arg, close));
                }
                let app = maybe_prefix(func, arg).expect_unspaced()?;
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
                let arg = expression.as_ref().map(|e| self.translate(e)).unwrap_or_default();
                if !newlines.is_empty() {
                    error!("Multiline expression must be handled in translate_lines.");
                }
                let app = maybe_prefix(func, arg).expect_unspaced()?;
                self.finish_ast(app, builder)
            }
            tree::Variant::Documented(tree::Documented { documentation: _, expression }) => {
                warn!("Multiline expression should have been handled in translate_lines.");
                self.translate(expression.as_ref()?).without_space()?
            }
            tree::Variant::Import(import) => {
                let span_info = self.translate_items(tree);
                let type_info = analyze_import(import).unwrap_or_default();
                let ast = ast::Tree::expression(span_info).with_type_info(type_info);
                self.finish_ast(ast, builder)
            }
            tree::Variant::TextLiteral(_) => {
                self.translate_items(tree);
                let ast = ast::Tree::text(tree.trimmed_code());
                self.finish_ast(ast, builder)
            }
            tree::Variant::Group(_) => {
                let span_info = self.translate_items(tree);
                let ast = ast::Tree::group(span_info);
                self.finish_ast(ast, builder)
            }
            _ => {
                let ast = ast::Tree::expression(self.translate_items(tree));
                self.finish_ast(ast, builder)
            }
        })
    }

    /// Translate [`Tree`]s to [`Ast`]s in a multi-line context (i.e. when building a block or
    /// module).
    fn translate_lines(&mut self, tree: &Tree, out: &mut Vec<WithInitialSpace<Option<Ast>>>) {
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
                let annotation = Ast::from(ast::Annotation { name: format!("{at}{annotation}") });
                out.extend_one(WithInitialSpace { space, body: Some(annotation) });
                self.translate_linebreaks(out, newlines);
                if let Some(expression) = expression {
                    self.translate_lines(expression, out);
                }
            }
            tree::Variant::Annotated(tree::Annotated {
                token,
                annotation,
                argument,
                newlines,
                expression,
            }) => {
                let space = self.visit_space(&tree.span);
                let at = self.visit_token(token).expect_unspaced();
                let annotation = self.visit_token(annotation).expect_unspaced();
                let body = Ast::from(ast::Annotation { name: format!("{at}{annotation}") });
                let mut annotation = WithInitialSpace { space, body };
                let argument =
                    argument.as_ref().and_then(|arg| ignore_space_if_empty(self.translate(arg)));
                if let Some(argument) = argument {
                    annotation = prefix(annotation, argument).map(Ast::from);
                }
                out.extend_one(annotation.map(Some));
                self.translate_linebreaks(out, newlines);
                if let Some(expression) = expression {
                    self.translate_lines(expression, out);
                }
            }
            tree::Variant::Documented(tree::Documented { documentation, expression }) => {
                let space = self.visit_space(&tree.span);
                self.translate_doc(space, documentation, out);
                if let Some(expression) = expression {
                    self.translate_lines(expression, out);
                }
            }
            _ => out.extend_one(self.translate(tree)),
        }
    }

    /// Translate a sequence of line breaks and trailing comments to [`Ast`] representation.
    fn translate_linebreaks(
        &mut self,
        out: &mut Vec<WithInitialSpace<Option<Ast>>>,
        newlines: &[syntax::token::Newline],
    ) {
        // In the [`Ast`] representation, each block line has one implicit newline.
        let out_newlines = newlines.len().saturating_sub(1);
        out.reserve(out_newlines);
        let mut prev = None;
        for token in newlines {
            let next = self.visit_token(token).split();
            if let Some((space, token)) = prev.replace(next) {
                if let Some(text) = into_comment(&token) {
                    append_comment(out, space, text);
                } else {
                    out.push(WithInitialSpace { space, body: None });
                }
            }
        }
    }

    /// Translate a documentation comment to [`Ast`] representation.
    fn translate_doc(
        &mut self,
        space: usize,
        documentation: &tree::DocComment,
        out: &mut Vec<WithInitialSpace<Option<Ast>>>,
    ) {
        let open = self.visit_token(&documentation.open);
        let mut span_info = SpanSeedBuilder::new();
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
        let rendered = documentation.content().into();
        let type_info = ast::TreeType::Documentation { rendered };
        let span_info = span_info.build().expect_unspaced();
        let body = Some(Ast::from(ast::Tree::expression(span_info).with_type_info(type_info)));
        out.extend_one(WithInitialSpace { space, body });
        self.translate_linebreaks(out, &documentation.newlines);
    }

    /// Lower a function definition to [`Ast`] representation.
    fn translate_function(&mut self, function: &tree::Function) -> ast::Shape<Ast> {
        let tree::Function { name, args, equals, body } = function;
        let non_empty_name = "A function name cannot be an (empty) block.";
        let name = self.translate(name).expect(non_empty_name);
        let mut lhs_terms = vec![name];
        lhs_terms.extend(args.iter().map(|a| self.translate_argument_definition(a)));
        let larg = lhs_terms
            .into_iter()
            .reduce(|func, arg| prefix(func, arg).map(Ast::from))
            .expect("`lhs_terms` has at least one value, because it was initialized with a value.");
        let opr = self.translate_operator(equals);
        let body = body.as_ref().map(|body| self.translate(body)).unwrap_or_default();
        infix(larg, opr, body).expect_unspaced()
    }

    /// Lower a foreign-function definition to [`Ast`] representation.
    fn translate_foreign_function(&mut self, func: &tree::ForeignFunction) -> ast::Shape<Ast> {
        let tree::ForeignFunction { foreign, language, name, args, equals, body } = func;
        let mut lhs_terms: Vec<_> = [foreign, language, name]
            .into_iter()
            .map(|ident| self.visit_token(ident).map(|name| Ast::from(ast::Var { name })))
            .collect();
        lhs_terms.extend(args.iter().map(|a| self.translate_argument_definition(a)));
        let lhs = lhs_terms
            .into_iter()
            .reduce(|func, arg| prefix(func, arg).map(Ast::from))
            .expect("`lhs_terms` has at least one value, because it was initialized with values.");
        let equals = self.translate_operator(equals);
        let body = self.translate(body);
        infix(lhs, equals, body).expect_unspaced()
    }

    /// Construct an operator application from [`Tree`] operands and a specific operator.
    fn opr_app(
        &mut self,
        lhs: &Tree,
        opr: &syntax::token::Operator,
        rhs: &Tree,
    ) -> WithInitialSpace<Ast> {
        let builder = self.start_ast();
        let lhs = self.translate(lhs);
        let opr = self.translate_operator(opr);
        let rhs = self.translate(rhs);
        infix(lhs, opr, rhs).map(|opr_app| self.finish_ast(opr_app, builder))
    }

    /// Translate an operator or multiple-operator error into the [`Ast`] representation.
    fn translate_operators(&mut self, opr: &tree::OperatorOrError) -> WithInitialSpace<Ast> {
        match opr {
            Ok(name) if name.properties.is_modifier() => {
                let opr_builder = self.start_ast();
                self.visit_token(name).map(|name| {
                    let name = name.strip_suffix('=').map(|s| s.to_owned()).unwrap_or(name);
                    self.finish_ast(ast::Mod { name }, opr_builder)
                })
            }
            Ok(name) => self.translate_operator(name),
            Err(names) => {
                let opr_builder = self.start_ast();
                let mut span_info = SpanSeedBuilder::new();
                for token in &names.operators {
                    span_info.token(self.visit_token(token));
                }
                let opr = span_info
                    .build()
                    .map(|span_info| ast::Shape::from(ast::Tree::expression(span_info)));
                opr.map(|opr| self.finish_ast(opr, opr_builder))
            }
        }
    }

    /// Translate an operator token into its [`Ast`] representation.
    fn translate_operator(&mut self, token: &syntax::token::Operator) -> WithInitialSpace<Ast> {
        let opr_builder = self.start_ast();
        let right_assoc = token.properties.associativity() == syntax::token::Associativity::Right;
        self.visit_token(token)
            .map(|name| self.finish_ast(ast::Opr { name, right_assoc }, opr_builder))
    }

    /// Translate a [`tree::BodyBlock`] into an [`Ast`] module.
    fn translate_module(&mut self, block: &tree::BodyBlock) -> Ast {
        // FIXME [mwu]
        //  The following code was changed as a workaround for the issue
        //  https://github.com/enso-org/enso/issues/6718.
        //  This makes the GUI follow the incorrect Engine behavior of assigning ID to the root
        //  `Module` AST node. It should have no ID, as it could collide with other node in case of
        //  trivial module code (like `foo`). See also: https://github.com/enso-org/enso/issues/2262
        //  In this case the workaround is safe, as GUI will never generate such a trivial module,
        //  it will contain at least the `main` function definition.
        let module_builder = self.start_ast();
        let (lines, _) =
            self.translate_block_lines(&block.statements).unwrap_or_default().expect_unspaced();
        self.finish_ast(ast::Module { lines }, module_builder)
    }

    /// Translate the lines of [`Tree`] block into the [`Ast`] block representation.
    fn translate_block<'a, 's: 'a>(
        &mut self,
        tree_lines: impl IntoIterator<Item = &'a tree::block::Line<'s>>,
    ) -> Option<WithInitialSpace<ast::Block<Ast>>> {
        let (space, (ast_lines, indent)) = self.translate_block_lines(tree_lines)?.split();
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

    /// Translate the lines of [`Tree`] block into [`Ast`] block lines.
    fn translate_block_lines<'a, 's: 'a>(
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
            let (trailing_space, newline) = self.visit_token(newline).split();
            if let Some(prev_line_comment) = into_comment(&newline) {
                append_comment_ast(&mut ast_lines, trailing_space, prev_line_comment);
                continue;
            }
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

    /// Lower an operator block into the [`Ast`] block representation.
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
            let (trailing_space, newline) = self.visit_token(newline).split();
            if let Some(prev_line_comment) = into_comment(&newline) {
                append_comment_ast(&mut ast_lines, trailing_space, prev_line_comment);
                continue;
            }
            *ast_lines.last_mut().map(|line| &mut line.off).unwrap_or(&mut space) = trailing_space;
            let elem = expression.as_ref().map(|expression| {
                let opr = self.translate_operators(&expression.operator);
                let non_block_operand = "Expression in operand-line cannot be an (empty) block.";
                let arg = self.translate(&expression.expression).expect(non_block_operand);
                let (space, elem) = section_right(opr, arg).split();
                if indent.is_none() {
                    indent = Some(space);
                }
                Ast::from(elem)
            });
            ast_lines.push(ast::BlockLine { elem, off });
        }
        let non_empty_block = "An operator block must have at least one operator line.";
        let indent = indent.expect(non_empty_block);
        let mut statement_lines = vec![];
        for tree::block::Line { newline, expression } in excess {
            let (trailing_space, newline) = self.visit_token(newline).split();
            if let Some(prev_line_comment) = into_comment(&newline) {
                append_comment_ast(&mut ast_lines, trailing_space, prev_line_comment);
                continue;
            }
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
        let first_line = first_line.expect(non_empty_block);
        let body = Ast::from(ast::Block { indent, empty_lines, first_line, lines });
        WithInitialSpace { space, body }
    }

    /// Lower an argument definition into the [`Ast`] representation.
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
        let suspension = suspension.as_ref().map(|token| self.translate_operator(token));
        let non_block_operand = "Pattern in argument definition cannot be an (empty) block.";
        let mut term = self.translate(pattern).expect(non_block_operand);
        if let Some(opr) = suspension {
            term = section_right(opr, term).map(Ast::from);
        }
        if let Some(tree::ArgumentType { operator, type_ }) = type_ {
            let opr = self.translate_operator(operator);
            let rarg = self.translate(type_);
            term = infix(term, opr, rarg).map(Ast::from);
        }
        let close2 = close2.as_ref().map(|token| self.visit_token(token));
        if let Some(open) = open2 && let Some(close) = close2 {
            term = open.map(|open| group(open, term, close));
        }
        if let Some(tree::ArgumentDefault { equals, expression }) = default {
            let opr = self.translate_operator(equals);
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
    fn translate_items(&mut self, tree: &syntax::tree::Tree<'_>) -> Vec<ast::SpanSeed<Ast>> {
        let mut span_info = SpanSeedBuilder::new();
        tree.visit_items(|item| match item {
            syntax::item::Ref::Token(token) => span_info.token(self.visit_token_ref(token)),
            syntax::item::Ref::Tree(tree) => {
                if let Some(ast) = ignore_space_if_empty(self.translate(tree)) {
                    span_info.child(ast);
                }
            }
        });
        span_info.build().expect_unspaced()
    }
}


// === Span-tracking ===

/// Tracks what input bytes are visited during the construction of a particular [`Ast`], and uses
/// that information to assign an ID from the ID map.
struct AstBuilder {
    start: usize,
}

impl Translate {
    /// Marks the beginning of the input byte range that will be included in a particular [`Ast`]
    /// node.
    fn start_ast(&mut self) -> AstBuilder {
        AstBuilder { start: self.offset }
    }

    /// Marks the end of the input byte range that will be included in a particular [`Ast`] node,
    /// and constructs the node from an [`ast::Shape`], using the calculated byte range to assign
    /// an ID.
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


// === Semantic Analysis ===

/// Analyze an import statement to identify the module referenced and the names imported.
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

/// Distinguish a plain newline from a trailing comment.
fn into_comment(mut newline: &str) -> Option<String> {
    if let Some(text) = newline.strip_suffix('\n') {
        newline = text;
    }
    if let Some(text) = newline.strip_suffix('\r') {
        newline = text;
    }
    (!newline.is_empty()).then(|| newline.to_string())
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
    /// If any initial space is present, emit a warning; forget the space and return the value.
    fn expect_unspaced(self) -> T {
        if DEBUG {
            debug_assert_eq!(self.space, 0, "Expected no space before term: {:?}", &self.body);
        } else if self.space != 0 {
            warn!("Expected no space before term: {:?}", &self.body);
        }
        self.body
    }
}

impl<T> WithInitialSpace<T> {
    /// Return the value, ignoring any initial space.
    fn without_space(self) -> T {
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

impl<T> WithInitialSpace<Option<T>> {
    /// Convenience function that applies [`Option::expect`] to the inner value.
    fn expect(self, message: &str) -> WithInitialSpace<T> {
        let WithInitialSpace { space, body } = self;
        WithInitialSpace { space, body: body.expect(message) }
    }
}

/// Convenience function that transposes an optional value that always has initial space count, to
/// an optional value that, if present, has an initial space count. Note that this is a lossy
/// operation.
fn ignore_space_if_empty<T>(spaced: WithInitialSpace<Option<T>>) -> Option<WithInitialSpace<T>> {
    spaced.body.map(|body| WithInitialSpace { space: spaced.space, body })
}


// === Shape-building helpers ===

/// Construct an [`ast::Prefix`], representing the initial spaces of the inputs appropriately.
fn prefix(
    func: WithInitialSpace<Ast>,
    arg: WithInitialSpace<Ast>,
) -> WithInitialSpace<ast::Shape<Ast>> {
    func.map(|func| {
        let (off, arg) = arg.split();
        (ast::Prefix { func, off, arg }).into()
    })
}

/// Construct an [`ast::Prefix`] if both operands are present; otherwise, returns a single operand
/// if present.
fn maybe_prefix<T: Into<Option<Ast>>, U: Into<Option<Ast>>>(
    func: WithInitialSpace<T>,
    arg: WithInitialSpace<U>,
) -> WithInitialSpace<Option<ast::Shape<Ast>>> {
    func.map(|func| {
        let arg = ignore_space_if_empty(arg.map(|arg| arg.into()));
        match (func.into(), arg) {
            (Some(func), Some(arg)) => {
                let (off, arg) = arg.split();
                Some((ast::Prefix { func, off, arg }).into())
            }
            (Some(func), None) => Some(func.shape().clone()),
            (None, Some(arg)) => Some(arg.expect_unspaced().shape().clone()),
            (None, None) => None,
        }
    })
}

/// Constructs an operator section for an operator with only a right operand.
fn section_right(
    opr: WithInitialSpace<Ast>,
    arg: WithInitialSpace<Ast>,
) -> WithInitialSpace<ast::Shape<Ast>> {
    opr.map(|opr| {
        let (off, arg) = arg.split();
        (ast::SectionRight { opr, off, arg }).into()
    })
}

/// Constructs an infix-application node. If any of the inputs are [`Option`] types, then an
/// operator section will be produced if appropriate.
fn infix<T: Into<Option<Ast>>, U: Into<Option<Ast>>>(
    larg: WithInitialSpace<T>,
    opr: WithInitialSpace<Ast>,
    rarg: WithInitialSpace<U>,
) -> WithInitialSpace<ast::Shape<Ast>> {
    larg.map(|larg| {
        let (opr_off, opr) = opr.split();
        let rarg = ignore_space_if_empty(rarg.map(|arg| arg.into()));
        match (larg.into(), rarg) {
            (Some(larg), Some(rarg)) => {
                let (roff, rarg) = rarg.split();
                (ast::Infix { larg, loff: opr_off, opr, roff, rarg }).into()
            }
            (Some(arg), None) => (ast::SectionLeft { arg, off: opr_off, opr }).into(),
            (None, Some(arg)) => {
                let (off, arg) = arg.split();
                (ast::SectionRight { opr, off, arg }).into()
            }
            (None, None) => (ast::SectionSides { opr }).into(),
        }
    })
}

/// Wrap an input in a parenthesized-group node.
fn group(open: String, body: WithInitialSpace<Ast>, close: WithInitialSpace<String>) -> Ast {
    let (body_space, body) = body.split();
    let (close_space, close) = close.split();
    let min_elements = 3; // There are always at least 3 elements: open, close, and body
    let mut span_info = Vec::with_capacity(min_elements);
    span_info.push(ast::SpanSeed::Token(ast::SpanSeedToken { token: open }));
    span_info.extend(ast::SpanSeed::space(body_space));
    span_info.push(ast::SpanSeed::Child(ast::SpanSeedChild { node: body }));
    span_info.extend(ast::SpanSeed::space(close_space));
    span_info.push(ast::SpanSeed::Token(ast::SpanSeedToken { token: close }));
    Ast::from(ast::Tree::expression(span_info))
}

/// Append a trailing-comment to the last line of the given block, creating a line to hold it if the
/// block is empty.
fn append_comment(lines: &mut Vec<WithInitialSpace<Option<Ast>>>, space: usize, comment: String) {
    let prev = lines.pop();
    let space_after_expression = match prev.as_ref().and_then(|spaced| spaced.body.as_ref()) {
        Some(_) => space,
        None => 0,
    };
    let prev = prev.unwrap_or(WithInitialSpace { space, body: None });
    let line = prev.map(|prev| {
        Some(Ast::from(ast::Tree::expression_with_comment(prev, space_after_expression, comment)))
    });
    lines.push(line);
}

/// Append a trailing-comment to the last line of the given block, creating a line to hold it if the
/// block is empty.
fn append_comment_ast(lines: &mut Vec<ast::BlockLine<Option<Ast>>>, space: usize, comment: String) {
    let prev = lines.pop();
    let off = prev.as_ref().map(|line| line.off).unwrap_or_default();
    let prev = prev.and_then(|line| line.elem);
    // If there's no expression before the comment, the space to the left of the comment
    // is already represented as the indent.
    let trailing_space = match prev {
        Some(_) => space,
        None => 0,
    };
    let elem = ast::Tree::expression_with_comment(prev, trailing_space, comment);
    lines.push(ast::BlockLine { elem: Some(Ast::from(elem)), off });
}


// === SpanSeedBuilder ===

/// Constructs a sequence of [`ast::SpanSeed`] values.
#[derive(Debug, Default)]
pub struct SpanSeedBuilder {
    space: Option<usize>,
    spans: Vec<ast::SpanSeed<Ast>>,
}

impl SpanSeedBuilder {
    fn new() -> Self {
        Self::default()
    }

    /// Append a token.
    fn token(&mut self, value: WithInitialSpace<String>) {
        let (space, value) = value.split();
        if self.space.is_none() {
            self.space = Some(space);
        } else {
            self.spans.extend(ast::SpanSeed::space(space));
        }
        self.spans.push(ast::SpanSeed::Token(ast::SpanSeedToken { token: value }));
    }

    /// Append a node.
    fn child(&mut self, node: WithInitialSpace<Ast>) {
        let (space, node) = node.split();
        if self.space.is_none() {
            self.space = Some(space);
        } else {
            self.spans.extend(ast::SpanSeed::space(space));
        }
        self.spans.push(ast::SpanSeed::Child(ast::SpanSeedChild { node }));
    }

    /// Construct the sequence.
    fn build(self) -> WithInitialSpace<Vec<ast::SpanSeed<Ast>>> {
        let space = self.space.unwrap_or_default();
        let body = self.spans;
        WithInitialSpace { space, body }
    }
}
