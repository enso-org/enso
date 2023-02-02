use ast::Ast;
use enso_parser::syntax;
use enso_parser::syntax::tree;
use enso_parser::syntax::tree::NonEmptyOperatorSequence;
use enso_parser::syntax::Tree;



// =======================
// === Translation API ===
// =======================

pub fn to_legacy_ast_module(tree: &Tree) -> Result<Ast, ()> {
    let context = Translate::default();
    match &*tree.variant {
        tree::Variant::BodyBlock(block) => Ok(context.translate_module(&block).into()),
        _ => Err(()),
    }
}

pub fn to_legacy_ast(tree: &Tree) -> Ast {
    let context = Translate::default();
    context.translate(tree).expect_unspaced()
}


// === Implementation ===

#[derive(Debug, Default)]
struct Translate {}

impl Translate {
    fn translate(&self, tree: &Tree) -> WithInitialSpace<Ast> {
        let space = tree.span.left_offset.visible.width_in_spaces;
        let body = match &*tree.variant {
            tree::Variant::BodyBlock(block) =>
                self.translate_block(&block.statements).unwrap().into(),
            tree::Variant::Ident(tree::Ident { token }) if token.is_type =>
                Ast::from(ast::Cons { name: token.code.to_string() }),
            tree::Variant::Ident(ident) => self.translate_var(&ident.token),
            tree::Variant::Number(tree::Number { base, integer, .. }) => Ast::from(ast::Number {
                base: base.as_ref().map(|base| base.code.to_string()),
                int:  integer.as_ref().map(|integer| integer.code.to_string()).unwrap_or_default(),
            }),
            tree::Variant::App(tree::App { func, arg }) => {
                let func = self.translate(func).expect_unspaced();
                let (off, arg) = self.translate(arg).split();
                Ast::from(ast::Prefix { func, off, arg })
            }
            tree::Variant::OprApp(tree::OprApp { lhs, opr, rhs }) =>
                self.translate_opr_app(lhs.as_ref(), opr, rhs.as_ref()),
            tree::Variant::OprSectionBoundary(tree::OprSectionBoundary { ast, .. }) =>
                self.translate(ast).expect_unspaced(),
            tree::Variant::Function(func) => self.translate_function(func),
            tree::Variant::ForeignFunction(func) => self.translate_foreign_function(func),
            tree::Variant::UnaryOprApp(tree::UnaryOprApp { opr, rhs }) => {
                let opr = Ast::from(ast::Opr { name: opr.code.to_string() });
                if let Some(arg) = rhs {
                    let (off, arg) = self.translate(arg).split();
                    Ast::from(ast::SectionRight { opr, off, arg })
                } else {
                    Ast::from(ast::SectionSides { opr })
                }
            }
            tree::Variant::Assignment(tree::Assignment { pattern, equals, expr }) =>
                self.opr_app(pattern, equals, expr),
            tree::Variant::OperatorBlockApplication(app) => {
                let tree::OperatorBlockApplication { lhs, expressions, excess } = app;
                let func = self.translate(lhs.as_ref().unwrap()).expect_unspaced();
                let arg = self.translate_operator_block(expressions);
                Ast::from(ast::Prefix { func, off: 0, arg })
            }
            tree::Variant::TemplateFunction(tree::TemplateFunction { ast, .. }) =>
                self.translate(ast).expect_unspaced(),
            tree::Variant::Wildcard(_) => Ast::from(ast::Blank {}),
            tree::Variant::ArgumentBlockApplication(app) => {
                let tree::ArgumentBlockApplication { lhs, arguments } = app;
                let func = self.translate(lhs.as_ref().unwrap()).expect_unspaced();
                let arg = self.translate_block(arguments).unwrap().into();
                Ast::from(ast::Prefix { func, off: 0, arg })
            }
            tree::Variant::DefaultApp(tree::DefaultApp { func, default }) => {
                let func = self.translate(func).expect_unspaced();
                let off = default.left_offset.visible.width_in_spaces;
                let arg = Ast::from(ast::Var { name: default.code.to_string() });
                Ast::from(ast::Prefix { func, off, arg })
            }
            tree::Variant::NamedApp(tree::NamedApp { func, open, name, equals, arg, close }) => {
                let func = self.translate(func).expect_unspaced();
                let off = name.left_offset.visible.width_in_spaces; // TODO: open || name
                let larg = Ast::from(ast::Var { name: name.code.to_string() });
                let loff = equals.left_offset.visible.width_in_spaces;
                let opr = Ast::from(ast::Opr { name: equals.code.to_string() });
                let (roff, rarg) = self.translate(arg).split();
                let arg = Ast::from(ast::Infix { larg, loff, opr, roff, rarg });
                Ast::from(ast::Prefix { func, off, arg })
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
                let func = Ast::from(ast::Annotation { name: format!("@{}", &annotation.code) });
                let (off, arg) = self.translate(expression.as_ref().unwrap()).split();
                Ast::from(ast::Prefix { func, off, arg })
            }
            tree::Variant::Documented(tree::Documented { documentation, expression }) =>
                self.translate(expression.as_ref().unwrap()).without_space(),
            tree::Variant::Invalid(_)
            | tree::Variant::AutoScope(_)
            | tree::Variant::TextLiteral(_)
            | tree::Variant::MultiSegmentApp(_)
            | tree::Variant::TypeDef(_)
            | tree::Variant::Import(_)
            | tree::Variant::Export(_)
            | tree::Variant::Group(_)
            | tree::Variant::CaseOf(_)
            | tree::Variant::Lambda(_)
            | tree::Variant::Array(_)
            | tree::Variant::Tuple(_)
            | tree::Variant::Annotated(_)
            | tree::Variant::ConstructorDefinition(_) => {
                let span_info = deconstruct_tree(tree, |t| self.translate(t));
                let type_info = ast::TreeType::Expression;
                Ast::from(ast::Tree { span_info, type_info })
            }
        };
        WithInitialSpace { space, body }
    }

    fn translate_lines(&self, tree: &Tree, mut out: impl Extend<WithInitialSpace<Ast>>) {
        match &*tree.variant {
            tree::Variant::AnnotatedBuiltin(tree::AnnotatedBuiltin {
                token,
                annotation,
                newlines,
                expression,
            }) if !newlines.is_empty() => todo!(),
            tree::Variant::Annotated(_) => todo!(),
            tree::Variant::Documented(tree::Documented { documentation, expression }) => {
                out.extend_one(self.translate_doc(documentation));
                out.extend(expression.as_ref().map(|e| self.translate(e)));
            }
            _ => out.extend_one(self.translate(tree)),
        }
    }

    fn translate_doc(&self, documentation: &tree::DocComment) -> WithInitialSpace<Ast> {
        let token = ast::RawSpanTree::Token(documentation.code());
        let rendered = documentation.code(); // TODO: render
        let type_info = ast::TreeType::Documentation { rendered };
        let span_info = vec![token];
        let space = 0; // TODO
        let body = Ast::from(ast::Tree { span_info, type_info });
        WithInitialSpace { space, body }
    }

    fn translate_function(&self, function: &tree::Function) -> Ast {
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
        let loff = equals.left_offset.visible.width_in_spaces;
        let opr = Ast::from(ast::Opr { name: equals.code.to_string() });
        match body {
            Some(body) => {
                let (roff, rarg) = self.translate(body).split();
                Ast::from(ast::Infix { larg, loff, opr, roff, rarg })
            }
            None => Ast::from(ast::SectionLeft { arg: larg, off: loff, opr }),
        }
    }

    fn translate_foreign_function(&self, func: &tree::ForeignFunction) -> Ast {
        let tree::ForeignFunction { foreign, language, name, args, equals, body } = func;
        let mut lhs_terms: Vec<_> = [foreign, language, name]
            .into_iter()
            .map(|ident| {
                WithInitialSpace::new(
                    self.translate_var(ident),
                    ident.left_offset.visible.width_in_spaces,
                )
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
        let loff = equals.left_offset.visible.width_in_spaces;
        let opr = Ast::from(ast::Opr { name: equals.code.to_string() });
        let (roff, rarg) = self.translate(body).split();
        Ast::from(ast::Infix { larg, loff, opr, roff, rarg })
    }

    fn translate_var(&self, token: &syntax::token::Ident) -> Ast {
        let name = token.code.to_string();
        Ast::from(ast::Var { name })
    }

    fn opr_app(&self, lhs: &Tree, opr: &syntax::token::Operator, rhs: &Tree) -> Ast {
        let larg = self.translate(lhs).expect_unspaced();
        let loff = opr.left_offset.visible.width_in_spaces;
        let opr = Ast::from(ast::Opr { name: opr.code.to_string() });
        let (roff, rarg) = self.translate(rhs).split();
        Ast::from(ast::Infix { larg, loff, opr, roff, rarg })
    }

    fn translate_opr(&self, opr: &tree::OperatorOrError) -> WithInitialSpace<Ast> {
        let name = match opr {
            Ok(name) => name.code.repr.to_string(),
            Err(names) => names
                .operators
                .iter()
                .map(|name| name.code.repr.to_string())
                .collect::<Vec<_>>()
                .join(""),
        };
        let space = opr.first_operator().left_offset.visible.width_in_spaces;
        let body = Ast::from(ast::Opr { name });
        WithInitialSpace { space, body }
    }

    fn translate_opr_app(
        &self,
        lhs: Option<&Tree>,
        opr: &tree::OperatorOrError,
        rhs: Option<&Tree>,
    ) -> Ast {
        let larg = lhs.map(|a| self.translate(a)).map(|e| e.expect_unspaced());
        let (loff, opr) = self.translate_opr(opr).split();
        let rarg = rhs.map(|a| self.translate(a)).map(|e| e.split());
        match (larg, rarg) {
            (Some(larg), Some((roff, rarg))) =>
                Ast::from(ast::Infix { larg, loff, opr, roff, rarg }),
            (Some(arg), None) => Ast::from(ast::SectionLeft { arg, off: loff, opr }),
            (None, Some((off, arg))) => Ast::from(ast::SectionRight { opr, off, arg }),
            (None, None) => Ast::from(ast::SectionSides { opr }),
        }
    }

    fn translate_module(&self, block: &tree::BodyBlock) -> Ast {
        let lines: Vec<_> = block
            .statements
            .iter()
            .map(|line| ast::BlockLine {
                elem: line.expression.as_ref().map(|e| self.translate(e).without_space()),
                off:  0,
            })
            .collect();
        Ast::from(ast::Module { lines })
    }

    fn translate_block<'a, 's: 'a>(
        &self,
        tree_lines: impl IntoIterator<Item = &'a tree::block::Line<'s>>,
    ) -> Option<ast::Block<Ast>> {
        let tree_lines = tree_lines.into_iter();
        let mut ast_lines = Vec::with_capacity(tree_lines.size_hint().0);
        for line in tree_lines {
            ast_lines.push(line.expression.as_ref().map(|e| self.translate(e)));
        }
        let mut indent = 0;
        let mut empty_lines = vec![];
        let mut first_line = None;
        let mut lines = vec![];
        for elem in ast_lines {
            if first_line.is_none() {
                if let Some(elem) = elem {
                    let (off0, elem) = elem.split();
                    indent = off0;
                    first_line = Some(ast::BlockLine { elem, off: 0 });
                } else {
                    empty_lines.push(0); // lossy
                }
            } else {
                let elem = elem.map(|e| e.without_space());
                lines.push(ast::BlockLine { elem, off: 0 });
            }
        }
        let first_line = first_line?;
        Some(ast::Block { indent, empty_lines, first_line, lines })
    }

    fn translate_operator_block<'a, 's: 'a>(
        &self,
        operator_lines: impl IntoIterator<Item = &'a tree::block::OperatorLine<'s>>,
    ) -> Ast {
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
            if first_line.is_none() {
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
                    first_line = Some(ast::BlockLine { elem, off: 0 });
                } else {
                    empty_lines.push(0); // lossy
                }
            } else {
                lines.push(ast::BlockLine { elem, off: 0 });
            }
        }
        let first_line = first_line.unwrap();
        Ast::from(ast::Block { indent, empty_lines, first_line, lines })
    }

    fn translate_argument_definition(
        &self,
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
        let mut term = self.translate(pattern);
        if let Some(opr) = suspension {
            let space = opr.left_offset.visible.width_in_spaces;
            let opr = Ast::from(ast::Opr { name: opr.code.to_string() });
            let (off, arg) = term.split();
            let body = Ast::from(ast::SectionRight { opr, off, arg });
            term = WithInitialSpace { space, body };
        }
        if let Some(tree::ArgumentDefault { equals, expression }) = default {
            let (space, larg) = term.split();
            let loff = equals.left_offset.visible.width_in_spaces;
            let opr = Ast::from(ast::Opr { name: equals.code.to_string() });
            let (roff, rarg) = self.translate(&expression).split();
            let body = Ast::from(ast::Infix { larg, loff, opr, roff, rarg });
            term = WithInitialSpace { space, body };
        }
        // TODO: handle all the other optional fields
        term
    }
}


// === DeconstructTree ===

/// Analyze a [`Tree`] and produce a representation used by the frontend.
fn deconstruct_tree<F>(tree: &syntax::tree::Tree<'_>, translate: F) -> Vec<ast::RawSpanTree>
where F: Fn(&Tree) -> WithInitialSpace<Ast> {
    use syntax::tree::ItemVisitable;
    let mut deconstructor = DeconstructTree { translate, particles: Default::default() };
    tree.variant.visit_item(&mut deconstructor);
    deconstructor.particles
}

#[derive(Default)]
struct DeconstructTree<F> {
    translate: F,
    particles: Vec<ast::RawSpanTree>,
}

impl<F> tree::Visitor for DeconstructTree<F> {}
impl<'s, 'a, F> tree::ItemVisitor<'s, 'a> for DeconstructTree<F>
where F: Fn(&Tree) -> WithInitialSpace<Ast>
{
    fn visit_item(&mut self, item: syntax::item::Ref<'s, 'a>) -> bool {
        match item {
            syntax::item::Ref::Token(token) => {
                if token.left_offset.visible.width_in_spaces > 0 {
                    self.particles
                        .push(ast::RawSpanTree::Space(token.left_offset.visible.width_in_spaces));
                }
                self.particles.push(ast::RawSpanTree::Token(token.code.to_string()));
            }
            syntax::item::Ref::Tree(tree) => {
                let (space, ast) = (self.translate)(tree).split();
                if space != 0 {
                    self.particles.push(ast::RawSpanTree::Space(space));
                }
                self.particles.push(ast::RawSpanTree::Child(ast));
            }
        }
        false
    }
}


// === WithInitialSpace ===

/// Helper for propagating spacing from children (Tree-style left offsets) to parents (Ast-style
/// top-down spacing).
struct WithInitialSpace<T> {
    space: usize,
    body:  T,
}

impl<T: core::fmt::Debug> WithInitialSpace<T> {
    fn new(body: T, space: usize) -> WithInitialSpace<T> {
        Self { body, space }
    }

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
