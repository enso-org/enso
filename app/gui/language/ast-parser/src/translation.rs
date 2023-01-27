use ast::{Ast, SpanAnalysis};
use enso_parser::syntax;
use enso_parser::syntax::tree;
use enso_parser::syntax::tree::NonEmptyOperatorSequence;
use enso_parser::syntax::Tree;

fn todo_ast(todo: Todo) -> Ast {
    Ast::from(ast::Var { name: format!("Todo::{:?}", todo) })
}

pub fn to_legacy_ast_module(tree: &Tree) -> Result<Ast, ()> {
    Ok(match &*tree.variant {
        tree::Variant::BodyBlock(block) => translate_module(&block).into(),
        _ => return Err(()),
    })
}

pub fn to_legacy_ast(tree: &Tree) -> Ast {
    try_to_legacy_ast(tree).unwrap_or_else(todo_ast)
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Todo {
    UnaryOprSection,
    Import,
    Export,
    Invalid,
    AutoScope,
    TextLiteral,
    MultiSegmentApp,
    TypeDef,
    CaseOf,
    Lambda,
    Tuple,
    Annotated,
    Array,
}

// TODO: Whitespace:
// Tree -> AstWithInitialSpace, recursively. `Shape::Tree` has no initial space, but other
// translations need it so as not to lose the initial `left_offset`.

pub fn try_to_legacy_ast(tree: &Tree) -> Result<Ast, Todo> {
    Ok(match &*tree.variant {
        tree::Variant::BodyBlock(block) => translate_block(&block.statements).into(),
        tree::Variant::Ident(tree::Ident { token }) if token.is_type =>
            Ast::from(ast::Cons { name: token.code.to_string() }),
        tree::Variant::Ident(tree::Ident { token }) =>
            Ast::from(ast::Var { name: token.code.to_string() }),
        tree::Variant::Number(tree::Number { base, integer, .. }) => Ast::from(ast::Number {
            base: base.as_ref().map(|base| base.code.to_string()),
            int:  integer.as_ref().map(|integer| integer.code.to_string()).unwrap_or_default(),
        }),
        tree::Variant::App(tree::App { func, arg }) => {
            // In `Ast` spaces between siblings belonged to the parent node, but in `Tree` every
            // node owns its preceding whitespace.
            //let off = arg.span.left_offset.visible.width_in_spaces;
            let off = 0;
            Ast::from(ast::Prefix { func: to_legacy_ast(func), off, arg: to_legacy_ast(arg) })
        }
        tree::Variant::OprApp(tree::OprApp { lhs, opr, rhs }) =>
            translate_opr_app(lhs.as_ref(), opr, rhs.as_ref()),
        tree::Variant::OprSectionBoundary(tree::OprSectionBoundary { ast, .. }) =>
            to_legacy_ast(ast),
        tree::Variant::Function(tree::Function { name, args, equals, body }) => {
            let name = to_legacy_ast(name);
            let mut lhs_terms = vec![name];
            lhs_terms.extend(args.into_iter().map(translate_argument_definition));
            let larg = lhs_terms
                .into_iter()
                .reduce(|func, arg| {
                    Ast::from(ast::Prefix {
                        func,
                        off: 1, // TODO
                        arg,
                    })
                })
                .unwrap();
            let loff = equals.left_offset.visible.width_in_spaces;
            let roff = body
                .as_ref()
                .map(|tree| tree.span.left_offset.visible.width_in_spaces)
                .unwrap_or_default();
            let opr = Ast::from(ast::Opr { name: equals.code.to_string() });
            match body {
                Some(body) => {
                    let rarg = to_legacy_ast(body);
                    Ast::from(ast::Infix { larg, loff, opr, roff, rarg })
                }
                None => Ast::from(ast::SectionLeft { arg: larg, off: loff, opr }),
            }
        }
        tree::Variant::ForeignFunction(func) => {
            let tree::ForeignFunction { foreign, language, name, args, equals, body } = func;
            let mut lhs_terms: Vec<_> = [foreign, language, name]
                .into_iter()
                .map(|ident| Ast::from(ast::Var { name: ident.code.to_string() }))
                .collect();
            lhs_terms.extend(args.into_iter().map(translate_argument_definition));
            let larg = lhs_terms
                .into_iter()
                .reduce(|func, arg| {
                    Ast::from(ast::Prefix {
                        func,
                        off: 1, // TODO
                        arg,
                    })
                })
                .unwrap();
            let loff = equals.left_offset.visible.width_in_spaces;
            let opr = Ast::from(ast::Opr { name: equals.code.to_string() });
            let roff = body.span.left_offset.visible.width_in_spaces;
            let rarg = to_legacy_ast(body);
            Ast::from(ast::Infix { larg, loff, opr, roff, rarg })
        }
        tree::Variant::UnaryOprApp(tree::UnaryOprApp { opr, rhs }) => {
            let opr = Ast::from(ast::Opr { name: opr.code.to_string() });
            let off = rhs
                .as_ref()
                .map(|tree| tree.span.left_offset.visible.width_in_spaces)
                .unwrap_or_default();
            let arg = rhs.as_ref().map(to_legacy_ast).ok_or(Todo::UnaryOprSection)?;
            Ast::from(ast::SectionRight { opr, off, arg })
        }
        tree::Variant::Assignment(tree::Assignment { pattern, equals, expr }) =>
            opr_app(pattern, equals, expr),
        tree::Variant::OperatorBlockApplication(app) => {
            let tree::OperatorBlockApplication { lhs, expressions, excess } = app;
            let func = lhs.as_ref().map(to_legacy_ast).unwrap();
            let arg = translate_operator_block(expressions);
            Ast::from(ast::Prefix { func, off: 0, arg })
        }
        tree::Variant::TemplateFunction(tree::TemplateFunction { ast, .. }) => to_legacy_ast(ast),
        tree::Variant::Wildcard(_) => Ast::from(ast::Blank {}),
        tree::Variant::ArgumentBlockApplication(app) => {
            let tree::ArgumentBlockApplication { lhs, arguments } = app;
            let func = lhs.as_ref().map(to_legacy_ast).unwrap();
            let arg = translate_block(arguments).into();
            Ast::from(ast::Prefix { func, off: 0, arg })
        }
        tree::Variant::DefaultApp(tree::DefaultApp { func, default }) => {
            let func = to_legacy_ast(func);
            let off = default.left_offset.visible.width_in_spaces;
            let arg = Ast::from(ast::Var { name: default.code.to_string() });
            Ast::from(ast::Prefix { func, off, arg })
        }
        tree::Variant::NamedApp(tree::NamedApp { func, open, name, equals, arg, close }) => {
            let func = to_legacy_ast(func);
            let off = name.left_offset.visible.width_in_spaces; // TODO: open || name
            let larg = Ast::from(ast::Var { name: name.code.to_string() });
            let loff = equals.left_offset.visible.width_in_spaces;
            let opr = Ast::from(ast::Opr { name: equals.code.to_string() });
            let roff = arg.span.left_offset.visible.width_in_spaces;
            let rarg = to_legacy_ast(arg);
            let arg = Ast::from(ast::Infix { larg, loff, opr, roff, rarg });
            Ast::from(ast::Prefix { func, off, arg })
        }
        tree::Variant::TypeSignature(tree::TypeSignature { variable, operator, type_ }) =>
            opr_app(variable, operator, type_),
        tree::Variant::TypeAnnotated(tree::TypeAnnotated { expression, operator, type_ }) =>
            opr_app(expression, operator, type_),
        tree::Variant::AnnotatedBuiltin(tree::AnnotatedBuiltin {
            token,
            annotation,
            newlines,
            expression,
        }) => {
            let func = Ast::from(ast::Annotation { name: format!("@{}", &annotation.code) });
            let off = expression.as_ref().unwrap().span.left_offset.visible.width_in_spaces;
            let arg = to_legacy_ast(expression.as_ref().unwrap());
            Ast::from(ast::Prefix { func, off, arg })
        }
        tree::Variant::Documented(tree::Documented { documentation, expression }) =>
        // TODO. Documented/Comment are spaceless...
            to_legacy_ast(expression.as_ref().unwrap()),
        tree::Variant::Group(tree::Group { open, body, close }) => Ast::from(ast::Tree {
            repr:          tree.code(),
            span_analysis: SpanAnalysis::new(tree),
        }),
        tree::Variant::Array(tree::Array { left, first, rest, right }) => return Err(Todo::Array),
        /*
           Ast::from(ast::Tree {
               repr: tree.code(),
               resolved: Ast::from_ast_id_len(ast::Shape::SequenceLiteral(ast::SequenceLiteral {
                   items: first.iter().chain(rest.iter().filter_map(|e| e.body.as_ref())).map(to_legacy_ast).collect(),
               }), None, 0),
           }),
        */
        tree::Variant::Import(_) => return Err(Todo::Import),
        tree::Variant::Export(_) => return Err(Todo::Export),
        tree::Variant::Invalid(_) => return Err(Todo::Invalid),
        tree::Variant::AutoScope(_) => return Err(Todo::AutoScope),
        tree::Variant::TextLiteral(_) => return Err(Todo::TextLiteral),
        tree::Variant::MultiSegmentApp(_) => return Err(Todo::MultiSegmentApp),
        tree::Variant::TypeDef(_) => return Err(Todo::TypeDef),
        tree::Variant::CaseOf(_) => return Err(Todo::CaseOf),
        tree::Variant::Lambda(_) => return Err(Todo::Lambda),
        tree::Variant::Tuple(_) => return Err(Todo::Tuple),
        tree::Variant::Annotated(_) => return Err(Todo::Annotated),
        // This type should only occur in the body of a `TypeDef`, or in an `Invalid`.
        tree::Variant::ConstructorDefinition(_) => todo!(),
    })
}

fn opr_app(lhs: &Tree, opr: &syntax::token::Operator, rhs: &Tree) -> Ast {
    let larg = to_legacy_ast(lhs);
    let loff = opr.left_offset.visible.width_in_spaces;
    let opr = Ast::from(ast::Opr { name: opr.code.to_string() });
    let roff = rhs.span.left_offset.visible.width_in_spaces;
    let rarg = to_legacy_ast(rhs);
    Ast::from(ast::Infix { larg, loff, opr, roff, rarg })
}

fn translate_opr(opr: &tree::OperatorOrError) -> Ast {
    let name = match opr {
        Ok(name) => name.code.repr.to_string(),
        Err(names) => names
            .operators
            .iter()
            .map(|name| name.code.repr.to_string())
            .collect::<Vec<_>>()
            .join(""),
    };
    Ast::from(ast::Opr { name })
}

fn translate_opr_app(lhs: Option<&Tree>, opr: &tree::OperatorOrError, rhs: Option<&Tree>) -> Ast {
    let larg = lhs.map(to_legacy_ast);
    let loff = opr.first_operator().left_offset.visible.width_in_spaces;
    let opr = translate_opr(opr);
    let roff = rhs.map(|tree| tree.span.left_offset.visible.width_in_spaces).unwrap_or_default();
    let rarg = rhs.map(to_legacy_ast);
    match (larg, rarg) {
        (Some(larg), Some(rarg)) => Ast::from(ast::Infix { larg, loff, opr, roff, rarg }),
        (Some(arg), None) => Ast::from(ast::SectionLeft { arg, off: loff, opr }),
        (None, Some(arg)) => Ast::from(ast::SectionRight { opr, off: roff, arg }),
        (None, None) => Ast::from(ast::SectionSides { opr }),
    }
}

pub fn translate_module(block: &tree::BodyBlock) -> Ast {
    let lines: Vec<_> = block
        .statements
        .iter()
        .map(|line| ast::BlockLine { elem: line.expression.as_ref().map(to_legacy_ast), off: 0 })
        .collect();
    Ast::from(ast::Module { lines })
}

fn translate_block<'a, 's: 'a>(
    block_lines: impl IntoIterator<Item = &'a tree::block::Line<'s>>,
) -> ast::Block<Ast> {
    let mut indent = 0;
    let mut empty_lines = vec![];
    let mut first_line = None;
    let mut lines = vec![];
    for line in block_lines {
        let expression = &line.expression;
        if first_line.is_none() {
            if let Some(expression) = expression {
                indent = expression.span.left_offset.visible.width_in_spaces;
                let elem = to_legacy_ast(expression);
                let off = 0;
                first_line = Some(ast::BlockLine { elem, off });
            } else {
                empty_lines.push(0); // lossy
            }
        } else {
            let elem = expression.as_ref().map(to_legacy_ast);
            lines.push(ast::BlockLine { elem, off: 0 });
        }
    }
    let first_line = first_line.unwrap();
    ast::Block { indent, empty_lines, first_line, lines, is_orphan: false }
}

fn translate_operator_block<'a, 's: 'a>(
    operator_lines: impl IntoIterator<Item = &'a tree::block::OperatorLine<'s>>,
) -> Ast {
    let mut indent = 0;
    let mut empty_lines = vec![];
    let mut first_line = None;
    let mut lines = vec![];
    for line in operator_lines {
        let elem = line.expression.as_ref().map(|expression| {
            let opr = translate_opr(&expression.operator);
            let off = expression.expression.span.left_offset.visible.width_in_spaces;
            let arg = to_legacy_ast(&expression.expression);
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
    Ast::from(ast::Block { indent, empty_lines, first_line, lines, is_orphan: false })
}

fn translate_argument_definition(arg: &tree::ArgumentDefinition) -> Ast {
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
    let mut term = to_legacy_ast(pattern);
    if let Some(opr) = suspension {
        let opr = Ast::from(ast::Opr { name: opr.code.to_string() });
        let off = pattern.span.left_offset.visible.width_in_spaces;
        let arg = term;
        term = Ast::from(ast::SectionRight { opr, off, arg });
    }
    if let Some(tree::ArgumentDefault { equals, expression }) = default {
        let loff = equals.left_offset.visible.width_in_spaces;
        let equals = Ast::from(ast::Opr { name: equals.code.to_string() });
        let roff = expression.span.left_offset.visible.width_in_spaces;
        let rarg = to_legacy_ast(&expression);
        term = Ast::from(ast::Infix { larg: term, loff, opr: equals, roff, rarg });
    }
    // TODO: handle all the other optional fields
    term
}
