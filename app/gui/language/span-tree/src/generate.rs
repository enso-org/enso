//! A module containing code related to SpanTree generation.

use crate::prelude::*;
use enso_text::unit::*;

use crate::generate::context::CalledMethodInfo;
use crate::node;
use crate::node::InsertionPointType;
use crate::node::Payload;
use crate::ArgumentInfo;
use crate::Node;
use crate::SpanTree;

use ast::assoc::Assoc;
use ast::crumbs::Located;
use ast::opr::GeneralizedInfix;
use ast::Ast;
use ast::HasRepr;
use ast::MacroAmbiguousSegment;
use ast::MacroMatchSegment;


// ==============
// === Export ===
// ==============

pub mod context;
pub mod macros;

pub use context::Context;



// =============
// === Trait ===
// =============

/// A trait for all types from which we can generate referred SpanTree. Meant to be implemented for
/// all AST-like structures.
pub trait SpanTreeGenerator<T> {
    /// Generate node with it's whole subtree.
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node<T>>;

    /// Generate tree for this AST treated as root for the whole expression.
    fn generate_tree(&self, context: &impl Context) -> FallibleResult<SpanTree<T>> {
        let root = self.generate_node(node::Kind::Root, context)?;
        Ok(SpanTree { root })
    }
}



// ==============
// === String ===
// ==============

impl<T: Payload> SpanTreeGenerator<T> for &str {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        _: &impl Context,
    ) -> FallibleResult<Node<T>> {
        Ok(Node::<T>::new().with_kind(kind).with_size(self.chars().count().into()))
    }
}

impl<T: Payload> SpanTreeGenerator<T> for String {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node<T>> {
        self.as_str().generate_node(kind, context)
    }
}



// =================
// === Utilities ===
// =================

// === Child Generator ===

/// An utility to generate children with increasing offsets.
#[derive(Debug, Default)]
struct ChildGenerator<T> {
    current_offset: ByteDiff,
    children:       Vec<node::Child<T>>,
}

impl<T: Payload> ChildGenerator<T> {
    /// Add spacing to current generator state. It will be taken into account for the next generated
    /// children's offsets
    fn spacing(&mut self, size: usize) {
        self.current_offset += (size as i32).byte_diff();
    }

    fn generate_ast_node(
        &mut self,
        child_ast: Located<Ast>,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<&mut node::Child<T>> {
        let kind = kind.into();
        let node = child_ast.item.generate_node(kind, context)?;
        Ok(self.add_node(child_ast.crumbs, node))
    }

    fn add_node(&mut self, ast_crumbs: ast::Crumbs, node: Node<T>) -> &mut node::Child<T> {
        let offset = self.current_offset;
        let child = node::Child { node, offset, ast_crumbs };
        self.current_offset += child.node.size;
        self.children.push(child);
        self.children.last_mut().unwrap()
    }

    fn generate_empty_node(&mut self, insert_type: InsertionPointType) -> &mut node::Child<T> {
        let child = node::Child {
            node:       Node::<T>::new().with_kind(insert_type),
            offset:     self.current_offset,
            ast_crumbs: vec![],
        };
        self.children.push(child);
        self.children.last_mut().unwrap()
    }

    fn reverse_children(&mut self) {
        self.children.reverse();
        for child in &mut self.children {
            child.offset = self.current_offset - child.offset - child.node.size;
        }
    }
}



/// =============================
/// === Trait Implementations ===
/// =============================

/// Helper structure constructed from Ast that consists base of prefix application.
///
/// It recognizes whether the base uses a method-style notation (`this.method` instead of
/// `method this`) and what is the invoked function name.
#[derive(Clone, Debug)]
struct ApplicationBase<'a> {
    /// The name of invoked function.
    function_name: Option<&'a str>,
    /// True when Ast uses method notation to pass this as an invocation target.
    has_target:    bool,
}

impl<'a> ApplicationBase<'a> {
    fn new(ast: &'a Ast) -> Self {
        if let Some(chain) = ast::opr::as_access_chain(ast) {
            let get_name = || -> Option<&'a str> {
                let crumbs = chain.enumerate_operands().last()??.crumbs;
                let ast = ast.get_traversing(&crumbs).ok()?;
                ast::identifier::name(ast)
            };
            ApplicationBase { function_name: get_name(), has_target: true }
        } else {
            ApplicationBase { function_name: ast::identifier::name(ast), has_target: false }
        }
    }

    fn prefix_params(
        &self,
        invocation_info: Option<CalledMethodInfo>,
    ) -> impl ExactSizeIterator<Item = ArgumentInfo> {
        let mut ret = invocation_info.map(|info| info.parameters).unwrap_or_default().into_iter();
        if self.has_target {
            ret.next();
        }
        ret
    }
}


// === AST ===

impl<T: Payload> SpanTreeGenerator<T> for Ast {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node<T>> {
        generate_node_for_ast(self, kind.into(), context)
    }
}

fn generate_node_for_ast<T: Payload>(
    ast: &Ast,
    kind: node::Kind,
    context: &impl Context,
) -> FallibleResult<Node<T>> {
    // Code like `ast.func` or `a+b+c`.
    if let Some(infix) = GeneralizedInfix::try_new(ast) {
        let chain = infix.flatten();
        let app_base = ApplicationBase::new(ast);
        let invocation = || -> Option<CalledMethodInfo> {
            context.call_info(ast.id?, Some(app_base.function_name?))
        }();

        // All prefix params are missing arguments, since there is no prefix application.
        let missing_args = app_base.prefix_params(invocation);
        let arity = missing_args.len();
        let base_node_kind = if arity == 0 { kind.clone() } else { node::Kind::Operation };
        let node = chain.generate_node(base_node_kind, context)?;
        let provided_prefix_arg_count = 0;
        Ok(generate_expected_arguments(node, kind, provided_prefix_arg_count, missing_args))
    } else {
        match ast.shape() {
            ast::Shape::Prefix(_) =>
                ast::prefix::Chain::from_ast(ast).unwrap().generate_node(kind, context),
            // Lambdas should fall in _ case, because we don't want to create subports for
            // them
            ast::Shape::Match(_) if ast::macros::as_lambda_match(ast).is_none() =>
                ast::known::Match::try_new(ast.clone_ref()).unwrap().generate_node(kind, context),
            ast::Shape::Ambiguous(_) => ast::known::Ambiguous::try_new(ast.clone_ref())
                .unwrap()
                .generate_node(kind, context),
            _ => {
                let size = (ast.len().value as i32).byte_diff();
                let ast_id = ast.id;
                let children = default();
                let name = ast::identifier::name(ast);
                let payload = default();
                if let Some(info) = ast.id.and_then(|id| context.call_info(id, name)) {
                    let node = {
                        let kind = node::Kind::Operation;
                        Node { kind, size, children, ast_id, payload }
                    };
                    // Note that in this place it is impossible that Ast is in form of
                    // `this.method` -- it is covered by the former if arm. As such, we don't
                    // need to use `ApplicationBase` here as we do elsewhere.
                    let provided_prefix_arg_count = 0;
                    let params = info.parameters.into_iter();
                    Ok(generate_expected_arguments(node, kind, provided_prefix_arg_count, params))
                } else {
                    Ok(Node { kind, size, children, ast_id, payload })
                }
            }
        }
    }
}


// === Operators (Sections and Infixes) ===

impl<T: Payload> SpanTreeGenerator<T> for ast::opr::Chain {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node<T>> {
        generate_node_for_opr_chain(self, kind.into(), context)
    }
}

fn generate_node_for_opr_chain<T: Payload>(
    this: &ast::opr::Chain,
    kind: node::Kind,
    context: &impl Context,
) -> FallibleResult<Node<T>> {
    // Removing operands is possible only when chain has at least 3 of them
    // (target and two arguments).
    let removable = this.args.len() >= 2;
    let node_and_offset: FallibleResult<(Node<T>, usize)> = match &this.target {
        Some(target) => {
            let kind = node::Kind::this().with_removable(removable);
            let node = target.arg.generate_node(kind, context)?;
            Ok((node, target.offset))
        }
        None => Ok((Node::<T>::new().with_kind(InsertionPointType::BeforeTarget), 0)),
    };

    // In this fold we pass last generated node and offset after it, wrapped in Result.
    let (node, _) = this.args.iter().enumerate().fold(node_and_offset, |result, (i, elem)| {
        // Here we generate children as the operator would be left-associative. Then, if it is
        // actually right associative, we just reverse the generated children and their offsets.
        let (node, off) = result?;
        let is_first = i == 0;
        let is_last = i + 1 == this.args.len();
        let has_left = !node.is_insertion_point();
        // Target is a first element of chain in this context.
        let has_target = is_first && has_left;
        let opr_crumbs = elem.crumb_to_operator(has_left);
        let opr_ast = Located::new(opr_crumbs, elem.operator.ast().clone_ref());
        let left_crumbs = if has_left { vec![elem.crumb_to_previous()] } else { vec![] };

        let mut gen = ChildGenerator::default();
        if has_target {
            gen.generate_empty_node(InsertionPointType::BeforeTarget);
        }
        gen.add_node(left_crumbs, node);
        if has_target {
            gen.generate_empty_node(InsertionPointType::AfterTarget);
        }
        gen.spacing(off);
        gen.generate_ast_node(opr_ast, node::Kind::Operation, context)?;
        if let Some(operand) = &elem.operand {
            let arg_crumbs = elem.crumb_to_operand(has_left);
            let arg_ast = Located::new(arg_crumbs, operand.arg.clone_ref());
            gen.spacing(operand.offset);

            gen.generate_ast_node(
                arg_ast,
                node::Kind::argument().with_removable(removable),
                context,
            )?;
        }
        gen.generate_empty_node(InsertionPointType::Append);

        if ast::opr::assoc(&this.operator) == Assoc::Right {
            gen.reverse_children();
        }

        Ok((
            Node {
                kind:     if is_last { kind.clone() } else { node::Kind::Chained },
                size:     gen.current_offset,
                children: gen.children,
                ast_id:   elem.infix_id,
                payload:  default(),
            },
            elem.offset,
        ))
    })?;
    Ok(node)
}


// === Application ===

impl<T: Payload> SpanTreeGenerator<T> for ast::prefix::Chain {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node<T>> {
        generate_node_for_prefix_chain(self, kind.into(), context)
    }
}

fn generate_node_for_prefix_chain<T: Payload>(
    this: &ast::prefix::Chain,
    kind: node::Kind,
    context: &impl Context,
) -> FallibleResult<Node<T>> {
    let base = ApplicationBase::new(&this.func);
    let invocation_info = this.id().and_then(|id| context.call_info(id, base.function_name));
    let known_args = invocation_info.is_some();
    let mut known_params = base.prefix_params(invocation_info);
    let prefix_arity = this.args.len().max(known_params.len());

    use ast::crumbs::PrefixCrumb::*;
    // Removing arguments is possible if there at least two of them
    let removable = this.args.len() >= 2;
    let node = this.func.generate_node(node::Kind::Operation, context);
    let ret = this.args.iter().enumerate().fold(node, |node, (i, arg)| {
        let node = node?;
        let is_first = i == 0;
        let is_last = i + 1 == prefix_arity;
        let arg_kind = if is_first && !base.has_target {
            node::Kind::from(node::Kind::this().with_removable(removable))
        } else {
            node::Kind::from(node::Kind::argument().with_removable(removable))
        };

        let mut gen = ChildGenerator::default();
        gen.add_node(vec![Func.into()], node);
        gen.spacing(arg.sast.off);
        if !known_args && matches!(arg_kind, node::Kind::This { .. }) {
            gen.generate_empty_node(InsertionPointType::BeforeTarget);
        }
        let arg_ast = arg.sast.wrapped.clone_ref();
        let arg_child: &mut node::Child<T> =
            gen.generate_ast_node(Located::new(Arg, arg_ast), arg_kind, context)?;
        if let Some(info) = known_params.next() {
            arg_child.node.set_argument_info(info)
        }
        if !known_args {
            gen.generate_empty_node(InsertionPointType::Append);
        }
        Ok(Node {
            kind:     if is_last { kind.clone() } else { node::Kind::Chained },
            size:     gen.current_offset,
            children: gen.children,
            ast_id:   arg.prefix_id,
            payload:  default(),
        })
    })?;

    Ok(generate_expected_arguments(ret, kind, this.args.len(), known_params))
}


// === Match ===

impl<T: Payload> SpanTreeGenerator<T> for ast::known::Match {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node<T>> {
        generate_node_for_known_match(self, kind.into(), context)
    }
}

fn generate_node_for_known_match<T: Payload>(
    this: &ast::known::Match,
    kind: node::Kind,
    context: &impl Context,
) -> FallibleResult<Node<T>> {
    let removable = false;
    let children_kind = node::Kind::argument().with_removable(removable);
    let mut gen = ChildGenerator::default();
    if let Some(pat) = &this.pfx {
        for macros::AstInPattern { ast, crumbs } in macros::all_ast_nodes_in_pattern(pat) {
            let ast_crumb = ast::crumbs::MatchCrumb::Pfx { val: crumbs };
            let located_ast = Located::new(ast_crumb, ast.wrapped);
            gen.generate_ast_node(located_ast, children_kind.clone(), context)?;
            gen.spacing(ast.off);
        }
    }
    let first_segment_index = 0;
    generate_children_from_segment(&mut gen, first_segment_index, &this.segs.head, context)?;
    for (index, segment) in this.segs.tail.iter().enumerate() {
        gen.spacing(segment.off);
        generate_children_from_segment(&mut gen, index + 1, &segment.wrapped, context)?;
    }
    Ok(Node {
        kind,
        size: gen.current_offset,
        children: gen.children,
        ast_id: this.id(),
        payload: default(),
    })
}

fn generate_children_from_segment<T: Payload>(
    gen: &mut ChildGenerator<T>,
    index: usize,
    segment: &MacroMatchSegment<Ast>,
    context: &impl Context,
) -> FallibleResult {
    // generate child for head
    let ast = segment.head.clone_ref();
    let segment_crumb = ast::crumbs::SegmentMatchCrumb::Head;
    let ast_crumb = ast::crumbs::MatchCrumb::Segs { val: segment_crumb, index };
    let located_ast = Located::new(ast_crumb, ast);
    gen.generate_ast_node(located_ast, node::Kind::Token, context)?;

    for macros::AstInPattern { ast, crumbs } in macros::all_ast_nodes_in_pattern(&segment.body) {
        let child_kind = match crumbs.last() {
            Some(ast::crumbs::PatternMatchCrumb::Tok) => node::Kind::Token,
            _ => node::Kind::argument().into(),
        };
        gen.spacing(ast.off);
        let segment_crumb = ast::crumbs::SegmentMatchCrumb::Body { val: crumbs };
        let ast_crumb = ast::crumbs::MatchCrumb::Segs { val: segment_crumb, index };
        let located_ast = Located::new(ast_crumb, ast.wrapped);
        gen.generate_ast_node(located_ast, child_kind, context)?;
    }
    Ok(())
}


// === Ambiguous ==

impl<T: Payload> SpanTreeGenerator<T> for ast::known::Ambiguous {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node<T>> {
        generate_node_for_known_ambiguous(self, kind.into(), context)
    }
}

fn generate_node_for_known_ambiguous<T: Payload>(
    this: &ast::known::Ambiguous,
    kind: node::Kind,
    context: &impl Context,
) -> FallibleResult<Node<T>> {
    let mut gen = ChildGenerator::default();
    let first_segment_index = 0;
    generate_children_from_ambiguous(&mut gen, first_segment_index, &this.segs.head, context)?;
    for (index, segment) in this.segs.tail.iter().enumerate() {
        gen.spacing(segment.off);
        generate_children_from_ambiguous(&mut gen, index + 1, &segment.wrapped, context)?;
    }
    Ok(Node {
        kind,
        size: gen.current_offset,
        children: gen.children,
        ast_id: this.id(),
        payload: default(),
    })
}

fn generate_children_from_ambiguous<T: Payload>(
    gen: &mut ChildGenerator<T>,
    index: usize,
    segment: &MacroAmbiguousSegment<Ast>,
    context: &impl Context,
) -> FallibleResult {
    let children_kind = node::Kind::argument();
    // generate child for head
    let ast = segment.head.clone_ref();
    let segment_crumb = ast::crumbs::AmbiguousSegmentCrumb::Head;
    let ast_crumb = ast::crumbs::AmbiguousCrumb { field: segment_crumb, index };
    let located_ast = Located::new(ast_crumb, ast);
    gen.generate_ast_node(located_ast, node::Kind::Token, context)?;

    if let Some(sast) = &segment.body {
        gen.spacing(sast.off);
        let field = ast::crumbs::AmbiguousSegmentCrumb::Body;
        let located_ast =
            Located::new(ast::crumbs::AmbiguousCrumb { index, field }, sast.clone_ref());
        gen.generate_ast_node(located_ast, children_kind, context)?;
    }
    Ok(())
}


// === Common Utility ==

/// Build a prefix application-like span tree structure where the prefix argument has not been
/// provided but instead its information is known from method's ArgumentInfo.
///
/// `index` is the argument's position in the prefix chain which may be different from parameter
/// index in the method's parameter list.
fn generate_expected_argument<T: Payload>(
    node: Node<T>,
    kind: node::Kind,
    index: usize,
    is_last: bool,
    argument_info: ArgumentInfo,
) -> Node<T> {
    let mut gen = ChildGenerator::default();
    gen.add_node(ast::Crumbs::new(), node);
    let arg_node = gen.generate_empty_node(InsertionPointType::ExpectedArgument(index));
    arg_node.node.set_argument_info(argument_info);
    Node {
        kind:     if is_last { kind } else { node::Kind::Chained },
        size:     gen.current_offset,
        children: gen.children,
        ast_id:   None,
        payload:  default(),
    }
}

fn generate_expected_arguments<T: Payload>(
    node: Node<T>,
    kind: node::Kind,
    supplied_prefix_arg_count: usize,
    expected_args: impl ExactSizeIterator<Item = ArgumentInfo>,
) -> Node<T> {
    let arity = supplied_prefix_arg_count + expected_args.len();
    (supplied_prefix_arg_count..).zip(expected_args).fold(node, |node, (index, parameter)| {
        let is_last = index + 1 == arity;
        generate_expected_argument(node, kind.clone(), index, is_last, parameter)
    })
}



// ===================
// === MockContext ===
// ===================

use ast::Id;

/// Mock version of `Context`. Useful for debugging and testing.
#[derive(Clone, Debug, Default)]
pub struct MockContext {
    map: HashMap<Id, CalledMethodInfo>,
}

impl MockContext {
    /// Constructor.
    pub fn new_single(id: Id, info: CalledMethodInfo) -> Self {
        let mut ret = Self::default();
        ret.map.insert(id, info);
        ret
    }
}

impl Context for MockContext {
    fn call_info(&self, id: Id, _name: Option<&str>) -> Option<CalledMethodInfo> {
        self.map.get(&id).cloned()
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    use crate::builder::TreeBuilder;
    use crate::generate::context::CalledMethodInfo;
    use crate::node;
    use crate::node::InsertionPointType::*;
    use crate::node::Payload;
    use crate::ArgumentInfo;

    use ast::crumbs::AmbiguousCrumb;
    use ast::crumbs::AmbiguousSegmentCrumb;
    use ast::crumbs::InfixCrumb;
    use ast::crumbs::PatternMatchCrumb;
    use ast::crumbs::PrefixCrumb;
    use ast::crumbs::SectionLeftCrumb;
    use ast::crumbs::SectionRightCrumb;
    use ast::crumbs::SectionSidesCrumb;
    use ast::Crumbs;
    use ast::IdMap;
    use parser_scala::Parser;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);


    /// A helper function which removes information about expression id from thw tree rooted at
    /// `node`.
    ///
    /// It is used in tests. Because parser can assign id as he pleases, therefore to keep tests
    /// cleaner the expression ids are removed before comparing trees.
    fn clear_expression_ids<T>(node: &mut Node<T>) {
        node.ast_id = None;
        for child in &mut node.children {
            clear_expression_ids(&mut child.node);
        }
    }

    /// A helper function which removes parameter information from nodes.
    ///
    /// It is used in tests. Because constructing trees with set parameter infos is troublesome,
    /// it is often more convenient to test them separately and then erase infos and test for shape.
    fn clear_parameter_infos<T: Payload>(node: &mut Node<T>) {
        node.set_argument_info(default());
        for child in &mut node.children {
            clear_parameter_infos(&mut child.node);
        }
    }

    #[wasm_bindgen_test]
    fn generating_span_tree() {
        let parser = Parser::new_or_panic();
        let mut id_map = IdMap::default();
        id_map.generate(0..15);
        id_map.generate(0..11);
        id_map.generate(12..13);
        id_map.generate(14..15);
        id_map.generate(4..11);
        let ast = parser.parse_line_ast_with_id_map("2 + foo bar - 3", id_map.clone()).unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();

        // Check the expression ids we defined:
        for id_map_entry in id_map.vec {
            let (span, id) = id_map_entry;
            let node = tree.root_ref().find_by_span(&span);
            assert!(node.is_some(), "Node with span {} not found", span);
            assert_eq!(node.unwrap().node.ast_id, Some(id));
        }

        // Check the other fields:
        clear_expression_ids(&mut tree.root);
        let expected = TreeBuilder::new(15)
            .add_empty_child(0, BeforeTarget)
            .add_child(0, 11, node::Kind::this(), InfixCrumb::LeftOperand)
            .add_empty_child(0, BeforeTarget)
            .add_leaf(0, 1, node::Kind::this(), InfixCrumb::LeftOperand)
            .add_empty_child(1, AfterTarget)
            .add_leaf(2, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_child(4, 7, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_empty_child(4, BeforeTarget)
            .add_leaf(4, 3, node::Kind::this(), PrefixCrumb::Arg)
            .add_empty_child(7, Append)
            .done()
            .add_empty_child(11, Append)
            .done()
            .add_empty_child(11, AfterTarget)
            .add_leaf(12, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(14, 1, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_empty_child(15, Append)
            .build();

        assert_eq!(expected, tree)
    }

    #[wasm_bindgen_test]
    fn generate_span_tree_with_chains() {
        let parser = Parser::new_or_panic();
        let ast = parser.parse_line_ast("2 + 3 + foo bar baz 13 + 5").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);

        let expected = TreeBuilder::new(26)
            .add_child(0, 22, node::Kind::Chained, InfixCrumb::LeftOperand)
            .add_child(0, 5, node::Kind::Chained, InfixCrumb::LeftOperand)
            .add_empty_child(0, BeforeTarget)
            .add_leaf(0, 1, node::Kind::this().removable(), InfixCrumb::LeftOperand)
            .add_empty_child(1, AfterTarget)
            .add_leaf(2, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(4, 1, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .add_empty_child(5, Append)
            .done()
            .add_leaf(6, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_child(8, 14, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .add_child(0, 11, node::Kind::Chained, PrefixCrumb::Func)
            .add_child(0, 7, node::Kind::Chained, PrefixCrumb::Func)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_empty_child(4, BeforeTarget)
            .add_leaf(4, 3, node::Kind::this().removable(), PrefixCrumb::Arg)
            .add_empty_child(7, Append)
            .done()
            .add_leaf(8, 3, node::Kind::argument().removable(), PrefixCrumb::Arg)
            .add_empty_child(11, Append)
            .done()
            .add_leaf(12, 2, node::Kind::argument().removable(), PrefixCrumb::Arg)
            .add_empty_child(14, Append)
            .done()
            .add_empty_child(22, Append)
            .done()
            .add_leaf(23, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(25, 1, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .add_empty_child(26, Append)
            .build();

        assert_eq!(expected, tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_right_assoc_operator() {
        let parser = Parser::new_or_panic();
        let ast = parser.parse_line_ast("1,2,3").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);

        let expected = TreeBuilder::new(5)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::argument().removable(), InfixCrumb::LeftOperand)
            .add_leaf(1, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_child(2, 3, node::Kind::Chained, InfixCrumb::RightOperand)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::argument().removable(), InfixCrumb::LeftOperand)
            .add_leaf(1, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(2, AfterTarget)
            .add_leaf(2, 1, node::Kind::this().removable(), InfixCrumb::RightOperand)
            .add_empty_child(3, BeforeTarget)
            .done()
            .build();

        assert_eq!(expected, tree)
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_section() {
        let parser = Parser::new_or_panic();
        // The star makes `SectionSides` ast being one of the parameters of + chain. First + makes
        // SectionRight, and last + makes SectionLeft.
        let ast = parser.parse_line_ast("+ * + + 2 +").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);

        let expected = TreeBuilder::new(11)
            .add_child(0, 9, node::Kind::Chained, SectionLeftCrumb::Arg)
            .add_child(0, 5, node::Kind::Chained, InfixCrumb::LeftOperand)
            .add_child(0, 3, node::Kind::Chained, SectionLeftCrumb::Arg)
            .add_empty_child(0, BeforeTarget)
            .add_leaf(0, 1, node::Kind::Operation, SectionRightCrumb::Opr)
            .add_child(2, 1, node::Kind::argument().removable(), SectionRightCrumb::Arg)
            .add_empty_child(0, BeforeTarget)
            .add_leaf(0, 1, node::Kind::Operation, SectionSidesCrumb)
            .add_empty_child(1, Append)
            .done()
            .add_empty_child(3, Append)
            .done()
            .add_leaf(4, 1, node::Kind::Operation, SectionLeftCrumb::Opr)
            .add_empty_child(5, Append)
            .done()
            .add_leaf(6, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(8, 1, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .add_empty_child(9, Append)
            .done()
            .add_leaf(10, 1, node::Kind::Operation, SectionLeftCrumb::Opr)
            .add_empty_child(11, Append)
            .build();

        assert_eq!(expected, tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_right_assoc_section() {
        let parser = Parser::new_or_panic();
        let ast = parser.parse_line_ast(",2,").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);

        let expected = TreeBuilder::new(3)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::Operation, SectionRightCrumb::Opr)
            .add_child(1, 2, node::Kind::Chained, SectionRightCrumb::Arg)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::argument().removable(), SectionLeftCrumb::Arg)
            .add_leaf(1, 1, node::Kind::Operation, SectionLeftCrumb::Opr)
            .add_empty_child(2, BeforeTarget)
            .done()
            .build();

        assert_eq!(expected, tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_matched_macros() {
        use PatternMatchCrumb::*;

        let parser = Parser::new_or_panic();
        let mut id_map = IdMap::default();
        id_map.generate(0..29);
        let expression = "if foo then (a + b) x else ()";
        let ast = parser.parse_line_ast_with_id_map(expression, id_map.clone()).unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();

        // Check if expression id is set
        let (_, expected_id) = id_map.vec.first().unwrap();
        assert_eq!(tree.root_ref().ast_id, Some(*expected_id));

        // Check the other fields
        clear_expression_ids(&mut tree.root);
        let seq = Seq { right: false };
        let if_then_else_cr = vec![seq, Or, Build];
        let parens_cr = vec![seq, Or, Or, Build];

        let expected = TreeBuilder::new(29)
            .add_leaf(0, 2, node::Kind::Token, segment_head_crumbs(0))
            .add_leaf(3, 3, node::Kind::argument(), segment_body_crumbs(0, &if_then_else_cr))
            .add_leaf(7, 4, node::Kind::Token, segment_head_crumbs(1))
            .add_child(12, 9, node::Kind::argument(), segment_body_crumbs(1, &if_then_else_cr))
            .add_child(0, 7, node::Kind::Operation, PrefixCrumb::Func)
            .add_leaf(0, 1, node::Kind::Token, segment_head_crumbs(0))
            .add_child(1, 5, node::Kind::argument(), segment_body_crumbs(0, &parens_cr))
            .add_empty_child(0, BeforeTarget)
            .add_leaf(0, 1, node::Kind::this(), InfixCrumb::LeftOperand)
            .add_empty_child(1, AfterTarget)
            .add_leaf(2, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(4, 1, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_empty_child(5, Append)
            .done()
            .add_leaf(6, 1, node::Kind::Token, segment_head_crumbs(1))
            .done()
            .add_empty_child(8, BeforeTarget)
            .add_leaf(8, 1, node::Kind::this(), PrefixCrumb::Arg)
            .add_empty_child(9, Append)
            .done()
            .add_leaf(22, 4, node::Kind::Token, segment_head_crumbs(2))
            .add_child(27, 2, node::Kind::argument(), segment_body_crumbs(2, &if_then_else_cr))
            .add_leaf(0, 1, node::Kind::Token, segment_head_crumbs(0))
            .add_leaf(1, 1, node::Kind::Token, segment_head_crumbs(1))
            .done()
            .build();

        assert_eq!(expected, tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_matched_list_macro() {
        use PatternMatchCrumb::*;

        let parser = Parser::new_or_panic();
        let expression = "[a,b]";
        let ast = parser.parse_line_ast(expression).unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();

        // Check the other fields
        clear_expression_ids(&mut tree.root);
        let left_seq = Seq { right: false };
        let right_seq = Seq { right: true };
        let many = Many { index: 0 };
        let first_element_cr = vec![left_seq, Or, Or, left_seq, Build];
        let second_element_cr = vec![left_seq, Or, Or, right_seq, many, right_seq, Build];
        let comma_cr = vec![left_seq, Or, Or, right_seq, many, left_seq, Tok];

        let expected = TreeBuilder::new(5)
            .add_leaf(0, 1, node::Kind::Token, segment_head_crumbs(0))
            .add_leaf(1, 1, node::Kind::argument(), segment_body_crumbs(0, &first_element_cr))
            .add_leaf(2, 1, node::Kind::Token, segment_body_crumbs(0, &comma_cr))
            .add_leaf(3, 1, node::Kind::argument(), segment_body_crumbs(0, &second_element_cr))
            .add_leaf(4, 1, node::Kind::Token, segment_head_crumbs(1))
            .build();

        assert_eq!(expected, tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_ambiguous_macros() {
        let parser = Parser::new_or_panic();
        let mut id_map = IdMap::default();
        id_map.generate(0..2);
        let ast = parser.parse_line_ast_with_id_map("(4", id_map.clone()).unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();

        // Check the expression id:
        let (_, expected_id) = id_map.vec.first().unwrap();
        assert_eq!(tree.root_ref().ast_id, Some(*expected_id));

        // Check the other fields:
        clear_expression_ids(&mut tree.root);
        let head_crumb = AmbiguousCrumb { index: 0, field: AmbiguousSegmentCrumb::Head };
        let body_crumb = AmbiguousCrumb { index: 0, field: AmbiguousSegmentCrumb::Body };
        let expected = TreeBuilder::new(2)
            .add_leaf(0, 1, node::Kind::Token, head_crumb)
            .add_leaf(1, 1, node::Kind::argument(), body_crumb)
            .build();

        assert_eq!(expected, tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_for_lambda() {
        let parser = Parser::new_or_panic();
        let ast = parser.parse_line_ast("foo a-> b + c").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);

        let expected = TreeBuilder::new(13)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_empty_child(4, BeforeTarget)
            .add_leaf(4, 9, node::Kind::this(), PrefixCrumb::Arg)
            .add_empty_child(13, Append)
            .build();

        assert_eq!(expected, tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_for_unfinished_call() {
        let parser = Parser::new_or_panic();
        let this_param =
            ArgumentInfo { name: Some("self".to_owned()), tp: Some("Any".to_owned()), ..default() };
        let param1 = ArgumentInfo {
            name: Some("arg1".to_owned()),
            tp: Some("Number".to_owned()),
            ..default()
        };
        let param2 = ArgumentInfo { name: Some("arg2".to_owned()), tp: None, ..default() };


        // === Single function name ===

        let ast = parser.parse_line_ast("foo").unwrap();
        let invocation_info = CalledMethodInfo { parameters: vec![this_param.clone()] };
        let ctx = MockContext::new_single(ast.id.unwrap(), invocation_info);
        let mut tree: SpanTree = SpanTree::new(&ast, &ctx).unwrap();
        match tree.root_ref().leaf_iter().collect_vec().as_slice() {
            [_func, arg0] => assert_eq!(arg0.argument_info().as_ref(), Some(&this_param)),
            sth_else => panic!("There should be 2 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(3)
            .add_leaf(0, 3, node::Kind::Operation, Crumbs::default())
            .add_empty_child(3, ExpectedArgument(0))
            .build();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);
        assert_eq!(tree, expected);


        // === Complete application chain ===

        let ast = parser.parse_line_ast("foo here").unwrap();
        let invocation_info = CalledMethodInfo { parameters: vec![this_param.clone()] };
        let ctx = MockContext::new_single(ast.id.unwrap(), invocation_info);
        let mut tree: SpanTree = SpanTree::new(&ast, &ctx).unwrap();
        match tree.root_ref().leaf_iter().collect_vec().as_slice() {
            [_func, arg0] => assert_eq!(arg0.argument_info().as_ref(), Some(&this_param)),
            sth_else => panic!("There should be 2 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(8)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_leaf(4, 4, node::Kind::this(), PrefixCrumb::Arg)
            .build();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);
        assert_eq!(tree, expected);


        // === Partial application chain ===

        let ast = parser.parse_line_ast("foo here").unwrap();
        let invocation_info = CalledMethodInfo {
            parameters: vec![this_param.clone(), param1.clone(), param2.clone()],
        };
        let ctx = MockContext::new_single(ast.id.unwrap(), invocation_info);
        let mut tree: SpanTree = SpanTree::new(&ast, &ctx).unwrap();
        match tree.root_ref().leaf_iter().collect_vec().as_slice() {
            [_func, arg0, arg1, arg2] => {
                assert_eq!(arg0.argument_info().as_ref(), Some(&this_param));
                assert_eq!(arg1.argument_info().as_ref(), Some(&param1));
                assert_eq!(arg2.argument_info().as_ref(), Some(&param2));
            }
            sth_else => panic!("There should be 4 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(8)
            .add_child(0, 8, node::Kind::Chained, Crumbs::default())
            .add_child(0, 8, node::Kind::Chained, Crumbs::default())
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_leaf(4, 4, node::Kind::this(), PrefixCrumb::Arg)
            .done()
            .add_empty_child(8, ExpectedArgument(1))
            .done()
            .add_empty_child(8, ExpectedArgument(2))
            .build();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);
        assert_eq!(tree, expected);


        // === Partial application chain - this argument ===

        let ast = parser.parse_line_ast("here.foo").unwrap();
        let invocation_info =
            CalledMethodInfo { parameters: vec![this_param, param1.clone(), param2.clone()] };
        let ctx = MockContext::new_single(ast.id.unwrap(), invocation_info);
        let mut tree: SpanTree = SpanTree::new(&ast, &ctx).unwrap();
        match tree.root_ref().leaf_iter().collect_vec().as_slice() {
            [_, _this, _, _, _func, _, arg1, arg2] => {
                assert_eq!(arg1.argument_info().as_ref(), Some(&param1));
                assert_eq!(arg2.argument_info().as_ref(), Some(&param2));
            }
            sth_else => panic!("There should be 8 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(8)
            .add_child(0, 8, node::Kind::Chained, Crumbs::default())
            .add_child(0, 8, node::Kind::Operation, Crumbs::default())
            .add_empty_child(0, BeforeTarget)
            .add_leaf(0, 4, node::Kind::this(), InfixCrumb::LeftOperand)
            .add_empty_child(4, AfterTarget)
            .add_leaf(4, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(5, 3, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_empty_child(8, Append)
            .done()
            .add_empty_child(8, ExpectedArgument(0))
            .done()
            .add_empty_child(8, ExpectedArgument(1))
            .build();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);
        assert_eq!(tree, expected);
    }

    fn segment_body_crumbs(
        index: usize,
        pattern_crumb: &[PatternMatchCrumb],
    ) -> ast::crumbs::MatchCrumb {
        let val = ast::crumbs::SegmentMatchCrumb::Body { val: pattern_crumb.to_vec() };
        ast::crumbs::MatchCrumb::Segs { val, index }
    }

    fn segment_head_crumbs(index: usize) -> ast::crumbs::MatchCrumb {
        let val = ast::crumbs::SegmentMatchCrumb::Head;
        ast::crumbs::MatchCrumb::Segs { val, index }
    }
}
