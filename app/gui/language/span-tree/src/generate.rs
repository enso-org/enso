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

use ast::crumbs::Located;
use ast::opr::GeneralizedInfix;
use ast::Ast;
use ast::HasRepr;
use ast::SpanSeed;
use std::collections::VecDeque;


// ==============
// === Export ===
// ==============

pub mod context;

pub use context::Context;



// =============
// === Trait ===
// =============

/// A trait for all types from which we can generate referred SpanTree. Meant to be implemented for
/// all AST-like structures.
pub trait SpanTreeGenerator<T> {
    /// Generate node with its whole subtree.
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
    function_name:        Option<Cow<'a, str>>,
    /// True when Ast uses method notation to pass `self` as an invocation target.
    uses_method_notation: bool,
    /// AST ID of the function application expression. The subject of [`Context::call_info`] call,
    /// which is used to get a list of expected arguments for this application.
    ///
    /// For an expression `target.method arg`, this is the `target.method` part.
    call_id:              Option<Id>,
}

impl<'a> ApplicationBase<'a> {
    /// Create `ApplicationBase` from infix expression.
    fn from_infix(infix: &'a GeneralizedInfix) -> Self {
        let call_id = infix.id;
        if infix.name() == ast::opr::predefined::ACCESS {
            // method-style notation: `this.that.method`
            // In this case the applied method is not the dot operator, but the name after the dot.
            let function = infix.argument_operand().as_ref();
            let function_name =
                function.and_then(|func| ast::identifier::name(&func.arg)).map(Into::into);
            ApplicationBase { function_name, call_id, uses_method_notation: true }
        } else {
            // Other chain type, e.g. `a + b + c`
            let function_name = Some(infix.name().into());
            ApplicationBase { function_name, call_id, uses_method_notation: false }
        }
    }

    /// Create `ApplicationBase` from flattened prefix expression chain.
    fn from_prefix_chain(chain: &'a ast::prefix::Chain) -> Self {
        let call_id = chain.id();
        // When first chain element is an infix access, derive the application base from it, but
        // treat the whole chain as a call expression.
        if let Some(access_infix) = GeneralizedInfix::try_new(&chain.func)
            .filter(|infix| infix.name() == ast::opr::predefined::ACCESS)
        {
            let base = ApplicationBase::from_infix(&access_infix).into_owned();
            return ApplicationBase { call_id, ..base };
        }

        // For any other prefix chain, the applied function is the first chain element.
        let function_name = ast::identifier::name(&chain.func).map(Into::into);
        ApplicationBase { function_name, call_id, uses_method_notation: false }
    }

    /// Get the list of method arguments expected by this application. Does not include `self`
    /// parameter when using method notation. Returns `None` when the call info is not present in
    /// the context or AST doesn't contain enough information to query it.
    fn known_arguments(&self, context: &impl Context) -> Option<VecDeque<ArgumentInfo>> {
        // If method notation is used, the function name is required to get relevant call info. Do
        // not attempt the call if method name is not available, as the returned data would be not
        // relevant.
        if self.uses_method_notation && self.function_name.is_none() {
            return None;
        }

        let invocation_info = context.call_info(self.call_id?, self.function_name.as_deref())?;
        let self_in_access = !invocation_info.is_constructor
            && (!invocation_info.called_on_type.unwrap_or(false) || invocation_info.is_static);

        let parameters = invocation_info.with_call_id(self.call_id).parameters;
        let mut deque: VecDeque<ArgumentInfo> = parameters.into();
        let first_argument_name = deque.front().and_then(|arg| arg.name.as_ref());
        let has_self_argument = first_argument_name.map_or(false, |name| name == node::This::NAME);

        // When a method notation is used on non-static invocation, the first `self` argument is
        // already present the access chain. Remove it from the list of expected prefix arguments.
        if self.uses_method_notation && has_self_argument && self_in_access {
            deque.pop_front();
        }
        Some(deque)
    }

    /// Switch the method application to use different expression as call id, but keep the same
    /// function name and notation.
    fn set_call_id(&mut self, new_call_id: Option<Id>) {
        self.call_id = new_call_id;
    }

    fn into_owned(self) -> ApplicationBase<'static> {
        let function_name = self.function_name.map(|s| s.into_owned().into());
        ApplicationBase { function_name, ..self }
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
    if let Some(infix) = GeneralizedInfix::try_new(ast) {
        infix.generate_node(kind, context)
    } else {
        match ast.shape() {
            ast::Shape::Prefix(_) =>
                ast::prefix::Chain::from_ast(ast).unwrap().generate_node(kind, context),
            ast::Shape::Tree(tree) if tree.type_info != ast::TreeType::Lambda =>
                tree_generate_node(tree, kind, context, ast.id),
            _ => {
                let size = (ast.len().value as i32).byte_diff();
                let ast_id = ast.id;
                let name = ast::identifier::name(ast);
                if let Some(info) = ast.id.and_then(|id| context.call_info(id, name)) {
                    let node = {
                        let kind = node::Kind::Operation;
                        Node { kind, size, ast_id, ..default() }
                    };
                    // Note that in this place it is impossible that Ast is in form of
                    // `this.method` -- it is covered by the former if arm. As such, we don't
                    // need to use `ApplicationBase` here as we do elsewhere.
                    let provided_prefix_arg_count = 0;
                    let info = info.with_call_id(ast.id);
                    let params = info.parameters.into_iter();
                    Ok(generate_expected_arguments(node, kind, provided_prefix_arg_count, params))
                } else {
                    Ok(Node { kind, size, ast_id, ..default() })
                }
            }
        }
    }
}


// === Operators (Sections and Infixes) ===

impl<T: Payload> SpanTreeGenerator<T> for GeneralizedInfix {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node<T>> {
        // Code like `ast.func` or `a+b+c`.
        let chain = self.flatten();
        let kind = kind.into();
        let mut app_base = ApplicationBase::from_infix(self);
        if app_base.uses_method_notation {
            // This is a standalone method access chain, missing method parameters needs to be
            // handled here. It is guaranteed that no existing prefix arguments are present, as
            // method calls inside prefix chains are handled by `generate_node_for_prefix_chain` and
            // never reach this point.
            let arguments = app_base.known_arguments(context);
            let args_resolved = arguments.is_some();
            let arguments = arguments.unwrap_or_default();
            let arity = arguments.len();
            let base_node_kind = if arity == 0 { kind.clone() } else { node::Kind::Operation };

            // When arguments were not resolved, clear the call information. Otherwise it would be
            // incorrectly assigned to the access chain target span.
            if !args_resolved {
                app_base.set_call_id(None);
            }
            let node = generate_node_for_opr_chain(chain, base_node_kind, app_base, context)?;
            let provided_prefix_arg_count = 0;
            let args_iter = arguments.into_iter();
            Ok(generate_expected_arguments(node, kind, provided_prefix_arg_count, args_iter))
        } else {
            // For non-access infix operators, missing arguments are not handled at this level.
            generate_node_for_opr_chain(chain, kind, app_base, context)
        }
    }
}

fn generate_node_for_opr_chain<T: Payload>(
    this: ast::opr::Chain,
    kind: node::Kind,
    mut app_base: ApplicationBase,
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
        let node = gen.add_node(left_crumbs, node);

        if app_base.uses_method_notation && is_last {
            // For method notation, the target of outermost chain element is considered the `self`
            // argument of the method application.
            let target_arg_info = ArgumentInfo::this(None, app_base.call_id);
            node.set_argument_info(target_arg_info);
        }

        let infix_right_argument_info = if !app_base.uses_method_notation {
            app_base.set_call_id(elem.infix_id);
            app_base.known_arguments(context).and_then(|mut infix_args| {
                // For resolved infix arguments, the arity should always be 2. First always
                // corresponds to already generated node, and second is the argument that is about
                // to be generated.
                if infix_args.len() != 2 {
                    error!("Infix operator should have arity 2, but got {:?}", infix_args.len());
                }

                let infix_left_argument_info = infix_args.pop_front();
                let infix_right_argument_info = infix_args.pop_front();

                if let Some(info) = infix_left_argument_info {
                    node.set_argument_info(info);
                }
                infix_right_argument_info
            })
        } else {
            None
        };

        if has_target {
            gen.generate_empty_node(InsertionPointType::AfterTarget);
        }
        gen.spacing(off);
        gen.generate_ast_node(opr_ast, node::Kind::Operation, context)?;
        if let Some(operand) = &elem.operand {
            let arg_crumbs = elem.crumb_to_operand(has_left);
            let arg_ast = Located::new(arg_crumbs, operand.arg.clone_ref());
            gen.spacing(operand.offset);

            let argument_kind = node::Kind::argument().with_removable(removable);
            let argument = gen.generate_ast_node(arg_ast, argument_kind, context)?;

            if let Some(info) = infix_right_argument_info {
                argument.node.set_argument_info(info);
            }
        }

        gen.generate_empty_node(InsertionPointType::Append);

        if this.operator.right_assoc {
            gen.reverse_children();
        }

        Ok((
            Node {
                kind:          if is_last { kind.clone() } else { node::Kind::chained().into() },
                parenthesized: false,
                size:          gen.current_offset,
                children:      gen.children,
                ast_id:        elem.infix_id,
                payload:       default(),
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
    let app_base = ApplicationBase::from_prefix_chain(this);
    let known_params = app_base.known_arguments(context);
    let uses_method_notation = app_base.uses_method_notation;
    let known_args = known_params.is_some();
    let mut known_params = known_params.unwrap_or_default();

    let prefix_arity = this.args.len().max(known_params.len());

    use ast::crumbs::PrefixCrumb::*;
    // Removing arguments is possible if there at least two of them
    let removable = this.args.len() >= 2;

    // When using method notation, expand the infix access chain manually to maintain correct method
    // application info and avoid generating expected arguments twice. We cannot use the
    // `generate_node` implementation on `GeneralizedInfix`, because it always treats the
    // access chain as a method call without any arguments applied.
    let node = if let Some(infix) = GeneralizedInfix::try_new(&this.func) {
        generate_node_for_opr_chain(infix.flatten(), node::Kind::Operation, app_base, context)
    } else {
        this.func.generate_node(node::Kind::Operation, context)
    };

    let ret = this.args.iter().enumerate().fold(node, |node, (i, arg)| {
        let node = node?;
        let is_first = i == 0;
        let is_last = i + 1 == prefix_arity;
        let arg_kind = if is_first && !uses_method_notation {
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
        if let Some(info) = known_params.pop_front() {
            arg_child.node.set_argument_info(info)
        }
        if !known_args {
            gen.generate_empty_node(InsertionPointType::Append);
        }
        Ok(Node {
            kind:          if is_last { kind.clone() } else { node::Kind::chained().into() },
            parenthesized: false,
            size:          gen.current_offset,
            children:      gen.children,
            ast_id:        arg.prefix_id,
            payload:       default(),
        })
    })?;

    Ok(generate_expected_arguments(ret, kind, this.args.len(), known_params.into_iter()))
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
        kind:          if is_last { kind } else { node::Kind::chained().into() },
        parenthesized: false,
        size:          gen.current_offset,
        children:      gen.children,
        ast_id:        None,
        payload:       default(),
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



// =========================
// === SpanTree for Tree ===
// =========================

fn tree_generate_node<T: Payload>(
    tree: &ast::Tree<Ast>,
    kind: node::Kind,
    context: &impl Context,
    ast_id: Option<Id>,
) -> FallibleResult<Node<T>> {
    let parenthesized = matches!(tree.type_info, ast::TreeType::Group);
    let mut children = vec![];
    let size;
    if let Some(leaf_info) = &tree.leaf_info {
        size = ByteDiff::from(leaf_info.len());
    } else {
        let mut offset = ByteDiff::from(0);
        for (index, raw_span_info) in tree.span_info.iter().enumerate() {
            match raw_span_info {
                SpanSeed::Space(ast::SpanSeedSpace { space }) => offset += ByteDiff::from(space),
                SpanSeed::Token(ast::SpanSeedToken { token }) => {
                    let kind = node::Kind::Token;
                    let size = ByteDiff::from(token.len());
                    let ast_crumbs = vec![ast::crumbs::TreeCrumb { index }.into()];
                    let node = Node { kind, size, ..default() };
                    children.push(node::Child { node, offset, ast_crumbs });
                    offset += size;
                }
                SpanSeed::Child(ast::SpanSeedChild { node }) => {
                    let kind = node::Kind::argument();
                    let node = node.generate_node(kind, context)?;
                    let child_size = node.size;
                    let ast_crumbs = vec![ast::crumbs::TreeCrumb { index }.into()];
                    children.push(node::Child { node, offset, ast_crumbs });
                    offset += child_size;
                }
            }
        }
        size = offset;
    }
    let payload = default();
    Ok(Node { kind, parenthesized, size, children, ast_id, payload })
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

    use ast::crumbs::InfixCrumb;
    use ast::crumbs::PrefixCrumb;
    use ast::crumbs::SectionLeftCrumb;
    use ast::crumbs::SectionRightCrumb;
    use ast::Crumbs;
    use ast::IdMap;
    use parser::Parser;


    /// A helper function which removes information about expression id from thw tree rooted at
    /// `node`.
    ///
    /// It is used in tests. Because parser can assign id as he pleases, therefore to keep tests
    /// cleaner the expression IDs are removed before comparing trees.
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

    #[test]
    fn generating_span_tree() {
        let parser = Parser::new();
        let mut id_map = IdMap::default();
        id_map.generate(0..15);
        id_map.generate(0..11);
        id_map.generate(12..13);
        id_map.generate(14..15);
        id_map.generate(4..11);
        let ast = parser.parse_line_ast_with_id_map("2 + foo bar - 3", id_map.clone()).unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();

        // Check the expression IDs we defined:
        for id_map_entry in id_map.vec {
            let (span, id) = id_map_entry;
            let node = tree.root_ref().find_by_span(&span);
            assert!(node.is_some(), "Node with span {span} not found");
            assert_eq!(node.unwrap().node.ast_id, Some(id), "Span: {span}");
        }

        // Check the other fields:
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);
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

    #[test]
    fn generate_span_tree_with_chains() {
        let parser = Parser::new();
        let ast = parser.parse_line_ast("2 + 3 + foo bar baz 13 + 5").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);

        let expected = TreeBuilder::new(26)
            .add_child(0, 22, node::Kind::chained(), InfixCrumb::LeftOperand)
            .add_child(0, 5, node::Kind::chained(), InfixCrumb::LeftOperand)
            .add_empty_child(0, BeforeTarget)
            .add_leaf(0, 1, node::Kind::this().removable(), InfixCrumb::LeftOperand)
            .add_empty_child(1, AfterTarget)
            .add_leaf(2, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(4, 1, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .add_empty_child(5, Append)
            .done()
            .add_leaf(6, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_child(8, 14, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .add_child(0, 11, node::Kind::chained(), PrefixCrumb::Func)
            .add_child(0, 7, node::Kind::chained(), PrefixCrumb::Func)
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

    #[test]
    #[ignore]
    fn generating_span_tree_from_right_assoc_operator() {
        let parser = Parser::new();
        let ast = parser.parse_line_ast("1<|2<|3").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);
        let expected = TreeBuilder::new(7)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::argument().removable(), InfixCrumb::LeftOperand)
            .add_leaf(1, 2, node::Kind::Operation, InfixCrumb::Operator)
            .add_child(3, 3, node::Kind::chained(), InfixCrumb::RightOperand)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::argument().removable(), InfixCrumb::LeftOperand)
            .add_leaf(1, 2, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(3, AfterTarget)
            .add_leaf(3, 1, node::Kind::this().removable(), InfixCrumb::RightOperand)
            .add_empty_child(4, BeforeTarget)
            .done()
            .build();
        assert_eq!(expected, tree)
    }

    #[test]
    #[ignore]
    fn generating_span_tree_from_right_assoc_section() {
        let parser = Parser::new();
        let ast = parser.parse_line_ast("<|2<|").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);
        let expected = TreeBuilder::new(5)
            .add_empty_child(0, Append)
            .add_leaf(0, 2, node::Kind::Operation, SectionRightCrumb::Opr)
            .add_child(2, 2, node::Kind::chained(), SectionRightCrumb::Arg)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::argument().removable(), SectionLeftCrumb::Arg)
            .add_leaf(1, 1, node::Kind::Operation, SectionLeftCrumb::Opr)
            .add_empty_child(2, BeforeTarget)
            .done()
            .build();
        assert_eq!(expected, tree);
    }

    #[test]
    fn generating_span_tree_for_lambda() {
        let parser = Parser::new();
        let ast = parser.parse_line_ast("foo a-> b + c").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);

        let expected = TreeBuilder::new(13)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_empty_child(4, BeforeTarget)
            .add_leaf(4, 9, node::Kind::this(), PrefixCrumb::Arg)
            .add_empty_child(13, Append)
            .build();

        assert_eq!(expected, tree);
    }

    #[test]
    fn generating_span_tree_for_unfinished_call() {
        let parser = Parser::new();
        let this_param = |call_id| ArgumentInfo {
            name: Some("self".to_owned()),
            tp: Some("Any".to_owned()),
            call_id,
            ..default()
        };
        let param1 = |call_id| ArgumentInfo {
            name: Some("arg1".to_owned()),
            tp: Some("Number".to_owned()),
            call_id,
            ..default()
        };
        let param2 = |call_id| ArgumentInfo {
            name: Some("arg2".to_owned()),
            tp: None,
            call_id,
            ..default()
        };


        // === Single function name ===

        let mut id_map = IdMap::default();
        let call_id = id_map.generate(0..3);
        let ast = parser.parse_line_ast_with_id_map("foo", id_map).unwrap();
        let invocation_info = CalledMethodInfo { parameters: vec![this_param(None)], ..default() };
        let ctx = MockContext::new_single(ast.id.unwrap(), invocation_info);
        let mut tree: SpanTree = SpanTree::new(&ast, &ctx).unwrap();
        match tree.root_ref().leaf_iter().collect_vec().as_slice() {
            [_func, arg0] => assert_eq!(arg0.argument_info(), Some(this_param(Some(call_id)))),
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

        let mut id_map = IdMap::default();
        let call_id = id_map.generate(0..8);
        let ast = parser.parse_line_ast_with_id_map("foo here", id_map).unwrap();
        let invocation_info = CalledMethodInfo { parameters: vec![this_param(None)], ..default() };
        let ctx = MockContext::new_single(ast.id.unwrap(), invocation_info);
        let mut tree: SpanTree = SpanTree::new(&ast, &ctx).unwrap();
        match tree.root_ref().leaf_iter().collect_vec().as_slice() {
            [_func, arg0] => assert_eq!(arg0.argument_info(), Some(this_param(Some(call_id)))),
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

        let mut id_map = IdMap::default();
        let call_id = Some(id_map.generate(0..8));
        let ast = parser.parse_line_ast_with_id_map("foo here", id_map).unwrap();
        let invocation_info = CalledMethodInfo {
            parameters: vec![this_param(None), param1(None), param2(None)],
            ..default()
        };
        let ctx = MockContext::new_single(ast.id.unwrap(), invocation_info);
        let mut tree: SpanTree = SpanTree::new(&ast, &ctx).unwrap();
        match tree.root_ref().leaf_iter().collect_vec().as_slice() {
            [_func, arg0, arg1, arg2] => {
                assert_eq!(arg0.argument_info(), Some(this_param(call_id)));
                assert_eq!(arg1.argument_info(), Some(param1(call_id)));
                assert_eq!(arg2.argument_info(), Some(param2(call_id)));
            }
            sth_else => panic!("There should be 4 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(8)
            .add_child(0, 8, node::Kind::chained(), Crumbs::default())
            .add_child(0, 8, node::Kind::chained(), Crumbs::default())
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
        let mut id_map = IdMap::default();
        let call_id = Some(id_map.generate(0..8));
        let ast = parser.parse_line_ast_with_id_map("here.foo", id_map).unwrap();
        let invocation_info = CalledMethodInfo {
            parameters: vec![this_param(None), param1(None), param2(None)],
            ..default()
        };
        let ctx = MockContext::new_single(ast.id.unwrap(), invocation_info);
        let mut tree: SpanTree = SpanTree::new(&ast, &ctx).unwrap();
        match tree.root_ref().leaf_iter().collect_vec().as_slice() {
            [_, _this, _, _, _func, _, arg1, arg2] => {
                assert_eq!(arg1.argument_info(), Some(param1(call_id)));
                assert_eq!(arg2.argument_info(), Some(param2(call_id)));
            }
            sth_else => panic!("There should be 8 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(8)
            .add_child(0, 8, node::Kind::chained(), Crumbs::default())
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
}
