//! A module containing code related to SpanTree generation.

use crate::prelude::*;
use enso_text::unit::*;

use crate::generate::context::CalledMethodInfo;
use crate::node;
use crate::node::InsertionPointType;
use crate::node::PortId;
use crate::ArgumentInfo;
use crate::Node;
use crate::SpanTree;

use ast::crumbs::BlockCrumb;
use ast::crumbs::InfixCrumb;
use ast::crumbs::Located;
use ast::crumbs::PrefixCrumb;
use ast::crumbs::TreeCrumb;
use ast::opr::GeneralizedInfix;
use ast::opr::NamedArgumentDef;
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
pub trait SpanTreeGenerator {
    /// Generate node with its whole subtree.
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node>;

    /// Generate tree for this AST treated as root for the whole expression.
    fn generate_tree(&self, context: &impl Context) -> FallibleResult<SpanTree> {
        let root = self.generate_node(node::Kind::Root, context)?;
        let port_id = root.port_id.or(Some(PortId::Root));
        let root = root.with_port_id(port_id);
        Ok(SpanTree { root })
    }
}



// ==============
// === String ===
// ==============

impl SpanTreeGenerator for &str {
    fn generate_node(&self, kind: impl Into<node::Kind>, _: &impl Context) -> FallibleResult<Node> {
        Ok(Node::new().with_kind(kind.into()).with_size(self.chars().count().into()))
    }
}

impl SpanTreeGenerator for String {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node> {
        self.as_str().generate_node(kind, context)
    }
}



// =================
// === Utilities ===
// =================

// === Child Generator ===

/// An utility to generate children with increasing offsets.
#[derive(Debug, Derivative)]
#[derivative(Default(bound = ""))]
struct ChildGenerator {
    current_offset: ByteDiff,
    sibling_offset: ByteDiff,
    children:       Vec<node::Child>,
}

impl ChildGenerator {
    /// Add spacing to current generator state. It will be taken into account for the next generated
    /// children's offsets
    fn spacing(&mut self, size: usize) {
        let offset = (size as i32).byte_diff();
        self.current_offset += offset;
        self.sibling_offset += offset;
    }

    fn generate_ast_node(
        &mut self,
        child_ast: Located<&Ast>,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<&mut node::Child> {
        let kind = kind.into();
        let node = child_ast.item.generate_node(kind, context)?;
        Ok(self.add_node(child_ast.crumbs, node))
    }

    fn add_node(&mut self, ast_crumbs: ast::Crumbs, node: Node) -> &mut node::Child {
        let parent_offset = self.current_offset;
        let sibling_offset = self.sibling_offset;
        let child = node::Child { node, parent_offset, sibling_offset, ast_crumbs };
        self.current_offset += child.node.size;
        self.sibling_offset = 0.byte_diff();
        self.children.push(child);
        self.children.last_mut().unwrap()
    }

    fn generate_empty_node(&mut self, insert_type: InsertionPointType) -> &mut node::Child {
        self.add_node(vec![], Node::new().with_kind(insert_type))
    }

    fn reverse_children(&mut self) {
        self.children.reverse();
        let mut last_parent_offset = 0.byte_diff();
        for child in &mut self.children {
            child.parent_offset = self.current_offset - child.parent_offset - child.node.size;
            child.sibling_offset = child.parent_offset - last_parent_offset;
            last_parent_offset = child.parent_offset;
        }
    }

    fn into_node(self, kind: impl Into<node::Kind>, ast_id: Option<Id>) -> Node {
        let kind = kind.into();
        let size = self.current_offset;
        let children = self.children;
        Node::new().with_kind(kind).with_size(size).with_children(children).with_ast_id(ast_id)
    }
}



// =============================
// === Trait Implementations ===
// =============================

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

    /// Get the list of method arguments expected by this application. Returns `None` when the call
    /// info is not present in the context or AST doesn't contain enough information to query it.
    fn resolve(&self, context: &impl Context) -> Option<ResolvedApplication> {
        // If method notation is used, the function name is required to get relevant call info. Do
        // not attempt the call if method name is not available, as the returned data would be not
        // relevant.
        if self.uses_method_notation && self.function_name.is_none() {
            return None;
        }

        let invocation_info = context.call_info(self.call_id?)?;
        let invocation_info = invocation_info.with_call_id(self.call_id);

        let self_in_access = || {
            !invocation_info.is_constructor
                && (!invocation_info.called_on_type.unwrap_or(false) || invocation_info.is_static)
        };

        let first_argument_is_self = || {
            let name = invocation_info.parameters.first().and_then(|arg| arg.name.as_ref());
            name.map_or(false, |name| name == node::Argument::THIS)
        };

        // When a method notation is used on non-static invocation, the first `self` argument is
        // positioned the access chain.
        let uses_method_notation = self.uses_method_notation;
        let has_argument_in_access =
            uses_method_notation && self_in_access() && first_argument_is_self();

        let mut param_iter = invocation_info.parameters.into_iter();
        let argument_in_access = if has_argument_in_access { param_iter.next() } else { None };
        let chain_arguments: VecDeque<(usize, ArgumentInfo)> = param_iter.enumerate().collect();
        let icon_name = invocation_info.icon_name;
        let suggestion_id = invocation_info.suggestion_id;
        Some(ResolvedApplication { argument_in_access, chain_arguments, suggestion_id, icon_name })
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

    fn raw_node_data(&self) -> crate::node::ApplicationData {
        crate::node::ApplicationData {
            suggestion_id:  None,
            icon_name:      None,
            self_in_access: self.uses_method_notation,
        }
    }
}

/// A result of resolving arguments of [`ApplicationBase`]. It contains information about the
/// expected arguments and their positions in the application.
struct ResolvedApplication {
    /// The ID of a resolved method suggestion used in this application, if available.
    suggestion_id:      Option<usize>,
    argument_in_access: Option<ArgumentInfo>,
    /// Arguments are paired with their original positions in the chain. That way the position
    /// information is preserved even after the arguments are popped or removed from deque in
    /// arbitrary order.
    chain_arguments:    VecDeque<(usize, ArgumentInfo)>,
    /// The icon to display on node when this is its main application.
    icon_name:          Option<ImString>,
}

impl ResolvedApplication {
    fn expect_infix(&self) {
        if self.chain_arguments.len() != 2 {
            error!("Infix operator should have arity 2, but got {:?}", self.chain_arguments.len());
        }
    }
    fn take_argument_in_access(&mut self) -> Option<ArgumentInfo> {
        self.argument_in_access.take()
    }
    fn next_argument_name(&self) -> Option<&str> {
        self.chain_arguments.front().and_then(|(_, info)| info.name.as_deref())
    }
    fn take_named_argument(&mut self, name: &str) -> Option<(usize, ArgumentInfo)> {
        self.chain_arguments
            .iter()
            .position(|(_, info)| info.name.as_deref() == Some(name))
            .map(|pos| self.chain_arguments.remove(pos).unwrap())
    }
    fn take_next_argument(&mut self) -> Option<(usize, ArgumentInfo)> {
        self.chain_arguments.pop_front()
    }
    fn take_next_maybe_named_argument(
        &mut self,
        name: Option<&str>,
    ) -> Option<(usize, ArgumentInfo)> {
        if let Some(name) = name {
            self.take_named_argument(name)
        } else {
            self.take_next_argument()
        }
    }
    fn put_back_argument(&mut self, argument: (usize, ArgumentInfo)) {
        self.chain_arguments.push_front(argument)
    }

    fn node_data(&self) -> crate::node::ApplicationData {
        crate::node::ApplicationData {
            suggestion_id:  self.suggestion_id,
            icon_name:      self.icon_name.clone(),
            self_in_access: self.argument_in_access.is_some(),
        }
    }
}

// === AST ===

impl SpanTreeGenerator for Ast {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node> {
        generate_node_for_ast(self, kind.into(), context)
    }
}

fn generate_node_for_ast(
    ast: &Ast,
    kind: node::Kind,
    context: &impl Context,
) -> FallibleResult<Node> {
    if let Some(infix) = GeneralizedInfix::try_new(ast) {
        infix.generate_node(kind, context)
    } else {
        match ast.shape() {
            ast::Shape::Prefix(_) =>
                ast::prefix::Chain::from_ast(ast).unwrap().generate_node(kind, context),
            ast::Shape::Tree(tree) => tree_generate_node(tree, kind, context, ast.id),
            ast::Shape::Block(block) => block_generate_node(block, kind, context, ast.id),
            _ => {
                let size = (ast.repr_len().value as i32).byte_diff();
                let ast_id = ast.id;
                if let Some(info) = ast_id.and_then(|id| context.call_info(id)) {
                    let node = Node::new()
                        .with_kind(node::Kind::Operation)
                        .with_size(size)
                        .with_ast_id(ast_id);
                    // Note that in this place it is impossible that Ast is in form of
                    // `this.method` -- it is covered by the former if arm. As such, we don't
                    // need to use `ApplicationBase` here as we do elsewhere.
                    let params = info.with_call_id(ast_id).parameters.into_iter();
                    Ok(generate_trailing_expected_arguments(node, params).with_kind(kind))
                } else {
                    Ok(Node::new().with_kind(kind).with_size(size).with_ast_id(ast_id))
                }
            }
        }
    }
}


// === Operators (Sections and Infixes) ===

impl SpanTreeGenerator for GeneralizedInfix {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node> {
        // Code like `ast.func` or `a+b+c`.
        let chain = self.flatten();
        let kind = kind.into();
        let mut app_base = ApplicationBase::from_infix(self);
        let mut application = app_base.resolve(context);
        let node_application =
            application.as_ref().map_or_else(|| app_base.raw_node_data(), |r| r.node_data());
        if app_base.uses_method_notation {
            // This is a standalone method access chain, missing method parameters needs to be
            // handled here. It is guaranteed that no existing prefix arguments are present, as
            // method calls inside prefix chains are handled by `generate_node_for_prefix_chain` and
            // never reach this point.
            let has_arguments =
                application.as_ref().map_or(false, |r| !r.chain_arguments.is_empty());

            let base_node_kind = if has_arguments { node::Kind::Operation } else { kind.clone() };

            // When arguments were not resolved, clear the call information. Otherwise it would be
            // incorrectly assigned to the access chain target span.
            if application.is_none() {
                app_base.set_call_id(None);
            }
            let node = generate_node_for_opr_chain(
                chain,
                base_node_kind,
                app_base,
                application.as_mut(),
                context,
            )?;

            let argument_infos =
                application.map_or_default(|i| i.chain_arguments).into_iter().map(|(_, info)| info);
            Ok(generate_trailing_expected_arguments(node, argument_infos)
                .with_kind(kind)
                .with_application(node_application))
        } else {
            // For non-access infix operators, missing arguments are not handled at this level.
            generate_node_for_opr_chain(chain, kind, app_base, application.as_mut(), context)
        }
    }
}

fn generate_node_for_opr_chain(
    this: ast::opr::Chain,
    kind: node::Kind,
    mut app_base: ApplicationBase,
    mut application: Option<&mut ResolvedApplication>,
    context: &impl Context,
) -> FallibleResult<Node> {
    // Allow removing the chain elements as long as it doesn't destroy the chain.
    let removable = this.args.len() + (this.target.is_some() as usize) > 2;

    let node_and_offset: FallibleResult<(Node, usize)> = match &this.target {
        Some(target) => {
            let kind = node::Kind::argument().with_removable(removable);
            let node = target.arg.generate_node(kind, context)?;
            Ok((node, target.offset))
        }
        None => Ok((Node::new().with_kind(InsertionPointType::BeforeArgument(0)), 0)),
    };

    // In this fold we pass last generated node and offset after it, wrapped in Result.
    let (node, _) = this.args.iter().enumerate().fold(node_and_offset, |result, (i, elem)| {
        // Here we generate children as the operator would be left-associative. Then, if it is
        // actually right associative, we just reverse the generated children and their offsets.
        let (node, off) = result?;
        let is_first = i == 0;
        let is_last = i + 1 == this.args.len();
        let has_left = !node.is_insertion_point();
        let opr_crumbs = elem.crumb_to_operator(has_left);
        let opr_ast = Located::new(opr_crumbs, elem.operator.ast());
        let left_crumbs = if has_left { vec![elem.crumb_to_previous()] } else { vec![] };

        let mut gen = ChildGenerator::default();
        if is_first && has_left && !app_base.uses_method_notation {
            gen.generate_empty_node(InsertionPointType::BeforeArgument(0));
        }
        let node = gen.add_node(left_crumbs, node);

        if app_base.uses_method_notation && is_last {
            // For method notation, the target of outermost chain element is considered the `self`
            // argument of the method application.

            // Even if the invocation is not resolved or doesn't actually use access position as
            // argument, we still want to generate it as an argument span. This allows it to be used
            // in widget queries before actual `self` argument is present or if it doesn't exist at
            // all, which is the case for type constructors.
            let fallback_target_arg_info = ArgumentInfo { call_id: app_base.call_id, ..default() };
            let target_arg_info = application
                .as_deref_mut()
                .and_then(|inv| inv.take_argument_in_access())
                .unwrap_or(fallback_target_arg_info);
            node.set_argument_info(target_arg_info);
            if let node::Kind::Argument(arg) = &mut node.kind {
                arg.removable = false;
            }
        }

        let infix_right_argument_info = if !app_base.uses_method_notation {
            app_base.set_call_id(elem.infix_id);
            app_base.resolve(context).and_then(|mut resolved| {
                // For resolved infix arguments, the arity should always be 2. First always
                // corresponds to already generated node, and second is the argument that is about
                // to be generated.
                resolved.expect_infix();
                let infix_left_argument_info = resolved.take_next_argument();
                let infix_right_argument_info = resolved.take_next_argument();

                if let Some((index, info)) = infix_left_argument_info {
                    node.set_argument_info(info);
                    node.set_definition_index(index);
                }
                infix_right_argument_info
            })
        } else {
            None
        };

        gen.spacing(off);
        gen.generate_ast_node(opr_ast, node::Kind::Operation, context)?;
        if let Some(operand) = &elem.operand {
            let arg_crumbs = elem.crumb_to_operand(has_left);
            let arg_ast = Located::new(arg_crumbs, &operand.arg);

            if has_left && !app_base.uses_method_notation {
                gen.generate_empty_node(InsertionPointType::BeforeArgument(i + 1));
            }
            gen.spacing(operand.offset);

            let argument_kind: node::Kind = if app_base.uses_method_notation {
                node::Kind::Access
            } else {
                node::Kind::argument().with_removable(removable).into()
            };
            let argument = gen.generate_ast_node(arg_ast, argument_kind, context)?;

            if let Some((index, info)) = infix_right_argument_info {
                argument.node.set_argument_info(info);
                argument.node.set_definition_index(index);
            }
        }

        if is_last && !app_base.uses_method_notation {
            gen.generate_empty_node(InsertionPointType::Append);
        }

        if this.operator.right_assoc {
            gen.reverse_children();
        }

        let kind = if is_last { kind.clone() } else { node::Kind::chained_infix() };
        Ok((gen.into_node(kind, elem.infix_id), elem.offset))
    })?;
    Ok(node)
}


// === Application ===

impl SpanTreeGenerator for ast::prefix::Chain {
    fn generate_node(
        &self,
        kind: impl Into<node::Kind>,
        context: &impl Context,
    ) -> FallibleResult<Node> {
        generate_node_for_prefix_chain(self, kind.into(), context)
    }
}

fn generate_node_for_prefix_chain(
    this: &ast::prefix::Chain,
    kind: node::Kind,
    context: &impl Context,
) -> FallibleResult<Node> {
    let app_base = ApplicationBase::from_prefix_chain(this);

    // If actual method arguments are not resolved, we still want to assign correct call ID to all
    // argument spans. This is required for correct handling of span tree actions, as it is used to
    // determine correct reinsertion point for removed span.
    let fallback_call_id = app_base.call_id;
    let mut application = app_base.resolve(context);
    let node_application =
        application.as_ref().map_or_else(|| app_base.raw_node_data(), |r| r.node_data());

    // When using method notation, expand the infix access chain manually to maintain correct method
    // application info and avoid generating expected arguments twice. We cannot use the
    // `generate_node` implementation on `GeneralizedInfix`, because it always treats the
    // access chain as a method call without any arguments applied.
    let node = if let Some(infix) = GeneralizedInfix::try_new(&this.func) {
        generate_node_for_opr_chain(
            infix.flatten(),
            node::Kind::Operation,
            app_base,
            application.as_mut(),
            context,
        )
    } else {
        this.func.generate_node(node::Kind::Operation, context)
    };

    let argument_positions = resolve_argument_positions(&this.args, application);
    let num_positions = argument_positions.len();

    let mut inserted_arguments = 0;

    argument_positions
        .into_iter()
        .enumerate()
        .fold(node, |node, (i, position)| {
            let node = node?;
            let is_first = i == 0;
            let is_last = i == num_positions - 1;
            let is_only_argument = is_first && is_last;

            match position {
                ArgumentPosition::ChainArgument { arg, named, info } => {
                    let mut gen = ChildGenerator::default();
                    gen.add_node(vec![PrefixCrumb::Func.into()], node);

                    let arg_name = named.as_ref().map(|named| named.name);

                    // Allow removing the prefix chain arguments only if it doesn't destroy the
                    // chain or influence the argument logical positions. We can always remove
                    // resolved arguments, as the remaining positions will be rewritten using known
                    // argument names and placeholders will be inserted. For unresolved arguments,
                    // we can remove them only if removal preserves the prefix application.
                    let resolved = info.is_some();
                    let removable = resolved || !is_only_argument;

                    let mut arg_kind = node::Kind::from(
                        node::Kind::argument()
                            .in_prefix_chain()
                            .with_removable(removable)
                            .with_name(arg_name.map(ToString::to_string)),
                    );

                    if let Some((index, info)) = info {
                        arg_kind.set_argument_info(info);
                        arg_kind.set_definition_index(index);
                    } else {
                        arg_kind.set_call_id(fallback_call_id);
                    }

                    if !resolved {
                        gen.generate_empty_node(InsertionPointType::BeforeArgument(
                            inserted_arguments,
                        ));
                    }
                    inserted_arguments += 1;
                    gen.spacing(arg.sast.off);

                    // For named arguments, we need to generate the named argument span-tree
                    // structure. The actual argument node is nested inside the named argument.
                    match named {
                        Some(named) => {
                            let node = generate_node_for_named_argument(named, arg_kind, context)?;
                            gen.add_node(vec![PrefixCrumb::Arg.into()], node);
                        }
                        None => {
                            let arg_ast = Located::new(PrefixCrumb::Arg, &arg.sast.wrapped);
                            gen.generate_ast_node(arg_ast, arg_kind, context)?;
                        }
                    }

                    if is_last && !resolved {
                        gen.generate_empty_node(InsertionPointType::Append);
                    }

                    Ok(gen.into_node(node::Kind::ChainedPrefix, arg.prefix_id))
                }
                ArgumentPosition::Placeholder { info, named } =>
                    Ok(generate_expected_argument(node, named, i, info)),
            }
        })
        .map(|node: Node| node.with_kind(kind).with_application(node_application))
}

fn generate_node_for_named_argument(
    this: NamedArgumentDef<'_>,
    arg_kind: node::Kind,
    context: &impl Context,
) -> FallibleResult<Node> {
    let NamedArgumentDef { id, larg, loff, opr, roff, rarg, .. } = this;
    let mut gen = ChildGenerator::default();

    gen.generate_ast_node(Located::new(InfixCrumb::LeftOperand, larg), node::Kind::Token, context)?;
    gen.spacing(loff);
    gen.generate_ast_node(Located::new(InfixCrumb::Operator, opr), node::Kind::Token, context)?;
    gen.spacing(roff);
    let arg_ast = Located::new(InfixCrumb::RightOperand, rarg);
    gen.generate_ast_node(arg_ast, arg_kind, context)?;
    Ok(gen.into_node(node::Kind::NamedArgument, id))
}

#[derive(Debug)]
enum ArgumentPosition<'a> {
    ChainArgument {
        arg:   &'a ast::prefix::Argument,
        named: Option<NamedArgumentDef<'a>>,
        info:  Option<(usize, ArgumentInfo)>,
    },
    Placeholder {
        info:  ArgumentInfo,
        named: bool,
    },
}

impl<'a> ArgumentPosition<'a> {
    fn not_resolved(arg: &'a ast::prefix::Argument) -> Self {
        ArgumentPosition::ChainArgument {
            arg,
            named: ast::opr::match_named_argument(&arg.sast.wrapped),
            info: None,
        }
    }
}

/// Resolves the positions of arguments in a chain, including placeholders for missing arguments.
/// The placeholders are inserted in the order of the known parameters, at the first position that
/// allows them to be inserted without changing the meaning of other parameters.
fn resolve_argument_positions(
    chain_args: &[ast::prefix::Argument],
    application: Option<ResolvedApplication>,
) -> Vec<ArgumentPosition<'_>> {
    // When application is not resolved, do not generate any placeholders.
    let Some(mut application) = application else {
        return chain_args.iter().map(ArgumentPosition::not_resolved).collect();
    };

    // The capacity estimate can be too low if there are named arguments in the chain that don't
    // actually match any known parameters. This is fine, as the vector will grow as needed.
    let resolved_capacity_estimate = chain_args.len().max(application.chain_arguments.len());
    let mut resolved = Vec::with_capacity(resolved_capacity_estimate);

    let last_positional_arg_index = chain_args
        .iter()
        .rposition(|arg| ast::opr::match_named_argument(&arg.sast.wrapped).is_none());

    let mut placeholder_already_inserted = false;

    // always insert a placeholder for the missing argument at the first position that is legal
    // and don't invalidate further named arguments, treating the named arguments at correct
    // position as if they were positional. foo a b c [d]
    for (position, arg) in chain_args.iter().enumerate() {
        let past_positional_arguments = last_positional_arg_index.map_or(true, |i| position > i);
        let next_position_name: Option<&str> = application.next_argument_name();

        let named_argument = ast::opr::match_named_argument(&arg.sast.wrapped);
        match named_argument {
            Some(named) if past_positional_arguments && Some(named.name) != next_position_name => {
                // Named argument that is not in its natural position, and there are no more
                // positional arguments to emit in the chain. At this point placeholders can be
                // inserted. We need to figure out which placeholders can be inserted before
                // emitting this named argument.

                // all remaining arguments must be named, as we are past all positional arguments.
                let remaining_arguments = &chain_args[position..];

                // For each subsequent argument in its current natural position, insert a
                // placeholder. Do that only if the argument is not defined further in the chain.
                while let Some((index, info)) = application.take_next_argument() {
                    let can_be_inserted = info.name.as_ref().map_or(false, |name| {
                        let is_defined_further = remaining_arguments
                            .iter()
                            .filter_map(|arg| ast::opr::match_named_argument(&arg.sast.wrapped))
                            .any(|named| named.name == name);
                        !is_defined_further
                    });

                    if can_be_inserted {
                        let named = placeholder_already_inserted;
                        placeholder_already_inserted = true;
                        resolved.push(ArgumentPosition::Placeholder { info, named });
                    } else {
                        application.put_back_argument((index, info));
                        break;
                    }
                }

                // Finally, we want to emit the named argument and remove it from the list of
                // remaining known params.
                let info = application.take_named_argument(named.name);
                resolved.push(ArgumentPosition::ChainArgument { arg, named: Some(named), info })
            }
            named => {
                let name = named.as_ref().map(|named| named.name);
                let info = application.take_next_maybe_named_argument(name);
                resolved.push(ArgumentPosition::ChainArgument { arg, named, info })
            }
        }
    }

    // If there are any remaining known parameters, they must be inserted as trailing placeholders.
    let remaining_arguments = application.chain_arguments.into_iter();
    resolved.extend(remaining_arguments.map(|(_, info)| {
        let named = placeholder_already_inserted;
        placeholder_already_inserted = true;
        ArgumentPosition::Placeholder { info, named }
    }));

    resolved
}


// === Common Utility ==

/// Build a prefix application-like span tree structure where the prefix argument has not been
/// provided but instead its information is known from method's ArgumentInfo.
///
/// `index` is the argument's position in the prefix chain which may be different from parameter
/// index in the method's parameter list.
fn generate_expected_argument(
    node: Node,
    named: bool,
    index: usize,
    argument_info: ArgumentInfo,
) -> Node {
    let mut gen = ChildGenerator::default();
    let extended_ast_id = node.ast_id.or(node.extended_ast_id);
    gen.add_node(ast::Crumbs::new(), node);
    let port_id = argument_info.call_id.map(|id| PortId::ArgPlaceholder { application: id, index });
    let arg_node = gen.generate_empty_node(InsertionPointType::ExpectedArgument { index, named });
    arg_node.node.set_argument_info(argument_info);
    arg_node.node.set_port_id(port_id);
    gen.into_node(node::Kind::chained_infix(), None).with_extended_ast_id(extended_ast_id)
}

/// Build a prefix application-like span tree structure where no prefix argument has been provided
/// so far. All prefix arguments will be generated as expected arguments..
fn generate_trailing_expected_arguments(
    node: Node,
    expected_args: impl Iterator<Item = ArgumentInfo>,
) -> Node {
    let mut inserted_any_argument = false;
    expected_args.enumerate().fold(node, |node, (index, parameter)| {
        inserted_any_argument = true;
        let named = index != 0;
        generate_expected_argument(node, named, index, parameter)
    })
}

/// A single child node produced out of the lambda argument and the `->` token.
#[derive(Debug)]
struct FoldedLambdaArguments {
    /// Both the lambda argument and the `->` token, as a single [`node::Kind::Token`] node.
    child:          node::Child,
    /// The number of tree nodes that were folded into the `child`.
    nodes_replaced: usize,
}

/// Fold the lambda arguments into a single [`node::Kind::Token`] node.
/// It is needed to ignore lambda arguments as connection targets, but still generate a valid
/// SpanTree from the lambda body.
fn fold_lambda_arguments(tree: &ast::Tree<Ast>) -> FoldedLambdaArguments {
    let is_arrow = |span_info| matches!(span_info, SpanSeed::Token(ast::SpanSeedToken { token }) if token == "->");
    let arrow_index = tree.span_info.iter().cloned().position(is_arrow).unwrap_or(0);
    let bytes_till_body = tree
        .span_info
        .iter()
        .take(arrow_index + 1)
        .map(|raw_span_info| match raw_span_info {
            SpanSeed::Space(ast::SpanSeedSpace { space }) => ByteDiff::from(space),
            SpanSeed::Token(ast::SpanSeedToken { token }) => ByteDiff::from(token.len()),
            SpanSeed::Child(ast::SpanSeedChild { node }) => node.repr_len().to_diff(),
        })
        .sum::<ByteDiff>();
    let size = bytes_till_body;
    let kind = node::Kind::Token;
    let node = Node::new().with_kind(kind).with_size(size);
    let ast_crumbs = vec![TreeCrumb { index: 0 }.into()];
    let nodes_replaced = arrow_index + 1;
    let child = node::Child {
        node,
        parent_offset: ByteDiff::from(0),
        sibling_offset: ByteDiff::from(0),
        ast_crumbs,
    };
    FoldedLambdaArguments { child, nodes_replaced }
}

fn tree_generate_node(
    tree: &ast::Tree<Ast>,
    kind: node::Kind,
    context: &impl Context,
    ast_id: Option<Id>,
) -> FallibleResult<Node> {
    let mut children = vec![];
    let size;
    if let Some(leaf_info) = &tree.leaf_info {
        size = ByteDiff::from(leaf_info.len());
    } else {
        let mut parent_offset = ByteDiff::from(0);
        let mut sibling_offset = ByteDiff::from(0);
        let first_token_or_child =
            tree.span_info.iter().find(|span| !matches!(span, SpanSeed::Space(_)));
        let is_array = matches!(first_token_or_child, Some(SpanSeed::Token(ast::SpanSeedToken { token })) if token == "[");
        let array_id = ast_id.filter(|_| is_array);
        let mut insert_port_iter = (0..)
            .map_while(|insert_at| array_id.map(|array| PortId::ArrayInsert { array, insert_at }));

        let last_token_index =
            tree.span_info.iter().rposition(|span| matches!(span, SpanSeed::Token(_)));

        // If the node is a lambda, we fold the lambda arguments into a single child node,
        // and then continue handling the lambda body as usual.
        let skip = if tree.type_info == ast::TreeType::Lambda {
            let FoldedLambdaArguments { child, nodes_replaced } = fold_lambda_arguments(tree);
            parent_offset += child.node.size;
            children.push(child);
            nodes_replaced
        } else {
            0
        };
        for (index, raw_span_info) in tree.span_info.iter().skip(skip).enumerate() {
            let index = index + skip;
            match raw_span_info {
                SpanSeed::Space(ast::SpanSeedSpace { space }) => {
                    parent_offset += ByteDiff::from(space);
                    sibling_offset += ByteDiff::from(space);
                }
                SpanSeed::Token(ast::SpanSeedToken { token }) => {
                    if is_array && Some(index) == last_token_index {
                        children.push(node::Child {
                            node: Node::new()
                                .with_kind(InsertionPointType::Append)
                                .with_port_id(insert_port_iter.next()),
                            parent_offset,
                            sibling_offset,
                            ast_crumbs: vec![],
                        });
                        sibling_offset = 0.byte_diff();
                    }
                    let kind = node::Kind::Token;
                    let size = ByteDiff::from(token.len());
                    let ast_crumbs = vec![TreeCrumb { index }.into()];
                    let node = Node::new().with_kind(kind).with_size(size);
                    children.push(node::Child { node, parent_offset, sibling_offset, ast_crumbs });
                    parent_offset += size;
                    sibling_offset = 0.byte_diff();
                }
                SpanSeed::Child(ast::SpanSeedChild { node }) => {
                    if is_array {
                        children.push(node::Child {
                            node: Node::new()
                                .with_kind(InsertionPointType::BeforeArgument(index))
                                .with_port_id(insert_port_iter.next()),
                            parent_offset,
                            sibling_offset,
                            ast_crumbs: vec![],
                        });
                        sibling_offset = 0.byte_diff();
                    }

                    let kind = node::Kind::argument().with_removable(is_array);
                    let node = node.generate_node(kind, context)?;
                    let child_size = node.size;
                    let ast_crumbs = vec![TreeCrumb { index }.into()];
                    children.push(node::Child { node, parent_offset, sibling_offset, ast_crumbs });
                    parent_offset += child_size;
                    sibling_offset = 0.byte_diff();
                }
            }
        }
        size = parent_offset;
    }

    let tree_type = Some(tree.type_info.clone());
    Ok(Node { kind, tree_type, size, children, ..default() }.with_ast_id(ast_id))
}

fn block_generate_node(
    block: &ast::Block<Ast>,
    kind: node::Kind,
    context: &impl Context,
    ast_id: Option<Id>,
) -> FallibleResult<Node> {
    let mut gen = ChildGenerator::default();
    let newline = Node::new().with_kind(node::Kind::Token).with_size(1.byte_diff());

    gen.add_node(vec![], newline.clone());
    for empty_line_space in &block.empty_lines {
        gen.spacing(*empty_line_space);
        gen.add_node(vec![], newline.clone());
    }
    gen.spacing(block.indent);
    let first_line = Located::new(BlockCrumb::HeadLine, &block.first_line.elem);
    gen.generate_ast_node(first_line, node::Kind::BlockLine, context)?;
    gen.spacing(block.first_line.off);
    for (tail_index, line) in block.lines.iter().enumerate() {
        gen.add_node(vec![], newline.clone());
        if let Some(elem) = &line.elem {
            gen.spacing(block.indent);
            let line = Located::new(BlockCrumb::TailLine { tail_index }, elem);
            gen.generate_ast_node(line, node::Kind::BlockLine, context)?;
        }
        gen.spacing(line.off);
    }
    Ok(gen.into_node(kind, ast_id))
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
    fn call_info(&self, id: Id) -> Option<CalledMethodInfo> {
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
    use crate::node::InsertionPoint;
    use crate::node::InsertionPointType::*;
    use crate::ArgumentInfo;

    use ast::crumbs::InfixCrumb;
    use ast::crumbs::PrefixCrumb;
    use ast::crumbs::SectionLeftCrumb;
    use ast::crumbs::SectionRightCrumb;
    use ast::Crumbs;
    use ast::IdMap;
    use parser::Parser;
    use pretty_assertions::assert_eq;


    /// A helper function which removes information about expression id from thw tree rooted at
    /// `node`.
    ///
    /// It is used in tests. Because parser can assign id as he pleases, therefore to keep tests
    /// cleaner the expression IDs are removed before comparing trees.
    fn clear_expression_ids(node: &mut Node) {
        node.ast_id = None;
        node.extended_ast_id = None;
        node.port_id = None;
        node.application = None;
        for child in &mut node.children {
            clear_expression_ids(&mut child.node);
        }
    }

    /// A helper function which removes parameter information from nodes.
    ///
    /// It is used in tests. Because constructing trees with set parameter infos is troublesome,
    /// it is often more convenient to test them separately and then erase infos and test for shape.
    fn clear_parameter_infos(node: &mut Node) {
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
            .add_empty_child(0, BeforeArgument(0))
            .add_child(0, 11, node::Kind::argument(), InfixCrumb::LeftOperand)
            .add_empty_child(0, BeforeArgument(0))
            .add_leaf(0, 1, node::Kind::argument(), InfixCrumb::LeftOperand)
            .add_leaf(2, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(3, BeforeArgument(1))
            .add_child(4, 7, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_empty_child(3, BeforeArgument(0))
            .add_leaf(4, 3, node::Kind::prefix_argument(), PrefixCrumb::Arg)
            .add_empty_child(7, Append)
            .done()
            .add_empty_child(11, Append)
            .done()
            .add_leaf(12, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(13, BeforeArgument(1))
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
            .add_child(0, 22, node::Kind::chained_infix(), InfixCrumb::LeftOperand)
            .add_child(0, 5, node::Kind::chained_infix(), InfixCrumb::LeftOperand)
            .add_empty_child(0, BeforeArgument(0))
            .add_leaf(0, 1, node::Kind::argument().removable(), InfixCrumb::LeftOperand)
            .add_leaf(2, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(3, BeforeArgument(1))
            .add_leaf(4, 1, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .done()
            .add_leaf(6, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(7, BeforeArgument(2))
            .add_child(8, 14, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .add_child(0, 11, node::Kind::ChainedPrefix, PrefixCrumb::Func)
            .add_child(0, 7, node::Kind::ChainedPrefix, PrefixCrumb::Func)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_empty_child(3, BeforeArgument(0))
            .add_leaf(4, 3, node::Kind::prefix_argument().removable(), PrefixCrumb::Arg)
            .done()
            .add_empty_child(7, BeforeArgument(1))
            .add_leaf(8, 3, node::Kind::prefix_argument().removable(), PrefixCrumb::Arg)
            .done()
            .add_empty_child(11, BeforeArgument(2))
            .add_leaf(12, 2, node::Kind::prefix_argument().removable(), PrefixCrumb::Arg)
            .add_empty_child(14, Append)
            .done()
            .done()
            .add_leaf(23, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(24, BeforeArgument(3))
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
            .add_child(3, 3, node::Kind::chained_infix(), InfixCrumb::RightOperand)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::argument().removable(), InfixCrumb::LeftOperand)
            .add_leaf(1, 2, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(3, 1, node::Kind::argument().removable(), InfixCrumb::RightOperand)
            .add_empty_child(4, BeforeArgument(0))
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
            .add_child(2, 2, node::Kind::chained_infix(), SectionRightCrumb::Arg)
            .add_empty_child(0, Append)
            .add_leaf(0, 1, node::Kind::argument().removable(), SectionLeftCrumb::Arg)
            .add_leaf(1, 1, node::Kind::Operation, SectionLeftCrumb::Opr)
            .add_empty_child(2, BeforeArgument(0))
            .done()
            .build();
        assert_eq!(expected, tree);
    }

    #[test]
    fn generating_span_tree_for_lambda() {
        let parser = Parser::new();


        // === Simple lambda ===

        let ast = parser.parse_line_ast("foo a-> b + c").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);

        let expected = TreeBuilder::new(13)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_empty_child(3, BeforeArgument(0))
            .add_child(4, 9, node::Kind::prefix_argument(), PrefixCrumb::Arg)
            .set_tree_type(Some(ast::TreeType::Lambda))
            .add_leaf(0, 3, node::Kind::Token, TreeCrumb { index: 0 })
            .add_child(4, 5, node::Kind::argument(), TreeCrumb { index: 3 })
            .add_empty_child(0, BeforeArgument(0))
            .add_leaf(0, 1, node::Kind::argument(), InfixCrumb::LeftOperand)
            .add_leaf(2, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(3, BeforeArgument(1))
            .add_leaf(4, 1, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_empty_child(5, Append)
            .done()
            .done()
            .add_empty_child(13, Append)
            .build();
        assert_eq!(expected, tree);


        // === Lambda with two arguments ===

        let ast = parser.parse_line_ast("foo a->b-> a + b").unwrap();
        let mut tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);

        let expected = TreeBuilder::new(16)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_empty_child(3, BeforeArgument(0))
            .add_child(4, 12, node::Kind::prefix_argument(), PrefixCrumb::Arg)
            .set_tree_type(Some(ast::TreeType::Lambda))
            .add_leaf(0, 3, node::Kind::Token, TreeCrumb { index: 0 })
            .add_child(3, 9, node::Kind::argument(), TreeCrumb { index: 2 })
            .set_tree_type(Some(ast::TreeType::Lambda))
            .add_leaf(0, 3, node::Kind::Token, TreeCrumb { index: 0 })
            .add_child(4, 5, node::Kind::argument(), TreeCrumb { index: 3 })
            .add_empty_child(0, BeforeArgument(0))
            .add_leaf(0, 1, node::Kind::argument(), InfixCrumb::LeftOperand)
            .add_leaf(2, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_empty_child(3, BeforeArgument(1))
            .add_leaf(4, 1, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_empty_child(5, Append)
            .done()
            .done()
            .done()
            .add_empty_child(16, Append)
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
            [_func, arg0] => assert_eq!(arg0.argument_info(), Some(&this_param(Some(call_id)))),
            sth_else => panic!("There should be 2 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(3)
            .add_leaf(0, 3, node::Kind::Operation, Crumbs::default())
            .add_empty_child(3, InsertionPoint::expected_argument(0))
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
            [_func, arg0] => assert_eq!(arg0.argument_info(), Some(&this_param(Some(call_id)))),
            sth_else => panic!("There should be 2 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(8)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_leaf(4, 4, node::Kind::prefix_argument().removable().indexed(0), PrefixCrumb::Arg)
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
                assert_eq!(arg0.argument_info(), Some(&this_param(call_id)));
                assert_eq!(arg1.argument_info(), Some(&param1(call_id)));
                assert_eq!(arg2.argument_info(), Some(&param2(call_id)));
            }
            sth_else => panic!("There should be 4 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(8)
            .add_child(0, 8, node::Kind::chained_infix(), Crumbs::default())
            .add_child(0, 8, node::Kind::ChainedPrefix, Crumbs::default())
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_leaf(4, 4, node::Kind::prefix_argument().removable().indexed(0), PrefixCrumb::Arg)
            .done()
            .add_empty_child(8, InsertionPoint::expected_argument(1))
            .done()
            .add_empty_child(8, InsertionPoint::expected_named_argument_erased(2))
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
            [_this, _operator, _access, arg1, arg2] => {
                assert_eq!(arg1.argument_info(), Some(&param1(call_id)));
                assert_eq!(arg2.argument_info(), Some(&param2(call_id)));
            }
            sth_else => panic!("There should be 5 leaves, found: {}", sth_else.len()),
        }
        let expected = TreeBuilder::new(8)
            .add_child(0, 8, node::Kind::chained_infix(), Crumbs::default())
            .add_child(0, 8, node::Kind::Operation, Crumbs::default())
            .add_leaf(0, 4, node::Kind::argument(), InfixCrumb::LeftOperand)
            .add_leaf(4, 1, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(5, 3, node::Kind::Access, InfixCrumb::RightOperand)
            .done()
            .add_empty_child(8, InsertionPoint::expected_argument(0))
            .done()
            .add_empty_child(8, InsertionPoint::expected_named_argument_erased(1))
            .build();
        clear_expression_ids(&mut tree.root);
        clear_parameter_infos(&mut tree.root);
        assert_eq!(tree, expected);
    }


    #[test]
    fn argument_placeholder_insertion() {
        #[derive(Debug, PartialEq, Eq)]
        enum TestArgKind<'a> {
            Placeholder(&'a str),
            Named(&'a str),
            Positional(&'a str),
        }

        #[derive(Debug)]
        struct Case {
            expression:          &'static str,
            expected_properties: Vec<TestArgKind<'static>>,
        }

        let cases = [
            Case::new("func a=x c=x      ", "=a ?b =c ?d"),
            Case::new("func a=x d=x      ", "=a ?b ?c =d"),
            Case::new("func a=x d=x b=x  ", "=a =d =b ?c"),
            Case::new("func a=x d=x c=x  ", "=a ?b =d =c"),
            Case::new("func b=x          ", "?a =b ?c ?d"),
            Case::new("func b=x c=x      ", "?a =b =c ?d"),
            Case::new("func b=x x x      ", "=b @a @c ?d"),
            Case::new("func c=x b=x x    ", "=c =b @a ?d"),
            Case::new("func d=x          ", "?a ?b ?c =d"),
            Case::new("func d=x a c=x    ", "=d @a ?b =c"),
            Case::new("func d=x x        ", "=d @a ?b ?c"),
            Case::new("func d=x x        ", "=d @a ?b ?c"),
            Case::new("func d=x x x      ", "=d @a @b ?c"),
            Case::new("func d=x x x x    ", "=d @a @b @c"),
            Case::new("func x            ", "@a ?b ?c ?d"),
            Case::new("func x b=x c=x    ", "@a =b =c ?d"),
            Case::new("func x b=x x      ", "@a =b @c ?d"),
            Case::new("func x d=x        ", "@a ?b ?c =d"),
            Case::new("func x x          ", "@a @b ?c ?d"),
            Case::new("func x x x        ", "@a @b @c ?d"),
            Case::new("func x x x x      ", "@a @b @c @d"),
        ];

        for case in cases {
            case.run();
        }

        impl Case {
            fn new(expression: &'static str, expected_pattern: &'static str) -> Self {
                let expected_properties = expected_pattern
                    .split_ascii_whitespace()
                    .map(|pat| match pat.split_at(1) {
                        ("@", rest) => TestArgKind::Positional(rest),
                        ("=", rest) => TestArgKind::Named(rest),
                        ("?", rest) => TestArgKind::Placeholder(rest),
                        _ => panic!("Invalid pattern: {pat}"),
                    })
                    .collect_vec();
                Self { expression: expression.trim(), expected_properties }
            }
            fn run(self) {
                let Case { expression, expected_properties } = self;

                let parser = Parser::new();
                let ast = parser.parse_line_ast(expression).unwrap();
                let call_id = ast.id;

                let param = |n: &str| ArgumentInfo { name: Some(n.into()), call_id, ..default() };
                let application = ResolvedApplication {
                    suggestion_id:      None,
                    argument_in_access: None,
                    chain_arguments:    [param("a"), param("b"), param("c"), param("d")]
                        .into_iter()
                        .enumerate()
                        .collect(),
                    icon_name:          None,
                };

                let chain = ast::prefix::Chain::from_ast(&ast).unwrap_or_else(|| {
                    panic!("Ast of expression {expression:?} is not a prefix chain: {ast:?}")
                });

                let resolved = resolve_argument_positions(&chain.args, Some(application));

                let resolved_properties = resolved
                    .iter()
                    .map(|pos| match pos {
                        ArgumentPosition::Placeholder { info, .. } =>
                            TestArgKind::Placeholder(info.name.as_deref().unwrap()),
                        ArgumentPosition::ChainArgument { named, info, .. } => {
                            let name = info.as_ref().and_then(|(_, i)| i.name.as_deref()).unwrap();
                            if named.is_some() {
                                TestArgKind::Named(name)
                            } else {
                                TestArgKind::Positional(name)
                            }
                        }
                    })
                    .collect_vec();

                assert_eq!(
                    resolved_properties, expected_properties,
                    "Resolved argument positions mismatch for expression {expression:?}",
                );
            }
        }
    }
}
