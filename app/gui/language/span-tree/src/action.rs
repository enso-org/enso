//! A module containing all actions provided by SpanTree.
//!
//! The actions are in WIP state - they will be implemented along connection operations.

use crate::prelude::*;
use ast::crumbs::*;

use crate::node;

use ast::opr::match_named_argument;
use ast::opr::ArgWithOffset;
use ast::Ast;
use ast::SpanSeed;



// ==============
// === Errors ===
// ==============

/// Error returned when tried to perform an action which is not available for specific SpanTree
/// node.
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Action {:?} not available for this SpanTree node.", operation)]
pub struct ActionNotAvailable {
    operation: Action,
}

/// Error returned when tried to do action but SpanTree does not seem to be generated from AST
/// passed as root.
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Cannot apply action: ast structure does not match SpanTree.")]
pub struct AstSpanTreeMismatch;



// =====================
// === Actions Trait ===
// =====================

/// Action enum used mainly for error messages.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(missing_docs)]
pub enum Action {
    Set,
    Erase,
}

/// A trait implementing SpanTree actions. Mean to be implemented on some SpanTree node
/// representation.
///
/// All actions take root AST which should be AST on which SpanTree was generated, and returns
/// processed AST.
pub trait Actions {
    /// Check if given action may be performed on this node.
    fn is_action_available(&self, action: Action) -> bool;

    /// Set the node's span to new AST. In case of empty nodes it adds new element to prefix and
    /// operator chains.
    ///
    /// It returns new ast root with performed action.
    fn set(&self, root: &Ast, to: Ast) -> FallibleResult<Ast>;

    /// Erase element pointed by this node from operator or prefix chain.
    ///
    /// It returns new ast root with performed action.
    fn erase(&self, root: &Ast) -> FallibleResult<Ast>;
}

impl<T: Implementation> Actions for T {
    fn is_action_available(&self, action: Action) -> bool {
        match action {
            Action::Set => self.set_impl().is_some(),
            Action::Erase => self.erase_impl().is_some(),
        }
    }

    fn set(&self, root: &Ast, to: Ast) -> FallibleResult<Ast> {
        let operation = Action::Set;
        let action = self.set_impl().ok_or(ActionNotAvailable { operation })?;
        action(root, to)
    }

    fn erase(&self, root: &Ast) -> FallibleResult<Ast> {
        let operation = Action::Erase;
        let action = self.erase_impl().ok_or(ActionNotAvailable { operation })?;
        action(root)
    }
}



// ==============================
// === Actions Implementation ===
// ==============================

const DEFAULT_OFFSET: usize = 1;

/// A concrete function for "set" operations on specific SpanTree node. It takes root and ast-to-set
/// as arguments and returns new root with action performed.
pub type SetOperation<'a> = Box<dyn FnOnce(&Ast, Ast) -> FallibleResult<Ast> + 'a>;

/// A concrete function for "set" operations on specific SpanTree node. It takes root ast
/// as argument and returns new root with action performed.
pub type EraseOperation<'a> = Box<dyn FnOnce(&Ast) -> FallibleResult<Ast> + 'a>;

/// Implementation of actions - this is for keeping in one place checking of actions availability
/// and the performing the action.
#[allow(missing_docs)]
pub trait Implementation {
    fn set_impl(&self) -> Option<SetOperation>;
    fn erase_impl(&self) -> Option<EraseOperation>;
}

impl<'a> Implementation for node::Ref<'a> {
    fn set_impl(&self) -> Option<SetOperation> {
        match &self.node.kind {
            node::Kind::InsertionPoint(ins_point) => Some(Box::new(move |root, new| {
                use node::InsertionPointType::*;
                let kind = &ins_point.kind;
                let ast = root.get_traversing(&self.ast_crumbs)?;
                let expect_arg = kind.is_expected_argument();
                let extended_infix =
                    (!expect_arg).and_option_from(|| ast::opr::Chain::try_new(ast));
                let mut inserted_positional_placeholder_at = None;
                let new_ast = modify_preserving_id(ast, |ast| {
                    if let Some(mut infix) = extended_infix {
                        let has_arg = infix.args.last().unwrap().operand.is_some();
                        let offset = infix
                            .enumerate_non_empty_operands()
                            .last()
                            .map_or(DEFAULT_OFFSET, |op| op.offset);
                        let last_elem = infix.args.last_mut().unwrap();
                        let item = ArgWithOffset { arg: new, offset };
                        let has_target = infix.target.is_some();
                        last_elem.offset = DEFAULT_OFFSET;
                        match kind {
                            ExpectedTarget => infix.target = Some(item),
                            BeforeArgument(0 | 1) if !has_target => infix.target = Some(item),
                            BeforeArgument(idx) => infix.insert_operand(*idx, item),
                            Append if has_arg => infix.push_operand(item),
                            Append | ExpectedOperand => last_elem.operand = Some(item),
                            ExpectedArgument { .. } => unreachable!(
                                "Expected arguments should be filtered out before this if block"
                            ),
                        };
                        Ok(infix.into_ast())
                    } else if let ast::Shape::Tree(tree) = ast.shape() {
                        let mut tree = tree.clone();
                        let span_info = &mut tree.span_info;
                        let has_children =
                            span_info.iter().any(|span| matches!(span, ast::SpanSeed::Child(_)));
                        match *kind {
                            BeforeArgument(index) => {
                                span_info.insert(index, ast::SpanSeed::child(new));
                                span_info.insert(index + 1, ast::SpanSeed::token(",".to_owned()));
                                span_info.insert(index + 2, ast::SpanSeed::space(1).unwrap());
                            }
                            Append => {
                                let last_token_index = span_info
                                    .iter()
                                    .rposition(|span| matches!(span, ast::SpanSeed::Token(_)));
                                let index = last_token_index.unwrap_or(0);
                                if has_children {
                                    span_info.insert(index, ast::SpanSeed::token(",".to_owned()));
                                    span_info.insert(index + 1, ast::SpanSeed::space(1).unwrap());
                                    span_info.insert(index + 2, ast::SpanSeed::child(new));
                                } else {
                                    span_info.insert(index, ast::SpanSeed::child(new));
                                }
                            }
                            _ => unreachable!("Wrong insertion point in tree."),
                        }
                        Ok(ast.with_shape(tree))
                    } else {
                        let mut prefix = ast::prefix::Chain::from_ast_non_strict(&ast);
                        let item = ast::prefix::Argument::new(new, DEFAULT_OFFSET, None);
                        match kind {
                            BeforeArgument(idx) => prefix.insert_arg(*idx, item),
                            Append => prefix.args.push(item),
                            ExpectedArgument { index, named } => {
                                let item = match self.node.name() {
                                    Some(name) if *named => item.into_named(name),
                                    _ => {
                                        inserted_positional_placeholder_at = Some(*index);
                                        item
                                    }
                                };
                                prefix.args.push(item)
                            }
                            ExpectedTarget | ExpectedOperand =>
                                unreachable!("Wrong insertion point in method call"),
                        }
                        Ok(prefix.into_ast())
                    }
                });

                let mut new_root = root.set_traversing(&self.ast_crumbs, new_ast?)?;

                // when inserting a positional argument, some further named arguments might end up
                // in their natural position. We can rewrite them to be positional. That way
                // performing erase and insert actions multiple times will not accumulate named
                // arguments.
                if let Some(insert_position) = inserted_positional_placeholder_at {
                    // The just inserted argument is part of a prefix expression, which itself might
                    // be a target of another prefix expression. We want to access that expression's
                    // arguments, so we need to go up two levels.
                    let mut next_parent = match self.parent()? {
                        Some(node) => node.parent()?,
                        None => None,
                    };
                    let mut next_positional_index = insert_position + 1;

                    while let Some(node) = next_parent {
                        next_parent = node.parent()?;
                        let argument_node = node
                            .get_descendant_by_ast_crumbs(&[Crumb::Prefix(PrefixCrumb::Arg)])
                            .filter(|found| found.ast_crumbs.is_empty());
                        match argument_node {
                            Some(found)
                                if found.ast_crumbs.is_empty()
                                    && found.node.is_named_argument() =>
                            {
                                let arg_expression = found.node.last_child().expect(
                                    "Named argument should have an expression as its last child.",
                                );
                                let definition_index = arg_expression.kind.definition_index();
                                if definition_index == Some(next_positional_index) {
                                    // We found an named argument that matches its own position.
                                    // Rewrite it to be positional.
                                    let expr_crumbs = &arg_expression.ast_crumbs;
                                    let arg_crumbs = &expr_crumbs[..expr_crumbs.len() - 1];
                                    let expr = root.get_traversing(expr_crumbs)?.clone();
                                    new_root = new_root.set_traversing(arg_crumbs, expr)?;
                                    next_positional_index += 1;
                                }
                            }
                            _ => {
                                // No named argument found. We either escaped the prefix chain or
                                // found a next placeholder. In either case, it is guaranteed that
                                // there aren't any more named arguments that could be rewritten to
                                // positional.
                                break;
                            }
                        }
                    }
                }

                Ok(new_root)
            })),
            node::Kind::Token => None,
            _ => match &self.ast_crumbs.last() {
                // Operators should be treated in a special way - setting functions in place in
                // a operator should replace Infix with Prefix with two applications.
                // TODO[ao] Maybe some day...
                Some(Crumb::Infix(InfixCrumb::Operator))
                | Some(Crumb::SectionLeft(SectionLeftCrumb::Opr))
                | Some(Crumb::SectionRight(SectionRightCrumb::Opr))
                | Some(Crumb::SectionSides(SectionSidesCrumb)) => None,
                _ => Some(Box::new(move |root, new| {
                    modify_preserving_id(root, |root| root.set_traversing(&self.ast_crumbs, new))
                })),
            },
        }
    }

    fn erase_impl(&self) -> Option<EraseOperation> {
        if self.node.kind.removable() {
            Some(Box::new(move |root| {
                let (mut last_crumb, mut parent_crumbs) =
                    self.ast_crumbs.split_last().expect("Erase target must have parent AST node");
                let mut ast = root.get_traversing(parent_crumbs)?;
                let is_named_argument = match_named_argument(ast).is_some();

                if is_named_argument {
                    // When erasing named argument, we need to remove the whole argument, not only
                    // the value part. The named argument AST is always a single infix expression,
                    // so we need to target the crumb of parent node.
                    (last_crumb, parent_crumbs) =
                        parent_crumbs.split_last().expect("Erase target must have parent AST node");
                    ast = root.get_traversing(parent_crumbs)?;
                }

                let new_ast = modify_preserving_id(ast, |ast| {
                    if let Some(mut infix) = ast::opr::Chain::try_new(&ast) {
                        let is_target = infix
                            .args
                            .first()
                            .map_or(true, |arg| arg.crumb_to_previous() == *last_crumb);
                        if is_target {
                            infix.erase_target();
                        } else {
                            infix.args.pop();
                        }
                        Ok(infix.into_ast())
                    } else if let (Crumb::Tree(crumb), ast::Shape::Tree(tree)) =
                        (last_crumb, ast.shape())
                    {
                        let index = crumb.index;
                        let mut tree = tree.clone();
                        let span_info: &mut Vec<_> = &mut tree.span_info;
                        let after = &span_info[index + 1..];
                        let before = &span_info[..index];
                        let is_child = |span: &SpanSeed<Ast>| span.is_child();
                        let child_after_offset = after.iter().position(is_child);
                        let child_before_offset = before.iter().rposition(is_child);
                        let removed_range = match (child_after_offset, child_before_offset) {
                            (Some(after), _) => index..=index + after,
                            (None, Some(before)) => before + 1..=index,
                            (None, None) => index..=index,
                        };

                        span_info.drain(removed_range);
                        Ok(ast.with_shape(tree))
                    } else {
                        let mut prefix = ast::prefix::Chain::from_ast_non_strict(&ast);
                        prefix.args.pop();
                        Ok(prefix.into_ast())
                    }
                });
                let mut new_root = root.set_traversing(parent_crumbs, new_ast?)?;


                // When erasing a positional or named argument, all further positional arguments
                // past its definition order will end up in wrong position. To fix that, we need to
                // rewrite them as named arguments.
                if let Some(erased_definition_index) = self.kind.definition_index() {
                    let mut next_parent = parent_crumbs
                        .split_last()
                        .and_then(|(_, crumbs)| {
                            self.span_tree.root_ref().get_descendant_by_ast_crumbs(crumbs)
                        })
                        .map(|found| found.node);

                    // To better understand the code, it's important to know the structure of
                    // the [`SpanTree`] for prefix chains:
                    //
                    // Root
                    // ├─Chained
                    // │ ├─Chained
                    // │ │ ├─Chained
                    // │ │ │ └─ Argument1
                    // │ │ └─ Argument2
                    // │ └─ Argument3
                    // └─ Argument4
                    //
                    // Note that the order of arguments is reversed.
                    //
                    // Usually, all modifications of the AST must also modify the SpanTree of the
                    // expression. In this case, however, we are iterating over the old SpanTree
                    // while modifying the AST, and we don't update the SpanTree in the process. The
                    // assumption here is that as we go, we only change the SpanTree deeper than we
                    // will look at in future iterations. That way, we can be sure that the
                    // SpanTree's "outer layers" still correspond to the original. It is done that
                    // way because we cannot dynamically adjust the span tree to be entirely correct
                    // on every step.
                    //
                    // This loop traverses the tree bottom to top, starting at one of the arguments,
                    // and continues until all arguments are covered. It operates as follows:
                    // 1. Retrieve the argument from the current level of the [`SpanTree`].
                    // 2. Rewrite the argument as necessary.
                    // 3. Update `next_parent` to be the parent of the current node. We use
                    // [`Ref::ast_parent`] instead of [`Ref::parent`] because a single AST node
                    // can contain multiple span tree levels, and iterating over all of them
                    // would cause multiple rewrites of the same argument.
                    while let Some(node) = next_parent {
                        next_parent = node.ast_parent()?;
                        let argument_node = node
                            .get_descendant_by_ast_crumbs(&[Crumb::Prefix(PrefixCrumb::Arg)])
                            .filter(|found| found.ast_crumbs.is_empty());

                        match argument_node {
                            Some(found) if found.node.is_argument() =>
                                if let Some(arg_name) = found.node.kind.argument_name() {
                                    let def_idx = found.node.kind.definition_index();
                                    let need_rewrite =
                                        def_idx.map_or(false, |idx| idx > erased_definition_index);

                                    if need_rewrite {
                                        let arg_crumbs = &found.node.ast_crumbs;
                                        let expression = new_root.get_traversing(arg_crumbs)?;
                                        let named_ast = Ast::named_argument(arg_name, expression);
                                        new_root =
                                            new_root.set_traversing(arg_crumbs, named_ast)?;
                                    }
                                },
                            Some(found) if found.node.is_named_argument() => {
                                // Found named argument. Continue searching for positional arguments
                                // past it, as it might be out of definition order.
                            }
                            _ => {
                                // No arguments found. We are no longer in prefix chain or found a
                                // placeholder. In either case, it is guaranteed that there aren't
                                // any more positional arguments to rewrite.
                                break;
                            }
                        }
                    }
                }

                Ok(new_root)
            }))
        } else {
            None
        }
    }
}

/// Helper functions for span-tree modification.
///
/// To keep nodes consistent, we don't want to have the changed ast-id being changed, so this
/// functions wrap given `modifier` and restore old id in returned AST.
fn modify_preserving_id<F>(ast: &Ast, modifier: F) -> FallibleResult<Ast>
where F: FnOnce(Ast) -> FallibleResult<Ast> {
    let ast_id = ast.id;
    let ast = ast.with_new_id();
    let new_ast = modifier(ast)?;
    if let Some(id) = ast_id {
        Ok(new_ast.with_id(id))
    } else {
        Ok(new_ast)
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use Action::*;

    use crate::builder::TreeBuilder;
    use crate::generate::context;
    use crate::node;
    use crate::node::InsertionPoint;
    use crate::SpanTree;

    use ast::HasRepr;
    use parser::Parser;

    #[test]
    fn actions_in_span_tree() {
        #[derive(Debug)]
        struct Case {
            expr:     &'static str,
            span:     Range<usize>,
            action:   Action,
            expected: &'static str,
        }

        impl Case {
            fn run(&self, parser: &Parser) {
                let context = context::Empty;
                let ast = parser.parse_line_ast(self.expr).unwrap();
                let ast_id = ast.id;
                let tree: SpanTree = ast.generate_tree(&context).unwrap();
                let node = tree.root_ref().find_by_span(&self.span.clone().into());
                let node = node.unwrap_or_else(|| {
                    panic!("Invalid case {:?}: no node with span {:?}", self, self.span)
                });
                let arg = Ast::new(ast::Var { name: "foo".to_string() }, None);
                let case = format!("{self:?}");
                let result = match &self.action {
                    Set => node.set(&ast, arg),
                    Erase => node.erase(&ast),
                }
                .expect(&case);
                let result_repr = result.repr();
                assert_eq!(result_repr, self.expected, "Wrong answer for case {self:?}");
                assert_eq!(ast_id, result.id, "Changed AST ID in case {self:?}");
            }
        }

        let cases: &[Case] = &
            // Setting
            [ Case{expr:"a + b"      , span:0..5  , action:Set  , expected:"foo"            }
            , Case{expr:"a + b"      , span:0..1  , action:Set  , expected:"foo + b"        }
            , Case{expr:"a + b"      , span:4..5  , action:Set  , expected:"a + foo"        }
            , Case{expr:"a + b + c"  , span:0..1  , action:Set  , expected:"foo + b + c"    }
            , Case{expr:"a + b + c"  , span:4..5  , action:Set  , expected:"a + foo + c"    }
            , Case{expr:"f a b"      , span:0..1  , action:Set  , expected:"foo a b"        }
            , Case{expr:"f a b"      , span:2..3  , action:Set  , expected:"f foo b"        }
            , Case{expr:"f a b"      , span:4..5  , action:Set  , expected:"f a foo"        }
            , Case{expr:"+ b"        , span:0..0  , action:Set  , expected:"foo + b"        }
            , Case{expr:"+ b"        , span:2..3  , action:Set  , expected:"+ foo"          }
            , Case{expr:"a +"        , span:0..1  , action:Set  , expected:"foo +"          }
            , Case{expr:"a +"        , span:3..3  , action:Set  , expected:"a + foo"        }
            , Case{expr:"+"          , span:0..0  , action:Set  , expected:"foo +"          }
            , Case{expr:"+"          , span:1..1  , action:Set  , expected:"+ foo"          }
            , Case{expr:"a + b"      , span:0..0  , action:Set  , expected:"foo + a + b"    }
            , Case{expr:"a + b"      , span:3..3  , action:Set  , expected:"a + foo + b"    }
            , Case{expr:"a + b"      , span:5..5  , action:Set  , expected:"a + b + foo"    }
            , Case{expr:"+ b"        , span:3..3  , action:Set  , expected:"+ b + foo"      }
            , Case{expr:"a + b + c"  , span:0..0  , action:Set  , expected:"foo + a + b + c"}
            , Case{expr:"a + b + c"  , span:7..7  , action:Set  , expected:"a + b + foo + c"}
            , Case{expr:", b"        , span:3..3  , action:Set  , expected:", b , foo"      }
            , Case{expr:"f a b"      , span:1..1  , action:Set  , expected:"f foo a b"      }
            , Case{expr:"f a b"      , span:3..3  , action:Set  , expected:"f a foo b"      }
            , Case{expr:"f a b"      , span:5..5  , action:Set  , expected:"f a b foo"      }
            , Case{expr:"if a then b", span:3..4  , action:Set  , expected: "if foo then b" }
            , Case{expr:"if a then b", span:10..11, action:Set  , expected: "if a then foo" }
            , Case{expr:"(a + b + c)", span:5..6  , action:Set  , expected: "(a + foo + c)" }
            , Case{expr:"(a + b + c" , span:5..6  , action:Set  , expected: "(a + foo + c"  }
            // Erasing
            , Case{expr:"a + b + c"  , span:0..1  , action:Erase, expected:"b + c"          }
            , Case{expr:"a + b + c"  , span:4..5  , action:Erase, expected:"a + c"          }
            , Case{expr:"a + b + c"  , span:8..9  , action:Erase, expected:"a + b"          }
            , Case{expr:"f a b"      , span:2..3  , action:Erase, expected:"f b"            }
            , Case{expr:"f a b"      , span:4..5  , action:Erase, expected:"f a"            }
            , Case{expr:"(a + b + c)", span:5..6  , action:Erase, expected: "(a + c)"       }
            , Case{expr:"(a + b + c" , span:5..6  , action:Erase, expected: "(a + c"        }
            ];
        let parser = Parser::new();
        for case in cases {
            case.run(&parser);
        }
    }

    #[test]
    fn possible_actions_in_span_tree() {
        #[derive(Debug)]
        struct Case {
            expr:     &'static str,
            span:     Range<usize>,
            expected: &'static [Action],
        }

        impl Case {
            fn run(&self, parser: &Parser) {
                let ast = parser.parse_line_ast(self.expr).unwrap();
                let tree: SpanTree = ast.generate_tree(&context::Empty).unwrap();
                let node = tree.root_ref().find_by_span(&self.span.clone().into());
                let node = node.unwrap_or_else(|| {
                    panic!("Invalid case {:?}: no node with span {:?}", self, self.span)
                });

                let expected: HashSet<Action> = self.expected.iter().cloned().collect();
                for action in &[Set, Erase] {
                    assert_eq!(
                        node.is_action_available(*action),
                        expected.contains(action),
                        "Availability mismatch for action {action:?} in case {self:?}"
                    )
                }
            }
        }
        let cases: &[Case] = &[
            Case { expr: "abc", span: 0..3, expected: &[Set] },
            Case { expr: "a + b", span: 0..0, expected: &[Set] },
            Case { expr: "a + b", span: 0..1, expected: &[Set] },
            Case { expr: "a + b", span: 2..3, expected: &[] },
            Case { expr: "a + b", span: 3..3, expected: &[Set] },
            Case { expr: "a + b", span: 4..5, expected: &[Set] },
            Case { expr: "a + b", span: 5..5, expected: &[Set] },
            Case { expr: "a + b + c", span: 0..0, expected: &[Set] },
            Case { expr: "a + b + c", span: 0..1, expected: &[Set, Erase] },
            Case { expr: "a + b + c", span: 3..3, expected: &[Set] },
            Case { expr: "a + b + c", span: 4..5, expected: &[Set, Erase] },
            Case { expr: "a + b + c", span: 7..7, expected: &[Set] },
            Case { expr: "a + b + c", span: 8..9, expected: &[Set, Erase] },
            Case { expr: "a + b + c", span: 9..9, expected: &[Set] },
            Case { expr: "f a b", span: 0..1, expected: &[Set] },
            Case { expr: "f a b", span: 1..1, expected: &[Set] },
            Case { expr: "f a b", span: 2..3, expected: &[Set, Erase] },
            Case { expr: "f a b", span: 3..3, expected: &[Set] },
            Case { expr: "f a b", span: 4..5, expected: &[Set, Erase] },
            Case { expr: "f a b", span: 5..5, expected: &[Set] },
            Case { expr: "f a", span: 2..3, expected: &[Set] },
            Case { expr: "if a then b", span: 3..4, expected: &[Set] },
            Case { expr: "if a then b", span: 10..11, expected: &[Set] },
            Case { expr: "(a + b)", span: 0..1, expected: &[] },
            Case { expr: "[a,b]", span: 0..1, expected: &[] },
            Case { expr: "[a,b]", span: 4..5, expected: &[] },
            Case { expr: "(a + b + c)", span: 5..6, expected: &[Set, Erase] },
            Case { expr: "(a", span: 1..2, expected: &[Set] },
            Case { expr: "(a + b + c", span: 5..6, expected: &[Set, Erase] },
        ];
        let parser = Parser::new();
        for case in cases {
            case.run(&parser);
        }
    }

    #[test]
    fn setting_positional_arguments() {
        let baz = Ast::var("baz");

        // Consider Span Tree for `foo bar` where `foo` is a method known to take 3 parameters.
        // We can try setting each of 3 arguments to `baz`.
        let tree = TreeBuilder::new(7)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_leaf(4, 7, node::Kind::this(), PrefixCrumb::Arg)
            .add_empty_child(7, InsertionPoint::expected_argument(1))
            .add_empty_child(7, InsertionPoint::expected_named_argument(2, "b"))
            .build();

        let ast = Ast::prefix(Ast::var("foo"), Ast::var("bar"));
        let ast_id = ast.id;
        assert_eq!(ast.repr(), "foo bar");

        let after = tree.root_ref().child(1).unwrap().set(&ast, baz.clone_ref()).unwrap();
        assert_eq!(after.repr(), "foo baz");
        assert_eq!(after.id, ast_id);

        let after = tree.root_ref().child(2).unwrap().set(&ast, baz.clone_ref()).unwrap();
        assert_eq!(after.repr(), "foo bar baz");
        assert_eq!(after.id, ast_id);

        let after = tree.root_ref().child(3).unwrap().set(&ast, baz.clone_ref()).unwrap();
        assert_eq!(after.repr(), "foo bar b=baz");
        assert_eq!(after.id, ast_id);

        // Another case is Span Tree for `Main . foo` where `foo` is a method known to take 2
        // parameters. We can try setting each of 2 arguments to `baz`.
        let tree: SpanTree = TreeBuilder::new(10)
            .add_leaf(0, 4, node::Kind::this(), InfixCrumb::LeftOperand)
            .add_leaf(5, 6, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(7, 10, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_empty_child(10, InsertionPoint::expected_argument(0))
            .add_empty_child(10, InsertionPoint::expected_named_argument(1, "b"))
            .build();

        let ast = Ast::infix(Ast::cons("Main"), ".", Ast::var("foo"));
        let ast_id = ast.id;
        assert_eq!(ast.repr(), "Main . foo");

        let after = tree.root_ref().child(3).unwrap().set(&ast, baz.clone_ref()).unwrap();
        assert_eq!(after.repr(), "Main . foo baz");
        assert_eq!(after.id, ast_id);

        let after = tree.root_ref().child(4).unwrap().set(&ast, baz.clone_ref()).unwrap();
        assert_eq!(after.repr(), "Main . foo b=baz");
        assert_eq!(after.id, ast_id);
    }
}
