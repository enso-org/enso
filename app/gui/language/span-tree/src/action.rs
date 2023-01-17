//! A module containing all actions provided by SpanTree.
//!
//! The actions are in WIP state - they will be implemented along connection operations.

use crate::prelude::*;
use ast::crumbs::*;

use crate::node;

use ast::opr::ArgWithOffset;
use ast::Ast;
use ast::Shifted;



/// ==============
/// === Errors ===
/// ==============

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



/// =====================
/// === Actions Trait ===
/// =====================

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

impl<'a, T> Implementation for node::Ref<'a, T> {
    fn set_impl(&self) -> Option<SetOperation> {
        match &self.node.kind {
            node::Kind::InsertionPoint(ins_point) => Some(Box::new(move |root, new| {
                use node::InsertionPointType::*;
                let kind = &ins_point.kind;
                let ast = root.get_traversing(&self.ast_crumbs)?;
                let expect_arg = matches!(kind, ExpectedArgument(_));
                let extended_infix =
                    (!expect_arg).and_option_from(|| ast::opr::Chain::try_new(ast));
                let new_ast = modify_preserving_id(ast, |ast| {
                    if let Some(mut infix) = extended_infix {
                        let item = ArgWithOffset { arg: new, offset: DEFAULT_OFFSET };
                        let has_target = infix.target.is_some();
                        let has_arg = infix.args.last().unwrap().operand.is_some();
                        let last_elem = infix.args.last_mut().unwrap();
                        let last_arg = &mut last_elem.operand;
                        last_elem.offset = DEFAULT_OFFSET;
                        match kind {
                            BeforeTarget if has_target => infix.push_front_operand(item),
                            AfterTarget if has_target => infix.insert_operand(1, item),
                            BeforeTarget | AfterTarget => infix.target = Some(item),
                            Append if has_arg => infix.push_operand(item),
                            Append => *last_arg = Some(item),
                            ExpectedArgument(_) => panic!(
                                "Expected arguments should be filtered out befire this if block"
                            ),
                        };
                        Ok(infix.into_ast())
                    } else {
                        let mut prefix = ast::prefix::Chain::from_ast_non_strict(&ast);
                        let item = ast::prefix::Argument {
                            sast:      Shifted { wrapped: new, off: DEFAULT_OFFSET },
                            prefix_id: None,
                        };
                        match kind {
                            BeforeTarget => prefix.insert_arg(0, item),
                            AfterTarget => prefix.insert_arg(1, item),
                            Append => prefix.args.push(item),
                            ExpectedArgument(i) => prefix.insert_arg(*i, item),
                        }
                        Ok(prefix.into_ast())
                    }
                });
                root.set_traversing(&self.ast_crumbs, new_ast?)
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
        if (self.node.kind.is_argument() || self.node.kind.is_this()) && self.node.kind.removable()
        {
            Some(Box::new(move |root| {
                let parent_crumb = &self.ast_crumbs[..self.ast_crumbs.len() - 1];
                let ast = root.get_traversing(parent_crumb)?;
                let new_ast = modify_preserving_id(ast, |ast| {
                    if let Some(mut infix) = ast::opr::Chain::try_new(&ast) {
                        match self.node.kind {
                            node::Kind::This { .. } => {
                                infix.erase_target();
                            }
                            _ => {
                                infix.args.pop();
                            }
                        }
                        Ok(infix.into_ast())
                    } else {
                        let mut prefix = ast::prefix::Chain::from_ast_non_strict(&ast);
                        prefix.args.pop();
                        Ok(prefix.into_ast())
                    }
                });
                root.set_traversing(parent_crumb, new_ast?)
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
    use crate::node::InsertionPointType::ExpectedArgument;
    use crate::SpanTree;

    use ast::HasRepr;
    use parser_scala::Parser;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
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
                let ast = parser.parse_line_ast(self.expr).unwrap();
                let ast_id = ast.id;
                let tree = ast.generate_tree(&context::Empty).unwrap(): SpanTree;
                let node = tree.root_ref().find_by_span(&self.span.clone().into());
                let node = node.unwrap_or_else(|| {
                    panic!("Invalid case {:?}: no node with span {:?}", self, self.span)
                });
                let arg = Ast::new(ast::Var { name: "foo".to_string() }, None);
                let result = match &self.action {
                    Set => node.set(&ast, arg),
                    Erase => node.erase(&ast),
                }
                .unwrap();
                let result_repr = result.repr();
                assert_eq!(result_repr, self.expected, "Wrong answer for case {self:?}");
                assert_eq!(ast_id, result.id, "Changed AST id in case {self:?}");
            }
        }

        let cases: &[Case] = &
            // Setting
            [ Case{expr:"a + b"      , span:0..5  , action:Set  , expected:"foo"            }
            , Case{expr:"a + b"      , span:0..1  , action:Set  , expected:"foo + b"        }
            , Case{expr:"a + b"      , span:4..5  , action:Set  , expected:"a + foo"        }
            , Case{expr:"a + b + c"  , span:0..1  , action:Set  , expected:"foo + b + c"    }
            , Case{expr:"a + b + c"  , span:4..5  , action:Set  , expected:"a + foo + c"    }
            , Case{expr:"a , b , c"  , span:0..1  , action:Set  , expected:"foo , b , c"    }
            , Case{expr:"a , b , c"  , span:4..5  , action:Set  , expected:"a , foo , c"    }
            , Case{expr:"a , b , c"  , span:8..9  , action:Set  , expected:"a , b , foo"    }
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
            , Case{expr:"a + b"      , span:1..1  , action:Set  , expected:"a + foo + b"    }
            , Case{expr:"a + b"      , span:5..5  , action:Set  , expected:"a + b + foo"    }
            , Case{expr:"+ b"        , span:3..3  , action:Set  , expected:"+ b + foo"      }
            , Case{expr:"a + b + c"  , span:0..0  , action:Set  , expected:"foo + a + b + c"}
            , Case{expr:"a + b + c"  , span:5..5  , action:Set  , expected:"a + b + foo + c"}
            , Case{expr:"a , b , c"  , span:0..0  , action:Set  , expected:"foo , a , b , c"}
            , Case{expr:"a , b , c"  , span:4..4  , action:Set  , expected:"a , foo , b , c"}
            , Case{expr:"a , b , c"  , span:8..8  , action:Set  , expected:"a , b , foo , c"}
            , Case{expr:"a , b , c"  , span:9..9  , action:Set  , expected:"a , b , c , foo"}
            , Case{expr:", b"        , span:3..3  , action:Set  , expected:", b , foo"      }
            , Case{expr:"f a b"      , span:2..2  , action:Set  , expected:"f foo a b"      }
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
            , Case{expr:"a , b , c"  , span:0..1  , action:Erase, expected:"b , c"          }
            , Case{expr:"a , b , c"  , span:4..5  , action:Erase, expected:"a , c"          }
            , Case{expr:"a , b , c"  , span:8..9  , action:Erase, expected:"a , b"          }
            , Case{expr:"f a b"      , span:2..3  , action:Erase, expected:"f b"            }
            , Case{expr:"f a b"      , span:4..5  , action:Erase, expected:"f a"            }
            , Case{expr:"(a + b + c)", span:5..6  , action:Erase, expected: "(a + c)"       }
            , Case{expr:"(a + b + c" , span:5..6  , action:Erase, expected: "(a + c"        }
            ];
        let parser = Parser::new_or_panic();
        for case in cases {
            case.run(&parser);
        }
    }

    #[wasm_bindgen_test]
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
            Case { expr: "a + b", span: 1..1, expected: &[Set] },
            Case { expr: "a + b", span: 2..3, expected: &[] },
            Case { expr: "a + b", span: 4..5, expected: &[Set] },
            Case { expr: "a + b", span: 5..5, expected: &[Set] },
            Case { expr: "a + b + c", span: 0..0, expected: &[Set] },
            Case { expr: "a + b + c", span: 0..1, expected: &[Set, Erase] },
            Case { expr: "a + b + c", span: 1..1, expected: &[Set] },
            Case { expr: "a + b + c", span: 4..5, expected: &[Set, Erase] },
            Case { expr: "a + b + c", span: 5..5, expected: &[Set] },
            Case { expr: "a + b + c", span: 8..9, expected: &[Set, Erase] },
            Case { expr: "a + b + c", span: 9..9, expected: &[Set] },
            Case { expr: "f a b", span: 0..1, expected: &[Set] },
            Case { expr: "f a b", span: 2..2, expected: &[Set] },
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
            Case { expr: "(a", span: 0..1, expected: &[] },
            Case { expr: "(a + b + c", span: 5..6, expected: &[Set, Erase] },
        ];
        let parser = Parser::new_or_panic();
        for case in cases {
            case.run(&parser);
        }
    }

    #[test]
    fn setting_positional_arguments() {
        let baz = Ast::var("baz");

        // Consider Span Tree for `foo bar` where `foo` is a method known to take 3 parameters.
        // We can try setting each of 3 arguments to `baz`.
        let tree = TreeBuilder::<()>::new(7)
            .add_leaf(0, 3, node::Kind::Operation, PrefixCrumb::Func)
            .add_leaf(4, 7, node::Kind::this(), PrefixCrumb::Arg)
            .add_empty_child(7, ExpectedArgument(1))
            .add_empty_child(7, ExpectedArgument(2))
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
        assert_eq!(after.repr(), "foo bar _ baz");
        assert_eq!(after.id, ast_id);

        // Another case is Span Tree for `Main . foo` where `foo` is a method known to take 2
        // parameters. We can try setting each of 2 arguments to `baz`.
        let tree: SpanTree = TreeBuilder::new(10)
            .add_leaf(0, 4, node::Kind::this(), InfixCrumb::LeftOperand)
            .add_leaf(5, 6, node::Kind::Operation, InfixCrumb::Operator)
            .add_leaf(7, 10, node::Kind::argument(), InfixCrumb::RightOperand)
            .add_empty_child(10, ExpectedArgument(0))
            .add_empty_child(10, ExpectedArgument(1))
            .build();

        let ast = Ast::infix(Ast::cons("Main"), ".", Ast::var("foo"));
        let ast_id = ast.id;
        assert_eq!(ast.repr(), "Main . foo");

        let after = tree.root_ref().child(3).unwrap().set(&ast, baz.clone_ref()).unwrap();
        assert_eq!(after.repr(), "Main . foo baz");
        assert_eq!(after.id, ast_id);

        let after = tree.root_ref().child(4).unwrap().set(&ast, baz.clone_ref()).unwrap();
        assert_eq!(after.repr(), "Main . foo _ baz");
        assert_eq!(after.id, ast_id);
    }
}
