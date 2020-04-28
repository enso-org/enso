//! A module containing code related to SpanTree generation.

use crate::prelude::*;

use crate::node;
use crate::node::InsertType;
use crate::Node;
use crate::SpanTree;

use ast::Ast;
use ast::assoc::Assoc;
use ast::crumbs::Located;
use ast::HasLength;
use ast::opr::GeneralizedInfix;
use data::text::Size;


// =============
// === Trait ===
// =============

/// A trait for all types from which we can generate referred SpanTree. Meant to be implemented for
/// all AST-like structures.
pub trait SpanTreeGenerator {
    /// Generate node with it's whole subtree.
    fn generate_node(&self, kind:node::Kind) -> FallibleResult<Node>;

    /// Generate tree for this AST treated as root for the whole expression.
    fn generate_tree(&self) -> FallibleResult<SpanTree> {
        Ok(SpanTree {
            root : self.generate_node(node::Kind::Root)?
        })
    }
}


// =================
// === Utilities ===
// =================

// === Child Generator ===

/// An utility to generate children with increasing offsets.
#[derive(Debug,Default)]
struct ChildGenerator {
    current_offset : Size,
    children       : Vec<node::Child>,
}

impl ChildGenerator {
    /// Add spacing to current generator state. It will be taken into account for the next generated
    /// children's offsets
    fn spacing(&mut self, size:usize) {
        self.current_offset += Size::new(size);
    }

    fn generate_ast_node
    (&mut self, child_ast:Located<Ast>, kind:node::Kind) -> FallibleResult<&node::Child> {
        let node = child_ast.item.generate_node(kind)?;
        Ok(self.add_node(child_ast.crumbs,node))
    }

    fn add_node(&mut self, ast_crumbs:ast::Crumbs, node:Node) -> &node::Child {
        let offset = self.current_offset;
        let child = node::Child {node,ast_crumbs,offset};
        self.current_offset += child.node.size;
        self.children.push(child);
        self.children.last().unwrap()
    }

    fn generate_empty_node(&mut self, insert_type:InsertType) -> &node::Child {
        let child = node::Child {
            node                : Node::new_empty(insert_type),
            offset              : self.current_offset,
            ast_crumbs          : vec![]
        };
        self.children.push(child);
        self.children.last().unwrap()
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


// === AST ===

impl SpanTreeGenerator for Ast {
    fn generate_node(&self, kind:node::Kind) -> FallibleResult<Node> {
        if let Some(infix) = GeneralizedInfix::try_new(self) {
            infix.flatten().generate_node(kind)
        } else {
            match self.shape() {
                ast::Shape::Prefix {..} =>
                    ast::prefix::Chain::try_new(self).unwrap().generate_node(kind),
                // TODO[a] add other shapes, e.g. macros
                _  => Ok(Node {kind,
                    size     : Size::new(self.len()),
                    children : default(),
                }),
            }
        }
    }
}


// === Operators (Sections and Infixes) ===

impl SpanTreeGenerator for ast::opr::Chain {
    fn generate_node(&self, kind:node::Kind) -> FallibleResult<Node> {
        // Removing operands is possible only when chain has at least 3 of them
        // (target and two arguments).
        let removable       = self.args.len() >= 2;
        let node_and_offset = match &self.target {
            Some(target) =>
                target.arg.generate_node(node::Kind::Target {removable}).map(|n| (n,target.offset)),
            None => Ok((Node::new_empty(InsertType::BeforeTarget),0)),
        };

        // In this fold we pass last generated node and offset after it, wrapped in Result.
        let (node,_) = self.args.iter().enumerate().fold(node_and_offset, |result,(i,elem)| {
            // Here we generate children as the operator would be left-associative. Then, if it is
            // actually right associative, we just reverse the generated children and their offsets.
            let (node,off)  = result?;
            let is_first    = i == 0;
            let is_last     = i + 1 == self.args.len();
            let has_left    = !node.is_empty();
            // Target is a first element of chain in this context.
            let has_target  = is_first && has_left;
            let opr_crumbs  = elem.crumb_to_operator(has_left);
            let opr_ast     = Located::new(opr_crumbs,elem.operator.ast().clone_ref());
            let left_crumbs = if has_left { vec![elem.crumb_to_previous()] } else { vec![] };

            let mut gen  = ChildGenerator::default();
            if has_target { gen.generate_empty_node(InsertType::BeforeTarget); }
            gen.add_node(left_crumbs,node);
            if has_target { gen.generate_empty_node(InsertType::AfterTarget); }
            gen.spacing(off);
            gen.generate_ast_node(opr_ast,node::Kind::Operation)?;
            if let Some(operand) = &elem.operand {
                let arg_crumbs = elem.crumb_to_operand(has_left);
                let arg_ast    = Located::new(arg_crumbs,operand.arg.clone_ref());
                gen.spacing(operand.offset);
                gen.generate_ast_node(arg_ast,node::Kind::Argument {removable})?;
            }
            gen.generate_empty_node(InsertType::Append);

            if ast::opr::assoc(&self.operator) == Assoc::Right {
                gen.reverse_children();
            }

            Ok((Node {
                kind     : if is_last {kind} else {node::Kind::Chained},
                size     : gen.current_offset,
                children : gen.children,
            }, elem.offset))
        })?;
        Ok(node)
    }
}


// === Application ===

impl SpanTreeGenerator for ast::prefix::Chain {
    fn generate_node(&self, kind:node::Kind) -> FallibleResult<Node> {
        use ast::crumbs::PrefixCrumb::*;
        // Removing arguments is possible if there at least two of them
        let removable = self.args.len() >= 2;
        let node      = self.func.generate_node(node::Kind::Operation);
        self.args.iter().enumerate().fold(node, |node,(i,arg)| {
            let node     = node?;
            let is_first = i == 0;
            let is_last  = i + 1 == self.args.len();
            let arg_kind = if is_first { node::Kind::Target {removable} }
                else { node::Kind::Argument {removable} };

            let mut gen = ChildGenerator::default();
            gen.add_node(vec![Func.into()],node);
            gen.spacing(arg.off);
            if let node::Kind::Target {..} = arg_kind {
                gen.generate_empty_node(InsertType::BeforeTarget);
            }
            gen.generate_ast_node(Located::new(Arg,arg.wrapped.clone_ref()), arg_kind)?;
            gen.generate_empty_node(InsertType::Append);
            Ok(Node {
                kind     : if is_last {kind} else {node::Kind::Chained},
                size     : gen.current_offset,
                children : gen.children,
            })
        })
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    use crate::builder::TreeBuilder;
    use crate::node::Kind::*;
    use crate::node::InsertType::*;

    use ast::crumbs::InfixCrumb;
    use ast::crumbs::PrefixCrumb;
    use ast::crumbs::SectionLeftCrumb;
    use ast::crumbs::SectionRightCrumb;
    use ast::crumbs::SectionSidesCrumb;
    use parser::Parser;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    fn generating_span_tree() {
        let parser    = Parser::new_or_panic();
        let ast       = parser.parse_line("2 + foo bar - 3").unwrap();
        let tree      = ast.generate_tree().unwrap();
        let removable = false;

        let expected = TreeBuilder::new(15)
            .add_empty_child(0,BeforeTarget)
            .add_child(0,11,Target{removable},vec![InfixCrumb::LeftOperand])
                .add_empty_child(0,BeforeTarget)
                .add_leaf (0,1,Target{removable},vec![InfixCrumb::LeftOperand])
                .add_empty_child(1,AfterTarget)
                .add_leaf (2,1,Operation,vec![InfixCrumb::Operator])
                .add_child(4,7,Argument{removable} ,vec![InfixCrumb::RightOperand])
                    .add_leaf(0,3,Operation,vec![PrefixCrumb::Func])
                    .add_empty_child(4,BeforeTarget)
                    .add_leaf(4,3,Target{removable},vec![PrefixCrumb::Arg])
                    .add_empty_child(7,Append)
                    .done()
                .add_empty_child(11,Append)
                .done()
            .add_empty_child(11,AfterTarget)
            .add_leaf(12,1,Operation,vec![InfixCrumb::Operator])
            .add_leaf(14,1,Argument{removable},vec![InfixCrumb::RightOperand])
            .add_empty_child(15,Append)
            .build();

        assert_eq!(expected,tree)
    }

    #[wasm_bindgen_test]
    fn generate_span_tree_with_chains() {
        let parser    = Parser::new_or_panic();
        let ast       = parser.parse_line("2 + 3 + foo bar baz 13 + 5").unwrap();
        let tree      = ast.generate_tree().unwrap();
        let removable = true;

        let expected = TreeBuilder::new(26)
            .add_child(0,22,Chained,vec![InfixCrumb::LeftOperand])
                .add_child(0,5,Chained,vec![InfixCrumb::LeftOperand])
                    .add_empty_child(0,BeforeTarget)
                    .add_leaf(0,1,Target{removable},vec![InfixCrumb::LeftOperand])
                    .add_empty_child(1,AfterTarget)
                    .add_leaf(2,1,Operation,vec![InfixCrumb::Operator])
                    .add_leaf(4,1,Argument{removable},vec![InfixCrumb::RightOperand])
                    .add_empty_child(5,Append)
                    .done()
                .add_leaf (6,1 ,Operation,vec![InfixCrumb::Operator])
                .add_child(8,14,Argument{removable},vec![InfixCrumb::RightOperand])
                    .add_child(0,11,Chained,vec![PrefixCrumb::Func])
                        .add_child(0,7,Chained,vec![PrefixCrumb::Func])
                            .add_leaf(0,3,Operation,vec![PrefixCrumb::Func])
                            .add_empty_child(4,BeforeTarget)
                            .add_leaf(4,3,Target{removable},vec![PrefixCrumb::Arg])
                            .add_empty_child(7,Append)
                            .done()
                        .add_leaf(8,3,Argument{removable},vec![PrefixCrumb::Arg])
                        .add_empty_child(11,Append)
                        .done()
                    .add_leaf(12,2,Argument{removable},vec![PrefixCrumb::Arg])
                    .add_empty_child(14,Append)
                    .done()
                .add_empty_child(22,Append)
                .done()
            .add_leaf(23,1,Operation,vec![InfixCrumb::Operator])
            .add_leaf(25,1,Argument{removable},vec![InfixCrumb::RightOperand])
            .add_empty_child(26,Append)
            .build();

        assert_eq!(expected,tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_right_assoc_operator() {
        let parser    = Parser::new_or_panic();
        let ast       = parser.parse_line("1,2,3").unwrap();
        let tree      = ast.generate_tree().unwrap();
        let removable = true;

        let expected = TreeBuilder::new(5)
            .add_empty_child(0,Append)
            .add_leaf (0,1,Argument{removable},vec![InfixCrumb::LeftOperand])
            .add_leaf (1,1,Operation,vec![InfixCrumb::Operator])
            .add_child(2,3,Chained  ,vec![InfixCrumb::RightOperand])
                .add_empty_child(0,Append)
                .add_leaf(0,1,Argument{removable},vec![InfixCrumb::LeftOperand])
                .add_leaf(1,1,Operation,vec![InfixCrumb::Operator])
                .add_empty_child(2,AfterTarget)
                .add_leaf(2,1,Target{removable},vec![InfixCrumb::RightOperand])
                .add_empty_child(3,BeforeTarget)
                .done()
            .build();

        assert_eq!(expected,tree)
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_section() {
        let parser = Parser::new_or_panic();
        // The star makes `SectionSides` ast being one of the parameters of + chain. First + makes
        // SectionRight, and last + makes SectionLeft.
        let ast       = parser.parse_line("+ * + + 2 +").unwrap();
        let tree      = ast.generate_tree().unwrap();
        let removable = true;

        let expected = TreeBuilder::new(11)
            .add_child(0,9,Chained,vec![SectionLeftCrumb::Arg])
                .add_child(0,5,Chained,vec![InfixCrumb::LeftOperand])
                    .add_child(0,3,Chained,vec![SectionLeftCrumb::Arg])
                        .add_empty_child(0,BeforeTarget)
                        .add_leaf (0,1,Operation,vec![SectionRightCrumb::Opr])
                        .add_child(2,1,Argument{removable},vec![SectionRightCrumb::Arg])
                            .add_empty_child(0,BeforeTarget)
                            .add_leaf(0,1,Operation,vec![SectionSidesCrumb])
                            .add_empty_child(1,Append)
                            .done()
                        .add_empty_child(3,Append)
                        .done()
                    .add_leaf(4,1,Operation,vec![SectionLeftCrumb::Opr])
                    .add_empty_child(5,Append)
                    .done()
                .add_leaf(6,1,Operation,vec![InfixCrumb::Operator])
                .add_leaf(8,1,Argument{removable},vec![InfixCrumb::RightOperand])
                .add_empty_child(9,Append)
                .done()
            .add_leaf(10,1,Operation,vec![SectionLeftCrumb::Opr])
            .add_empty_child(11,Append)
            .build();

        assert_eq!(expected,tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_right_assoc_section() {
        let parser    = Parser::new_or_panic();
        let ast       = parser.parse_line(",2,").unwrap();
        let tree      = ast.generate_tree().unwrap();
        let removable = true;

        let expected = TreeBuilder::new(3)
            .add_empty_child(0,Append)
            .add_leaf (0,1,Operation,vec![SectionRightCrumb::Opr])
            .add_child(1,2,Chained  ,vec![SectionRightCrumb::Arg])
                .add_empty_child(0,Append)
                .add_leaf(0,1,Argument{removable},vec![SectionLeftCrumb::Arg])
                .add_leaf(1,1,Operation,vec![SectionLeftCrumb::Opr])
                .add_empty_child(2,BeforeTarget)
                .done()
            .build();

        assert_eq!(expected,tree);
    }
}
