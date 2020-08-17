//! A module containing code related to SpanTree generation.
pub mod macros;

use crate::prelude::*;

use crate::node;
use crate::node::InsertType;
use crate::Node;
use crate::SpanTree;

use ast::Ast;
use ast::MacroMatchSegment;
use ast::MacroAmbiguousSegment;
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
                ast::Shape::Prefix(_) =>
                    ast::prefix::Chain::from_ast(self).unwrap().generate_node(kind),
                // Lambdas should fall in _ case, because we don't want to create subports for
                // them
                ast::Shape::Match(_) if ast::macros::as_lambda_match(self).is_none() =>
                    ast::known::Match::try_new(self.clone_ref()).unwrap().generate_node(kind),
                ast::Shape::Ambiguous(_) =>
                    ast::known::Ambiguous::try_new(self.clone_ref()).unwrap().generate_node(kind),
                _  => {
                    let size          = Size::new(self.len());
                    let children      = default();
                    let expression_id = self.id;
                    Ok(Node {kind,size,children,expression_id})
                },
            }
        }
    }
}


// === Operators (Sections and Infixes) ===

impl SpanTreeGenerator for ast::opr::Chain {
    fn generate_node(&self, kind:node::Kind) -> FallibleResult<Node> {
        // Removing operands is possible only when chain has at least 3 of them
        // (target and two arguments).
        let is_removable                                 = self.args.len() >= 2;
        let node_and_offset:FallibleResult<(Node,usize)> = match &self.target {
            Some(target) => {
                let node = target.arg.generate_node(node::Kind::Target {is_removable})?;
                Ok((node,target.offset))
            },
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

                gen.generate_ast_node(arg_ast,node::Kind::Argument {is_removable})?;
            }
            gen.generate_empty_node(InsertType::Append);

            if ast::opr::assoc(&self.operator) == Assoc::Right {
                gen.reverse_children();
            }

            Ok((Node {
                kind          : if is_last {kind} else {node::Kind::Chained},
                size          : gen.current_offset,
                children      : gen.children,
                expression_id : elem.infix_id,
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
        let is_removable = self.args.len() >= 2;
        let node         = self.func.generate_node(node::Kind::Operation);
        self.args.iter().enumerate().fold(node, |node,(i,arg)| {
            let node     = node?;
            let is_first = i == 0;
            let is_last  = i + 1 == self.args.len();
            let arg_kind = if is_first { node::Kind::Target {is_removable} }
                else { node::Kind::Argument {is_removable} };

            let mut gen = ChildGenerator::default();
            gen.add_node(vec![Func.into()],node);
            gen.spacing(arg.sast.off);
            if let node::Kind::Target {..} = arg_kind {
                gen.generate_empty_node(InsertType::BeforeTarget);
            }
            gen.generate_ast_node(Located::new(Arg,arg.sast.wrapped.clone_ref()), arg_kind)?;
            gen.generate_empty_node(InsertType::Append);
            Ok(Node {
                kind          : if is_last {kind} else {node::Kind::Chained},
                size          : gen.current_offset,
                children      : gen.children,
                expression_id : arg.prefix_id,
            })
        })
    }
}


// === Match ===

impl SpanTreeGenerator for ast::known::Match {
    fn generate_node(&self, kind:node::Kind) -> FallibleResult<Node> {
        let is_removable  = false;
        let children_kind = node::Kind::Argument {is_removable};
        let mut gen   = ChildGenerator::default();
        if let Some(pat) = &self.pfx {
            for macros::AstInPattern {ast,crumbs} in macros::all_ast_nodes_in_pattern(&pat) {
                let ast_crumb   = ast::crumbs::MatchCrumb::Pfx {val:crumbs};
                let located_ast = Located::new(ast_crumb,ast.wrapped);
                gen.generate_ast_node(located_ast,children_kind)?;
                gen.spacing(ast.off);
            }
        }
        let first_segment_index = 0;
        generate_children_from_segment(&mut gen,first_segment_index,&self.segs.head)?;
        for (index,segment) in self.segs.tail.iter().enumerate() {
            gen.spacing(segment.off);
            generate_children_from_segment(&mut gen,index+1,&segment.wrapped)?;
        }
        Ok(Node {kind,
            size          : gen.current_offset,
            children      : gen.children,
            expression_id : self.id(),
        })
    }
}

fn generate_children_from_segment
(gen:&mut ChildGenerator, index:usize, segment:&MacroMatchSegment<Ast>) -> FallibleResult<()> {
    let is_removable  = false;
    let children_kind = node::Kind::Argument {is_removable};
    gen.spacing(segment.head.len());
    for macros::AstInPattern {ast,crumbs} in macros::all_ast_nodes_in_pattern(&segment.body) {
        gen.spacing(ast.off);
        let segment_crumb = ast::crumbs::SegmentMatchCrumb::Body {val:crumbs};
        let ast_crumb     = ast::crumbs::MatchCrumb::Segs{val:segment_crumb, index};
        let located_ast   = Located::new(ast_crumb,ast.wrapped);
        gen.generate_ast_node(located_ast,children_kind)?;
    }
    Ok(())
}


// === Ambiguous ==

impl SpanTreeGenerator for ast::known::Ambiguous {
    fn generate_node(&self, kind:node::Kind) -> FallibleResult<Node> {
        let mut gen             = ChildGenerator::default();
        let first_segment_index = 0;
        generate_children_from_abiguous_segment(&mut gen,first_segment_index,&self.segs.head)?;
        for (index,segment) in self.segs.tail.iter().enumerate() {
            gen.spacing(segment.off);
            generate_children_from_abiguous_segment(&mut gen, index+1, &segment.wrapped)?;
        }
        Ok(Node{kind,
            size          : gen.current_offset,
            children      : gen.children,
            expression_id : self.id(),
        })
    }
}

fn generate_children_from_abiguous_segment
(gen:&mut ChildGenerator, index:usize, segment:&MacroAmbiguousSegment<Ast>) -> FallibleResult<()> {
    let is_removable  = false;
    let children_kind = node::Kind::Argument {is_removable};
    gen.spacing(segment.head.len());
    if let Some(sast) = &segment.body {
        gen.spacing(sast.off);
        let field       = ast::crumbs::AmbiguousSegmentCrumb::Body;
        let located_ast = Located::new(ast::crumbs::AmbiguousCrumb{index,field}, sast.clone_ref());
        gen.generate_ast_node(located_ast,children_kind)?;
    }
    Ok(())
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

    use ast::crumbs::AmbiguousCrumb;
    use ast::crumbs::AmbiguousSegmentCrumb;
    use ast::crumbs::InfixCrumb;
    use ast::crumbs::PatternMatchCrumb;
    use ast::crumbs::PrefixCrumb;
    use ast::crumbs::SectionLeftCrumb;
    use ast::crumbs::SectionRightCrumb;
    use ast::crumbs::SectionSidesCrumb;
    use parser::Parser;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;
    use ast::IdMap;

    wasm_bindgen_test_configure!(run_in_browser);

    /// A helper function which removes information about expression id from thw tree rooted at
    /// `node`.
    ///
    /// It is used in tests. Because parser can assign id as he pleases, therefore to keep tests
    /// cleaner the expression ids are removed before comparing trees.
    fn clear_expression_ids(node:&mut Node) {
        node.expression_id = None;
        for child in &mut node.children {
            clear_expression_ids(&mut child.node);
        }
    }

    #[wasm_bindgen_test]
    fn generating_span_tree() {
        let parser     = Parser::new_or_panic();
        let mut id_map = IdMap::default();
        id_map.generate(0..15);
        id_map.generate(0..11);
        id_map.generate(12..13);
        id_map.generate(14..15);
        id_map.generate(4..11);
        let ast      = parser.parse_line_with_id_map("2 + foo bar - 3",id_map.clone()).unwrap();
        let mut tree = ast.generate_tree().unwrap();

        // Check the expression ids we defined:
        for id_map_entry in id_map.vec {
            let (span,id) = id_map_entry;
            let node      = tree.root_ref().find_by_span(&span);
            assert!(node.is_some(), "Node with span {} not found", span);
            assert_eq!(node.unwrap().node.expression_id, Some(id));
        }

        // Check the other fields:
        clear_expression_ids(&mut tree.root);
        let is_removable = false;
        let expected     = TreeBuilder::new(15)
            .add_empty_child(0,BeforeTarget)
            .add_child(0,11,Target{is_removable},InfixCrumb::LeftOperand)
                .add_empty_child(0,BeforeTarget)
                .add_leaf (0,1,Target{is_removable},InfixCrumb::LeftOperand)
                .add_empty_child(1,AfterTarget)
                .add_leaf (2,1,Operation,InfixCrumb::Operator)
                .add_child(4,7,Argument{is_removable} ,InfixCrumb::RightOperand)
                    .add_leaf(0,3,Operation,PrefixCrumb::Func)
                    .add_empty_child(4,BeforeTarget)
                    .add_leaf(4,3,Target{is_removable},PrefixCrumb::Arg)
                    .add_empty_child(7,Append)
                    .done()
                .add_empty_child(11,Append)
                .done()
            .add_empty_child(11,AfterTarget)
            .add_leaf(12,1,Operation,InfixCrumb::Operator)
            .add_leaf(14,1,Argument{is_removable},InfixCrumb::RightOperand)
            .add_empty_child(15,Append)
            .build();

        assert_eq!(expected,tree)
    }

    #[wasm_bindgen_test]
    fn generate_span_tree_with_chains() {
        let parser   = Parser::new_or_panic();
        let ast      = parser.parse_line("2 + 3 + foo bar baz 13 + 5").unwrap();
        let mut tree = ast.generate_tree().unwrap();
        clear_expression_ids(&mut tree.root);

        let is_removable = true;
        let expected     = TreeBuilder::new(26)
            .add_child(0,22,Chained,InfixCrumb::LeftOperand)
                .add_child(0,5,Chained,InfixCrumb::LeftOperand)
                    .add_empty_child(0,BeforeTarget)
                    .add_leaf(0,1,Target{is_removable},InfixCrumb::LeftOperand)
                    .add_empty_child(1,AfterTarget)
                    .add_leaf(2,1,Operation,InfixCrumb::Operator)
                    .add_leaf(4,1,Argument{is_removable},InfixCrumb::RightOperand)
                    .add_empty_child(5,Append)
                    .done()
                .add_leaf (6,1 ,Operation,InfixCrumb::Operator)
                .add_child(8,14,Argument{is_removable},InfixCrumb::RightOperand)
                    .add_child(0,11,Chained,PrefixCrumb::Func)
                        .add_child(0,7,Chained,PrefixCrumb::Func)
                            .add_leaf(0,3,Operation,PrefixCrumb::Func)
                            .add_empty_child(4,BeforeTarget)
                            .add_leaf(4,3,Target{is_removable},PrefixCrumb::Arg)
                            .add_empty_child(7,Append)
                            .done()
                        .add_leaf(8,3,Argument{is_removable},PrefixCrumb::Arg)
                        .add_empty_child(11,Append)
                        .done()
                    .add_leaf(12,2,Argument{is_removable},PrefixCrumb::Arg)
                    .add_empty_child(14,Append)
                    .done()
                .add_empty_child(22,Append)
                .done()
            .add_leaf(23,1,Operation,InfixCrumb::Operator)
            .add_leaf(25,1,Argument{is_removable},InfixCrumb::RightOperand)
            .add_empty_child(26,Append)
            .build();

        assert_eq!(expected,tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_right_assoc_operator() {
        let parser   = Parser::new_or_panic();
        let ast      = parser.parse_line("1,2,3").unwrap();
        let mut tree = ast.generate_tree().unwrap();
        clear_expression_ids(&mut tree.root);

        let is_removable = true;
        let expected     = TreeBuilder::new(5)
            .add_empty_child(0,Append)
            .add_leaf (0,1,Argument{is_removable},InfixCrumb::LeftOperand)
            .add_leaf (1,1,Operation,InfixCrumb::Operator)
            .add_child(2,3,Chained  ,InfixCrumb::RightOperand)
                .add_empty_child(0,Append)
                .add_leaf(0,1,Argument{is_removable},InfixCrumb::LeftOperand)
                .add_leaf(1,1,Operation,InfixCrumb::Operator)
                .add_empty_child(2,AfterTarget)
                .add_leaf(2,1,Target{is_removable},InfixCrumb::RightOperand)
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
        let ast      = parser.parse_line("+ * + + 2 +").unwrap();
        let mut tree = ast.generate_tree().unwrap();
        clear_expression_ids(&mut tree.root);

        let is_removable = true;
        let expected     = TreeBuilder::new(11)
            .add_child(0,9,Chained,SectionLeftCrumb::Arg)
                .add_child(0,5,Chained,InfixCrumb::LeftOperand)
                    .add_child(0,3,Chained,SectionLeftCrumb::Arg)
                        .add_empty_child(0,BeforeTarget)
                        .add_leaf (0,1,Operation,SectionRightCrumb::Opr)
                        .add_child(2,1,Argument{is_removable},SectionRightCrumb::Arg)
                            .add_empty_child(0,BeforeTarget)
                            .add_leaf(0,1,Operation,SectionSidesCrumb)
                            .add_empty_child(1,Append)
                            .done()
                        .add_empty_child(3,Append)
                        .done()
                    .add_leaf(4,1,Operation,SectionLeftCrumb::Opr)
                    .add_empty_child(5,Append)
                    .done()
                .add_leaf(6,1,Operation,InfixCrumb::Operator)
                .add_leaf(8,1,Argument{is_removable},InfixCrumb::RightOperand)
                .add_empty_child(9,Append)
                .done()
            .add_leaf(10,1,Operation,SectionLeftCrumb::Opr)
            .add_empty_child(11,Append)
            .build();

        assert_eq!(expected,tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_right_assoc_section() {
        let parser   = Parser::new_or_panic();
        let ast      = parser.parse_line(",2,").unwrap();
        let mut tree = ast.generate_tree().unwrap();
        clear_expression_ids(&mut tree.root);

        let is_removable = true;
        let expected     = TreeBuilder::new(3)
            .add_empty_child(0,Append)
            .add_leaf (0,1,Operation,SectionRightCrumb::Opr)
            .add_child(1,2,Chained  ,SectionRightCrumb::Arg)
                .add_empty_child(0,Append)
                .add_leaf(0,1,Argument{is_removable},SectionLeftCrumb::Arg)
                .add_leaf(1,1,Operation,SectionLeftCrumb::Opr)
                .add_empty_child(2,BeforeTarget)
                .done()
            .build();

        assert_eq!(expected,tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_matched_macros() {
        use PatternMatchCrumb::*;

        let parser     = Parser::new_or_panic();
        let mut id_map = IdMap::default();
        id_map.generate(0..29);
        let expression = "if foo then (a + b) x else ()";
        let ast        = parser.parse_line_with_id_map(expression,id_map.clone()).unwrap();
        let mut tree   = ast.generate_tree().unwrap();

        // Check if expression id is set
        let (_,expected_id) = id_map.vec.first().unwrap();
        assert_eq!(tree.root_ref().expression_id,Some(*expected_id));

        // Check the other fields
        clear_expression_ids(&mut tree.root);
        let is_removable        = false;
        let if_then_else_cr     = vec![Seq { right: false }, Or, Build];
        let parens_cr           = vec![Seq { right: false }, Or, Or, Build];
        let segment_body_crumbs = |index:usize, pattern_crumb:&Vec<PatternMatchCrumb>| {
            let val = ast::crumbs::SegmentMatchCrumb::Body {val:pattern_crumb.clone()};
            ast::crumbs::MatchCrumb::Segs {val,index}
        };

        let expected = TreeBuilder::new(29)
            .add_leaf(3,3,Argument {is_removable},segment_body_crumbs(0,&if_then_else_cr))
            .add_child(12,9,Argument {is_removable},segment_body_crumbs(1,&if_then_else_cr))
                .add_child(0,7,Operation,PrefixCrumb::Func)
                    .add_child(1,5,Argument {is_removable},segment_body_crumbs(0,&parens_cr))
                        .add_empty_child(0,BeforeTarget)
                        .add_leaf(0,1,Target {is_removable},InfixCrumb::LeftOperand)
                        .add_empty_child(1,AfterTarget)
                        .add_leaf(2,1,Operation,InfixCrumb::Operator)
                        .add_leaf(4,1,Argument {is_removable},InfixCrumb::RightOperand)
                        .add_empty_child(5,Append)
                        .done()
                    .done()
                .add_empty_child(8,BeforeTarget)
                .add_leaf(8,1,Target {is_removable},PrefixCrumb::Arg)
                .add_empty_child(9,Append)
                .done()
            .add_leaf(27,2,Argument {is_removable},segment_body_crumbs(2,&if_then_else_cr))
            .build();

        assert_eq!(expected,tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_from_ambiguous_macros() {
        let parser     = Parser::new_or_panic();
        let mut id_map = IdMap::default();
        id_map.generate(0..2);
        let ast      = parser.parse_line_with_id_map("(4",id_map.clone()).unwrap();
        let mut tree = ast.generate_tree().unwrap();

        // Check the expression id:
        let (_,expected_id) = id_map.vec.first().unwrap();
        assert_eq!(tree.root_ref().expression_id,Some(*expected_id));

        // Check the other fields:
        clear_expression_ids(&mut tree.root);
        let is_removable = false;
        let crumb        = AmbiguousCrumb{index:0, field:AmbiguousSegmentCrumb::Body};
        let expected     = TreeBuilder::new(2)
            .add_leaf(1,1,Argument {is_removable},crumb)
            .build();

        assert_eq!(expected,tree);
    }

    #[wasm_bindgen_test]
    fn generating_span_tree_for_lambda() {
        let parser   = Parser::new_or_panic();
        let ast      = parser.parse_line("foo a-> b + c").unwrap();
        let mut tree = ast.generate_tree().unwrap();
        clear_expression_ids(&mut tree.root);

        let is_removable = false;
        let expected     = TreeBuilder::new(13)
            .add_leaf(0,3,Operation,PrefixCrumb::Func)
            .add_empty_child(4,BeforeTarget)
            .add_leaf(4,9,Target{is_removable},PrefixCrumb::Arg)
            .add_empty_child(13,Append)
            .build();

        assert_eq!(expected,tree);
    }
}
