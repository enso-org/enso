//! A module with SpanTree structure definition.

use crate::prelude::*;

use crate::iter::LeafIterator;
use crate::iter::TreeFragment;

use data::text::Index;
use data::text::Size;



// ====================
// === Helper Types ===
// ====================

/// An enum describing kind of node.
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum Kind {
    /// A root of the expression this tree was generated.
    Root,
    /// A node chained with parent node. See crate's docs for more info about chaining.
    Chained,
    /// A node representing operation (operator or function) of parent Infix, Section or Prefix.
    Operation,
    /// A node being a target (or "self") parameter of parent Infix, Section or Prefix.
    Target {
        /// Indicates if this node can be erased from SpanTree.
        removable:bool
    },
    /// A node being a normal (not target) parameter of parent Infix, Section or Prefix.
    Argument {
        /// Indicates if this node can be erased from SpanTree.
        removable:bool
    },
    /// A node being a placeholder for inserting new child to Prefix or Operator chain. It should
    /// have assigned span of length 0 and should not have any child.
    Empty(InsertType),
}

/// A helpful information about how the new AST should be inserted during Set action. See `action`
/// module.
#[allow(missing_docs)]
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum InsertType {BeforeTarget,AfterTarget,Append}

/// A type which identifies some node in SpanTree. This is essentially a iterator over child
/// indices, so `[4]` means _root's fifth child_, `[4, 2]`means _the third child of root's fifth
/// child_ and so on.
pub trait Crumbs = IntoIterator<Item=usize>;


// === Node ===

/// SpanTree Node.
///
/// Each node in SpanTree is bound to some span of code, and potentially may have corresponding
/// AST node.
#[derive(Debug,Eq,PartialEq)]
#[allow(missing_docs)]
pub struct Node {
    pub kind     : Kind,
    pub size     : Size,
    pub children : Vec<Child>,
}

impl Node {
    /// Create Empty node.
    pub fn new_empty(insert_type:InsertType) -> Self {
        Node {
            kind     : Kind::Empty(insert_type),
            size     : Size::new(0),
            children : Vec::new(),
        }
    }

    /// Is this node empty?
    pub fn is_empty(&self) -> bool {
        match self.kind {
            Kind::Empty(_) => true,
            _              => false,
        }
    }
}

/// A structure which contains `Node` being a child of some parent. It contains some additional
/// data regarding this relation
#[derive(Debug,Eq,PartialEq)]
pub struct Child {
    /// A child node.
    pub node                : Node,
    /// An offset counted from the parent node starting index to the start of this node's span.
    pub offset              : Size,
    /// AST crumbs which lead from parent to child associated AST node.
    pub ast_crumbs          : ast::Crumbs,
}


// === Node Reference ===

/// A reference to node inside some specific tree.
#[derive(Clone,Debug)]
pub struct Ref<'a> {
    /// The node's ref.
    pub node       : &'a Node,
    /// Span begin being an index counted from the root expression.
    pub span_begin : Index,
    /// Crumbs specifying this node position related to root. See `Crumbs` docs.
    pub crumbs     : Vec<usize>,
    /// Ast crumbs locating associated AST node, related to the root's AST node.
    pub ast_crumbs : ast::Crumbs,
}

impl<'a> Ref<'a> {
    /// Get span of current node.
    pub fn span(&self) -> data::text::Span {
        data::text::Span::new(self.span_begin,self.node.size)
    }

    /// Get the reference to child with given index. Returns None if index if out of bounds.
    pub fn child(mut self, index:usize) -> Option<Ref<'a>> {
        self.node.children.get(index).map(|child| {
            self.crumbs.push(index);
            self.ast_crumbs.extend(child.ast_crumbs.clone());
            self.span_begin += child.offset;
            self.node = &child.node;
            self
        })
    }

    /// Iterator over all direct children producing `Ref`s.
    pub fn children_iter(self) -> impl Iterator<Item=Ref<'a>> {
        let children_count = self.node.children.len();
        (0..children_count).map(move |i| self.clone().child(i).unwrap())
    }

    /// Iterator over all leaves of subtree rooted in the `self`.
    pub fn leaf_iter(self) -> impl Iterator<Item=Ref<'a>> {
        LeafIterator::new(self, TreeFragment::AllNodes)
    }

    /// Iterator over all children of operator/prefix chain starting from this node. See crate's
    /// documentation for more information about _chaining_.
    pub fn chain_children_iter(self) -> impl Iterator<Item=Ref<'a>> {
        LeafIterator::new(self, TreeFragment::ChainAndDirectChildren)
    }

    /// Get the sub-node (child, or further descendant) identified by `crumbs`.
    pub fn traverse_subnode(self, crumbs:impl Crumbs) -> Option<Ref<'a>> {
        let mut iter = crumbs.into_iter();
        match iter.next() {
            Some(index) => self.child(index).and_then(|child| child.traverse_subnode(iter)),
            None        => Some(self)
        }
    }

    /// Get the node which exactly matches the given Span. If there many such node's, it pick first
    /// found by DFS.
    pub fn find_by_span(self, span:&data::text::Span) -> Option<Ref<'a>> {
        if self.span() == *span {
            Some(self)
        } else {
            self.children_iter().find_map(|ch|
                ch.span().contains_span(span).and_option_from(|| ch.find_by_span(&span))
            )
        }
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use crate::builder::Builder;
    use crate::builder::TreeBuilder;
    use crate::node::Kind::*;

    use ast::crumbs::InfixCrumb;

    #[test]
    fn traversing_tree() {
        use InfixCrumb::*;
        let removable = false;
        let tree      = TreeBuilder::new(7)
            .add_leaf (0,1,Target{removable},vec![LeftOperand])
            .add_leaf (1,1,Operation,vec![Operator])
            .add_child(2,5,Argument{removable},vec![RightOperand])
                .add_leaf(0,2,Target{removable},vec![LeftOperand])
                .add_leaf(3,1,Operation,vec![Operator])
                .add_leaf(4,1,Argument{removable},vec![RightOperand])
                .done()
            .build();

        let root         = tree.root_ref();
        let child1       = root.clone().traverse_subnode(vec![0]).unwrap();
        let child2       = root.clone().traverse_subnode(vec![2]).unwrap();
        let grand_child1 = root.clone().traverse_subnode(vec![2,0]).unwrap();
        let grand_child2 = child2.clone().traverse_subnode(vec![1]).unwrap();

        // Span begin.
        assert_eq!(root.span_begin.value        , 0);
        assert_eq!(child1.span_begin.value      , 0);
        assert_eq!(child2.span_begin.value      , 2);
        assert_eq!(grand_child1.span_begin.value, 2);
        assert_eq!(grand_child2.span_begin.value, 5);

        // Length
        assert_eq!(root.node.size.value, 7);
        assert_eq!(child1.node.size.value, 1);
        assert_eq!(child2.node.size.value, 5);
        assert_eq!(grand_child1.node.size.value, 2);
        assert_eq!(grand_child2.node.size.value, 1);

        // crumbs
        assert_eq!(root.crumbs        , Vec::<usize>::new());
        assert_eq!(child1.crumbs      , [0]            );
        assert_eq!(child2.crumbs      , [2]            );
        assert_eq!(grand_child1.crumbs, [2,0]          );
        assert_eq!(grand_child2.crumbs, [2,1]          );

        // AST crumbs
        assert_eq!(root.ast_crumbs        , []                                      );
        assert_eq!(child1.ast_crumbs      , [LeftOperand.into()]                    );
        assert_eq!(child2.ast_crumbs      , [RightOperand.into()]                   );
        assert_eq!(grand_child1.ast_crumbs, [RightOperand.into(),LeftOperand.into()]);
        assert_eq!(grand_child2.ast_crumbs, [RightOperand.into(),Operator.into()]   );

        // Not existing nodes
        assert!(root.clone().traverse_subnode(vec![3]).is_none());
        assert!(root.clone().traverse_subnode(vec![1,0]).is_none());
        assert!(root.clone().traverse_subnode(vec![2,1,0]).is_none());
        assert!(root.clone().traverse_subnode(vec![2,5]).is_none());
        assert!(root.traverse_subnode(vec![2,5,0]).is_none());
    }
}
