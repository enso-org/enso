//! A module with SpanTree structure definition.

use crate::prelude::*;

use crate::iter::LeafIterator;
use crate::iter::TreeFragment;

use data::text::Index;
use data::text::Size;



// ====================
// === Helper Types ===
// ====================

// === Kind ===

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
        is_removable:bool
    },
    /// A node being a normal (not target) parameter of parent Infix, Section or Prefix.
    Argument {
        /// Indicates if this node can be erased from SpanTree.
        is_removable:bool
    },
    /// A node being a placeholder for inserting new child to Prefix or Operator chain. It should
    /// have assigned span of length 0 and should not have any child.
    Empty(InsertType),
}

impl Kind {
    /// Match the value with `Kind::Empty{..}`.
    pub fn is_empty(self) -> bool {
        match self {
            Self::Empty(_) => true,
            _              => false
        }
    }
}

/// A helpful information about how the new AST should be inserted during Set action. See `action`
/// module.
#[allow(missing_docs)]
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum InsertType {
    BeforeTarget,
    AfterTarget,
    Append,
    /// Ast should be inserted as an argument at given index into the chain.
    /// Note that this is just argument index in the application, it may be not the same as the
    /// index of the function parameter, as `this` argument might be passed using the `this.func`
    /// notation.
    ExpectedArgument(usize),
}


// === Errors ===

#[allow(missing_docs)]
#[fail(display = "The crumb `{}` is invalid, only {} children present. Traversed crumbs: {:?}.",
    crumb,count,context)]
#[derive(Debug,Fail,Clone)]
pub struct InvalidCrumb {
    /// Crumb that was attempted.
    pub crumb : Crumb,
    /// Available children count.
    pub count : usize,
    /// Already traversed crumbs.
    pub context : Vec<Crumb>,
}


// === Crumbs ===

/// Identifies subtree within a node. It is the index of the child node.
pub type Crumb = usize;

/// Convert crumbs to crumbs pointing to a parent.
pub fn parent_crumbs(crumbs:&[Crumb]) -> Option<&[Crumb]> {
    crumbs.len().checked_sub(1).map(|new_len| &crumbs[..new_len])
}

// === Node ===

/// SpanTree Node.
///
/// Each node in SpanTree is bound to some span of code, and potentially may have corresponding
/// AST node.
#[derive(Clone,Debug,Eq,PartialEq)]
#[allow(missing_docs)]
pub struct Node {
    pub kind           : Kind,
    pub size           : Size,
    pub children       : Vec<Child>,
    pub expression_id  : Option<ast::Id>,
    pub parameter_info : Option<crate::ParameterInfo>,
}

impl Node {
    /// Create Empty node.
    pub fn new_empty(insert_type:InsertType) -> Self {
        Node {
            kind           : Kind::Empty(insert_type),
            size           : Size::new(0),
            children       : Vec::new(),
            expression_id  : None,
            parameter_info : None,
        }
    }

    /// Is this node empty?
    pub fn is_empty(&self) -> bool {
        self.kind.is_empty()
    }
}

/// A structure which contains `Node` being a child of some parent. It contains some additional
/// data regarding this relation
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct Child {
    /// A child node.
    pub node       : Node,
    /// An offset counted from the parent node starting index to the start of this node's span.
    pub offset     : Size,
    /// AST crumbs which lead from parent to child associated AST node.
    pub ast_crumbs : ast::Crumbs,
}


// === Node Reference ===

/// Crumbs specifying this node position related to root.
pub type Crumbs = Vec<Crumb>;

/// A reference to node inside some specific tree.
#[derive(Clone,Debug)]
pub struct Ref<'a> {
    /// The node's ref.
    pub node       : &'a Node,
    /// Span begin being an index counted from the root expression.
    pub span_begin : Index,
    /// Crumbs specifying this node position related to root.
    pub crumbs     : Vec<Crumb>,
    /// Ast crumbs locating associated AST node, related to the root's AST node.
    pub ast_crumbs : ast::Crumbs,
}

/// A result of `get_subnode_by_ast_crumbs`
#[derive(Clone,Debug)]
pub struct NodeFoundByAstCrumbs<'a,'b> {
    /// A node being a result of the lookup.
    pub node       : Ref<'a>,
    /// AST crumbs locating the searched AST node inside the AST of found SpanTree node.
    pub ast_crumbs : &'b [ast::Crumb],
}

impl<'a> Ref<'a> {
    /// Get span of current node.
    pub fn span(&self) -> data::text::Span {
        data::text::Span::new(self.span_begin,self.node.size)
    }

    /// Get the reference to child with given index. Returns None if index if out of bounds.
    pub fn child(mut self, index:usize) -> FallibleResult<Ref<'a>> {
        let err = || InvalidCrumb {
            crumb   : index,
            count   : self.node.children.len(),
            context : self.crumbs.clone()
        }.into();

        self.node.children.get(index).ok_or_else(err).map(|child| {
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
    pub fn leaf_iter(self) -> Box<dyn Iterator<Item=Ref<'a>> + 'a> {
        // FIXME rather should be part of the `LeafIterator`,
        //       see https://github.com/enso-org/ide/issues/698
        if self.children.is_empty() {
            Box::new(std::iter::once(self))
        } else {
            Box::new(LeafIterator::new(self, TreeFragment::AllNodes))
        }
    }

    /// Iterator over all children of operator/prefix chain starting from this node. See crate's
    /// documentation for more information about _chaining_.
    pub fn chain_children_iter(self) -> impl Iterator<Item=Ref<'a>> {
        LeafIterator::new(self, TreeFragment::ChainAndDirectChildren)
    }

    /// Get the sub-node (child, or further descendant) identified by `crumbs`.
    pub fn get_descendant<'b>
    (self, crumbs:impl IntoIterator<Item=&'b Crumb>) -> FallibleResult<Ref<'a>> {
        let mut iter = crumbs.into_iter();
        match iter.next() {
            Some(index) => self.child(*index).and_then(|child| child.get_descendant(iter)),
            None        => Ok(self)
        }
    }

    /// Get the sub-node by AST crumbs.
    ///
    /// The returned node will be node having corresponding AST node located by given `ast_crumbs`,
    /// or a leaf whose AST _contains_ node located by `ast_crumbs` - in that case returned
    /// structure will have non-empty `ast_crumbs` field.
    pub fn get_descendant_by_ast_crumbs<'b>
    (self, ast_crumbs:&'b [ast::Crumb]) -> Option<NodeFoundByAstCrumbs<'a,'b>> {
        if self.node.children.is_empty() || ast_crumbs.is_empty() {
            let node                 = self;
            let remaining_ast_crumbs = ast_crumbs;
            Some(NodeFoundByAstCrumbs{node, ast_crumbs: remaining_ast_crumbs })
        } else {
            let mut children = self.node.children.iter();
            // Please be advised, that the `ch.ast_crumhs` is not a field of Ref, but Child, and
            // therefore have different meaning!
            let next = children.find_position(|ch| {
                !ch.ast_crumbs.is_empty() && ast_crumbs.starts_with(&ch.ast_crumbs)
            });
            next.and_then(|(id,child)| {
                let ast_subcrumbs = &ast_crumbs[child.ast_crumbs.len()..];
                self.child(id).unwrap().get_descendant_by_ast_crumbs(ast_subcrumbs)
            })
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

impl<'a> Deref for Ref<'a> {
    type Target = Node;
    fn deref(&self) -> &Self::Target {
        &self.node
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

    use ast::crumbs;
    use crate::node::InsertType;

    #[test]
    fn node_lookup() {
        use ast::crumbs::InfixCrumb::*;

        let is_removable = false;
        let tree      = TreeBuilder::new(7)
            .add_leaf (0,1,Target{is_removable},vec![LeftOperand])
            .add_leaf (1,1,Operation,vec![Operator])
            .add_child(2,5,Argument{is_removable},vec![RightOperand])
                .add_leaf(0,2,Target{is_removable},vec![LeftOperand])
                .add_leaf(3,1,Operation,vec![Operator])
                .add_leaf(4,1,Argument{is_removable},vec![RightOperand])
                .done()
            .build();

        let root         = tree.root_ref();
        let child1       = root.clone().  get_descendant(&vec![0]).unwrap();
        let child2       = root.clone().  get_descendant(&vec![2]).unwrap();
        let grand_child1 = root.clone().  get_descendant(&vec![2, 0]).unwrap();
        let grand_child2 = child2.clone().get_descendant(&vec![1]).unwrap();

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
        assert!(root.clone().get_descendant(&vec![3]).is_err());
        assert!(root.clone().get_descendant(&vec![1, 0]).is_err());
        assert!(root.clone().get_descendant(&vec![2, 1, 0]).is_err());
        assert!(root.clone().get_descendant(&vec![2, 5]).is_err());
        assert!(root.get_descendant(&vec![2, 5, 0]).is_err());
    }

    #[test]
    fn node_lookup_by_ast_crumbs() {
        use ast::crumbs::BlockCrumb::*;
        use ast::crumbs::InfixCrumb::*;
        use ast::crumbs::PrefixCrumb::*;

        let is_removable = false;
        let tree      = TreeBuilder::new(7)
            .add_leaf (0,1,Target{is_removable},vec![LeftOperand])
            .add_empty_child(1,InsertType::AfterTarget)
            .add_leaf (1,1,Operation,vec![Operator])
            .add_child(2,5,Argument{is_removable},vec![RightOperand])
                .add_leaf(0,3,Operation,vec![Func])
                .add_leaf(3,1,Target{is_removable},vec![Arg])
            .done()
            .build();

        let root  = tree.root_ref();
        let cases:&[(ast::Crumbs,&[usize],ast::Crumbs)] = &
            [ (crumbs![LeftOperand]              ,&[0]  ,crumbs![])
            , (crumbs![RightOperand]             ,&[3]  ,crumbs![])
            , (crumbs![RightOperand,Func]        ,&[3,0],crumbs![])
            , (crumbs![RightOperand,Arg]         ,&[3,1],crumbs![])
            , (crumbs![RightOperand,Arg,HeadLine],&[3,1],crumbs![HeadLine])
            ];

        for case in cases {
            let (crumbs,expected_crumbs,expected_remaining_ast_crumbs) = case;
            let result = root.clone().get_descendant_by_ast_crumbs(&crumbs).unwrap();
            assert_eq!(result.node.crumbs.as_slice(), *expected_crumbs);
            assert_eq!(result.ast_crumbs, expected_remaining_ast_crumbs.as_slice());
        }
    }
}
