//! An utility builder to be used in tests.

use crate::node;
use crate::node::Payload;
use crate::Node;
use crate::SpanTree;

use ast::crumbs::IntoCrumbs;



// =====================
// === Builder Trait ===
// =====================

// FIXME[WD]: This builder is obsolete. Please use `ChildBuilder` instead.
/// A trait with common operations for all builders.
pub trait Builder<T: Payload>: Sized {
    /// Reference to currently built  node.
    fn node_being_built(&mut self) -> &mut Node<T>;

    /// Add new AST-type child to node. Returns the child's builder which may be used to further
    /// extend this branch of the tree.
    fn add_child(
        self,
        offset: usize,
        len: usize,
        kind: impl Into<node::Kind>,
        crumbs: impl IntoCrumbs,
    ) -> ChildBuilder<Self, T> {
        let kind = kind.into();
        let node = Node::<T>::new().with_kind(kind).with_size(len.into());
        let child = node::Child { node, offset: offset.into(), ast_crumbs: crumbs.into_crumbs() };
        ChildBuilder { built: child, parent: self }
    }

    /// Add a leaf AST-type child to node.
    fn add_leaf(
        self,
        offset: usize,
        len: usize,
        kind: impl Into<node::Kind>,
        crumbs: impl IntoCrumbs,
    ) -> Self {
        self.add_child(offset, len, kind, crumbs).done()
    }

    /// Add an Empty-type child to node.
    fn add_empty_child(mut self, offset: usize, kind: impl Into<node::Kind>) -> Self {
        let child = node::Child {
            node:       Node::<T>::new().with_kind(kind),
            offset:     offset.into(),
            ast_crumbs: vec![],
        };
        self.node_being_built().children.push(child);
        self
    }

    /// Set expression id for this node.
    fn set_ast_id(mut self, id: ast::Id) -> Self {
        self.node_being_built().ast_id = Some(id);
        self
    }
}



/// ================
/// === Builders ===
/// ================

// === SpanTree Builder ===

/// The main builder for SpanTree.
#[derive(Debug)]
pub struct TreeBuilder<T = ()> {
    built: Node<T>,
}

impl<T: Payload> TreeBuilder<T> {
    /// Create new builder for tree with root having length `len`.
    pub fn new(len: usize) -> Self {
        let built = Node::<T>::new().with_kind(node::Kind::Root).with_size(len.into());
        TreeBuilder { built }
    }

    /// Return the built SpanTree.
    pub fn build(self) -> SpanTree<T> {
        let root = self.built;
        SpanTree { root }
    }
}

impl<T: Payload> Builder<T> for TreeBuilder<T> {
    fn node_being_built(&mut self) -> &mut Node<T> {
        &mut self.built
    }
}


// === Child Node Builder ===

/// A builder for some child node. This builder may be returned from `add_ast_child` function.
#[derive(Debug)]
pub struct ChildBuilder<Parent, T> {
    built:  node::Child<T>,
    parent: Parent,
}

impl<Parent: Builder<T>, T: Payload> ChildBuilder<Parent, T> {
    /// Finish child building and return builder of the node's Parent.
    pub fn done(mut self) -> Parent {
        self.parent.node_being_built().children.push(self.built);
        self.parent
    }
}

impl<Parent, T: Payload> Builder<T> for ChildBuilder<Parent, T> {
    fn node_being_built(&mut self) -> &mut Node<T> {
        &mut self.built.node
    }
}
