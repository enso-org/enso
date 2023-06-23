//! An utility builder to be used in tests.

use crate::prelude::*;

use crate::node;
use crate::Node;
use crate::SpanTree;

use ast::crumbs::IntoCrumbs;



// =====================
// === Builder Trait ===
// =====================

// === SpanTree Builder ===

/// The main builder for SpanTree.
pub type TreeBuilder = ChildBuilder<()>;

impl TreeBuilder {
    /// Create new builder for tree with root having length `len`.
    pub fn new(len: usize) -> Self {
        let node = Node::new().with_kind(node::Kind::Root).with_size(len.into());
        let built = node::Child { node, ..default() };
        TreeBuilder { built, parent: () }
    }

    /// Return the built SpanTree.
    pub fn build(self) -> SpanTree {
        let root = self.built.node;
        SpanTree { root }
    }
}


// === Child Node Builder ===

/// A builder for some child node. This builder may be returned from `add_ast_child` function.
#[derive(Debug)]
pub struct ChildBuilder<Parent> {
    built:  node::Child,
    parent: Parent,
}

impl<Grandparent> ChildBuilder<ChildBuilder<Grandparent>> {
    /// Finish child building and return builder of the node's Parent.
    pub fn done(mut self) -> ChildBuilder<Grandparent> {
        self.parent.built.node.children.push(self.built);
        self.parent
    }
}

impl<Parent> ChildBuilder<Parent> {
    /// Add new AST-type child to node. Returns the child's builder which may be used to further
    /// extend this branch of the tree.
    pub fn add_child(
        self,
        parent_offset: usize,
        len: usize,
        kind: impl Into<node::Kind>,
        crumbs: impl IntoCrumbs,
    ) -> ChildBuilder<Self> {
        let kind = kind.into();
        let node = Node::new().with_kind(kind).with_size(len.into());
        let prev_child = self.built.node.children.last();
        let prev_child_end = prev_child.map_or(0, |c| (c.parent_offset + c.node.size).as_usize());
        let sibling_offset = parent_offset.saturating_sub(prev_child_end);
        let child = node::Child {
            node,
            parent_offset: parent_offset.into(),
            sibling_offset: sibling_offset.into(),
            ast_crumbs: crumbs.into_crumbs(),
        };
        ChildBuilder { built: child, parent: self }
    }

    /// Add a leaf AST-type child to node.
    pub fn add_leaf(
        self,
        offset: usize,
        len: usize,
        kind: impl Into<node::Kind>,
        crumbs: impl IntoCrumbs,
    ) -> Self {
        self.add_child(offset, len, kind, crumbs).done()
    }

    /// Add an Empty-type child to node.
    pub fn add_empty_child(self, offset: usize, kind: impl Into<node::Kind>) -> Self {
        self.add_leaf(offset, 0, kind, ast::crumbs![])
    }

    /// Set expression id for this node.
    pub fn set_ast_id(mut self, id: ast::Id) -> Self {
        self.built.node.ast_id = Some(id);
        self
    }
}
