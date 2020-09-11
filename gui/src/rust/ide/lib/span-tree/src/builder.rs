//! An utility builder to be used in tests.
use crate::node;
use crate::Node;
use crate::SpanTree;

use data::text::Size;
use ast::crumbs::IntoCrumbs;



// =====================
// === Builder Trait ===
// =====================

/// A trait with common operations for all builders.
pub trait Builder : Sized {
    /// Reference to currently built  node.
    fn node_being_built(&mut self) -> &mut Node;

    /// Add new AST-type child to node. Returns the child's builder which may be used to further
    /// extend this branch of the tree.
    fn add_child
    (self, offset:usize, len:usize, kind:node::Kind, crumbs:impl IntoCrumbs) -> ChildBuilder<Self> {
        let node = Node {kind,
            size          : Size::new(len),
            children      : vec![],
            expression_id : None,
            parameter_info: None,
        };
        let child = node::Child { node,
            offset              : Size::new(offset),
            ast_crumbs          : crumbs.into_crumbs()
        };
        ChildBuilder {
            built  : child,
            parent : self
        }
    }

    /// Add a leaf AST-type child to node.
    fn add_leaf(self, offset:usize, len:usize, kind:node::Kind, crumbs:impl IntoCrumbs) -> Self {
        self.add_child(offset,len,kind,crumbs).done()
    }

    /// Add an Empty-type child to node.
    fn add_empty_child(mut self, offset:usize, insert_type:node::InsertType) -> Self {
        let child = node::Child {
            node                : Node::new_empty(insert_type),
            offset              : Size::new(offset),
            ast_crumbs          : vec![]
        };
        self.node_being_built().children.push(child);
        self
    }

    /// Set expression id for this node.
    fn set_expression_id(mut self, id:ast::Id) -> Self {
        self.node_being_built().expression_id = Some(id);
        self
    }
}



/// ================
/// === Builders ===
/// ================

// === SpanTree Builder ===

/// The main builder for SpanTree.
#[derive(Debug)]
pub struct TreeBuilder {
    built : Node,
}

impl TreeBuilder {
    /// Create new builder for tree with root having length `len`.
    pub fn new(len:usize) -> Self {
        TreeBuilder {
            built : Node {
                kind          : node::Kind::Root,
                size          : Size::new(len),
                children      : vec![],
                expression_id : None,
                parameter_info: None,
            }
        }
    }

    /// Return the built SpanTree.
    pub fn build(self) -> SpanTree {
        SpanTree {
            root : self.built
        }
    }
}

impl Builder for TreeBuilder {
    fn node_being_built(&mut self) -> &mut Node {
        &mut self.built
    }
}


// === Child Node Builder ===

/// A builder for some child node. This builder may be returned from `add_ast_child` function.
#[derive(Debug)]
pub struct ChildBuilder<Parent> {
    built  : node::Child,
    parent : Parent,
}

impl<Parent:Builder> ChildBuilder<Parent> {
    /// Finish child building and return builder of the node's Parent.
    pub fn done(mut self) -> Parent {
        self.parent.node_being_built().children.push(self.built);
        self.parent
    }
}

impl<T> Builder for ChildBuilder<T> {
    fn node_being_built(&mut self) -> &mut Node {
        &mut self.built.node
    }
}
