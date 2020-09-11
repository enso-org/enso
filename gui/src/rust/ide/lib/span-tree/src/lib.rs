//! SpanTree module
//!
//! SpanTree is a structure describing expression with nodes mapped to expression text spans. It can
//! be considered a layer over AST, that adds an information about chains (you can
//! iterate over all elements of infix chain like `1 + 2 + 3` or prefix chain like `foo bar baz`),
//! and provides interface for AST operations like set node to a new AST or add new element to
//! operator chain.

#![feature(associated_type_bounds)]
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(matches_macro)]
#![feature(exact_size_is_empty)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

pub mod action;
pub mod generate;
pub mod iter;
pub mod node;
pub mod builder;

pub use node::Node;
pub use node::Crumb;
pub use node::Crumbs;

/// Module gathering all commonly used traits for massive importing.
pub mod traits {
    pub use crate::action::Actions;
    pub use crate::generate::SpanTreeGenerator;
    pub use crate::builder::Builder;
}

/// Common types that should be visible across the whole crate.
pub mod prelude {
    pub use crate::traits::*;
    pub use ast::traits::*;
    pub use enso_prelude::*;
    pub use utils::fail::FallibleResult;
}

use traits::*;
use prelude::*;

use crate::generate::Context;



// =====================
// === ParameterInfo ===
// =====================

/// Additional information available for nodes being function arguments or their placeholders.
#[derive(Clone,Debug,Eq,PartialEq)]
#[allow(missing_docs)]
pub struct ParameterInfo {
    pub name     : Option<String>,
    pub typename : Option<String>,
}



// ================
// === SpanTree ===
// ================

/// A SpanTree main structure.
///
/// This structure is used to have some specific node marked as root node, to avoid confusion
/// regarding SpanTree crumbs and AST crumbs.
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct SpanTree {
    /// A root node of the tree.
    pub root : Node
}

impl SpanTree {
    /// Create span tree from something that could generate it (usually AST).
    pub fn new(generator:&impl SpanTreeGenerator, context:&impl Context) -> FallibleResult<Self> {
        generator.generate_tree(context)
    }

    /// Get the `NodeRef` of root node.
    pub fn root_ref(&self) -> node::Ref {
        node::Ref {
            node       : &self.root,
            span_begin : default(),
            crumbs     : default(),
            ast_crumbs : default()
        }
    }

    /// Get the node (root, child, or further descendant) identified by `crumbs`.
    pub fn get_node<'a>
    (&self, crumbs:impl IntoIterator<Item=&'a Crumb>) -> FallibleResult<node::Ref> {
        self.root_ref().get_descendant(crumbs)
    }
}

impl Default for SpanTree {
    fn default() -> Self {
        let expression_id  = None;
        let kind           = node::Kind::Root;
        let size           = default();
        let children       = default();
        let parameter_info = default();
        let root          = Node {kind,size,children,expression_id,parameter_info};
        Self {root}
    }
}
