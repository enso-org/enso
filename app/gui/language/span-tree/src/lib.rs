//! SpanTree module
//!
//! SpanTree is a structure describing expression with nodes mapped to expression text spans. It can
//! be considered a layer over AST, that adds an information about chains (you can
//! iterate over all elements of infix chain like `1 + 2 + 3` or prefix chain like `foo bar baz`),
//! and provides interface for AST operations like set node to a new AST or add new element to
//! operator chain.

// === Features ===
#![feature(associated_type_bounds)]
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(type_ascription)]
#![feature(exact_size_is_empty)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]


// ==============
// === Export ===
// ==============

pub mod action;
pub mod builder;
pub mod generate;
pub mod iter;
pub mod node;

pub use node::Crumb;
pub use node::Crumbs;
pub use node::Node;
pub use node::Payload;



/// Module gathering all commonly used traits for massive importing.
pub mod traits {
    pub use crate::action::Actions;
    pub use crate::builder::Builder;
    pub use crate::generate::SpanTreeGenerator;
}

/// Common types that should be visible across the whole crate.
pub mod prelude {
    pub use crate::traits::*;
    pub use ast::traits::*;
    pub use enso_prelude::*;
    pub use enso_profiler as profiler;
    pub use enso_profiler::prelude::*;
}

use prelude::*;


use crate::generate::Context;



// =====================
// === ArgumentInfo ===
// =====================

/// Additional information available for nodes being function arguments or their placeholders.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct ArgumentInfo {
    pub name:       Option<String>,
    pub tp:         Option<String>,
    /// The AST ID of the call expression that this argument is passed to.
    /// See [`ApplicationBase`] for more details.
    pub call_id:    Option<ast::Id>,
    pub tag_values: Vec<String>,
}

impl ArgumentInfo {
    /// Constructor.
    pub fn new(
        name: Option<String>,
        tp: Option<String>,
        call_id: Option<ast::Id>,
        tag_values: Vec<String>,
    ) -> Self {
        Self { name, tp, call_id, tag_values }
    }

    /// Specialized constructor for "this" argument.
    pub fn this(tp: Option<String>, call_id: Option<ast::Id>) -> Self {
        let name = Some(node::This::NAME.into());
        Self { name, tp, call_id, tag_values: Vec::new() }
    }

    /// Extend the argument info with the given call id.
    pub fn with_call_id(self, call_id: Option<ast::Id>) -> Self {
        Self { call_id, ..self }
    }
}



// ================
// === SpanTree ===
// ================

/// A SpanTree main structure.
///
/// This structure is used to have some specific node marked as root node, to avoid confusion
/// regarding SpanTree crumbs and AST crumbs.
///
/// ## Design
/// Please note that `SpanTree` was designed in such a way, that its leaves cover all visual tokens
/// in the code. Even in the case of parenthesed expressions, like `(foo)`, the parentheses are also
/// `SpanTree` tokens.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpanTree<T = ()> {
    /// A root node of the tree.
    pub root: Node<T>,
}

impl<T: Payload> SpanTree<T> {
    /// Create span tree from something that could generate it (usually AST).
    pub fn new(gen: &impl SpanTreeGenerator<T>, context: &impl Context) -> FallibleResult<Self> {
        gen.generate_tree(context)
    }

    /// Get a reference to the root node.
    pub fn root_ref(&self) -> node::Ref<T> {
        node::Ref::new(&self.root)
    }

    /// Get a mutable reference to the root node.
    pub fn root_ref_mut(&mut self) -> node::RefMut<T> {
        node::RefMut::new(&mut self.root)
    }

    /// Get the node (root, child, or further descendant) identified by `crumbs`.
    pub fn get_node<'a>(
        &self,
        crumbs: impl IntoIterator<Item = &'a Crumb>,
    ) -> FallibleResult<node::Ref<T>> {
        self.root_ref().get_descendant(crumbs)
    }

    /// Payload mapping utility.
    pub fn map<S>(self, f: impl Copy + Fn(T) -> S) -> SpanTree<S> {
        let root = self.root.map(f);
        SpanTree { root }
    }
}


// === Getters ===

impl<T: Payload> SpanTree<T> {
    /// Get `ast::Id` of the nested node, if exists.
    pub fn nested_ast_id(&self, crumbs: &Crumbs) -> Option<ast::Id> {
        if self.root_ref().crumbs == crumbs {
            self.root.ast_id
        } else {
            let span_tree_descendant = self.root_ref().get_descendant(crumbs);
            span_tree_descendant.map(|t| t.ast_id).ok().flatten()
        }
    }
}


// == Impls ===

impl<T: Payload> Default for SpanTree<T> {
    fn default() -> Self {
        let root = Node::<T>::new().with_kind(node::Kind::Root);
        Self { root }
    }
}

// == Debug utils ==

impl<T: Payload> SpanTree<T> {
    #[allow(dead_code)]
    /// Get pretty-printed representation of this span tree for debugging purposes. The `code`
    /// argument should be identical to the expression that was used during generation of this
    /// span-tree. It will be used to print code fragments associated with each span.
    ///
    /// Example output with AST ids removed for clarity:
    /// ```text
    /// operator6.join operator31 Join_Kind.Inner ["County"] Root
    /// operator6.join operator31 Join_Kind.Inner ["County"] ├── Chained
    /// operator6.join operator31 Join_Kind.Inner ["County"] │   ├── Chained
    /// operator6.join operator31 Join_Kind.Inner            │   │   ├── Chained
    /// operator6.join operator31                            │   │   │   ├── Chained
    /// operator6.join                                       │   │   │   │   ├── Operation
    /// ▲                                                    │   │   │   │   │   ├── InsertionPoint(BeforeTarget)
    /// operator6                                            │   │   │   │   │   ├── This
    ///          ▲                                           │   │   │   │   │   ├── InsertionPoint(AfterTarget)
    ///          .                                           │   │   │   │   │   ├── Operation
    ///           join                                       │   │   │   │   │   ├── Argument
    ///               ▲                                      │   │   │   │   │   ╰── InsertionPoint(Append)
    ///                operator31                            │   │   │   │   ╰── Argument name="right"
    ///                           Join_Kind.Inner            │   │   │   ╰── Argument name="join_kind"
    ///                           ▲                          │   │   │       ├── InsertionPoint(BeforeTarget)
    ///                           Join_Kind                  │   │   │       ├── This
    ///                                    ▲                 │   │   │       ├── InsertionPoint(AfterTarget)
    ///                                    .                 │   │   │       ├── Operation
    ///                                     Inner            │   │   │       ├── Argument
    ///                                          ▲           │   │   │       ╰── InsertionPoint(Append)
    ///                                           ["County"] │   │   ╰── Argument name="on"
    ///                                           [          │   │       ├── Token
    ///                                            "County"  │   │       ├── Argument
    ///                                                    ] │   │       ╰── Token
    ///                                                     ▲│   ╰── InsertionPoint(ExpectedArgument(3)) name="right_prefix"
    ///                                                     ▲╰── InsertionPoint(ExpectedArgument(4)) name="on_problems"
    /// ```
    pub fn debug_print(&self, code: &str) -> String {
        use std::fmt::Write;

        let mut buffer = String::new();
        let span_padding = " ".repeat(code.len() + 1);

        struct PrintState {
            indent:       String,
            num_children: usize,
        }
        let state = PrintState { indent: String::new(), num_children: 1 };
        self.root_ref().dfs_with_layer_data(state, |node, state| {
            let span = node.span();
            let node_code = &code[span];
            buffer.push_str(&span_padding[0..node.span_offset.into()]);
            let mut written = node.span_offset.into();
            if node_code.is_empty() {
                buffer.push('▲');
                written += 1;
            } else {
                buffer.push_str(node_code);
                written += node_code.len();
            }
            buffer.push_str(&span_padding[written..]);

            let indent = if let Some(index) = node.crumbs.last() {
                let is_last = *index == state.num_children - 1;
                let indent_targeted = if is_last { "╰── " } else { "├── " };
                let indent_continue = if is_last { "    " } else { "│   " };

                buffer.push_str(&state.indent);
                buffer.push_str(indent_targeted);
                format!("{}{}", state.indent, indent_continue)
            } else {
                state.indent.clone()
            };

            buffer.push_str(node.kind.variant_name());
            if let node::Kind::InsertionPoint(inner) = &node.kind {
                write!(buffer, "({:?})", inner.kind).unwrap();
            }

            if let Some(name) = node.kind.name() {
                write!(buffer, " name={name:?}").unwrap();
            }

            if let Some(call_id) = node.kind.call_id() {
                write!(buffer, " call_id={call_id:?}").unwrap();
            }

            if let Some(ast_id) = node.ast_id {
                write!(buffer, " ast_id={ast_id:?}").unwrap();
            }
            buffer.push('\n');

            let num_children = node.children.len();
            PrintState { indent, num_children }
        });
        buffer
    }
}
