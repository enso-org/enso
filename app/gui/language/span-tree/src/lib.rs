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



// ================
// === TagValue ===
// ================

/// Argument tag values with resolved labels. Represents statically defined choices of values for a
/// function argument.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct TagValue {
    /// An import that is required to be present when the value is inserted. `None` when no import
    /// is required. Note that the import might or might not be already present in the module.
    /// It should only be added if it is not present.
    pub required_import: Option<String>,
    /// Code expression that should be inserted into the node AST when this value is selected.
    pub expression:      String,
    /// Shortened label for the value. `None` when shortening wasn't possible. In that case, the
    /// `expression` should be used as a label.
    pub label:           Option<String>,
}



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
    pub tag_values: Vec<TagValue>,
}

impl ArgumentInfo {
    /// Constructor.
    pub fn new(
        name: Option<String>,
        tp: Option<String>,
        call_id: Option<ast::Id>,
        tag_values: Vec<TagValue>,
    ) -> Self {
        Self { name, tp, call_id, tag_values }
    }

    /// Specialized constructor with argument name.
    pub fn named(name: impl Str) -> Self {
        Self::new(Some(name.into()), None, None, default())
    }

    /// Specialized constructor for "this" argument.
    pub fn this(tp: Option<String>, call_id: Option<ast::Id>) -> Self {
        Self::new(Some(node::Argument::THIS.into()), tp, call_id, default())
    }

    /// Extend the argument info with the given call id.
    pub fn with_call_id(self, call_id: Option<ast::Id>) -> Self {
        Self { call_id, ..self }
    }

    /// Check if this argument info represents an argument with specified name.
    pub fn has_name(&self, name: &str) -> bool {
        self.name.as_ref().map_or(false, |n| n == name)
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

impl<T> SpanTree<T> {
    /// Create span tree from something that could generate it (usually AST).
    pub fn new(gen: &impl SpanTreeGenerator<T>, context: &impl Context) -> FallibleResult<Self> {
        gen.generate_tree(context)
    }

    /// Get a reference to the root node.
    pub fn root_ref(&self) -> node::Ref<T> {
        node::Ref::root(self)
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

    /// Map over the nodes in given crumbs chain, from the root to the node identified by the
    /// crumbs. Terminates on the first `Some(R)` result and returns it.
    pub fn find_map_in_chain<'a, R, F>(
        &self,
        crumbs: impl IntoIterator<Item = &'a Crumb>,
        mut f: F,
    ) -> Option<R>
    where
        F: FnMut(usize, &Node<T>) -> Option<R>,
    {
        let mut crumbs = crumbs.into_iter();
        let mut current_node = &self.root;
        let mut idx = 0;
        loop {
            let result = f(idx, current_node);
            if result.is_some() {
                return result;
            }
            idx += 1;
            current_node = current_node.children.get(*crumbs.next()?)?;
        }
    }
}


// === Getters ===

impl<T> SpanTree<T> {
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

impl<T> SpanTree<T> {
    #[allow(dead_code)]
    /// Get pretty-printed representation of this span tree for debugging purposes. The `code`
    /// argument should be identical to the expression that was used during generation of this
    /// span-tree. It will be used to print code fragments associated with each span.
    ///
    /// Example output with AST ids removed for clarity:
    /// ```text
    /// ▷operator4.join operator2 Join_Kind.Inner ["County"]◁Root
    /// ▷operator4.join operator2 Join_Kind.Inner ["County"]◁ ├─Chained
    /// ▷operator4.join operator2 Join_Kind.Inner ["County"]◁ │  ├─Chained
    /// ▷operator4.join operator2 Join_Kind.Inner◁            │  │  ├─Chained
    /// ▷operator4.join operator2◁                            │  │  │  ├─Chained
    /// ▷operator4.join◁                                      │  │  │  │  ├─Operation
    /// ▷◁                                                    │  │  │  │  │  ├─InsertionPoint(BeforeArgument(0))
    /// ▷operator4◁                                           │  │  │  │  │  ├─Argument name="self"
    ///          ▷.◁                                          │  │  │  │  │  ├─Operation
    ///           ▷◁                                          │  │  │  │  │  ├─InsertionPoint(BeforeArgument(1))
    ///           ▷join◁                                      │  │  │  │  │  ├─Argument
    ///               ▷◁                                      │  │  │  │  │  ╰─InsertionPoint(Append)
    ///                ▷operator2◁                            │  │  │  │  ╰─Argument name="right"
    ///                          ▷Join_Kind.Inner◁            │  │  │  ╰─Argument name="join_kind"
    ///                          ▷◁                           │  │  │     ├─InsertionPoint(BeforeArgument(0))
    ///                          ▷Join_Kind◁                  │  │  │     ├─Argument
    ///                                   ▷.◁                 │  │  │     ├─Operation
    ///                                    ▷◁                 │  │  │     ├─InsertionPoint(BeforeArgument(1))
    ///                                    ▷Inner◁            │  │  │     ├─Argument
    ///                                         ▷◁            │  │  │     ╰─InsertionPoint(Append)
    ///                                          ▷["County"]◁ │  │  ╰─Argument name="on"
    ///                                          ▷[◁          │  │     ├─Token
    ///                                           ▷"County"◁  │  │     ├─Argument
    ///                                                   ▷]◁ │  │     ╰─Token
    ///                                                    ▷◁ │  ╰─InsertionPoint(ExpectedArgument) name="right_prefix"
    ///                                                    ▷◁ ╰─InsertionPoint(ExpectedArgument) name="on_problems"
    /// ```
    pub fn debug_print(&self, code: &str) -> String {
        use std::fmt::Write;

        let mut code = code.to_string();
        let code_padding = self.root.size.as_usize().saturating_sub(code.len());
        for _ in 0..code_padding {
            code.push(' ');
        }

        let mut buffer = String::new();
        let span_padding = " ".repeat(code.len() + 2);

        struct PrintState {
            indent:       String,
            num_children: usize,
        }
        let state = PrintState { indent: String::new(), num_children: 1 };
        self.root_ref().dfs_with_layer_data(state, |node, state| {
            let span = node.span();
            let node_code = &code[span];
            buffer.push_str(&span_padding[0..node.span_offset.value]);
            buffer.push('▷');
            buffer.push_str(node_code);
            buffer.push('◁');
            let written = node.span_offset.value + node_code.len() + 2;
            buffer.push_str(&span_padding[written..]);

            let indent = if let Some(index) = node.crumbs.last() {
                let is_last = *index == state.num_children - 1;
                let indent_targeted = if is_last { " ╰─" } else { " ├─" };
                let indent_continue = if is_last { "   " } else { " │ " };

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
            } else if let Some(ext_id) = node.extended_ast_id {
                write!(buffer, " ext_id={ext_id:?}").unwrap();
            }

            if let Some(tt) = node.tree_type.as_ref() {
                write!(buffer, " tt={tt:?}").unwrap();
            }

            buffer.push('\n');

            let num_children = node.children.len();
            PrintState { indent, num_children }
        });
        buffer
    }
}
