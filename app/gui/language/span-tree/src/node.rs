//! A module with SpanTree structure definition.

use crate::prelude::*;
use enso_text::index::*;
use enso_text::unit::*;

use crate::iter::LeafIterator;
use crate::iter::TreeFragment;
use crate::ArgumentInfo;
use crate::SpanTree;

use enso_text as text;


// ==============
// === Export ===
// ==============

pub mod kind;

pub use kind::*;



// ============
// === Node ===
// ============

/// The node payload constraints.
pub trait Payload = Default + Clone;

/// SpanTree Node.
///
/// Each node in SpanTree is bound to some span of code, and potentially may have corresponding
/// AST node.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Node<T> {
    pub kind:          Kind,
    pub size:          ByteDiff,
    pub children:      Vec<Child<T>>,
    pub ast_id:        Option<ast::Id>,
    pub parenthesized: bool,
    pub payload:       T,
}

impl<T> Deref for Node<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.payload
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.payload
    }
}


// === API ===

impl<T> Node<T> {
    /// Constructor.
    pub fn new() -> Self
    where T: Default {
        default()
    }

    /// Payload mapping utility.
    pub fn map<S>(self, f: impl Copy + Fn(T) -> S) -> Node<S> {
        let kind = self.kind;
        let parenthesized = self.parenthesized;
        let size = self.size;
        let children = self.children.into_iter().map(|t| t.map(f)).collect_vec();
        let ast_id = self.ast_id;
        let payload = f(self.payload);
        Node { kind, parenthesized, size, children, ast_id, payload }
    }
}

// === Kind utils ===

#[allow(missing_docs)]
impl<T> Node<T> {
    pub fn parenthesized(&self) -> bool {
        self.parenthesized
    }
    pub fn is_root(&self) -> bool {
        self.kind.is_root()
    }
    pub fn is_chained(&self) -> bool {
        self.kind.is_chained()
    }
    pub fn is_operation(&self) -> bool {
        self.kind.is_operation()
    }
    pub fn is_this(&self) -> bool {
        self.kind.is_this()
    }
    pub fn is_argument(&self) -> bool {
        self.kind.is_argument()
    }
    pub fn is_named_argument(&self) -> bool {
        self.kind.is_named_argument()
    }
    pub fn is_token(&self) -> bool {
        self.kind.is_token()
    }
    pub fn is_insertion_point(&self) -> bool {
        self.kind.is_insertion_point()
    }
    pub fn is_positional_insertion_point(&self) -> bool {
        self.kind.is_positional_insertion_point()
    }
    pub fn is_expected_argument(&self) -> bool {
        self.kind.is_expected_argument()
    }
    pub fn is_function_parameter(&self) -> bool {
        self.kind.is_function_parameter()
    }
}


// === Setters ===

#[allow(missing_docs)]
impl<T> Node<T> {
    pub fn with_kind(mut self, kind: impl Into<Kind>) -> Self {
        self.kind = kind.into();
        self
    }
    pub fn with_size(mut self, size: ByteDiff) -> Self {
        self.size = size;
        self
    }
    pub fn with_children(mut self, ts: Vec<Child<T>>) -> Self {
        self.children = ts;
        self
    }
    pub fn with_ast_id(mut self, id: ast::Id) -> Self {
        self.ast_id = Some(id);
        self
    }
    pub fn with_payload(mut self, payload: T) -> Self {
        self.payload = payload;
        self
    }
}


// === Kind getters & setters ===

#[allow(missing_docs)]
impl<T> Node<T> {
    pub fn name(&self) -> Option<&str> {
        self.kind.name()
    }
    pub fn tp(&self) -> Option<&String> {
        self.kind.tp()
    }
    pub fn argument_info(&self) -> Option<ArgumentInfo> {
        self.kind.argument_info()
    }
    pub fn set_argument_info(&mut self, info: ArgumentInfo) {
        self.kind.set_argument_info(info);
    }
    pub fn set_definition_index(&mut self, definition_index: usize) {
        self.kind.set_definition_index(definition_index);
    }
}



// =============
// === Child ===
// =============

/// A structure which contains `Node` being a child of some parent. It contains some additional
/// data regarding this relation
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Child<T = ()> {
    /// A child node.
    pub node:           Node<T>,
    /// An offset counted from the parent node starting index to the start of this node's span.
    pub parent_offset:  ByteDiff,
    /// The offset counted from the end of previous sibling node.
    pub sibling_offset: ByteDiff,
    /// AST crumbs which lead from parent to child associated AST node.
    pub ast_crumbs:     ast::Crumbs,
}

impl<T> Child<T> {
    /// Payload mapping utility.
    pub fn map<S>(self, f: impl Copy + Fn(T) -> S) -> Child<S> {
        let node = self.node.map(f);
        let parent_offset = self.parent_offset;
        let ast_crumbs = self.ast_crumbs;
        let sibling_offset = self.sibling_offset;
        Child { node, parent_offset, sibling_offset, ast_crumbs }
    }
}

impl<T> Deref for Child<T> {
    type Target = Node<T>;
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> DerefMut for Child<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}



// ==============
// === Crumbs ===
// ==============

/// Identifies subtree within a node. It is the index of the child node.
pub type Crumb = usize;

/// Crumbs specifying this node position related to root.
#[derive(Debug, Clone, CloneRef, Default, Eq, Hash, PartialEq)]
#[allow(missing_docs)]
pub struct Crumbs {
    pub vec: Rc<Vec<Crumb>>,
}

impl Deref for Crumbs {
    type Target = Rc<Vec<Crumb>>;
    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl Crumbs {
    /// Constructor from raw crumbs list.
    pub fn new(crumbs: Vec<Crumb>) -> Self {
        Self { vec: Rc::new(crumbs) }
    }

    /// Create sub-crumbs with the provided child crumb.
    pub fn sub(&self, child: Crumb) -> Self {
        let vec = Rc::new(self.vec.deref().clone().pushed(child));
        Self { vec }
    }

    /// Create sub-crumbs with the provided child crumb.
    pub fn into_sub(mut self, child: Crumb) -> Self {
        let vec = Rc::make_mut(&mut self.vec);
        vec.push(child);
        self
    }

    /// Create crumbs to the sibling node, which is `offset` nodes away from the current node.
    pub fn relative_sibling(&self, offset: isize) -> Self {
        let mut vec = self.vec.deref().clone();
        if let Some(last) = vec.last_mut() {
            *last = last.saturating_add_signed(offset)
        }
        Self { vec: Rc::new(vec) }
    }
}

impl<T: IntoIterator<Item = Crumb>> From<T> for Crumbs {
    fn from(crumbs: T) -> Self {
        Self::new(crumbs.into_iter().collect())
    }
}


// === Impls ===

impl PartialEq<&Self> for Crumbs {
    fn eq(&self, other: &&Self) -> bool {
        self.eq(*other)
    }
}

impl<'a> IntoIterator for &'a Crumbs {
    type Item = &'a Crumb;
    type IntoIter = std::slice::Iter<'a, Crumb>;
    fn into_iter(self) -> Self::IntoIter {
        (*self.vec).iter()
    }
}

/// Convert crumbs to crumbs pointing to a parent.
pub fn parent_crumbs(crumbs: &[Crumb]) -> Option<&[Crumb]> {
    crumbs.len().checked_sub(1).map(|new_len| &crumbs[..new_len])
}


// === Invalid ===

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(
    display = "The crumb `{}` is invalid, only {} children present. Traversed crumbs: {:?}.",
    crumb, count, context
)]
pub struct InvalidCrumb {
    /// Crumb that was attempted.
    pub crumb:   Crumb,
    /// Available children count.
    pub count:   usize,
    /// Already traversed crumbs.
    pub context: Vec<Crumb>,
}

impl InvalidCrumb {
    /// Constructor.
    pub fn new(crumb: Crumb, count: usize, context: &Crumbs) -> Self {
        let context = context.vec.deref().clone();
        Self { crumb, count, context }
    }
}



// ===========
// === Ref ===
// ===========

/// A reference to node inside some specific tree.
#[derive(Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Ref<'a, T = ()> {
    /// The span tree that the node is a part of.
    pub span_tree:      &'a SpanTree<T>,
    /// The node's ref.
    pub node:           &'a Node<T>,
    /// Span begin's offset counted from the root expression.
    pub span_offset:    Byte,
    /// The offset counted from the end of previous sibling node.
    pub sibling_offset: ByteDiff,
    /// Crumbs specifying this node position related to root.
    pub crumbs:         Crumbs,
    /// Ast crumbs locating associated AST node, related to the root's AST node.
    pub ast_crumbs:     ast::Crumbs,
}

/// A result of `get_subnode_by_ast_crumbs`
#[derive(Clone, Debug)]
pub struct NodeFoundByAstCrumbs<'a, 'b, T = ()> {
    /// A node being a result of the lookup.
    pub node:       Ref<'a, T>,
    /// AST crumbs locating the searched AST node inside the AST of found SpanTree node.
    pub ast_crumbs: &'b [ast::Crumb],
}

impl<'a, T> Ref<'a, T> {
    /// Constructor.
    pub fn root(span_tree: &'a SpanTree<T>) -> Self {
        let span_offset = default();
        let sibling_offset = default();
        let crumbs = default();
        let ast_crumbs = default();
        let node = &span_tree.root;
        Self { span_tree, node, span_offset, sibling_offset, crumbs, ast_crumbs }
    }

    /// Get span of current node.
    pub fn span(&self) -> text::Range<Byte> {
        let start = self.span_offset;
        let end = self.span_offset + self.node.size;
        (start..end).into()
    }

    /// Get the reference to child with given index. Fails if index if out of bounds.
    pub fn child(self, index: usize) -> FallibleResult<Self> {
        let Ref { span_tree, node, mut span_offset, crumbs, mut ast_crumbs, .. } = self;

        match node.children.get(index) {
            None => Err(InvalidCrumb::new(node.children.len(), index, &crumbs).into()),
            Some(child) => {
                let node = &child.node;
                span_offset += child.parent_offset;
                let sibling_offset = child.sibling_offset;
                let crumbs = crumbs.into_sub(index);
                ast_crumbs.extend_from_slice(&child.ast_crumbs);
                Ok(Self { span_tree, node, span_offset, sibling_offset, crumbs, ast_crumbs })
            }
        }
    }

    /// Get the reference to parent of this node. Returns `None` when this is a root. Note that this
    /// is a potentially expensive operation, as it traverses the tree towards the parent starting
    /// from the root node.
    pub fn parent(&self) -> FallibleResult<Option<Self>> {
        match self.crumbs.split_last() {
            Some((_, parent_crumbs)) =>
                Ok(Some(self.span_tree.root_ref().get_descendant(parent_crumbs)?)),
            None => Ok(None),
        }
    }

    /// Iterator over all direct children producing `Ref`s.
    pub fn children_iter(self) -> impl DoubleEndedIterator<Item = Ref<'a, T>> + Clone {
        let children_count = self.node.children.len();
        (0..children_count).map(move |i| self.clone().child(i).unwrap())
    }

    /// Iterator over all leaves of subtree rooted in the `self`.
    pub fn leaf_iter(self) -> Box<dyn Iterator<Item = Ref<'a, T>> + 'a> {
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
    pub fn chain_children_iter(self) -> LeafIterator<'a, T> {
        LeafIterator::new(self, TreeFragment::ChainAndDirectChildren)
    }

    /// Get the sub-node (child, or further descendant) identified by `crumbs`.
    pub fn get_descendant<'b>(
        self,
        crumbs: impl IntoIterator<Item = &'b Crumb>,
    ) -> FallibleResult<Ref<'a, T>> {
        crumbs.into_iter().try_fold(self, |node, crumb| node.child(*crumb))
    }

    /// Get the reference to last child of this node. Returns `None` when this node has no children.
    pub fn last_child(self) -> Option<Self> {
        if self.node.children.is_empty() {
            None
        } else {
            let index = self.node.children.len() - 1;
            Some(self.child(index).expect("Index was already checked to be in bounds."))
        }
    }

    /// Get the sub-node by AST crumbs.
    ///
    /// The returned node will be node having corresponding AST node located by given `ast_crumbs`,
    /// or a leaf whose AST _contains_ node located by `ast_crumbs` - in that case returned
    /// structure will have non-empty `ast_crumbs` field.
    pub fn get_descendant_by_ast_crumbs<'b>(
        self,
        ast_crumbs: &'b [ast::Crumb],
    ) -> Option<NodeFoundByAstCrumbs<'a, 'b, T>> {
        if self.node.children.is_empty() || ast_crumbs.is_empty() {
            let node = self;
            let remaining_ast_crumbs = ast_crumbs;
            Some(NodeFoundByAstCrumbs { node, ast_crumbs: remaining_ast_crumbs })
        } else {
            // Please be advised, that the `ch.ast_crumbs` is not a field of Ref, but Child, and
            // therefore have different meaning!
            let next = self
                .node
                .children
                .iter()
                .find_position(|ch| {
                    !ch.ast_crumbs.is_empty() && ast_crumbs.starts_with(&ch.ast_crumbs)
                })
                .or_else(|| {
                    // We try to find appropriate node second time, this time expecting case of
                    // "prefix-like" nodes with `InsertionPoint(ExpectedArgument(_))`. See also docs
                    // for `generate::generate_expected_argument`.
                    // TODO[ao]: As implementation of SpanTree will extend there may be some day
                    // more  cases. Should be reconsidered in https://github.com/enso-org/ide/issues/787
                    self.node.children.iter().find_position(|ch| {
                        ch.ast_crumbs.is_empty() && !ch.kind.is_insertion_point()
                    })
                });
            next.and_then(|(id, child)| {
                let ast_subcrumbs = &ast_crumbs[child.ast_crumbs.len()..];
                self.child(id).unwrap().get_descendant_by_ast_crumbs(ast_subcrumbs)
            })
        }
    }

    /// Get the node which exactly matches the given Span. If there are many such nodes, pick first
    /// found by DFS.
    pub fn find_by_span(self, span: &text::Range<Byte>) -> Option<Ref<'a, T>> {
        if self.span() == *span {
            Some(self)
        } else {
            self.children_iter().find_map(|ch| {
                ch.span().contains_range(span).and_option_from(|| ch.find_by_span(span))
            })
        }
    }
}

impl<'a, T> Deref for Ref<'a, T> {
    type Target = Node<T>;
    fn deref(&self) -> &Self::Target {
        self.node
    }
}

// === Specialized Iterators ===

impl<'a, T> Ref<'a, T> {
    /// Perform a depth-first-search algorithm on the `SpanTree`. The order of the layers will be
    /// preserved - after all children of a node will be traversed, the next node sibling will be
    /// traversed and then it's children.
    ///
    /// # Control Flow
    /// This algorithm allows for some kind of control flow. Return `false` as the first tuple value
    /// to stop traversing children of the current branch.
    ///
    /// # Layer Data
    /// This algorithm allows passing any kind of data to layers. In order to set data for all
    /// children of the current branch, return it as the second argument of the tuple. Please note
    /// that callbacks get mutable access to the passed data, so they can freely modify it.
    #[profile(Debug)]
    pub fn partial_dfs_with_layer_data<D>(
        self,
        mut data: D,
        mut on_node: impl FnMut(&mut Self, &mut D) -> (bool, D),
    ) {
        let mut layer = vec![self];
        let mut layers = vec![];
        loop {
            match layer.pop() {
                None => match layers.pop() {
                    None => break,
                    Some((l, d)) => {
                        layer = l;
                        data = d;
                    }
                },
                Some(mut node) => {
                    let (ok, mut sub_data) = on_node(&mut node, &mut data);
                    if ok {
                        let mut children = node.children_iter().collect_vec().reversed();
                        mem::swap(&mut sub_data, &mut data);
                        mem::swap(&mut children, &mut layer);
                        layers.push((children, sub_data));
                    }
                }
            }
        }
    }

    /// Perform a full (traverse all nodes) depth-first-search algorithm on the `SpanTree`. See docs
    /// of `partial_dfs` to learn more.
    pub fn dfs_with_layer_data<D>(self, data: D, mut on_node: impl FnMut(&mut Self, &mut D) -> D) {
        self.partial_dfs_with_layer_data(data, |t, s| (true, on_node(t, s)))
    }

    /// Perform a full (traverse all nodes) depth-first-search algorithm on the `SpanTree`. Just
    /// like `dfs_with_layer_data`, but without data attached.
    pub fn dfs(self, mut on_node: impl FnMut(&mut Self)) {
        self.partial_dfs_with_layer_data((), |t, _| (true, on_node(t)))
    }

    /// Perform a depth-first-search algorithm on the `SpanTree`. Stop searching after the first
    /// node for which the predicate returns `true`.
    pub fn find_node(self, mut predicate: impl FnMut(&Self) -> bool) -> Option<Self> {
        let mut queue = vec![self];
        loop {
            match queue.pop() {
                None => break None,
                Some(node) if predicate(&node) => break Some(node),
                Some(node) => queue.extend(node.children_iter().rev()),
            }
        }
    }
}



// ==============
// === RefMut ===
// ==============

/// A mutable reference to node inside some specific tree. Please note that tree structure
/// modification is disallowed. The only part that can be modified is the payload.
#[derive(Debug)]
pub struct RefMut<'a, T = ()> {
    /// The node's ref.
    node:              &'a mut Node<T>,
    /// An offset counted from the parent node start to the start of this node's span.
    pub parent_offset: ByteDiff,
    /// Span begin's offset counted from the root expression.
    pub span_offset:   Byte,
    /// Crumbs specifying this node position related to root.
    pub crumbs:        Crumbs,
    /// Ast crumbs locating associated AST node, related to the root's AST node.
    pub ast_crumbs:    ast::Crumbs,
}

impl<'a, T> RefMut<'a, T> {
    /// Constructor.
    pub fn new(node: &'a mut Node<T>) -> Self {
        let parent_offset = default();
        let span_begin = default();
        let crumbs = default();
        let ast_crumbs = default();
        Self { node, parent_offset, span_offset: span_begin, crumbs, ast_crumbs }
    }

    /// Payload accessor.
    pub fn payload(&self) -> &T {
        &self.node.payload
    }

    /// Mutable payload accessor.
    pub fn payload_mut(&mut self) -> &mut T {
        &mut self.node.payload
    }

    /// Get span of current node.
    pub fn span(&self) -> text::Range<Byte> {
        text::Range::new(self.span_offset, self.span_offset + self.size)
    }

    /// Helper function for building child references.
    fn child_from_ref(
        index: usize,
        child: &'a mut Child<T>,
        span_begin: Byte,
        crumbs: Crumbs,
        mut ast_crumbs: ast::Crumbs,
    ) -> RefMut<'a, T> {
        let parent_offset = child.parent_offset;
        let node = &mut child.node;
        let span_offset = span_begin + parent_offset;
        let crumbs = crumbs.into_sub(index);
        ast_crumbs.extend(child.ast_crumbs.iter().cloned());
        Self { node, parent_offset, span_offset, crumbs, ast_crumbs }
    }

    /// Get the reference to child with given index. Fails if index if out of bounds.
    pub fn child(self, index: usize) -> FallibleResult<RefMut<'a, T>> {
        let node = self.node;
        let span_begin = self.span_offset;
        let crumbs = self.crumbs;
        let ast_crumbs = self.ast_crumbs;
        let count = node.children.len();
        match node.children.get_mut(index) {
            None => Err(InvalidCrumb::new(count, index, &crumbs).into()),
            Some(child) => Ok(Self::child_from_ref(index, child, span_begin, crumbs, ast_crumbs)),
        }
    }

    /// Iterator over all direct children producing `RefMut`s.
    pub fn children_iter(self) -> impl Iterator<Item = RefMut<'a, T>> {
        let span_begin = self.span_offset;
        let crumbs = self.crumbs;
        let ast_crumbs = self.ast_crumbs;
        self.node.children.iter_mut().enumerate().map(move |(index, child)| {
            Self::child_from_ref(index, child, span_begin, crumbs.clone(), ast_crumbs.clone())
        })
    }

    /// Get the sub-node (child, or further descendant) identified by `crumbs`.
    pub fn get_descendant<'b>(
        self,
        crumbs: impl IntoIterator<Item = &'b Crumb>,
    ) -> FallibleResult<Self> {
        let mut iter = crumbs.into_iter();
        match iter.next() {
            Some(index) => self.child(*index).and_then(|child| child.get_descendant(iter)),
            None => Ok(self),
        }
    }
}

impl<'a, T> Deref for RefMut<'a, T> {
    type Target = Node<T>;
    fn deref(&self) -> &Self::Target {
        self.node
    }
}

impl<'a, T> DerefMut for RefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.node
    }
}


// === Specialized Iterators ===

impl<'a, T: Payload> RefMut<'a, T> {
    /// Perform a depth-first-search algorithm on the `SpanTree`. The order of the layers will be
    /// preserved - after all children of a node will be traversed, the next node sibling will be
    /// traversed and then it's children.
    ///
    /// # Control Flow
    /// This algorithm allows for some kind of control flow. Return `false` as the first tuple value
    /// to stop traversing children of the current branch.
    ///
    /// # Layer Data
    /// This algorithm allows passing any kind of data to layers. In order to set data for all
    /// children of the current branch, return it as the second argument of the tuple. Please note
    /// that callbacks get mutable access to the passed data, so they can freely modify it.
    #[profile(Debug)]
    pub fn partial_dfs_with_layer_data<D>(
        self,
        mut data: D,
        mut on_node: impl FnMut(&mut Self, &mut D) -> (bool, D),
    ) {
        let mut layer = vec![self];
        let mut layers = vec![];
        loop {
            match layer.pop() {
                None => match layers.pop() {
                    None => break,
                    Some((l, d)) => {
                        layer = l;
                        data = d;
                    }
                },
                Some(mut node) => {
                    let (ok, mut sub_data) = on_node(&mut node, &mut data);
                    if ok {
                        let mut children = node.children_iter().collect_vec().reversed();
                        mem::swap(&mut sub_data, &mut data);
                        mem::swap(&mut children, &mut layer);
                        layers.push((children, sub_data));
                    }
                }
            }
        }
    }

    /// Perform a full (traverse all nodes) depth-first-search algorithm on the `SpanTree`. See docs
    /// of `partial_dfs` to learn more.
    pub fn dfs_with_layer_data<D>(self, data: D, mut on_node: impl FnMut(&mut Self, &mut D) -> D) {
        self.partial_dfs_with_layer_data(data, |t, s| (true, on_node(t, s)))
    }

    /// Perform a full (traverse all nodes) depth-first-search algorithm on the `SpanTree`. Just
    /// like `dfs`, but without data attached.
    pub fn dfs(self, mut on_node: impl FnMut(&mut Self)) {
        self.partial_dfs_with_layer_data((), |t, _| (true, on_node(t)))
    }

    /// Just like `partial_dfs` but traversing two `SpanTree`s at the same time. The children are
    /// traversed on pair. If one node has more children than the other one, the un-paired children
    /// will be skipped.
    pub fn partial_zipped_dfs<D>(
        self,
        other: Self,
        mut data: D,
        mut on_node: impl FnMut(&mut Self, &mut Self, &mut D) -> (bool, D),
    ) {
        let mut layer = vec![(self, other)];
        let mut layers = vec![];
        loop {
            match layer.pop() {
                None => match layers.pop() {
                    None => break,
                    Some((l, d)) => {
                        layer = l;
                        data = d;
                    }
                },
                Some((mut node1, mut node2)) => {
                    let (ok, mut sub_data) = on_node(&mut node1, &mut node2, &mut data);
                    if ok {
                        let children = node1.children_iter().zip(node2.children_iter());
                        let mut children = children.collect_vec().reversed();
                        mem::swap(&mut sub_data, &mut data);
                        mem::swap(&mut children, &mut layer);
                        layers.push((children, sub_data));
                    }
                }
            }
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
    use crate::node;
    use crate::node::InsertionPoint;
    use crate::node::InsertionPointType;
    use crate::Crumbs;
    use crate::SpanTree;

    use ast::crumbs;
    use enso_text::index::*;

    #[test]
    fn node_lookup() {
        use ast::crumbs::InfixCrumb::*;

        let tree: SpanTree = TreeBuilder::new(7)
            .add_leaf(0, 1, node::Kind::this(), vec![LeftOperand])
            .add_leaf(1, 1, node::Kind::Operation, vec![Operator])
            .add_child(2, 5, node::Kind::argument(), vec![RightOperand])
            .add_leaf(0, 2, node::Kind::this(), vec![LeftOperand])
            .add_leaf(3, 1, node::Kind::Operation, vec![Operator])
            .add_leaf(4, 1, node::Kind::argument(), vec![RightOperand])
            .done()
            .build();

        let root = tree.root_ref();
        let child1 = root.clone().get_descendant(&vec![0]).unwrap();
        let child2 = root.clone().get_descendant(&vec![2]).unwrap();
        let grand_child1 = root.clone().get_descendant(&vec![2, 0]).unwrap();
        let grand_child2 = child2.clone().get_descendant(&vec![1]).unwrap();

        // Span begin.
        assert_eq!(root.span_offset, 0.byte());
        assert_eq!(child1.span_offset, 0.byte());
        assert_eq!(child2.span_offset, 2.byte());
        assert_eq!(grand_child1.span_offset, 2.byte());
        assert_eq!(grand_child2.span_offset, 5.byte());

        // Length
        assert_eq!(root.node.size.value, 7);
        assert_eq!(child1.node.size.value, 1);
        assert_eq!(child2.node.size.value, 5);
        assert_eq!(grand_child1.node.size.value, 2);
        assert_eq!(grand_child2.node.size.value, 1);

        // crumbs
        assert_eq!(root.crumbs, node::Crumbs::default());
        assert_eq!(child1.crumbs, node::Crumbs::new(vec![0]));
        assert_eq!(child2.crumbs, node::Crumbs::new(vec![2]));
        assert_eq!(grand_child1.crumbs, node::Crumbs::new(vec![2, 0]));
        assert_eq!(grand_child2.crumbs, node::Crumbs::new(vec![2, 1]));

        // AST crumbs
        assert_eq!(root.ast_crumbs, []);
        assert_eq!(child1.ast_crumbs, [LeftOperand.into()]);
        assert_eq!(child2.ast_crumbs, [RightOperand.into()]);
        assert_eq!(grand_child1.ast_crumbs, [RightOperand.into(), LeftOperand.into()]);
        assert_eq!(grand_child2.ast_crumbs, [RightOperand.into(), Operator.into()]);

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

        let tree: SpanTree = TreeBuilder::new(7)
            .add_leaf(0, 1, node::Kind::this(), vec![LeftOperand])
            .add_leaf(1, 1, node::Kind::Operation, vec![Operator])
            .add_empty_child(2, InsertionPointType::BeforeArgument(0))
            .add_child(2, 5, node::Kind::argument(), vec![RightOperand])
            .add_leaf(0, 3, node::Kind::Operation, vec![Func])
            .add_leaf(3, 1, node::Kind::this(), vec![Arg])
            .done()
            .build();

        let root = tree.root_ref();
        let cases: &[(ast::Crumbs, &[usize], ast::Crumbs)] = &[
            (crumbs![LeftOperand], &[0], crumbs![]),
            (crumbs![RightOperand], &[3], crumbs![]),
            (crumbs![RightOperand, Func], &[3, 0], crumbs![]),
            (crumbs![RightOperand, Arg], &[3, 1], crumbs![]),
            (crumbs![RightOperand, Arg, HeadLine], &[3, 1], crumbs![HeadLine]),
        ];

        for case in cases {
            let (crumbs, expected_crumbs, expected_remaining_ast_crumbs) = case;
            let result = root.clone().get_descendant_by_ast_crumbs(crumbs).unwrap();
            assert_eq!(result.node.crumbs.as_slice(), *expected_crumbs, "{case:?}");
            assert_eq!(result.ast_crumbs, expected_remaining_ast_crumbs.as_slice(), "{case:?}");
        }
    }

    #[test]
    fn node_lookup_by_ast_crumbs_expected_arguments_case() {
        use ast::crumbs::InfixCrumb::*;

        // An example with single call and expected arguments.
        // See also `generate::test::generating_span_tree_for_unfinished_call`
        let tree: SpanTree = TreeBuilder::new(8)
            .add_child(0, 8, node::Kind::chained_this(), ast::crumbs::Crumbs::default())
            .add_child(0, 8, node::Kind::Operation, ast::crumbs::Crumbs::default())
            .add_leaf(0, 4, node::Kind::this(), LeftOperand)
            .add_leaf(4, 1, node::Kind::Operation, Operator)
            .add_leaf(5, 3, node::Kind::argument(), RightOperand)
            .done()
            .add_empty_child(8, InsertionPoint::expected_argument(0))
            .done()
            .add_empty_child(8, InsertionPoint::expected_named_argument(1, "foo"))
            .build();

        let cases =
            &[(crumbs!(LeftOperand), vec![0, 0, 0]), (crumbs!(RightOperand), vec![0, 0, 2])];

        for (ast_crumbs, expected_crumbs) in cases {
            let result = tree.root_ref().get_descendant_by_ast_crumbs(ast_crumbs).unwrap();
            assert_eq!(result.node.crumbs, Crumbs::new(expected_crumbs.clone()));
        }
    }
}
