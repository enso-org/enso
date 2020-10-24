//! A module with SpanTree structure definition.

use crate::prelude::*;

use crate::iter::LeafIterator;
use crate::iter::TreeFragment;

use enso_data::text::Index;
use enso_data::text::Size;
use ast::crumbs::IntoCrumbs;
use crate::ArgumentInfo;

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
#[derive(Clone,Debug,Default,Eq,PartialEq)]
#[allow(missing_docs)]
pub struct Node<T> {
    pub kind     : Kind,
    pub size     : Size,
    pub children : Vec<Child<T>>,
    pub ast_id   : Option<ast::Id>,
    pub payload  : T
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

impl<T:Payload> Node<T> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Define a new child by using the `ChildBuilder` pattern.
    pub fn add_child_builder(&mut self, f:impl FnOnce(ChildBuilder<T>)->ChildBuilder<T>) {
        let mut new_child = Child::default();
        let offset        = self.size;
        new_child.offset  = offset;
        let builder       = ChildBuilder::new(new_child);
        let child         = f(builder).child;
        let offset_diff   = child.offset - offset;
        self.size += child.size + offset_diff;
        self.children.push(child);
    }

    /// Define a new child by using the `ChildBuilder` pattern. Consumes self.
    pub fn new_child(mut self, f:impl FnOnce(ChildBuilder<T>)->ChildBuilder<T>) -> Self {
        self.add_child_builder(f);
        self
    }

    /// Is this node empty?
    pub fn is_insertion_point(&self) -> bool {
        self.kind.is_insertion_point()
    }
}


// === Setters ===

#[allow(missing_docs)]
impl<T> Node<T> {
    pub fn with_kind     (mut self, k:impl Into<Kind>) -> Self { self.kind = k.into(); self }
    pub fn with_size     (mut self, size:Size)         -> Self { self.size = size; self }
    pub fn with_children (mut self, ts:Vec<Child<T>>)  -> Self { self.children = ts; self }
    pub fn with_ast_id   (mut self, id:ast::Id)        -> Self { self.ast_id = Some(id); self }
    pub fn with_payload  (mut self, payload:T)         -> Self { self.payload = payload; self }
}


// === Kind getters & setters ===

#[allow(missing_docs)]
impl<T> Node<T> {
    pub fn name          (&self) -> Option<&String>      { self.kind.name() }
    pub fn tp            (&self) -> Option<&String>      { self.kind.tp() }
    pub fn argument_info (&self) -> Option<ArgumentInfo> { self.kind.argument_info() }
    pub fn set_argument_info(&mut self, i:ArgumentInfo)  { self.kind.set_argument_info(i); }
}



// =============
// === Child ===
// =============

/// A structure which contains `Node` being a child of some parent. It contains some additional
/// data regarding this relation
#[derive(Clone,Debug,Default,Eq,PartialEq)]
pub struct Child<T=()> {
    /// A child node.
    pub node       : Node<T>,
    /// An offset counted from the parent node starting index to the start of this node's span.
    pub offset     : Size,
    /// AST crumbs which lead from parent to child associated AST node.
    pub ast_crumbs : ast::Crumbs,
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



// ====================
// === ChildBuilder ===
// ====================

/// A builder pattern for `SpanTree`. A think wrapper for `Child` which adds useful methods for
/// building properties of the current node.
///
/// This builder exposes two main functions - `new_child`, and `add_child`. The former provides a
/// nice, user-friendly interface for building a `SpanTree`, while the later provides a very
/// explicit argument setting interface meant for building `SpanTree` for shape testing purposes.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct ChildBuilder<T=()> {
    pub child : Child<T>,
}

impl<T> Deref for ChildBuilder<T> {
    type Target = Child<T>;
    fn deref(&self) -> &Self::Target {
        &self.child
    }
}

impl<T> DerefMut for ChildBuilder<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.child
    }
}

impl<T:Payload> ChildBuilder<T> {
    /// Constructor.
    pub fn new(child:Child<T>) -> Self {
        Self {child}
    }

    /// Add new child and use the `ChildBuilder` pattern to define its properties. This is a smart
    /// child constructor. This function will automatically compute all not provided properties,
    /// such as span or offset. Moreover, it will default all other not provided fields.
    pub fn new_child(mut self, f:impl FnOnce(Self)->Self) -> Self {
        self.node.add_child_builder(f);
        self
    }

    /// Add new child and use the `ChildBuilder` pattern to define its properties. This function
    /// accepts explicit list of arguments and disables all automatic computation of spans and
    /// offsets. It is useful for testing purposes.
    pub fn add_child
    ( mut self
    , offset : usize
    , size   : usize
    , kind   : Kind
    , crumbs : impl IntoCrumbs
    , f      : impl FnOnce(Self)->Self) -> Self {
        let child : ChildBuilder<T> = ChildBuilder::new(default());
        let child = f(child.offset(offset).size(size).kind(kind).crumbs(crumbs));
        self.node.children.push(child.child);
        self
    }

    /// Offset setter.
    pub fn offset(mut self, offset:usize) -> Self {
        self.offset = Size::new(offset);
        self
    }

    /// Crumbs setter.
    pub fn crumbs(mut self, crumbs:impl IntoCrumbs) -> Self {
        self.ast_crumbs = crumbs.into_crumbs();
        self
    }

    /// Kind setter.
    pub fn kind(mut self, kind:impl Into<Kind>) -> Self {
        self.node.kind = kind.into();
        self
    }

    /// Size setter.
    pub fn size(mut self, size:usize) -> Self {
        self.node.size = Size::new(size);
        self
    }

    /// Expression ID setter.
    pub fn ast_id(mut self, id:ast::Id) -> Self {
        self.node.ast_id = Some(id);
        self
    }

    /// Expression ID generator.
    pub fn new_ast_id(self) -> Self {
        self.ast_id(ast::Id::new_v4())
    }
}



// ==============
// === Crumbs ===
// ==============

/// Identifies subtree within a node. It is the index of the child node.
pub type Crumb = usize;

/// Crumbs specifying this node position related to root.
pub type Crumbs = Vec<Crumb>;

/// Convert crumbs to crumbs pointing to a parent.
pub fn parent_crumbs(crumbs:&[Crumb]) -> Option<&[Crumb]> {
    crumbs.len().checked_sub(1).map(|new_len| &crumbs[..new_len])
}


// === Invalid ===

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
    pub context : Crumbs,
}



// ===========
// === Ref ===
// ===========

/// A reference to node inside some specific tree.
#[derive(Clone,Debug)]
pub struct Ref<'a,T=()> {
    /// The node's ref.
    pub node       : &'a Node<T>,
    /// Span begin being an index counted from the root expression.
    pub span_begin : Index,
    /// Crumbs specifying this node position related to root.
    pub crumbs     : Crumbs,
    /// Ast crumbs locating associated AST node, related to the root's AST node.
    pub ast_crumbs : ast::Crumbs,
}

/// A result of `get_subnode_by_ast_crumbs`
#[derive(Clone,Debug)]
pub struct NodeFoundByAstCrumbs<'a,'b,T=()> {
    /// A node being a result of the lookup.
    pub node       : Ref<'a,T>,
    /// AST crumbs locating the searched AST node inside the AST of found SpanTree node.
    pub ast_crumbs : &'b [ast::Crumb],
}

impl<'a,T:Payload> Ref<'a,T> {
    /// Get span of current node.
    pub fn span(&self) -> enso_data::text::Span {
        enso_data::text::Span::new(self.span_begin,self.node.size)
    }

    /// Get the reference to child with given index. Fails if index if out of bounds.
    pub fn child(mut self, index:usize) -> FallibleResult<Ref<'a,T>> {
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
    pub fn children_iter(self) -> impl Iterator<Item=Ref<'a,T>>
    where T:Clone {
        let children_count = self.node.children.len();
        (0..children_count).map(move |i| self.clone().child(i).unwrap())
    }

    /// Iterator over all leaves of subtree rooted in the `self`.
    pub fn leaf_iter(self) -> Box<dyn Iterator<Item=Ref<'a,T>> + 'a> {
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
    pub fn chain_children_iter(self) -> impl Iterator<Item=Ref<'a,T>> {
        LeafIterator::new(self, TreeFragment::ChainAndDirectChildren)
    }

    /// Get the sub-node (child, or further descendant) identified by `crumbs`.
    pub fn get_descendant<'b>
    (self, crumbs:impl IntoIterator<Item=&'b Crumb>) -> FallibleResult<Ref<'a,T>> {
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
    (self, ast_crumbs:&'b [ast::Crumb]) -> Option<NodeFoundByAstCrumbs<'a,'b,T>> {
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
    pub fn find_by_span(self, span:&enso_data::text::Span) -> Option<Ref<'a,T>> {
        if self.span() == *span {
            Some(self)
        } else {
            self.children_iter().find_map(|ch|
                ch.span().contains_span(span).and_option_from(|| ch.find_by_span(&span))
            )
        }
    }
}

impl<'a,T> Deref for Ref<'a,T> {
    type Target = Node<T>;
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
    use crate::node;
    use crate::SpanTree;

    use ast::crumbs;
    use crate::node::InsertionPointType;

    #[test]
    fn node_lookup() {
        use ast::crumbs::InfixCrumb::*;

        let tree : SpanTree = TreeBuilder::new(7)
            .add_leaf (0,1,node::Kind::this(),vec![LeftOperand])
            .add_leaf (1,1,node::Kind::Operation,vec![Operator])
            .add_child(2,5,node::Kind::argument(),vec![RightOperand])
                .add_leaf(0,2,node::Kind::this(),vec![LeftOperand])
                .add_leaf(3,1,node::Kind::Operation,vec![Operator])
                .add_leaf(4,1,node::Kind::argument(),vec![RightOperand])
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

        let tree : SpanTree = TreeBuilder::new(7)
            .add_leaf (0,1,node::Kind::this(),vec![LeftOperand])
            .add_empty_child(1,InsertionPointType::AfterTarget)
            .add_leaf (1,1,node::Kind::Operation,vec![Operator])
            .add_child(2,5,node::Kind::argument(),vec![RightOperand])
                .add_leaf(0,3,node::Kind::Operation,vec![Func])
                .add_leaf(3,1,node::Kind::this(),vec![Arg])
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
