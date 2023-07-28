//! Crumbs for AST. Crumb identifies children node location in AST node. The access should be
//! possible in a constant time.

use crate::prelude::*;
use enso_text::index::*;

use crate::enumerate_non_empty_lines;
use crate::known;
use crate::HasTokens;
use crate::Shape;
use crate::ShiftedVec1;
use crate::SpanSeed;
use crate::TokenConsumer;

use enso_text as text;



// ==============
// === Errors ===
// ==============

trait IndexedAccess {
    type Item;

    fn get_or_err(&self, index: usize, name: impl Str) -> Result<&Self::Item, IndexOutOfBounds>;

    fn get_mut_or_err(
        &mut self,
        index: usize,
        name: impl Str,
    ) -> Result<&mut Self::Item, IndexOutOfBounds>;
}

impl<T> IndexedAccess for Vec<T> {
    type Item = T;

    fn get_or_err(&self, index: usize, name: impl Str) -> Result<&Self::Item, IndexOutOfBounds> {
        self.get(index).ok_or_else(|| IndexOutOfBounds(name.into()))
    }

    fn get_mut_or_err(
        &mut self,
        index: usize,
        name: impl Str,
    ) -> Result<&mut Self::Item, IndexOutOfBounds> {
        self.get_mut(index).ok_or_else(|| IndexOutOfBounds(name.into()))
    }
}

impl<T> IndexedAccess for ShiftedVec1<T> {
    type Item = T;

    fn get_or_err(&self, index: usize, name: impl Str) -> Result<&Self::Item, IndexOutOfBounds> {
        if index == 0 {
            Ok(&self.head)
        } else {
            Ok(&self.tail.get(index - 1).ok_or_else(|| IndexOutOfBounds(name.into()))?.wrapped)
        }
    }

    fn get_mut_or_err(
        &mut self,
        index: usize,
        name: impl Str,
    ) -> Result<&mut Self::Item, IndexOutOfBounds> {
        if index == 0 {
            Ok(&mut self.head)
        } else {
            Ok(&mut self
                .tail
                .get_mut(index - 1)
                .ok_or_else(|| IndexOutOfBounds(name.into()))?
                .wrapped)
        }
    }
}


#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "The crumb refers to a {} which is not present.", _0)]
pub struct NotPresent(String);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "The crumb refers to {} by index that is out of bounds.", _0)]
pub struct IndexOutOfBounds(String);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(
    display = "The line designated by crumb {:?} does not contain any AST. Context AST was {}.",
    crumb, repr
)]
pub struct LineDoesNotContainAst {
    repr:  String,
    crumb: Crumb,
}

impl LineDoesNotContainAst {
    /// Creates a new instance of error about missing AST in the designated line.
    pub fn new(repr: impl HasRepr, crumb: impl Into<Crumb>) -> LineDoesNotContainAst {
        let repr = repr.repr();
        let crumb = crumb.into();
        LineDoesNotContainAst { repr, crumb }
    }
}

#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "The crumb '{}' is not applicable to '{}' shape.", crumb, shape)]
struct MismatchedCrumbType {
    shape: &'static str,
    crumb: &'static str,
}

#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "The crumb refers to a non-Child tree variant.")]
struct NonChildTreeCrumb;


// =============
// === Crumb ===
// =============

// === Ast ===

/// Trait automatically implemented for all IntoIterators of crumbs.
///
/// It provides way to easily convert vector of specific crumbs (e.g.
/// `[InfixCrumb::LeftoOperand, ..]` without calling into on each element.
pub trait IntoCrumbs: IntoIterator<Item: Into<Crumb>> + Sized {
    /// Convert to the actual Crumbs structure.
    fn into_crumbs(self) -> Crumbs {
        iter_crumbs(self).collect()
    }
}

impl<T: IntoIterator<Item: Into<Crumb>> + Sized> IntoCrumbs for T {}

/// Converts `IntoCrumbs` value into a `Crumb`-yielding iterator.
pub fn iter_crumbs(crumbs: impl IntoCrumbs) -> impl Iterator<Item = Crumb> {
    crumbs.into_iter().map(|crumb| crumb.into())
}

/// Sequence of `Crumb`s describing traversal path through AST.
pub type Crumbs = Vec<Crumb>;

/// Helper macro. Behaves like `vec!` but converts each element into `Crumb`.
#[macro_export]
macro_rules! crumbs {
    ( ) => {
        Vec::<$crate::crumbs::Crumb>::new()
    };
    ( $( $x:expr ),* ) => {
        vec![$($crate::crumbs::Crumb::from($x)),*]
    };
}

/// Crumb identifies location of child AST in an AST node. Allows for a single step AST traversal.
/// The enum variants are paired with Shape variants. For example, `ModuleCrumb` allows obtaining
/// (or setting) `Ast` stored within a `Module` shape.
///
/// As `Ast` can store any `Shape`, this `Crumb` may store any `Shape`-specific crumb type.
///
/// The location format should allow constant time traversal step, so e.g. indices should be used
/// rather than names or similar.
///
/// Crumbs are potentially invalidated by any AST change.

// === Prefix ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixCrumb {
    Func,
    Arg,
}


// === Infix ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InfixCrumb {
    LeftOperand,
    Operator,
    RightOperand,
}


// === SectionLeft ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SectionLeftCrumb {
    Arg,
    Opr,
}


// === SectionRight ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SectionRightCrumb {
    Opr,
    Arg,
}


// === SectionSides ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SectionSidesCrumb;


// === Module ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleCrumb {
    pub line_index: usize,
}


// === Block ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BlockCrumb {
    /// The first non-empty line in block.
    HeadLine,
    /// Index in the sequence of "rest of" lines (not counting the HeadLine).
    TailLine { tail_index: usize },
}


// === Conversion Traits ===

macro_rules! from_crumb {
    ($id:ident,$crumb_id:ident) => {
        impl From<$crumb_id> for Crumb {
            fn from(crumb: $crumb_id) -> Self {
                Crumb::$id(crumb)
            }
        }

        impl From<&$crumb_id> for Crumb {
            fn from(crumb: &$crumb_id) -> Self {
                Crumb::$id(crumb.clone())
            }
        }

        impl IntoIterator for $crumb_id {
            type Item = Crumb;
            type IntoIter = std::iter::Once<Self::Item>;
            fn into_iter(self) -> Self::IntoIter {
                std::iter::once(Crumb::from(self))
            }
        }
    };
}

macro_rules! impl_crumbs {
    ($(($id:ident,$crumb_id:ident,$matcher:ident)),*) => {
        $(from_crumb!{$id,$crumb_id})*

        impl Crumbable for Shape<Ast> {
            type Crumb = Crumb;
            fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
                match (self,crumb) {
                    $((Shape::$id(shape),Crumb::$id(crumb)) => shape.get(crumb),)*
                    (shape, crumb) => Err(MismatchedCrumbType { shape: shape.variant_name(), crumb: crumb.variant_name() }.into())
                }
            }

            fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
                match (self,crumb) {
                    $((Shape::$id(shape),Crumb::$id(crumb)) => Ok(shape.set(crumb,new_ast)?.into()),)*
                    (shape, crumb) => Err(MismatchedCrumbType { shape: shape.variant_name(), crumb: crumb.variant_name() }.into())
                }
            }

            fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
                match self {
                    $(Shape::$id(shape) => Box::new(shape.iter_subcrumbs().map(Crumb::$id)),)*
                    _ => Box::new(std::iter::empty())
                }
            }
        }

        /// Crumb identifies location of child AST in an AST node. Allows for a single step AST traversal.
        #[derive(Clone,Copy,Debug,PartialEq,Eq,PartialOrd,Ord,Hash)]
        #[allow(missing_docs)]
        pub enum Crumb {
            $($id($crumb_id),)*
        }

        impl Crumb {
            $(
                /// Constructor checker.
                pub fn $matcher(&self) -> bool {
                    match self {
                        Self::$id{..} => true,
                        _             => false,
                    }
                }
            )*

            /// Get the Crumb variant name as a static string. Does not contain the module path.
            pub fn variant_name(&self) -> &'static str {
                match self {
                    $(Self::$id{..} => stringify!($id),)*
                }
            }
        }
    }
}

impl IntoIterator for Crumb {
    type Item = Crumb;
    type IntoIter = std::iter::Once<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self)
    }
}


impl_crumbs! {
    // Translated types
    ( Prefix        , PrefixCrumb        , is_prefix         ),
    ( Infix         , InfixCrumb         , is_infix          ),
    ( SectionLeft   , SectionLeftCrumb   , is_section_left   ),
    ( SectionRight  , SectionRightCrumb  , is_section_right  ),
    ( SectionSides  , SectionSidesCrumb  , is_section_sides  ),
    ( Module        , ModuleCrumb        , is_module         ),
    ( Block         , BlockCrumb         , is_block          ),
    // Tree
    ( Tree          , TreeCrumb          , is_tree           )
}



// =================
// === Crumbable ===
// =================

/// Interface for items that allow getting/setting stored Ast located by arbitrary `Crumb`.
pub trait Crumbable {
    /// Specific `Crumb` type used by `Self` to locate child Asts.
    type Crumb: Into<Crumb> + IntoIterator<Item = Crumb>;

    /// Retrieves `Ast` under the crumb.
    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast>;

    /// Sets `Ast` under the crumb, returns updated entity.
    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self>
    where Self: Sized;

    /// Iterates all valid crumbs available for `self`.
    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a>;

    /// Iterates pairs (crumb,child_ast) for `self`.
    fn enumerate<'a>(&'a self) -> Box<dyn Iterator<Item = (Self::Crumb, &'a Ast)> + 'a> {
        let indices = self.iter_subcrumbs();
        let iter = indices.map(move |crumb| {
            // NOTE Safe if this module is correct - children crumbs are always accessible.
            let child = self.get(&crumb).unwrap();
            (crumb, child)
        });
        Box::new(iter)
    }

    /// Returns child Ast subtree while keeping knowledge of its location.
    fn get_located(&self, crumb: Self::Crumb) -> FallibleResult<Located<&Ast>> {
        let child = self.get(&crumb)?;
        Ok(Located::new(crumb, child))
    }

    /// Enumerates all AST being a direct children of the given AST node.
    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>> + 'a> {
        let iter = self.enumerate().map(|(crumb, ast)| ChildAst::new(crumb, ast));
        Box::new(iter)
    }
}

impl Crumbable for crate::Prefix<Ast> {
    type Crumb = PrefixCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        let ret = match crumb {
            PrefixCrumb::Func => &self.func,
            PrefixCrumb::Arg => &self.arg,
        };
        Ok(ret)
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        let target = match crumb {
            PrefixCrumb::Func => &mut ret.func,
            PrefixCrumb::Arg => &mut ret.arg,
        };
        *target = new_ast;
        Ok(ret)
    }

    fn iter_subcrumbs(&self) -> Box<dyn Iterator<Item = Self::Crumb>> {
        const CHILDREN: [PrefixCrumb; 2] = [PrefixCrumb::Func, PrefixCrumb::Arg];
        Box::new(CHILDREN.iter().copied())
    }
}

impl Crumbable for crate::Infix<Ast> {
    type Crumb = InfixCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        let ret = match crumb {
            InfixCrumb::LeftOperand => &self.larg,
            InfixCrumb::Operator => &self.opr,
            InfixCrumb::RightOperand => &self.rarg,
        };
        Ok(ret)
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        let target = match crumb {
            InfixCrumb::LeftOperand => &mut ret.larg,
            InfixCrumb::Operator => &mut ret.opr,
            InfixCrumb::RightOperand => &mut ret.rarg,
        };
        *target = new_ast;
        Ok(ret)
    }

    fn iter_subcrumbs(&self) -> Box<dyn Iterator<Item = Self::Crumb>> {
        const CHILDREN: [InfixCrumb; 3] =
            [InfixCrumb::LeftOperand, InfixCrumb::Operator, InfixCrumb::RightOperand];
        Box::new(CHILDREN.iter().copied())
    }
}

impl Crumbable for crate::SectionLeft<Ast> {
    type Crumb = SectionLeftCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        let ret = match crumb {
            SectionLeftCrumb::Arg => &self.arg,
            SectionLeftCrumb::Opr => &self.opr,
        };
        Ok(ret)
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        let target = match crumb {
            SectionLeftCrumb::Arg => &mut ret.arg,
            SectionLeftCrumb::Opr => &mut ret.opr,
        };
        *target = new_ast;
        Ok(ret)
    }

    fn iter_subcrumbs(&self) -> Box<dyn Iterator<Item = Self::Crumb>> {
        const CHILDREN: [SectionLeftCrumb; 2] = [SectionLeftCrumb::Arg, SectionLeftCrumb::Opr];
        Box::new(CHILDREN.iter().copied())
    }
}

impl Crumbable for crate::SectionRight<Ast> {
    type Crumb = SectionRightCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        let ret = match crumb {
            SectionRightCrumb::Arg => &self.arg,
            SectionRightCrumb::Opr => &self.opr,
        };
        Ok(ret)
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        let target = match crumb {
            SectionRightCrumb::Arg => &mut ret.arg,
            SectionRightCrumb::Opr => &mut ret.opr,
        };
        *target = new_ast;
        Ok(ret)
    }

    fn iter_subcrumbs(&self) -> Box<dyn Iterator<Item = Self::Crumb>> {
        const CHILDREN: [SectionRightCrumb; 2] = [SectionRightCrumb::Opr, SectionRightCrumb::Arg];
        Box::new(CHILDREN.iter().copied())
    }
}

impl Crumbable for crate::SectionSides<Ast> {
    type Crumb = SectionSidesCrumb;

    fn get(&self, _crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        Ok(&self.opr)
    }

    fn set(&self, _crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        ret.opr = new_ast;
        Ok(ret)
    }

    fn iter_subcrumbs(&self) -> Box<dyn Iterator<Item = Self::Crumb>> {
        Box::new(std::iter::once(SectionSidesCrumb))
    }
}

impl Crumbable for crate::Module<Ast> {
    type Crumb = ModuleCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        let line = self.lines.get_or_err(crumb.line_index, "line")?;
        line.elem.as_ref().ok_or_else(|| LineDoesNotContainAst::new(self, crumb).into())
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut module = self.clone();
        let line = module.lines.get_mut_or_err(crumb.line_index, "line")?;
        line.elem.replace(new_ast);
        Ok(module)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let indices = non_empty_line_indices(self.lines.iter());
        let crumbs = indices.map(|line_index| ModuleCrumb { line_index });
        Box::new(crumbs)
    }
}

impl Crumbable for crate::Block<Ast> {
    type Crumb = BlockCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            BlockCrumb::HeadLine => Ok(&self.first_line.elem),
            BlockCrumb::TailLine { tail_index } => {
                let line = self.lines.get_or_err(*tail_index, "line")?;
                line.elem.as_ref().ok_or_else(|| LineDoesNotContainAst::new(self, crumb).into())
            }
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut block = self.clone();
        match crumb {
            BlockCrumb::HeadLine => block.first_line.elem = new_ast,
            BlockCrumb::TailLine { tail_index } => {
                let line = block.lines.get_mut_or_err(*tail_index, "line")?;
                line.elem.replace(new_ast);
            }
        }
        Ok(block)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let first_line = std::iter::once(BlockCrumb::HeadLine);
        let tail_line_indices = non_empty_line_indices(self.lines.iter());
        let tail_lines = tail_line_indices.map(|tail_index| BlockCrumb::TailLine { tail_index });
        Box::new(first_line.chain(tail_lines))
    }
}

/// Just delegates the implementation to shape.
impl Crumbable for Ast {
    type Crumb = Crumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        self.shape().get(crumb)
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let new_shape = self.shape().set(crumb, new_ast)?;
        Ok(self.with_shape(new_shape))
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        self.shape().iter_subcrumbs()
    }
}

/// Just delegates to Ast.
impl<T, E> Crumbable for known::KnownAst<T>
where
    for<'t> &'t Shape<Ast>: TryInto<&'t T, Error = E>,
    E: failure::Fail,
{
    type Crumb = Crumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        self.ast().get(crumb)
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let new_ast = self.ast().set(crumb, new_ast)?;
        let ret = known::KnownAst::try_new(new_ast)?;
        Ok(ret)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        self.ast().iter_subcrumbs()
    }
}



// ===================
// === Tree crumbs ===
// ===================

impl Crumbable for crate::Tree<Ast> {
    type Crumb = TreeCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        match self
            .span_info
            .get(crumb.index)
            .ok_or_else(|| IndexOutOfBounds("Tree child".into()))?
        {
            SpanSeed::Child(crate::SpanSeedChild { node }) => Ok(node),
            _ => Err(NonChildTreeCrumb.into()),
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut result = self.clone();
        let child = result
            .span_info
            .get_mut(crumb.index)
            .ok_or_else(|| IndexOutOfBounds("Tree child".into()))?;
        *child = SpanSeed::Child(crate::SpanSeedChild { node: new_ast });
        Ok(result)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        Box::new(self.span_info.iter().enumerate().filter_map(|(index, thing)| {
            matches!(thing, SpanSeed::Child(_)).as_some(TreeCrumb { index })
        }))
    }
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TreeCrumb {
    pub index: usize,
}



// ===========================
// === Recursive Traversal ===
// ===========================

/// Interface for recursive AST traversal using `Crumb` sequence.
///
/// Intended for `Ast` and `Ast`-like types, like `KnownAst`.
pub trait TraversableAst: Sized {
    /// Returns rewritten AST where child AST under location designated by `crumbs` is updated.
    ///
    /// Works recursively.
    fn set_traversing(&self, crumbs: &[Crumb], new_ast: Ast) -> FallibleResult<Self>;

    /// Recursively traverses AST to retrieve AST node located by given crumbs sequence.
    fn get_traversing(&self, crumbs: &[Crumb]) -> FallibleResult<&Ast>;

    /// Get the `Ast` node corresponding to `Self`.
    fn my_ast(&self) -> FallibleResult<&Ast> {
        self.get_traversing(&[])
    }

    /// Calculate the span of the descendent AST node described by given crumbs.
    fn range_of_descendant_at(&self, crumbs: &[Crumb]) -> FallibleResult<text::Range<Byte>> {
        let mut position = 0.byte();
        let mut ast = self.my_ast()?;
        for crumb in crumbs {
            let child = ast.get(crumb)?;
            let child_offset = ast.child_offset(child)?;
            position += child_offset;
            ast = child;
        }
        Ok(text::Range::new(position, position + ast.repr_len()))
    }
}

impl TraversableAst for Ast {
    fn set_traversing(&self, crumbs: &[Crumb], new_ast: Ast) -> FallibleResult<Self> {
        match crumbs {
            [] => Ok(new_ast),
            [first_crumb, tail_crumbs @ ..] => {
                let child = self.get(first_crumb)?;
                let updated_child = child.set_traversing(tail_crumbs, new_ast)?;
                self.set(first_crumb, updated_child)
            }
        }
    }

    fn get_traversing(&self, crumbs: &[Crumb]) -> FallibleResult<&Ast> {
        if let Some(first_crumb) = crumbs.first() {
            let child = self.get(first_crumb)?;
            child.get_traversing(&crumbs[1..])
        } else {
            Ok(self)
        }
    }
}

impl<T, E> TraversableAst for known::KnownAst<T>
where
    for<'t> &'t Shape<Ast>: TryInto<&'t T, Error = E>,
    E: failure::Fail,
{
    fn set_traversing(&self, crumbs: &[Crumb], new_ast: Ast) -> FallibleResult<Self> {
        let updated_ast = self.ast().set_traversing(crumbs, new_ast)?;
        Ok(Self::try_new(updated_ast)?)
    }

    fn get_traversing(&self, crumbs: &[Crumb]) -> FallibleResult<&Ast> {
        self.ast().get_traversing(crumbs)
    }
}



// ===============
// === Utility ===
// ===============

/// Iterates over indices of non-empty lines in a line sequence.
pub fn non_empty_line_indices<'a, T: 'a>(
    iter: impl Iterator<Item = &'a crate::BlockLine<Option<T>>> + 'a,
) -> impl Iterator<Item = usize> + 'a {
    enumerate_non_empty_lines(iter).map(|(index, _ast)| index)
}



// ===============
// === Located ===
// ===============

/// Item which location is identified by `Crumbs`.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Deref)]
pub struct Located<T> {
    /// Crumbs from containing parent.
    pub crumbs: Crumbs,
    /// The sub-item representation.
    #[deref]
    pub item:   T,
}

impl<T> Located<T> {
    /// Creates a new located item.
    pub fn new(crumbs: impl IntoCrumbs, item: T) -> Located<T> {
        let crumbs = crumbs.into_crumbs();
        Located { crumbs, item }
    }

    /// Creates a new item in a root location (empty crumbs list).
    pub fn new_root(item: T) -> Located<T> {
        let crumbs = default();
        Located { crumbs, item }
    }

    /// Uses given function to map over the item.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Located<U> {
        Located::new(self.crumbs, f(self.item))
    }

    /// Descends into a child described from `item` by given function.
    pub fn entered<U>(&self, f: impl FnOnce(&T) -> Located<U>) -> Located<U> {
        let child = f(&self.item);
        self.descendant(child.crumbs, child.item)
    }

    /// Takes crumbs relative to self and item that will be wrapped.
    pub fn descendant<U>(&self, crumbs: impl IntoCrumbs, child: U) -> Located<U> {
        let crumbs_so_far = self.crumbs.iter().cloned();
        let crumbs = crumbs_so_far.chain(crumbs.into_crumbs());
        Located::new(crumbs, child)
    }

    /// Maps into child, concatenating this crumbs and child crumbs.
    pub fn into_descendant<U>(self, child: Located<U>) -> Located<U> {
        let Located { crumbs, item } = child;
        let mut ret = self.map(|_| item);
        ret.crumbs.extend(crumbs);
        ret
    }
}

impl<T> Located<Option<T>> {
    /// Propagates Option from the stored value onto self.
    pub fn into_opt(self) -> Option<Located<T>> {
        let Located { item, crumbs } = self;
        item.map(|item| Located { crumbs, item })
    }
}

impl<T: HasTokens> HasTokens for Located<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        self.item.feed_to(consumer)
    }
}

/// Reference to AST stored under some known crumbs path.
pub type ChildAst<'a> = Located<&'a Ast>;



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;

    /// Gets item under given crumb and checks if its representation is as expected.
    fn expect_repr<C: Crumbable>(item: &C, crumb: &C::Crumb, expected_repr: impl Str) {
        assert_eq!(item.get(crumb).unwrap().repr(), expected_repr.as_ref());
    }

    #[test]
    fn module_crumb() {
        let lines = [Some(Ast::var("foo")), None, Some(Ast::var("bar"))];

        let module = crate::Module::from_lines(&lines);


        // === Getting ===
        expect_repr(&module, &ModuleCrumb { line_index: 0 }, "foo");
        assert!(module.get(&ModuleCrumb { line_index: 1 }).is_err());
        expect_repr(&module, &ModuleCrumb { line_index: 2 }, "bar");
        assert!(module.get(&ModuleCrumb { line_index: 3 }).is_err());

        let module2 = module.set(&ModuleCrumb { line_index: 0 }, Ast::var("foo2")).unwrap();
        assert_eq!(module2.repr(), "foo2\n\nbar");
        let module3 = module.set(&ModuleCrumb { line_index: 1 }, Ast::var("foo2")).unwrap();
        assert_eq!(module3.repr(), "foo\nfoo2\nbar");
        let module4 = module.set(&ModuleCrumb { line_index: 3 }, Ast::var("foo2"));
        assert!(module4.is_err());
    }

    #[test]
    fn block_crumb() {
        let first_line = Ast::var("first_line");
        let tail_lines = [Some(Ast::var("tail0")), None, Some(Ast::var("tail2"))];
        let block = crate::Block::from_lines(&first_line, &tail_lines);

        expect_repr(&block, &BlockCrumb::HeadLine, "first_line");
        expect_repr(&block, &BlockCrumb::TailLine { tail_index: 0 }, "tail0");
        assert!(block.get(&BlockCrumb::TailLine { tail_index: 1 }).is_err());
        expect_repr(&block, &BlockCrumb::TailLine { tail_index: 2 }, "tail2");
        assert!(block.get(&BlockCrumb::TailLine { tail_index: 3 }).is_err());

        let block2 = block.set(&BlockCrumb::HeadLine, Ast::var("first_line2")).unwrap();
        assert_eq!(block2.repr(), "\nfirst_line2\ntail0\n\ntail2");
        let block3 = block.set(&BlockCrumb::TailLine { tail_index: 1 }, Ast::var("tail1")).unwrap();
        assert_eq!(block3.repr(), "\nfirst_line\ntail0\ntail1\ntail2");
        let block4 =
            block.set(&BlockCrumb::TailLine { tail_index: 2 }, Ast::var("tail22")).unwrap();
        assert_eq!(block4.repr(), "\nfirst_line\ntail0\n\ntail22");
    }

    fn get<T, F: FnOnce(T) -> Crumb>(f: F, ast: &Ast, crumb: T) -> FallibleResult<&Ast> {
        let crumb = f(crumb);
        ast.get(&crumb)
    }

    fn set<T, F: FnOnce(T) -> Crumb>(
        f: F,
        ast: &Ast,
        crumb: T,
        internal_ast: Ast,
    ) -> FallibleResult<Ast> {
        let crumb = f(crumb);
        ast.set(&crumb, internal_ast)
    }


    // === Infix ===

    #[test]
    fn infix_crumb() -> FallibleResult {
        let infix = Ast::infix_var("foo", "+", "bar");
        let to_crumb_enum = Crumb::Infix;
        let baz = Ast::var("baz");
        let times = Ast::opr("*");

        assert_eq!(infix.repr(), "foo + bar");

        assert_eq!(get(to_crumb_enum, &infix, InfixCrumb::LeftOperand)?.repr(), "foo");
        assert_eq!(get(to_crumb_enum, &infix, InfixCrumb::Operator)?.repr(), "+");
        assert_eq!(get(to_crumb_enum, &infix, InfixCrumb::RightOperand)?.repr(), "bar");

        assert_eq!(
            set(to_crumb_enum, &infix, InfixCrumb::LeftOperand, baz.clone())?.repr(),
            "baz + bar"
        );
        assert_eq!(set(to_crumb_enum, &infix, InfixCrumb::Operator, times)?.repr(), "foo * bar");
        assert_eq!(set(to_crumb_enum, &infix, InfixCrumb::RightOperand, baz)?.repr(), "foo + baz");

        Ok(())
    }

    #[test]
    fn iterate_infix() {
        let sum = crate::Infix::from_vars("foo", "+", "bar");
        let (larg, opr, rarg) = sum.iter_subcrumbs().expect_tuple();
        assert_eq!(larg, InfixCrumb::LeftOperand);
        assert_eq!(opr, InfixCrumb::Operator);
        assert_eq!(rarg, InfixCrumb::RightOperand);
    }

    #[test]
    fn nested_infix() -> FallibleResult {
        use InfixCrumb::*;

        let sum = Ast::infix_var("foo", "+", "bar");
        let infix = Ast::infix(Ast::var("main"), "=", sum);
        assert_eq!(infix.repr(), "main = foo + bar");

        let set = |crumbs: &[InfixCrumb], ast| {
            let crumbs = crumbs.iter().map(|c| Crumb::Infix(*c)).collect_vec();
            infix.set_traversing(&crumbs, ast)
        };
        let get = |crumbs: &[InfixCrumb]| {
            let crumbs = crumbs.iter().map(|c| Crumb::Infix(*c)).collect_vec();
            infix.get_traversing(&crumbs)
        };

        assert_eq!(set(&[RightOperand, LeftOperand], Ast::var("baz"))?.repr(), "main = baz + bar");
        assert_eq!(set(&[LeftOperand], Ast::var("baz"))?.repr(), "baz = foo + bar");


        assert_eq!(get(&[Operator])?.repr(), "=");
        assert_eq!(get(&[RightOperand])?.repr(), "foo + bar");
        assert_eq!(get(&[RightOperand, LeftOperand])?.repr(), "foo");
        assert_eq!(get(&[RightOperand, RightOperand])?.repr(), "bar");
        Ok(())
    }


    // === Prefix ===

    #[test]
    fn prefix_crumb() -> FallibleResult {
        let prefix = Ast::prefix(Ast::var("func"), Ast::var("arg"));
        let get = |prefix_crumb| {
            let crumb = Crumb::Prefix(prefix_crumb);
            prefix.get(&crumb)
        };
        let set = |prefix_crumb, ast| {
            let crumb = Crumb::Prefix(prefix_crumb);
            prefix.set(&crumb, ast)
        };
        let foo = Ast::var("foo");
        let x = Ast::var("x");

        assert_eq!(prefix.repr(), "func arg");

        assert_eq!(get(PrefixCrumb::Func)?.repr(), "func");
        assert_eq!(get(PrefixCrumb::Arg)?.repr(), "arg");

        assert_eq!(set(PrefixCrumb::Func, foo)?.repr(), "foo arg");
        assert_eq!(set(PrefixCrumb::Arg, x)?.repr(), "func x");

        Ok(())
    }

    #[test]
    fn iterate_prefix() -> FallibleResult {
        let prefix = Ast::prefix(Ast::var("func"), Ast::var("arg"));

        let (func, arg) = prefix.iter_subcrumbs().expect_tuple();

        assert_eq!(func, Crumb::Prefix(PrefixCrumb::Func));
        assert_eq!(arg, Crumb::Prefix(PrefixCrumb::Arg));

        Ok(())
    }


    // === SectionLeft ===

    #[test]
    fn section_left_crumb() -> FallibleResult {
        let app = Ast::section_left(Ast::var("foo"), "bar");
        let get = |app_crumb| {
            let crumb = Crumb::SectionLeft(app_crumb);
            app.get(&crumb)
        };
        let set = |app_crumb, ast| {
            let crumb = Crumb::SectionLeft(app_crumb);
            app.set(&crumb, ast)
        };
        let arg = Ast::var("arg");
        let opr = Ast::var("opr");

        assert_eq!(app.repr(), "foo bar");

        assert_eq!(get(SectionLeftCrumb::Arg)?.repr(), "foo");
        assert_eq!(get(SectionLeftCrumb::Opr)?.repr(), "bar");

        assert_eq!(set(SectionLeftCrumb::Arg, arg)?.repr(), "arg bar");
        assert_eq!(set(SectionLeftCrumb::Opr, opr)?.repr(), "foo opr");

        Ok(())
    }

    #[test]
    fn iterate_section_left() -> FallibleResult {
        let app = Ast::section_left(Ast::var("foo"), "bar");

        let (arg, opr) = app.iter_subcrumbs().expect_tuple();
        assert_eq!(arg, Crumb::SectionLeft(SectionLeftCrumb::Arg));
        assert_eq!(opr, Crumb::SectionLeft(SectionLeftCrumb::Opr));

        Ok(())
    }


    // === SectionRight ===

    #[test]
    fn section_right_crumb() -> FallibleResult {
        let app = Ast::section_right("foo", Ast::var("bar"));
        let get = |app_crumb| {
            let crumb = Crumb::SectionRight(app_crumb);
            app.get(&crumb)
        };
        let set = |app_crumb, ast| {
            let crumb = Crumb::SectionRight(app_crumb);
            app.set(&crumb, ast)
        };
        let arg = Ast::var("arg");
        let opr = Ast::var("opr");

        assert_eq!(app.repr(), "foo bar");

        assert_eq!(get(SectionRightCrumb::Opr)?.repr(), "foo");
        assert_eq!(get(SectionRightCrumb::Arg)?.repr(), "bar");

        assert_eq!(set(SectionRightCrumb::Opr, opr)?.repr(), "opr bar");
        assert_eq!(set(SectionRightCrumb::Arg, arg)?.repr(), "foo arg");

        Ok(())
    }

    #[test]
    fn iterate_section_right() -> FallibleResult {
        let app = Ast::section_right("foo", Ast::var("bar"));

        let (opr, arg) = app.iter_subcrumbs().expect_tuple();
        assert_eq!(arg, Crumb::SectionRight(SectionRightCrumb::Arg));
        assert_eq!(opr, Crumb::SectionRight(SectionRightCrumb::Opr));

        Ok(())
    }


    // === SectionSides ===

    #[test]
    fn section_sides_crumb() -> FallibleResult {
        let app = Ast::section_sides("foo");
        let get = |app_crumb| {
            let crumb = Crumb::SectionSides(app_crumb);
            app.get(&crumb)
        };
        let set = |app_crumb, ast| {
            let crumb = Crumb::SectionSides(app_crumb);
            app.set(&crumb, ast)
        };
        let opr = Ast::var("opr");

        assert_eq!(app.repr(), "foo");

        assert_eq!(get(SectionSidesCrumb)?.repr(), "foo");
        assert_eq!(set(SectionSidesCrumb, opr)?.repr(), "opr");

        Ok(())
    }

    #[test]
    fn iterate_section_sides() -> FallibleResult {
        let app = Ast::section_sides("foo");

        let mut iter = app.iter_subcrumbs();

        assert_eq!(iter.next(), Some(Crumb::SectionSides(SectionSidesCrumb)));
        assert_eq!(iter.next(), None);

        Ok(())
    }


    // === Module ===

    #[test]
    fn iterate_module() {
        let var = crate::Ast::var("foo");
        let lines = [Some(var.clone_ref()), None, Some(var.clone_ref())];
        let module = crate::Module::from_lines(&lines);
        assert_eq!(module.repr(), "foo\n\nfoo");

        let (line0, line2) = module.iter_subcrumbs().expect_tuple();
        assert_eq!(line0.line_index, 0);
        assert_eq!(line2.line_index, 2);
    }


    // === Block ===

    #[test]
    fn iterate_block() {
        let first_line = crate::Ast::var("foo");
        let lines = [Some(crate::Ast::var("bar")), None, Some(crate::Ast::var("baz"))];
        let block = crate::Block::from_lines(&first_line, &lines);
        let (line0, line1, line3) = block.iter_subcrumbs().expect_tuple();
        assert_eq!(line0, BlockCrumb::HeadLine);
        assert_eq!(line1, BlockCrumb::TailLine { tail_index: 0 });
        assert_eq!(line3, BlockCrumb::TailLine { tail_index: 2 });
    }

    #[test]
    fn mismatched_crumb() {
        let sum = Ast::infix_var("foo", "+", "bar");
        let crumb = Crumb::Module(ModuleCrumb { line_index: 0 });
        let first_line = sum.get(&crumb);
        first_line.expect_err("Using module crumb on infix should fail");
    }

    #[test]
    fn located() {
        let item = Located::new_root("zero");
        assert_eq!(item.item, "zero");
        assert!(item.crumbs.is_empty());

        let child_item = Located::new(InfixCrumb::Operator, "two");
        let item = item.into_descendant(child_item);
        assert_eq!(item.item, "two");
        let (crumb0,) = item.crumbs.iter().expect_tuple();
        assert_eq!(crumb0, &Crumb::Infix(InfixCrumb::Operator));

        let item2 = item.clone().map(|item| item.len());
        assert_eq!(item2.item, 3);
        assert_eq!(item.crumbs, item2.crumbs);
    }


    // === TraversableAst ===

    #[test]
    fn traversable_ast() {
        let ast = Ast::prefix(Ast::prefix(Ast::var("add"), Ast::number(2)), Ast::number(4));
        let expected_code = "add 2 4";
        assert_eq!(ast.repr(), expected_code);
        assert_eq!(ast.my_ast().unwrap(), &ast);

        let crumbs_to_two = [PrefixCrumb::Func, PrefixCrumb::Arg].into_crumbs();
        let two = ast.get_traversing(&crumbs_to_two).unwrap();
        assert_eq!(two.repr(), "2");

        let two_span = ast.range_of_descendant_at(&crumbs_to_two).unwrap();
        assert_eq!(two_span, 4.byte()..5.byte());
        assert_eq!(&expected_code[two_span], "2");
    }
}
