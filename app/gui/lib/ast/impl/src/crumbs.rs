//! Crumbs for AST. Crumb identifies children node location in AST node. The access should be
//! possible in a constant time.

use crate::prelude::*;

use crate::enumerate_non_empty_lines;
use crate::known;
use crate::HasTokens;
use crate::MacroPatternMatch;
use crate::Shape;
use crate::Shifted;
use crate::ShiftedVec1;
use crate::TokenConsumer;

use enso_data::text::Index;
use enso_data::text::Size;
use enso_data::text::Span;



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

#[derive(Debug, Display, Fail, Clone, Copy)]
struct MismatchedCrumbType;



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

// === InvalidSuffix ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidSuffixCrumb;


// === TextLineFmt ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextLineFmtCrumb {
    pub segment_index: usize,
}


// === TextBlockFmt ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextBlockFmtCrumb {
    pub text_line_index: usize,
    pub segment_index:   usize,
}


// === TextUnclosed ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextUnclosedCrumb {
    pub text_line_crumb: TextLineFmtCrumb,
}


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


// === Import ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImportCrumb {
    Path { index: usize },
    Rename,
    OnlyNames { index: usize },
    HidingNames { index: usize },
}

// === Export ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExportCrumb {
    Path { index: usize },
    Rename,
    OnlyNames { index: usize },
    HidingNames { index: usize },
}


// === Mixfix ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MixfixCrumb {
    Name { index: usize },
    Args { index: usize },
}


// === Group ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GroupCrumb;


// === Def ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DefCrumb {
    Name,
    Args { index: usize },
    Body,
}


// === Match ===

#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum MatchCrumb {
    Pfx { val: Vec<PatternMatchCrumb> },
    Segs { val: SegmentMatchCrumb, index: usize },
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum PatternMatchCrumb {
    Begin,
    End,
    Nothing,
    Build,
    Err,
    Tok,
    Blank,
    Var,
    Cons,
    Opr,
    Annotation,
    Mod,
    Num,
    Text,
    Block,
    Macro,
    Invalid,
    Except,
    Tag,
    Cls,
    Or,
    Seq { right: bool },
    Many { index: usize },
}

#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SegmentMatchCrumb {
    Head,
    Body { val: Vec<PatternMatchCrumb> },
}


// === Ambiguous ===

#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AmbiguousCrumb {
    pub index: usize,
    pub field: AmbiguousSegmentCrumb,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AmbiguousSegmentCrumb {
    Head,
    Body,
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
                    _ => Err(MismatchedCrumbType.into())
                }
            }

            fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
                match (self,crumb) {
                    $((Shape::$id(shape),Crumb::$id(crumb)) => Ok(shape.set(crumb,new_ast)?.into()),)*
                    _ => Err(MismatchedCrumbType.into())
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
        #[derive(Clone,Debug,PartialEq,Eq,PartialOrd,Ord,Hash)]
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
    ( InvalidSuffix , InvalidSuffixCrumb , is_invalid_suffix ),
    ( TextLineFmt   , TextLineFmtCrumb   , is_text_line_fmt  ),
    ( TextBlockFmt  , TextBlockFmtCrumb  , is_text_block_fmt ),
    ( TextUnclosed  , TextUnclosedCrumb  , is_text_unclosed  ),
    ( Prefix        , PrefixCrumb        , is_prefix         ),
    ( Infix         , InfixCrumb         , is_infix          ),
    ( SectionLeft   , SectionLeftCrumb   , is_section_left   ),
    ( SectionRight  , SectionRightCrumb  , is_section_right  ),
    ( SectionSides  , SectionSidesCrumb  , is_section_sides  ),
    ( Module        , ModuleCrumb        , is_module         ),
    ( Block         , BlockCrumb         , is_block          ),
    ( Match         , MatchCrumb         , is_match          ),
    ( Ambiguous     , AmbiguousCrumb     , is_ambiguous      ),
    ( Import        , ImportCrumb        , is_import         ),
    ( Export        , ExportCrumb        , is_export         ),
    ( Mixfix        , MixfixCrumb        , is_mixfix         ),
    ( Group         , GroupCrumb         , is_group          ),
    ( Def           , DefCrumb           , is_def            )
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

impl Crumbable for crate::InvalidSuffix<Ast> {
    type Crumb = InvalidSuffixCrumb;

    fn get(&self, _crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        Ok(&self.elem)
    }

    fn set(&self, _crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        ret.elem = new_ast;
        Ok(ret)
    }

    fn iter_subcrumbs(&self) -> Box<dyn Iterator<Item = Self::Crumb>> {
        Box::new(std::iter::once(InvalidSuffixCrumb))
    }
}

impl Crumbable for crate::TextLineFmt<Ast> {
    type Crumb = TextLineFmtCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        let segment = self.text.get_or_err(crumb.segment_index, "text segment")?;
        if let crate::SegmentFmt::SegmentExpr(expr) = segment {
            expr.value.as_ref().ok_or_else(|| NotPresent("expression".into()).into())
        } else {
            Err(NotPresent("expression segment".into()).into())
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut text = self.clone();
        let segment = text.text.get_mut_or_err(crumb.segment_index, "text segment")?;
        if let crate::SegmentFmt::SegmentExpr(expr) = segment {
            expr.value = Some(new_ast);
            Ok(text)
        } else {
            Err(NotPresent("expression segment".into()).into())
        }
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        Box::new(self.text.iter().enumerate().filter_map(|(segment_index, segment)| {
            if let crate::SegmentFmt::SegmentExpr(_) = segment {
                Some(TextLineFmtCrumb { segment_index })
            } else {
                None
            }
        }))
    }
}

impl Crumbable for crate::TextBlockFmt<Ast> {
    type Crumb = TextBlockFmtCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        let line = self.text.get_or_err(crumb.text_line_index, "line")?;
        let segment = line.text.get_or_err(crumb.segment_index, "segment")?;
        if let crate::SegmentFmt::SegmentExpr(expr) = segment {
            expr.value.as_ref().ok_or_else(|| NotPresent("expression value".into()).into())
        } else {
            Err(NotPresent("expression segment".into()).into())
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut text = self.clone();
        let line = text.text.get_mut_or_err(crumb.text_line_index, "line")?;
        let segment = line.text.get_mut_or_err(crumb.segment_index, "segment")?;
        if let crate::SegmentFmt::SegmentExpr(expr) = segment {
            expr.value = Some(new_ast);
            Ok(text)
        } else {
            Err(NotPresent("expression segment".into()).into())
        }
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        Box::new(self.text.iter().enumerate().flat_map(|(text_line_index, line)| {
            line.text
                .iter()
                .enumerate()
                .filter(|(_, segment)| matches!(segment, crate::SegmentFmt::SegmentExpr(_)))
                .map(move |(segment_index, _)| TextBlockFmtCrumb { text_line_index, segment_index })
        }))
    }
}

impl Crumbable for crate::TextUnclosed<Ast> {
    type Crumb = TextUnclosedCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        if let crate::TextLine::TextLineFmt(text_line) = &self.line {
            text_line.get(&crumb.text_line_crumb)
        } else {
            Err(NotPresent("formatted text line".into()).into())
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut text = self.clone();
        if let crate::TextLine::TextLineFmt(text_line) = text.line {
            let text_line_fmt = text_line.set(&crumb.text_line_crumb, new_ast)?;
            text.line = crate::TextLine::TextLineFmt(text_line_fmt);
            Ok(text)
        } else {
            Err(NotPresent("formatted text line".into()).into())
        }
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        if let crate::TextLine::TextLineFmt(text_line) = &self.line {
            Box::new(
                text_line
                    .iter_subcrumbs()
                    .map(|text_line_crumb| TextUnclosedCrumb { text_line_crumb }),
            )
        } else {
            Box::new(std::iter::empty())
        }
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

/// Helper function for getting an Ast from MacroPatternMatch based on crumb position.
fn pattern_get<'a>(
    pat: &'a MacroPatternMatch<Shifted<Ast>>,
    path: &[PatternMatchCrumb],
) -> FallibleResult<&'a Ast> {
    use crate::MacroPatternMatchRaw::*;

    let missing = |str: &str| Result::Err(NotPresent(str.into()).into());

    let mut pattern = pat;
    for crumb in path {
        match (pattern.deref(), crumb) {
            (Begin(_), PatternMatchCrumb::Begin) => return missing("elem"),
            (End(_), PatternMatchCrumb::End) => return missing("elem"),
            (Nothing(_), PatternMatchCrumb::Nothing) => return missing("elem"),
            (Build(pat), PatternMatchCrumb::Build) => return Ok(&pat.elem.wrapped),
            (Err(pat), PatternMatchCrumb::Err) => return Ok(&pat.elem.wrapped),
            (Tok(pat), PatternMatchCrumb::Tok) => return Ok(&pat.elem.wrapped),
            (Blank(pat), PatternMatchCrumb::Blank) => return Ok(&pat.elem.wrapped),
            (Var(pat), PatternMatchCrumb::Var) => return Ok(&pat.elem.wrapped),
            (Cons(pat), PatternMatchCrumb::Cons) => return Ok(&pat.elem.wrapped),
            (Opr(pat), PatternMatchCrumb::Opr) => return Ok(&pat.elem.wrapped),
            (Annotation(pat), PatternMatchCrumb::Annotation) => return Ok(&pat.elem.wrapped),
            (Mod(pat), PatternMatchCrumb::Mod) => return Ok(&pat.elem.wrapped),
            (Num(pat), PatternMatchCrumb::Num) => return Ok(&pat.elem.wrapped),
            (Text(pat), PatternMatchCrumb::Text) => return Ok(&pat.elem.wrapped),
            (Block(pat), PatternMatchCrumb::Block) => return Ok(&pat.elem.wrapped),
            (Macro(pat), PatternMatchCrumb::Macro) => return Ok(&pat.elem.wrapped),
            (Invalid(pat), PatternMatchCrumb::Invalid) => return Ok(&pat.elem.wrapped),
            (Except(pat), PatternMatchCrumb::Except) => pattern = &pat.elem,
            (Tag(pat), PatternMatchCrumb::Tag) => pattern = &pat.elem,
            (Cls(pat), PatternMatchCrumb::Cls) => pattern = &pat.elem,
            (Or(pat), PatternMatchCrumb::Or) => pattern = &pat.elem,
            (Seq(pat), PatternMatchCrumb::Seq { right }) =>
                pattern = if *right { &pat.elem.1 } else { &pat.elem.0 },
            (Many(pat), PatternMatchCrumb::Many { index }) =>
                pattern = pat.elem.get_or_err(*index, "elem")?,
            _ => return missing("crumb"),
        }
    }
    missing("crumb")
}

/// Helper function that updates Ast in MacroPatternMatch based on crumb position.
fn pattern_set(
    pat: &MacroPatternMatch<Shifted<Ast>>,
    path: &[PatternMatchCrumb],
    ast: Ast,
) -> FallibleResult<MacroPatternMatch<Shifted<Ast>>> {
    use crate::MacroPatternMatchRaw::*;

    let missing = |str: &str| Result::Err(NotPresent(str.into()).into());

    let mut result = pat.clone();
    let mut pattern = Rc::make_mut(&mut result);
    for crumb in path {
        match (pattern, crumb) {
            (Begin(_), PatternMatchCrumb::Begin) => return missing("elem"),
            (End(_), PatternMatchCrumb::End) => return missing("elem"),
            (Nothing(_), PatternMatchCrumb::Nothing) => return missing("elem"),
            (Build(pat), PatternMatchCrumb::Build) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Err(pat), PatternMatchCrumb::Err) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Tok(pat), PatternMatchCrumb::Tok) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Blank(pat), PatternMatchCrumb::Blank) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Var(pat), PatternMatchCrumb::Var) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Cons(pat), PatternMatchCrumb::Cons) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Opr(pat), PatternMatchCrumb::Opr) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Annotation(pat), PatternMatchCrumb::Annotation) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Mod(pat), PatternMatchCrumb::Mod) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Num(pat), PatternMatchCrumb::Num) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Text(pat), PatternMatchCrumb::Text) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Block(pat), PatternMatchCrumb::Block) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Macro(pat), PatternMatchCrumb::Macro) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Invalid(pat), PatternMatchCrumb::Invalid) => {
                pat.elem.wrapped = ast;
                break;
            }
            (Except(pat), PatternMatchCrumb::Except) => {
                pat.elem = pat.elem.clone();
                pattern = Rc::make_mut(&mut pat.elem);
            }
            (Tag(pat), PatternMatchCrumb::Tag) => {
                pat.elem = pat.elem.clone();
                pattern = Rc::make_mut(&mut pat.elem);
            }
            (Cls(pat), PatternMatchCrumb::Cls) => {
                pat.elem = pat.elem.clone();
                pattern = Rc::make_mut(&mut pat.elem);
            }
            (Or(pat), PatternMatchCrumb::Or) => {
                pat.elem = pat.elem.clone();
                pattern = Rc::make_mut(&mut pat.elem);
            }
            (Seq(pat), PatternMatchCrumb::Seq { right }) =>
                if *right {
                    pat.elem.1 = pat.elem.1.clone();
                    pattern = Rc::make_mut(&mut pat.elem.1);
                } else {
                    pat.elem.0 = pat.elem.0.clone();
                    pattern = Rc::make_mut(&mut pat.elem.0);
                },
            (Many(pat), PatternMatchCrumb::Many { index }) => {
                let elem = pat.elem.get_mut_or_err(*index, "elem")?;
                *elem = elem.clone();
                pattern = Rc::make_mut(elem);
            }
            _ => return missing("crumb"),
        }
    }
    Ok(result)
}

/// Helper function that returns subcrumbs for MacroPatternMatch.
fn pattern_subcrumbs(pat: &MacroPatternMatch<Shifted<Ast>>) -> Vec<Vec<PatternMatchCrumb>> {
    use crate::MacroPatternMatchRaw::*;

    let mut crumbs = vec![];
    let mut patterns = vec![(vec![], pat)];
    while let Some((mut crumb, pattern)) = patterns.pop() {
        match pattern.deref() {
            Begin(_) => (),
            End(_) => (),
            Nothing(_) => (),
            Build(_) => {
                crumb.push(PatternMatchCrumb::Build);
                crumbs.push(crumb)
            }
            Err(_) => {
                crumb.push(PatternMatchCrumb::Err);
                crumbs.push(crumb)
            }
            Tok(_) => {
                crumb.push(PatternMatchCrumb::Tok);
                crumbs.push(crumb)
            }
            Blank(_) => {
                crumb.push(PatternMatchCrumb::Blank);
                crumbs.push(crumb)
            }
            Var(_) => {
                crumb.push(PatternMatchCrumb::Var);
                crumbs.push(crumb)
            }
            Cons(_) => {
                crumb.push(PatternMatchCrumb::Cons);
                crumbs.push(crumb)
            }
            Opr(_) => {
                crumb.push(PatternMatchCrumb::Opr);
                crumbs.push(crumb)
            }
            Annotation(_) => {
                crumb.push(PatternMatchCrumb::Annotation);
                crumbs.push(crumb)
            }
            Mod(_) => {
                crumb.push(PatternMatchCrumb::Mod);
                crumbs.push(crumb)
            }
            Num(_) => {
                crumb.push(PatternMatchCrumb::Num);
                crumbs.push(crumb)
            }
            Text(_) => {
                crumb.push(PatternMatchCrumb::Text);
                crumbs.push(crumb)
            }
            Block(_) => {
                crumb.push(PatternMatchCrumb::Block);
                crumbs.push(crumb)
            }
            Macro(_) => {
                crumb.push(PatternMatchCrumb::Macro);
                crumbs.push(crumb)
            }
            Invalid(_) => {
                crumb.push(PatternMatchCrumb::Invalid);
                crumbs.push(crumb)
            }
            FailedMatch(_) => (),
            Except(pat) => {
                crumb.push(PatternMatchCrumb::Except);
                patterns.push((crumb, &pat.elem))
            }
            Tag(pat) => {
                crumb.push(PatternMatchCrumb::Tag);
                patterns.push((crumb, &pat.elem))
            }
            Cls(pat) => {
                crumb.push(PatternMatchCrumb::Cls);
                patterns.push((crumb, &pat.elem))
            }
            Or(pat) => {
                crumb.push(PatternMatchCrumb::Or);
                patterns.push((crumb, &pat.elem));
            }
            Seq(pat) => {
                let mut crumb1 = crumb.clone();
                let mut crumb2 = crumb.clone();
                crumb1.push(PatternMatchCrumb::Seq { right: false });
                crumb2.push(PatternMatchCrumb::Seq { right: true });
                patterns.push((crumb2, &pat.elem.1));
                patterns.push((crumb1, &pat.elem.0));
            }
            Many(pat) =>
                for (index, pat) in pat.elem.iter().enumerate().rev() {
                    let mut new_crumb = crumb.clone();
                    new_crumb.push(PatternMatchCrumb::Many { index });
                    patterns.push((new_crumb, pat));
                },
        }
    }
    crumbs
}

impl Crumbable for crate::Match<Ast> {
    type Crumb = MatchCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            MatchCrumb::Pfx { val } => match &self.pfx {
                None => Err(NotPresent("prefix".into()).into()),
                Some(pat) => pattern_get(pat, val),
            },
            MatchCrumb::Segs { val, index } => match &val {
                SegmentMatchCrumb::Head => Ok(&self.segs.get_or_err(*index, "elem")?.head),
                SegmentMatchCrumb::Body { val } =>
                    pattern_get(&self.segs.get_or_err(*index, "elem")?.body, val),
            },
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self>
    where Self: Sized {
        let mut new_self = self.clone();

        match crumb {
            MatchCrumb::Pfx { val } => match new_self.pfx {
                None => return Err(NotPresent("prefix".into()).into()),
                Some(pat) => new_self.pfx = Some(pattern_set(&pat, val, new_ast)?),
            },

            MatchCrumb::Segs { index, val: SegmentMatchCrumb::Body { val } } => {
                let mut seg = new_self.segs.get_mut_or_err(*index, "segment")?;
                seg.body = pattern_set(&seg.body, val, new_ast)?
            }
            MatchCrumb::Segs { index, val: SegmentMatchCrumb::Head } => {
                let mut seg = new_self.segs.get_mut_or_err(*index, "segment")?;
                seg.head = new_ast
            }
        }
        Ok(new_self)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let mut crumbs = vec![];
        if let Some(pat) = &self.pfx {
            for crumb in pattern_subcrumbs(pat) {
                crumbs.push(MatchCrumb::Pfx { val: crumb });
            }
        }
        for (index, seg) in self.segs.iter().enumerate() {
            crumbs.push(MatchCrumb::Segs { index, val: SegmentMatchCrumb::Head });
            for crumb in pattern_subcrumbs(&seg.body) {
                crumbs.push(MatchCrumb::Segs { index, val: SegmentMatchCrumb::Body { val: crumb } })
            }
        }
        Box::new(crumbs.into_iter())
    }
}

impl Crumbable for crate::Ambiguous<Ast> {
    type Crumb = AmbiguousCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        let seg = self.segs.get_or_err(crumb.index, "seg")?;
        let ast = match crumb.field {
            AmbiguousSegmentCrumb::Head => &seg.head,
            AmbiguousSegmentCrumb::Body =>
                &seg.body.as_ref().ok_or_else(|| NotPresent("body".into()))?.wrapped,
        };
        Ok(ast)
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut new_self = self.clone();
        let mut seg = new_self.segs.get_mut_or_err(crumb.index, "seg")?;
        match crumb.field {
            AmbiguousSegmentCrumb::Head => seg.head = new_ast,
            AmbiguousSegmentCrumb::Body =>
                seg.body.as_mut().ok_or_else(|| NotPresent("body".into()))?.wrapped = new_ast,
        };
        Ok(new_self)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let head = |index| AmbiguousCrumb { index, field: AmbiguousSegmentCrumb::Head };
        let body = |index| AmbiguousCrumb { index, field: AmbiguousSegmentCrumb::Body };
        let crumbs = self.segs.iter().enumerate().flat_map(move |(index, seg)| {
            iter::once(head(index)).chain(seg.body.iter().map(move |_| body(index)))
        });
        Box::new(crumbs)
    }
}

impl Crumbable for crate::Import<Ast> {
    type Crumb = ImportCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            ImportCrumb::Path { index } =>
                self.path.get_or_err(*index, "path").map_err(|err| err.into()),
            ImportCrumb::Rename =>
                self.rename.as_ref().ok_or_else(|| NotPresent("rename".into()).into()),
            ImportCrumb::OnlyNames { index } => self
                .onlyNames
                .as_ref()
                .ok_or_else(|| failure::Error::from(NotPresent("onlyNames".into())))?
                .get_or_err(*index, "onlyNames")
                .map_err(|err| err.into()),
            ImportCrumb::HidingNames { index } => self
                .hidingNames
                .as_ref()
                .ok_or_else(|| failure::Error::from(NotPresent("hidingNames".into())))?
                .get_or_err(*index, "hidingNames")
                .map_err(|err| err.into()),
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut import = self.clone();
        match crumb {
            ImportCrumb::Path { index } => {
                let path = import.path.get_mut_or_err(*index, "path")?;
                *path = new_ast;
            }
            ImportCrumb::Rename => {
                import.rename = Some(new_ast);
            }
            ImportCrumb::OnlyNames { index } => {
                let only_names = import.onlyNames.clone();
                let mut only_names = only_names
                    .ok_or_else(|| failure::Error::from(NotPresent("onlyNames".into())))?;
                let elem = only_names.get_mut_or_err(*index, "onlyNames")?;
                *elem = new_ast;
                import.onlyNames = Some(only_names);
            }
            ImportCrumb::HidingNames { index } => {
                let hiding_names = import.onlyNames.clone();
                let mut hiding_names = hiding_names
                    .ok_or_else(|| failure::Error::from(NotPresent("hidingNames".into())))?;
                let elem = hiding_names.get_mut_or_err(*index, "hidingNames")?;
                *elem = new_ast;
                import.hidingNames = Some(hiding_names);
            }
        }
        Ok(import)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let path_iter = self.path.iter().enumerate().map(|(index, _)| ImportCrumb::Path { index });
        let rename_iter = self.rename.iter().map(|_| ImportCrumb::Rename);
        let only_iter = self
            .onlyNames
            .iter()
            .flat_map(|v| v.iter().enumerate().map(|(index, _)| ImportCrumb::OnlyNames { index }));
        let hiding_iter = self.hidingNames.iter().flat_map(|v| {
            v.iter().enumerate().map(|(index, _)| ImportCrumb::HidingNames { index })
        });
        Box::new(path_iter.chain(rename_iter).chain(only_iter).chain(hiding_iter))
    }
}

impl Crumbable for crate::Export<Ast> {
    type Crumb = ExportCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            ExportCrumb::Path { index } =>
                self.path.get_or_err(*index, "path").map_err(|err| err.into()),
            ExportCrumb::Rename =>
                self.rename.as_ref().ok_or_else(|| NotPresent("rename".into()).into()),
            ExportCrumb::OnlyNames { index } => self
                .onlyNames
                .as_ref()
                .ok_or_else(|| failure::Error::from(NotPresent("onlyNames".into())))?
                .get_or_err(*index, "onlyNames")
                .map_err(|err| err.into()),
            ExportCrumb::HidingNames { index } => self
                .hidingNames
                .as_ref()
                .ok_or_else(|| failure::Error::from(NotPresent("hidingNames".into())))?
                .get_or_err(*index, "hidingNames")
                .map_err(|err| err.into()),
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut export = self.clone();
        match crumb {
            ExportCrumb::Path { index } => {
                let path = export.path.get_mut_or_err(*index, "path")?;
                *path = new_ast;
            }
            ExportCrumb::Rename => {
                export.rename = Some(new_ast);
            }
            ExportCrumb::OnlyNames { index } => {
                let only_names = export.onlyNames.clone();
                let mut only_names = only_names
                    .ok_or_else(|| failure::Error::from(NotPresent("onlyNames".into())))?;
                let elem = only_names.get_mut_or_err(*index, "onlyNames")?;
                *elem = new_ast;
                export.onlyNames = Some(only_names);
            }
            ExportCrumb::HidingNames { index } => {
                let hiding_names = export.onlyNames.clone();
                let mut hiding_names = hiding_names
                    .ok_or_else(|| failure::Error::from(NotPresent("hidingNames".into())))?;
                let elem = hiding_names.get_mut_or_err(*index, "hidingNames")?;
                *elem = new_ast;
                export.hidingNames = Some(hiding_names);
            }
        }
        Ok(export)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let path_iter = self.path.iter().enumerate().map(|(index, _)| ExportCrumb::Path { index });
        let rename_iter = self.rename.iter().map(|_| ExportCrumb::Rename);
        let only_iter = self
            .onlyNames
            .iter()
            .flat_map(|v| v.iter().enumerate().map(|(index, _)| ExportCrumb::OnlyNames { index }));
        let hiding_iter = self.hidingNames.iter().flat_map(|v| {
            v.iter().enumerate().map(|(index, _)| ExportCrumb::HidingNames { index })
        });
        Box::new(path_iter.chain(rename_iter).chain(only_iter).chain(hiding_iter))
    }
}

impl Crumbable for crate::Mixfix<Ast> {
    type Crumb = MixfixCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            MixfixCrumb::Name { index } =>
                self.name.get_or_err(*index, "name").map_err(|err| err.into()),
            MixfixCrumb::Args { index } =>
                self.args.get_or_err(*index, "arg").map_err(|err| err.into()),
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut mixfix = self.clone();
        match crumb {
            MixfixCrumb::Name { index } => {
                *mixfix.name.get_mut_or_err(*index, "name")? = new_ast;
            }
            MixfixCrumb::Args { index } => {
                *mixfix.args.get_mut_or_err(*index, "arg")? = new_ast;
            }
        }
        Ok(mixfix)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let name_iter = self.name.iter().enumerate().map(|(index, _)| MixfixCrumb::Name { index });
        let args_iter = self.args.iter().enumerate().map(|(index, _)| MixfixCrumb::Args { index });
        Box::new(name_iter.chain(args_iter))
    }
}

impl Crumbable for crate::Group<Ast> {
    type Crumb = GroupCrumb;

    fn get(&self, _crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        Ok(self.body.as_ref().ok_or_else(|| NotPresent("body".into()))?)
    }

    fn set(&self, _crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut group = self.clone();
        group.body = Some(new_ast);
        Ok(group)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        Box::new(self.body.iter().map(|_| GroupCrumb))
    }
}

impl Crumbable for crate::Def<Ast> {
    type Crumb = DefCrumb;

    fn get(&self, crumb: &Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            DefCrumb::Name => Ok(&self.name),
            DefCrumb::Args { index } =>
                self.args.get_or_err(*index, "arg").map_err(|err| err.into()),
            DefCrumb::Body => self.body.as_ref().ok_or_else(|| NotPresent("body".into()).into()),
        }
    }

    fn set(&self, crumb: &Self::Crumb, new_ast: Ast) -> FallibleResult<Self> {
        let mut def = self.clone();
        match crumb {
            DefCrumb::Name => def.name = new_ast,
            DefCrumb::Args { index } => {
                let arg = def.args.get_mut_or_err(*index, "arg")?;
                *arg = new_ast;
            }
            DefCrumb::Body => def.body = Some(new_ast),
        }
        Ok(def)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let name_iter = std::iter::once(DefCrumb::Name);
        let args_iter = self.args.iter().enumerate().map(|(index, _)| DefCrumb::Args { index });
        let body_iter = self.body.iter().map(|_| DefCrumb::Body);
        Box::new(name_iter.chain(args_iter).chain(body_iter))
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

    /// Get the `Ast` node corresponging to `Self`.
    fn my_ast(&self) -> FallibleResult<&Ast> {
        self.get_traversing(&[])
    }

    /// Calculate the span of the descendent AST node described by given crumbs..
    fn span_of_descendent_at(&self, crumbs: &[Crumb]) -> FallibleResult<Span> {
        let mut position = Index::new(0);
        let mut ast = self.my_ast()?;
        for crumb in crumbs {
            let child = ast.get(crumb)?;
            let child_ix = ast.child_offset(child)?;
            position += Span::from_beginning_to(child_ix).size;
            ast = child;
        }
        Ok(Span::new(position, Size::new(ast.len())))
    }
}

impl TraversableAst for Ast {
    fn set_traversing(&self, crumbs: &[Crumb], new_ast: Ast) -> FallibleResult<Self> {
        let updated_ast = if let Some(first_crumb) = crumbs.first() {
            let child = self.get(first_crumb)?;
            let updated_child = child.set_traversing(&crumbs[1..], new_ast)?;
            self.set(first_crumb, updated_child)?
        } else {
            new_ast
        };
        Ok(updated_ast)
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
#[derive(Clone, Debug, Shrinkwrap, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Located<T> {
    /// Crumbs from containing parent.
    pub crumbs: Crumbs,
    /// The sub-item representation.
    #[shrinkwrap(main_field)]
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


    // === InvalidSuffix ===

    #[test]
    fn invalid_suffix_crumb() -> FallibleResult {
        let elem = Ast::var("foo");
        let ast = Ast::invalid_suffix(elem, "@");
        let to_crumb_enum = Crumb::InvalidSuffix;
        let baz = Ast::var("baz");

        assert_eq!(ast.repr(), "foo@");
        assert_eq!(get(to_crumb_enum, &ast, InvalidSuffixCrumb)?.repr(), "foo");
        assert_eq!(set(to_crumb_enum, &ast, InvalidSuffixCrumb, baz)?.repr(), "baz@");

        Ok(())
    }

    #[test]
    fn iterate_invalid_suffix() -> FallibleResult {
        let elem = Ast::var("foo");
        let ast = Ast::invalid_suffix(elem, "@");

        let mut iter = ast.iter_subcrumbs();

        assert_eq!(iter.next(), Some(Crumb::InvalidSuffix(InvalidSuffixCrumb)));
        assert_eq!(iter.next(), None);

        Ok(())
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
        assert_eq!(
            set(to_crumb_enum, &infix, InfixCrumb::Operator, times.clone())?.repr(),
            "foo * bar"
        );
        assert_eq!(
            set(to_crumb_enum, &infix, InfixCrumb::RightOperand, baz.clone())?.repr(),
            "foo + baz"
        );

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



    // ===========
    // == Text ===
    // ===========


    // === TextLineFmt ===

    #[test]
    fn text_line_fmt_crumb() {
        let expr = SegmentExpr { value: Some(Ast::var("foo")) };
        let text = vec![SegmentFmt::SegmentExpr(expr)];
        let ast = Ast::text_line_fmt(text);
        let to_crumb_enum = Crumb::TextLineFmt;
        let bar = Ast::var("bar");
        let crumb = TextLineFmtCrumb { segment_index: 0 };

        assert_eq!(ast.repr(), "'`foo`'");
        assert_eq!(get(to_crumb_enum, &ast, crumb).unwrap().repr(), "foo");
        assert_eq!(set(to_crumb_enum, &ast, crumb, bar).unwrap().repr(), "'`bar`'");
    }

    #[test]
    fn iterate_text_line_fmt() {
        let expr1 = SegmentExpr { value: Some(Ast::var("foo")) };
        let expr2 = SegmentPlain { value: "qux".into() };
        let expr3 = SegmentExpr { value: Some(Ast::var("bar")) };
        let text = vec![
            SegmentFmt::SegmentExpr(expr1),
            SegmentFmt::SegmentPlain(expr2),
            SegmentFmt::SegmentExpr(expr3),
        ];
        let ast = Ast::text_line_fmt(text);

        let (segment1, segment2) = ast.iter_subcrumbs().expect_tuple();

        assert_eq!(segment1, Crumb::TextLineFmt(TextLineFmtCrumb { segment_index: 0 }));
        assert_eq!(segment2, Crumb::TextLineFmt(TextLineFmtCrumb { segment_index: 2 }));
    }

    // === TextBlockFmt ===

    #[test]
    fn text_block_fmt_crumb() {
        let empty_lines = default();
        let expr = SegmentExpr { value: Some(Ast::var("foo")) };
        let text = vec![SegmentFmt::SegmentExpr(expr)];
        let line1 = TextBlockLine { empty_lines, text };

        let empty_lines = default();
        let expr = SegmentExpr { value: Some(Ast::var("bar")) };
        let text = vec![SegmentFmt::SegmentExpr(expr)];
        let line2 = TextBlockLine { empty_lines, text };

        let lines = vec![line1, line2];
        let ast = Ast::text_block_fmt(lines, 0);
        let qux = Ast::var("qux");
        let baz = Ast::var("baz");

        let to_crumb_enum = Crumb::TextBlockFmt;
        assert_eq!(ast.repr(), "'''\n`foo`\n`bar`");

        let crumb1 = TextBlockFmtCrumb { text_line_index: 0, segment_index: 0 };
        let crumb2 = TextBlockFmtCrumb { text_line_index: 1, segment_index: 0 };

        assert_eq!(get(to_crumb_enum, &ast, crumb1).unwrap().repr(), "foo");
        assert_eq!(get(to_crumb_enum, &ast, crumb2).unwrap().repr(), "bar");

        assert_eq!(set(to_crumb_enum, &ast, crumb1, qux).unwrap().repr(), "'''\n`qux`\n`bar`");
        assert_eq!(set(to_crumb_enum, &ast, crumb2, baz).unwrap().repr(), "'''\n`foo`\n`baz`");
    }

    #[test]
    fn iterate_text_block_fmt() {
        let empty_lines = default();
        let expr = SegmentExpr { value: Some(Ast::var("foo")) };
        let text = vec![SegmentFmt::SegmentExpr(expr)];
        let line1 = TextBlockLine { empty_lines, text };

        let empty_lines = default();
        let expr = SegmentPlain { value: "qux".into() };
        let text = vec![SegmentFmt::SegmentPlain(expr)];
        let line2 = TextBlockLine { empty_lines, text };

        let empty_lines = default();
        let expr1 = SegmentPlain { value: "qux".into() };
        let expr2 = SegmentExpr { value: Some(Ast::var("bar")) };
        let text = vec![SegmentFmt::SegmentPlain(expr1), SegmentFmt::SegmentExpr(expr2)];
        let line3 = TextBlockLine { empty_lines, text };

        let lines = vec![line1, line2, line3];
        let ast = Ast::text_block_fmt(lines, 0);

        let crumb1 = TextBlockFmtCrumb { text_line_index: 0, segment_index: 0 };
        let crumb2 = TextBlockFmtCrumb { text_line_index: 2, segment_index: 1 };

        let (line1, line2) = ast.iter_subcrumbs().expect_tuple();

        assert_eq!(line1, Crumb::TextBlockFmt(crumb1));
        assert_eq!(line2, Crumb::TextBlockFmt(crumb2));
    }


    // == TextUnclosed ===

    #[test]
    fn text_unclosed_crumb() {
        let expr = SegmentExpr { value: Some(Ast::var("foo")) };
        let text = vec![SegmentFmt::SegmentExpr(expr)];
        let text_line = TextLineFmt { text };
        let line = TextLine::TextLineFmt(text_line);
        let ast = Ast::text_unclosed(line);
        let to_crumb_enum = Crumb::TextUnclosed;
        let bar = Ast::var("bar");
        let text_line_crumb = TextLineFmtCrumb { segment_index: 0 };
        let crumb = TextUnclosedCrumb { text_line_crumb };

        assert_eq!(ast.repr(), "'`foo`");
        assert_eq!(get(to_crumb_enum, &ast, crumb).unwrap().repr(), "foo");
        assert_eq!(set(to_crumb_enum, &ast, crumb, bar).unwrap().repr(), "'`bar`");
    }

    #[test]
    fn iterate_text_unclosed() {
        let expr1 = SegmentExpr { value: Some(Ast::var("foo")) };
        let expr2 = SegmentExpr { value: Some(Ast::var("bar")) };
        let text = vec![SegmentFmt::SegmentExpr(expr1), SegmentFmt::SegmentExpr(expr2)];
        let text_line = TextLineFmt { text };
        let line = TextLine::TextLineFmt(text_line);
        let ast = Ast::text_unclosed(line);
        let text_line_crumb = TextLineFmtCrumb { segment_index: 0 };
        let crumb1 = TextUnclosedCrumb { text_line_crumb };
        let text_line_crumb = TextLineFmtCrumb { segment_index: 1 };
        let crumb2 = TextUnclosedCrumb { text_line_crumb };

        let (segment1, segment2) = ast.iter_subcrumbs().expect_tuple();
        assert_eq!(segment1, Crumb::TextUnclosed(crumb1));
        assert_eq!(segment2, Crumb::TextUnclosed(crumb2));
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

        assert_eq!(set(PrefixCrumb::Func, foo.clone())?.repr(), "foo arg");
        assert_eq!(set(PrefixCrumb::Arg, x.clone())?.repr(), "func x");

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

        assert_eq!(set(SectionLeftCrumb::Arg, arg.clone())?.repr(), "arg bar");
        assert_eq!(set(SectionLeftCrumb::Opr, opr.clone())?.repr(), "foo opr");

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

        assert_eq!(set(SectionRightCrumb::Opr, opr.clone())?.repr(), "opr bar");
        assert_eq!(set(SectionRightCrumb::Arg, arg.clone())?.repr(), "foo arg");

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
        assert_eq!(set(SectionSidesCrumb, opr.clone())?.repr(), "opr");

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


    // === Match ===

    fn match_() -> Match<Ast> {
        let var = Ast::var("");
        let pat = Rc::new(MacroPatternRaw::Nothing(MacroPatternRawNothing {}));
        let tok = Rc::new(MacroPatternMatchRaw::Tok(MacroPatternMatchRawTok {
            pat:  MacroPatternRawTok { spaced: None, ast: var.clone() },
            elem: Shifted { off: 0, wrapped: var.clone() },
        }));
        let body = Rc::new(MacroPatternMatchRaw::Seq(MacroPatternMatchRawSeq {
            pat:  MacroPatternRawSeq { pat1: pat.clone(), pat2: pat.clone() },
            elem: (tok.clone(), tok.clone()),
        }));
        let segs = ShiftedVec1 {
            head: MacroMatchSegment { head: var.clone(), body: body.clone() },
            tail: vec![],
        };
        Match { pfx: Some(body), segs, resolved: Some(var.clone()) }
    }

    #[test]
    fn iterate_match() {
        let crumb1 = vec![PatternMatchCrumb::Seq { right: false }, PatternMatchCrumb::Tok];
        let crumb2 = vec![PatternMatchCrumb::Seq { right: true }, PatternMatchCrumb::Tok];
        let (c1, c2, c3, c4, c5) = match_().iter_subcrumbs().expect_tuple();
        assert_eq!(c1, MatchCrumb::Pfx { val: crumb1.clone() });
        assert_eq!(c2, MatchCrumb::Pfx { val: crumb2.clone() });
        assert_eq!(c3, MatchCrumb::Segs { val: SegmentMatchCrumb::Head, index: 0 });
        assert_eq!(c4, MatchCrumb::Segs {
            val:   SegmentMatchCrumb::Body { val: crumb1 },
            index: 0,
        });
        assert_eq!(c5, MatchCrumb::Segs {
            val:   SegmentMatchCrumb::Body { val: crumb2 },
            index: 0,
        });
    }

    #[test]
    fn mismatch_match() {
        let match_ = match_();
        let incorrect1 = match_.get(&MatchCrumb::Pfx { val: vec![] });
        let incorrect2 =
            match_.get(&MatchCrumb::Pfx { val: vec![PatternMatchCrumb::Seq { right: true }] });
        let incorrect3 = match_.get(&MatchCrumb::Pfx {
            val: vec![PatternMatchCrumb::Seq { right: false }, PatternMatchCrumb::Seq {
                right: true,
            }],
        });
        incorrect1.expect_err("Using empty  crumb on match should fail");
        incorrect2.expect_err("Using 1'seq' crumb on match should fail");
        incorrect3.expect_err("Using 2'seq' crumb on match should fail");
    }

    #[test]
    fn modify_match() {
        let crumb1 = vec![PatternMatchCrumb::Seq { right: false }, PatternMatchCrumb::Tok];
        let crumb2 = vec![PatternMatchCrumb::Seq { right: true }, PatternMatchCrumb::Tok];
        let crumb3 = MatchCrumb::Pfx { val: crumb1.clone() };
        let crumb4 = MatchCrumb::Pfx { val: crumb2.clone() };
        let crumb5 = MatchCrumb::Segs { val: SegmentMatchCrumb::Head, index: 0 };
        let crumb6 = MatchCrumb::Segs { val: SegmentMatchCrumb::Body { val: crumb1 }, index: 0 };
        let crumb7 = MatchCrumb::Segs { val: SegmentMatchCrumb::Body { val: crumb2 }, index: 0 };
        let match1 = match_();
        let ast = [match1.resolved.clone().unwrap(), Ast::var("X"), Ast::var("Y"), Ast::var("Z")];
        let match2 = match1.set(&crumb3, ast[1].clone()).unwrap();
        let match3 = match2.set(&crumb5, ast[2].clone()).unwrap();
        let match4 = match3.set(&crumb7, ast[3].clone()).unwrap();

        assert_eq!(match1.get(&crumb3).unwrap(), &ast[0]);
        assert_eq!(match1.get(&crumb4).unwrap(), &ast[0]);
        assert_eq!(match1.get(&crumb5).unwrap(), &ast[0]);
        assert_eq!(match1.get(&crumb6).unwrap(), &ast[0]);
        assert_eq!(match1.get(&crumb7).unwrap(), &ast[0]);

        assert_eq!(match4.get(&crumb3).unwrap(), &ast[1]);
        assert_eq!(match4.get(&crumb4).unwrap(), &ast[0]);
        assert_eq!(match4.get(&crumb5).unwrap(), &ast[2]);
        assert_eq!(match4.get(&crumb6).unwrap(), &ast[0]);
        assert_eq!(match4.get(&crumb7).unwrap(), &ast[3]);
    }

    #[test]
    fn ambiguous() {
        let ast = [Ast::var("A"), Ast::var("B"), Ast::var("C")];
        let body = Some(Shifted::new(0, ast[1].clone()));
        let seg1 = MacroAmbiguousSegment { head: ast[0].clone(), body };
        let seg2 = MacroAmbiguousSegment { head: ast[2].clone(), body: None };
        let segs = ShiftedVec1 { head: seg1, tail: vec![Shifted::new(0, seg2)] };
        let paths = Tree { value: None, branches: vec![] };
        let shape = Ambiguous { segs, paths };

        let (c1, c2, c3) = shape.iter_subcrumbs().expect_tuple();

        assert_eq!(c1, AmbiguousCrumb { index: 0, field: AmbiguousSegmentCrumb::Head });
        assert_eq!(c2, AmbiguousCrumb { index: 0, field: AmbiguousSegmentCrumb::Body });
        assert_eq!(c3, AmbiguousCrumb { index: 1, field: AmbiguousSegmentCrumb::Head });

        assert_eq!(shape.set(&c1, ast[1].clone()).unwrap().get(&c1).unwrap(), &ast[1]);
        assert_eq!(shape.set(&c2, ast[2].clone()).unwrap().get(&c2).unwrap(), &ast[2]);
        assert_eq!(shape.set(&c3, ast[1].clone()).unwrap().get(&c3).unwrap(), &ast[1]);

        assert_eq!(shape.get(&c1).unwrap(), &ast[0]);
        assert_eq!(shape.get(&c2).unwrap(), &ast[1]);
        assert_eq!(shape.get(&c3).unwrap(), &ast[2]);
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

        let two_span = ast.span_of_descendent_at(&crumbs_to_two).unwrap();
        assert_eq!(two_span, Span::from(4..5));
        assert_eq!(&expected_code[two_span], "2");
    }
}
