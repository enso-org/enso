//! Crumbs for AST. Crumb identifies children node location in AST node. The access should be
//! possible in a constant time.

use crate::prelude::*;

use crate::known;
use crate::HasTokens;
use crate::Shape;
use crate::TokenConsumer;

use utils::fail::FallibleResult;



// ==============
// === Errors ===
// ==============

trait IndexedAccess {
    type Item;

    fn get_or_err(&self, index:usize, name:impl Str) -> Result<&Self::Item, IndexOutOfBounds>;

    fn get_mut_or_err
    (&mut self, index:usize, name:impl Str) -> Result<&mut Self::Item, IndexOutOfBounds>;
}

impl<T> IndexedAccess for Vec<T> {
    type Item = T;

    fn get_or_err(&self, index:usize, name:impl Str) -> Result<&Self::Item, IndexOutOfBounds> {
        self.get(index).ok_or_else(|| IndexOutOfBounds(name.into()))
    }

    fn get_mut_or_err
    (&mut self, index:usize, name:impl Str) -> Result<&mut Self::Item, IndexOutOfBounds> {
        self.get_mut(index).ok_or_else(|| IndexOutOfBounds(name.into()))
    }
}

#[allow(missing_docs)]
#[fail(display = "The crumb refers to a {} which is not present.", _0)]
#[derive(Debug,Fail,Clone)]
pub struct NotPresent(String);

#[allow(missing_docs)]
#[fail(display = "The crumb refers to {} by index that is out of bounds.", _0)]
#[derive(Debug,Fail,Clone)]
pub struct IndexOutOfBounds(String);

#[allow(missing_docs)]
#[derive(Debug,Fail,Clone)]
#[fail(display = "The line designated by crumb {:?} does not contain any AST. Context AST was {}.",
crumb,repr)]
pub struct LineDoesNotContainAst {
    repr  : String,
    crumb : Crumb,
}

impl LineDoesNotContainAst {
    /// Creates a new instance of error about missing AST in the designated line.
    pub fn new(repr:impl HasRepr, crumb:impl Into<Crumb>) -> LineDoesNotContainAst {
        let repr = repr.repr();
        let crumb = crumb.into();
        LineDoesNotContainAst {repr,crumb}
    }
}

#[derive(Debug,Display,Fail,Clone,Copy)]
struct MismatchedCrumbType;



// =============
// === Crumb ===
// =============

// === Ast ===

/// Sequence of `Crumb`s describing traversal path through AST.
pub type Crumbs = Vec<Crumb>;

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
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub struct InvalidSuffixCrumb;


// === TextLineFmt ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub struct TextLineFmtCrumb {pub segment_index:usize}


// === TextBlockFmt ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub struct TextBlockFmtCrumb {
    pub text_line_index : usize,
    pub segment_index   : usize
}


// === TextUnclosed ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub struct TextUnclosedCrumb {
    pub text_line_crumb : TextLineFmtCrumb
}


// === Prefix ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub enum PrefixCrumb {
    Func,
    Arg
}


// === Infix ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub enum InfixCrumb {
    LeftOperand,
    Operator,
    RightOperand,
}


// === SectionLeft ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub enum SectionLeftCrumb {
    Arg,
    Opr
}


// === SectionRight ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub enum SectionRightCrumb {
    Opr,
    Arg
}


// === SectionSides ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub struct SectionSidesCrumb;


// === Module ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub struct ModuleCrumb {pub line_index:usize}


// === Block ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub enum BlockCrumb {
    /// The first non-empty line in block.
    HeadLine,
    /// Index in the sequence of "rest of" lines (not counting the HeadLine).
    TailLine {tail_index:usize},
}


// === Import ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub struct ImportCrumb {pub index:usize}


// === Mixfix ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub enum MixfixCrumb {
    Name {index:usize},
    Args {index:usize}
}


// === Group ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub struct GroupCrumb;


// === Def ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
pub enum DefCrumb {
    Name,
    Args {index:usize},
    Body
}


// === Conversion Traits ===

macro_rules! from_crumb {
    ($id:ident,$crumb_id:ident) => {
        impl From<$crumb_id> for Crumb {
            fn from(crumb:$crumb_id) -> Self {
                Crumb::$id(crumb)
            }
        }

        impl From<&$crumb_id> for Crumb {
            fn from(crumb:&$crumb_id) -> Self {
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
    }
}

macro_rules! impl_crumbs {
    ($(($id:ident,$crumb_id:ident)),*) => {
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
        #[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
        #[allow(missing_docs)]
        pub enum Crumb {
            $($id($crumb_id),)*
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


impl_crumbs!{
    (InvalidSuffix,InvalidSuffixCrumb),
    (TextLineFmt  ,TextLineFmtCrumb),
    (TextBlockFmt ,TextBlockFmtCrumb),
    (TextUnclosed ,TextUnclosedCrumb),
    (Prefix       ,PrefixCrumb),
    (Infix        ,InfixCrumb),
    (SectionLeft  ,SectionLeftCrumb),
    (SectionRight ,SectionRightCrumb),
    (SectionSides ,SectionSidesCrumb),
    (Module       ,ModuleCrumb),
    (Block        ,BlockCrumb),
    (Import       ,ImportCrumb),
    (Mixfix       ,MixfixCrumb),
    (Group        ,GroupCrumb),
    (Def          ,DefCrumb)
}



// =================
// === Crumbable ===
// =================

/// Interface for items that allow getting/setting stored Ast located by arbitrary `Crumb`.
pub trait Crumbable {
    /// Specific `Crumb` type used by `Self` to locate child Asts.
    type Crumb : Into<Crumb> + IntoIterator<Item=Crumb>;

    /// Retrieves `Ast` under the crumb.
    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast>;

    /// Sets `Ast` under the crumb, returns updated entity.
    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> where Self:Sized;

    /// Iterates all valid crumbs available for `self`.
    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a>;

    /// Iterates pairs (crumb,child_ast) for `self`.
    fn enumerate<'a>(&'a self) -> Box<dyn Iterator<Item = (Self::Crumb,&'a Ast)> + 'a> {
        let indices = self.iter_subcrumbs();
        let iter = indices.map(move |crumb| {
            // NOTE Safe if this module is correct - children crumbs are always accessible.
            let child = self.get(&crumb).unwrap();
            (crumb,child)
        });
        Box::new(iter)
    }
}

impl Crumbable for crate::InvalidSuffix<Ast> {
    type Crumb = InvalidSuffixCrumb;

    fn get(&self, _crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        Ok(&self.elem)
    }

    fn set(&self, _crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
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

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        let segment = self.text.get_or_err(crumb.segment_index, "text segment")?;
        if let crate::SegmentFmt::SegmentExpr(expr) = segment {
            expr.value.as_ref().ok_or_else(|| NotPresent("expression".into()).into())
        } else {
            Err(NotPresent("expression segment".into()).into())
        }
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut text = self.clone();
        let segment = text.text.get_mut_or_err(crumb.segment_index,"text segment")?;
        if let crate::SegmentFmt::SegmentExpr(expr) = segment {
            expr.value = Some(new_ast);
            Ok(text)
        } else {
            Err(NotPresent("expression segment".into()).into())
        }
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        Box::new(self.text.iter().enumerate().filter_map(|(segment_index,segment)| {
            if let crate::SegmentFmt::SegmentExpr(_) = segment {
                Some(TextLineFmtCrumb{segment_index})
            } else {
                None
            }
        }))
    }
}

impl Crumbable for crate::TextBlockFmt<Ast> {
    type Crumb = TextBlockFmtCrumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        let line = self.text.get_or_err(crumb.text_line_index,"line")?;
        let segment = line.text.get_or_err(crumb.segment_index,"segment")?;
        if let crate::SegmentFmt::SegmentExpr(expr) = segment {
            expr.value.as_ref().ok_or_else(|| {
                NotPresent("expression value".into()).into()
            })
        } else {
            Err(NotPresent("expression segment".into()).into())
        }
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut text = self.clone();
        let line    = text.text.get_mut_or_err(crumb.text_line_index,"line")?;
        let segment = line.text.get_mut_or_err(crumb.segment_index,"segment")?;
        if let crate::SegmentFmt::SegmentExpr(expr) = segment {
            expr.value = Some(new_ast);
            Ok(text)
        } else {
            Err(NotPresent("expression segment".into()).into())
        }
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        Box::new(self.text.iter().enumerate().flat_map(|(text_line_index,line)| {
            line.text.iter().enumerate().filter(|(_,segment)| {
                matches!(segment, crate::SegmentFmt::SegmentExpr(_))
            }).map(move |(segment_index,_)| {
                TextBlockFmtCrumb{text_line_index,segment_index}
            })
        }))
    }
}

impl Crumbable for crate::TextUnclosed<Ast> {
    type Crumb = TextUnclosedCrumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        if let crate::TextLine::TextLineFmt(text_line) = &self.line {
            text_line.get(&crumb.text_line_crumb)
        } else {
            Err(NotPresent("formatted text line".into()).into())
        }
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut text = self.clone();
        if let crate::TextLine::TextLineFmt(text_line) = text.line {
            let text_line_fmt = text_line.set(&crumb.text_line_crumb,new_ast)?;
            text.line         = crate::TextLine::TextLineFmt(text_line_fmt);
            Ok(text)
        } else {
            Err(NotPresent("formatted text line".into()).into())
        }
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        if let crate::TextLine::TextLineFmt(text_line) = &self.line {
            Box::new(text_line.iter_subcrumbs().map(|text_line_crumb| {
                TextUnclosedCrumb{text_line_crumb}
            }))
        } else {
            Box::new(std::iter::empty())
        }
    }
}

impl Crumbable for crate::Prefix<Ast> {
    type Crumb = PrefixCrumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        let ret = match crumb {
            PrefixCrumb::Func => &self.func,
            PrefixCrumb::Arg  => &self.arg
        };
        Ok(ret)
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        let target  = match crumb {
            PrefixCrumb::Func => &mut ret.func,
            PrefixCrumb::Arg  => &mut ret.arg
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

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        let ret = match crumb {
            InfixCrumb::LeftOperand  => &self.larg,
            InfixCrumb::Operator     => &self.opr ,
            InfixCrumb::RightOperand => &self.rarg,
        };
        Ok(ret)
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        let target  = match crumb {
            InfixCrumb::LeftOperand  => &mut ret.larg,
            InfixCrumb::Operator     => &mut ret.opr ,
            InfixCrumb::RightOperand => &mut ret.rarg,
        };
        *target = new_ast;
        Ok(ret)
    }

    fn iter_subcrumbs(&self) -> Box<dyn Iterator<Item = Self::Crumb>> {
        const CHILDREN: [InfixCrumb; 3] = [InfixCrumb::LeftOperand, InfixCrumb::Operator, InfixCrumb::RightOperand];
        Box::new(CHILDREN.iter().copied())
    }
}

impl Crumbable for crate::SectionLeft<Ast> {
    type Crumb = SectionLeftCrumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        let ret = match crumb {
            SectionLeftCrumb::Arg => &self.arg,
            SectionLeftCrumb::Opr => &self.opr
        };
        Ok(ret)
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        let target  = match crumb {
            SectionLeftCrumb::Arg => &mut ret.arg,
            SectionLeftCrumb::Opr => &mut ret.opr
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

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        let ret = match crumb {
            SectionRightCrumb::Arg => &self.arg,
            SectionRightCrumb::Opr => &self.opr
        };
        Ok(ret)
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut ret = self.clone();
        let target  = match crumb {
            SectionRightCrumb::Arg => &mut ret.arg,
            SectionRightCrumb::Opr => &mut ret.opr
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

    fn get(&self, _crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        Ok(&self.opr)
    }

    fn set(&self, _crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
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

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        let line = self.lines.get_or_err(crumb.line_index,"line")?;
        line.elem.as_ref().ok_or_else(|| LineDoesNotContainAst::new(self,crumb).into())
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut module = self.clone();
        let line = module.lines.get_mut_or_err(crumb.line_index,"line")?;
        line.elem.replace(new_ast);
        Ok(module)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let indices = non_empty_line_indices(self.lines.iter());
        let crumbs  = indices.map(|line_index| ModuleCrumb {line_index});
        Box::new(crumbs)
    }
}

impl Crumbable for crate::Block<Ast> {
    type Crumb = BlockCrumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            BlockCrumb::HeadLine => Ok(&self.first_line.elem),
            BlockCrumb::TailLine {tail_index} => {
                let line = self.lines.get_or_err(*tail_index,"line")?;
                line.elem.as_ref().ok_or_else(|| LineDoesNotContainAst::new(self,crumb).into())
            }
        }
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut block = self.clone();
        match crumb {
            BlockCrumb::HeadLine              => block.first_line.elem = new_ast,
            BlockCrumb::TailLine {tail_index} => {
                let line = block.lines.get_mut_or_err(*tail_index,"line")?;
                line.elem.replace(new_ast);
            }
        }
        Ok(block)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let first_line        = std::iter::once(BlockCrumb::HeadLine);
        let tail_line_indices = non_empty_line_indices(self.lines.iter());
        let tail_lines        = tail_line_indices.map(|tail_index| {
            BlockCrumb::TailLine {tail_index}
        });
        Box::new(first_line.chain(tail_lines))
    }
}

impl Crumbable for crate::Import<Ast> {
    type Crumb = ImportCrumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        self.path.get_or_err(crumb.index,"path").map_err(|err| err.into())
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut import = self.clone();
        let path = import.path.get_mut_or_err(crumb.index,"path")?;
        *path = new_ast;
        Ok(import)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let indices = self.path.iter().enumerate().map(|(indices,_)| indices);
        let crumbs  = indices.map(|path_index| ImportCrumb { index: path_index });
        Box::new(crumbs)
    }
}

impl Crumbable for crate::Mixfix<Ast> {
    type Crumb = MixfixCrumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            MixfixCrumb::Name {index} => {
                self.name.get_or_err(*index,"name").map_err(|err| err.into())
            },
            MixfixCrumb::Args {index} => {
                self.args.get_or_err(*index,"arg").map_err(|err| err.into())
            }
        }
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut mixfix = self.clone();
        match crumb {
            MixfixCrumb::Name {index} => {
                *mixfix.name.get_mut_or_err(*index,"name")? = new_ast;
            },
            MixfixCrumb::Args {index} => {
                *mixfix.args.get_mut_or_err(*index,"arg")? = new_ast;
            }
        }
        Ok(mixfix)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let name_iter = self.name.iter().enumerate().map(|(index,_)|{
            MixfixCrumb::Name{index}
        });
        let args_iter = self.args.iter().enumerate().map(|(index,_)| MixfixCrumb::Args{index});
        Box::new(name_iter.chain(args_iter))
    }
}

impl Crumbable for crate::Group<Ast> {
    type Crumb = GroupCrumb;

    fn get(&self, _crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        Ok(self.body.as_ref().ok_or_else(|| NotPresent("body".into()))?)
    }

    fn set(&self, _crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
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

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        match crumb {
            DefCrumb::Name         => Ok(&self.name),
            DefCrumb::Args {index} => self.args.get_or_err(*index,"arg").map_err(|err| err.into()),
            DefCrumb::Body         => self.body.as_ref().ok_or_else(|| {
                NotPresent("body".into()).into()
            })
        }
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let mut def = self.clone();
        match crumb {
            DefCrumb::Name         => def.name = new_ast,
            DefCrumb::Args {index} => {
                let arg = def.args.get_mut_or_err(*index,"arg")?;
                *arg = new_ast;
            },
            DefCrumb::Body         => def.body = Some(new_ast)
        }
        Ok(def)
    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        let name_iter = std::iter::once(DefCrumb::Name);
        let args_iter = self.args.iter().enumerate().map(|(index,_)| DefCrumb::Args{index});
        let body_iter = self.body.iter().map(|_| DefCrumb::Body);
        Box::new(name_iter.chain(args_iter).chain(body_iter))
    }
}

/// Just delegates the implementation to shape.
impl Crumbable for Ast {
    type Crumb = Crumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> {
        self.shape().get(crumb)
    }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let new_shape = self.shape().set(crumb,new_ast)?;
        Ok(self.with_shape(new_shape))

    }

    fn iter_subcrumbs<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Crumb> + 'a> {
        self.shape().iter_subcrumbs()
    }
}

/// Just delegates to Ast.
impl<T,E> Crumbable for known::KnownAst<T>
where for<'t> &'t Shape<Ast> : TryInto<&'t T, Error=E>,
      E                      : failure::Fail {
    type Crumb = Crumb;

    fn get(&self, crumb:&Self::Crumb) -> FallibleResult<&Ast> { self.ast().get(crumb) }

    fn set(&self, crumb:&Self::Crumb, new_ast:Ast) -> FallibleResult<Self> {
        let new_ast = self.ast().set(crumb,new_ast)?;
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
pub trait TraversableAst:Sized {
    /// Returns rewritten AST where child AST under location designated by `crumbs` is updated.
    ///
    /// Works recursively.
    fn set_traversing(&self, crumbs:&[Crumb], new_ast:Ast) -> FallibleResult<Self>;

    /// Recursively traverses AST to retrieve AST node located by given crumbs sequence.
    fn get_traversing<'a>(&'a self, crumbs:&[Crumb]) -> FallibleResult<&'a Ast>;
}

impl TraversableAst for Ast {
    fn set_traversing(&self, crumbs:&[Crumb], new_ast:Ast) -> FallibleResult<Self> {
        let updated_ast = if let Some(first_crumb) = crumbs.first() {
            let child = self.get(first_crumb)?;
            let updated_child = child.set_traversing(&crumbs[1..], new_ast)?;
            self.set(first_crumb,updated_child)?
        } else {
            new_ast
        };
        Ok(updated_ast)
    }

    fn get_traversing<'a>(&'a self, crumbs:&[Crumb]) -> FallibleResult<&'a Ast> {
        if let Some(first_crumb) = crumbs.first() {
            let child = self.get(first_crumb)?;
            child.get_traversing(&crumbs[1..])
        } else {
            Ok(self)
        }
    }
}

impl<T,E> TraversableAst for known::KnownAst<T>
where for<'t> &'t Shape<Ast> : TryInto<&'t T, Error=E>,
      E                      : failure::Fail {
    fn set_traversing(&self, crumbs:&[Crumb], new_ast:Ast) -> FallibleResult<Self> {
        let updated_ast = self.ast().set_traversing(crumbs,new_ast)?;
        Ok(Self::try_new(updated_ast)?)
    }

    fn get_traversing<'a>(&'a self, crumbs:&[Crumb]) -> FallibleResult<&'a Ast> {
        self.ast().get_traversing(crumbs)
    }
}



// ===============
// === Utility ===
// ===============

/// Iterates over indices of non-empty lines in a line sequence.
pub fn non_empty_line_indices<'a, T:'a>
(iter:impl Iterator<Item = &'a crate::BlockLine<Option<T>>> + 'a)
 -> impl Iterator<Item=usize> + 'a {
    iter.enumerate().filter_map(|(line_index,line)| {
        line.elem.as_ref().map(|_| line_index)
    })
}



// ===============
// === Located ===
// ===============

/// Item which location is identified by `Crumbs`.
#[derive(Clone,Debug,Shrinkwrap,PartialEq,Eq,Hash)]
pub struct Located<T> {
    /// Crumbs from containing parent.
    pub crumbs : Crumbs,
    /// The sub-item representation.
    #[shrinkwrap(main_field)]
    pub item   : T
}

impl<T> Located<T> {
    /// Creates a new located item.
    pub fn new<Cs>(crumbs:Cs, item:T) -> Located<T>
    where Cs : IntoIterator<Item:Into<Crumb>>, {
        let crumbs = crumbs.into_iter().map(|crumb| crumb.into()).collect();
        Located {crumbs,item}
    }

    /// Creates a new item in a root location (empty crumbs list).
    pub fn new_root(item:T) -> Located<T> {
        let crumbs = default();
        Located {crumbs,item}
    }

    /// Uses given function to map over the item.
    pub fn map<U>(self, f:impl FnOnce(T) -> U) -> Located<U> {
        Located::new(self.crumbs, f(self.item))
    }

    /// Takes crumbs relative to self and item that will be wrapped.
    pub fn descendant<Cs,U>(&self, crumbs:Cs, child:U) -> Located<U>
    where Cs : IntoIterator<Item:Into<Crumb>>,{
        let crumbs_so_far = self.crumbs.iter().copied();
        let crumbs_to_add = crumbs.into_iter().map(|crumb| crumb.into());
        let crumbs = crumbs_so_far.chain(crumbs_to_add);
        Located::new(crumbs, child)
    }

    /// Maps into child, concatenating this crumbs and child crumbs.
    pub fn into_descendant<U>(self, child:Located<U>) -> Located<U> {
        let Located {crumbs,item} = child;
        let mut ret = self.map(|_| item);
        ret.crumbs.extend(crumbs);
        ret
    }
}

impl<T:HasTokens> HasTokens for Located<T> {
    fn feed_to(&self, consumer:&mut impl TokenConsumer) {
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

    use crate::HasRepr;
    use crate::SegmentExpr;
    use crate::SegmentFmt;
    use crate::SegmentPlain;
    use crate::TextBlockLine;
    use crate::TextLine;
    use crate::TextLineFmt;

    use utils::test::ExpectTuple;

    /// Gets item under given crumb and checks if its representation is as expected.
    fn expect_repr<C:Crumbable>(item:&C, crumb:&C::Crumb, expected_repr:impl Str) {
        assert_eq!(item.get(crumb).unwrap().repr(), expected_repr.as_ref());
    }

    #[test]
    fn module_crumb() {
        let lines = [
           Some(Ast::var("foo")),
           None,
           Some(Ast::var("bar"))
        ];

        let module = crate::Module::from_lines(&lines);


        // === Getting ===
        expect_repr(&module,&ModuleCrumb {line_index:0}, "foo");
        assert!(module.get(&ModuleCrumb {line_index:1}).is_err());
        expect_repr(&module,&ModuleCrumb {line_index:2}, "bar");
        assert!(module.get(&ModuleCrumb {line_index:3}).is_err());

        let module2 = module.set(&ModuleCrumb {line_index:0}, Ast::var("foo2")).unwrap();
        assert_eq!(module2.repr(), "foo2\n\nbar");
        let module3 = module.set(&ModuleCrumb {line_index:1}, Ast::var("foo2")).unwrap();
        assert_eq!(module3.repr(), "foo\nfoo2\nbar");
        let module4 = module.set(&ModuleCrumb {line_index:3}, Ast::var("foo2"));
        assert!(module4.is_err());
    }

    #[test]
    fn block_crumb() {
        let first_line = Ast::var("first_line");
        let tail_lines = [Some(Ast::var("tail0")), None, Some(Ast::var("tail2"))];
        let block      = crate::Block::from_lines(&first_line,&tail_lines);

        expect_repr(&block, &BlockCrumb::HeadLine, "first_line");
        expect_repr(&block, &BlockCrumb::TailLine {tail_index:0}, "tail0");
        assert!(block.get(&BlockCrumb::TailLine {tail_index:1}).is_err());
        expect_repr(&block, &BlockCrumb::TailLine {tail_index:2}, "tail2");
        assert!(block.get(&BlockCrumb::TailLine {tail_index:3}).is_err());

        let block2 = block.set(&BlockCrumb::HeadLine, Ast::var("first_line2")).unwrap();
        assert_eq!(block2.repr(), "first_line2\ntail0\n\ntail2");
        let block3 = block.set(&BlockCrumb::TailLine {tail_index:1}, Ast::var("tail1")).unwrap();
        assert_eq!(block3.repr(), "first_line\ntail0\ntail1\ntail2");
        let block4 = block.set(&BlockCrumb::TailLine {tail_index:2}, Ast::var("tail22")).unwrap();
        assert_eq!(block4.repr(), "first_line\ntail0\n\ntail22");
    }

    fn get<T,F:FnOnce(T) -> Crumb>(f:F, ast:&Ast, crumb:T) -> FallibleResult<&Ast> {
        let crumb = f(crumb);
        ast.get(&crumb)
    }

    fn set<T,F:FnOnce(T) -> Crumb>(f:F, ast:&Ast, crumb:T,internal_ast:Ast) -> FallibleResult<Ast> {
        let crumb = f(crumb);
        ast.set(&crumb, internal_ast)
    }


    // === InvalidSuffix ===

    #[test]
    fn invalid_suffix_crumb() -> FallibleResult<()> {
        let elem          = Ast::var("foo");
        let ast           = Ast::invalid_suffix(elem,"@");
        let to_crumb_enum = Crumb::InvalidSuffix;
        let baz           = Ast::var("baz");

        assert_eq!(ast.repr(), "foo@");
        assert_eq!(get(to_crumb_enum,&ast,InvalidSuffixCrumb)?.repr(), "foo");
        assert_eq!(set(to_crumb_enum,&ast,InvalidSuffixCrumb,baz)?.repr(), "baz@");

        Ok(())
    }

    #[test]
    fn iterate_invalid_suffix() -> FallibleResult<()> {
        let elem   = Ast::var("foo");
        let ast    = Ast::invalid_suffix(elem,"@");

        let mut iter = ast.iter_subcrumbs();

        assert_eq!(iter.next(), Some(Crumb::InvalidSuffix(InvalidSuffixCrumb)));
        assert_eq!(iter.next(), None);

        Ok(())
    }

    // === Infix ===

    #[test]
    fn infix_crumb() -> FallibleResult<()> {
        let infix         = Ast::infix_var("foo","+","bar");
        let to_crumb_enum = Crumb::Infix;
        let baz           = Ast::var("baz");
        let times         = Ast::opr("*");

        assert_eq!(infix.repr(), "foo + bar");

        assert_eq!(get(to_crumb_enum,&infix,InfixCrumb::LeftOperand)?.repr(),  "foo");
        assert_eq!(get(to_crumb_enum,&infix,InfixCrumb::Operator)?.repr(),     "+");
        assert_eq!(get(to_crumb_enum,&infix,InfixCrumb::RightOperand)?.repr(), "bar");

        assert_eq!(set(to_crumb_enum,&infix,InfixCrumb::LeftOperand, baz.clone())?.repr(), "baz + bar");
        assert_eq!(set(to_crumb_enum,&infix,InfixCrumb::Operator, times.clone())?.repr(), "foo * bar");
        assert_eq!(set(to_crumb_enum,&infix,InfixCrumb::RightOperand, baz.clone())?.repr(), "foo + baz");

        Ok(())
    }

    #[test]
    fn iterate_infix() {
        let sum = crate::Infix::from_vars("foo", "+", "bar");
        let (larg,opr,rarg) = sum.iter_subcrumbs().expect_tuple();
        assert_eq!(larg, InfixCrumb::LeftOperand);
        assert_eq!(opr,  InfixCrumb::Operator);
        assert_eq!(rarg, InfixCrumb::RightOperand);
    }

    #[test]
    fn nested_infix() -> FallibleResult<()> {
        use InfixCrumb::*;

        let sum   = Ast::infix_var("foo", "+", "bar");
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

        assert_eq!(set(&[RightOperand,LeftOperand], Ast::var("baz"))?.repr(), "main = baz + bar");
        assert_eq!(set(&[LeftOperand], Ast::var("baz"))?.repr(), "baz = foo + bar");


        assert_eq!(get(&[Operator])?.repr(), "=");
        assert_eq!(get(&[RightOperand])?.repr(), "foo + bar");
        assert_eq!(get(&[RightOperand,LeftOperand])?.repr(), "foo");
        assert_eq!(get(&[RightOperand,RightOperand])?.repr(), "bar");
        Ok(())
    }



    // ===========
    // == Text ===
    // ===========


    // === TextLineFmt ===

    #[test]
    fn text_line_fmt_crumb() {
        let expr          = SegmentExpr { value : Some(Ast::var("foo")) };
        let text          = vec![SegmentFmt::SegmentExpr(expr)];
        let ast           = Ast::text_line_fmt(text);
        let to_crumb_enum = Crumb::TextLineFmt;
        let bar           = Ast::var("bar");
        let crumb         = TextLineFmtCrumb{segment_index:0};

        assert_eq!(ast.repr(), "'`foo`'");
        assert_eq!(get(to_crumb_enum,&ast,crumb).unwrap().repr(),  "foo");
        assert_eq!(set(to_crumb_enum,&ast,crumb,bar).unwrap().repr(), "'`bar`'");
    }

    #[test]
    fn iterate_text_line_fmt() {
        let expr1 = SegmentExpr { value : Some(Ast::var("foo")) };
        let expr2 = SegmentPlain { value : "qux".into() };
        let expr3 = SegmentExpr { value : Some(Ast::var("bar")) };
        let text  = vec![
            SegmentFmt::SegmentExpr(expr1),
            SegmentFmt::SegmentPlain(expr2),
            SegmentFmt::SegmentExpr(expr3)
        ];
        let ast   = Ast::text_line_fmt(text);

        let (segment1,segment2) = ast.iter_subcrumbs().expect_tuple();

        assert_eq!(segment1, Crumb::TextLineFmt(TextLineFmtCrumb{segment_index:0}));
        assert_eq!(segment2, Crumb::TextLineFmt(TextLineFmtCrumb{segment_index:2}));
    }

    // === TextBlockFmt ===

    #[test]
    fn text_block_fmt_crumb() {
        let empty_lines = default();
        let expr        = SegmentExpr { value : Some(Ast::var("foo")) };
        let text        = vec![SegmentFmt::SegmentExpr(expr)];
        let line1       = TextBlockLine{empty_lines,text};

        let empty_lines = default();
        let expr        = SegmentExpr { value : Some(Ast::var("bar")) };
        let text        = vec![SegmentFmt::SegmentExpr(expr)];
        let line2       = TextBlockLine{empty_lines,text};

        let lines       = vec![line1,line2];
        let ast         = Ast::text_block_fmt(lines,0);
        let qux         = Ast::var("qux");
        let baz         = Ast::var("baz");

        let to_crumb_enum = Crumb::TextBlockFmt;
        assert_eq!(ast.repr(), "'''\n`foo`\n`bar`");

        let crumb1 = TextBlockFmtCrumb {text_line_index:0, segment_index:0};
        let crumb2 = TextBlockFmtCrumb {text_line_index:1, segment_index:0};

        assert_eq!(get(to_crumb_enum,&ast,crumb1).unwrap().repr(), "foo");
        assert_eq!(get(to_crumb_enum,&ast,crumb2).unwrap().repr(), "bar");

        assert_eq!(set(to_crumb_enum,&ast,crumb1,qux).unwrap().repr(),"'''\n`qux`\n`bar`");
        assert_eq!(set(to_crumb_enum,&ast,crumb2,baz).unwrap().repr(),"'''\n`foo`\n`baz`");
    }

    #[test]
    fn iterate_text_block_fmt() {
        let empty_lines = default();
        let expr        = SegmentExpr { value : Some(Ast::var("foo")) };
        let text        = vec![SegmentFmt::SegmentExpr(expr)];
        let line1       = TextBlockLine{empty_lines,text};

        let empty_lines = default();
        let expr        = SegmentPlain { value : "qux".into() };
        let text        = vec![SegmentFmt::SegmentPlain(expr)];
        let line2       = TextBlockLine{empty_lines,text};

        let empty_lines = default();
        let expr1       = SegmentPlain { value : "qux".into() };
        let expr2       = SegmentExpr { value : Some(Ast::var("bar")) };
        let text        = vec![SegmentFmt::SegmentPlain(expr1),SegmentFmt::SegmentExpr(expr2)];
        let line3       = TextBlockLine{empty_lines,text};

        let lines       = vec![line1,line2,line3];
        let ast         = Ast::text_block_fmt(lines,0);

        let crumb1 = TextBlockFmtCrumb {text_line_index:0, segment_index:0};
        let crumb2 = TextBlockFmtCrumb {text_line_index:2, segment_index:1};

        let (line1,line2) = ast.iter_subcrumbs().expect_tuple();

        assert_eq!(line1, Crumb::TextBlockFmt(crumb1));
        assert_eq!(line2, Crumb::TextBlockFmt(crumb2));
    }


    // == TextUnclosed ===

    #[test]
    fn text_unclosed_crumb() {
        let expr            = SegmentExpr { value : Some(Ast::var("foo")) };
        let text            = vec![SegmentFmt::SegmentExpr(expr)];
        let text_line       = TextLineFmt{text};
        let line            = TextLine::TextLineFmt(text_line);
        let ast             = Ast::text_unclosed(line);
        let to_crumb_enum   = Crumb::TextUnclosed;
        let bar             = Ast::var("bar");
        let text_line_crumb = TextLineFmtCrumb{segment_index:0};
        let crumb           = TextUnclosedCrumb{text_line_crumb};

        assert_eq!(ast.repr(), "'`foo`");
        assert_eq!(get(to_crumb_enum,&ast,crumb).unwrap().repr(),  "foo");
        assert_eq!(set(to_crumb_enum,&ast,crumb,bar).unwrap().repr(), "'`bar`");
    }

    #[test]
    fn iterate_text_unclosed() {
        let expr1           = SegmentExpr { value : Some(Ast::var("foo")) };
        let expr2           = SegmentExpr { value : Some(Ast::var("bar")) };
        let text            = vec![SegmentFmt::SegmentExpr(expr1),SegmentFmt::SegmentExpr(expr2)];
        let text_line       = TextLineFmt{text};
        let line            = TextLine::TextLineFmt(text_line);
        let ast             = Ast::text_unclosed(line);
        let text_line_crumb = TextLineFmtCrumb{segment_index:0};
        let crumb1          = TextUnclosedCrumb{text_line_crumb};
        let text_line_crumb = TextLineFmtCrumb{segment_index:1};
        let crumb2          = TextUnclosedCrumb{text_line_crumb};

        let (segment1,segment2) = ast.iter_subcrumbs().expect_tuple();
        assert_eq!(segment1,Crumb::TextUnclosed(crumb1));
        assert_eq!(segment2,Crumb::TextUnclosed(crumb2));
    }


    // === Prefix ===

    #[test]
    fn prefix_crumb() -> FallibleResult<()> {
        let prefix = Ast::prefix(Ast::var("func"), Ast::var("arg"));
        let get   = |prefix_crumb| {
            let crumb = Crumb::Prefix(prefix_crumb);
            prefix.get(&crumb)
        };
        let set   = |prefix_crumb, ast| {
            let crumb = Crumb::Prefix(prefix_crumb);
            prefix.set(&crumb,ast)
        };
        let foo = Ast::var("foo");
        let x   = Ast::var("x");

        assert_eq!(prefix.repr(), "func arg");

        assert_eq!(get(PrefixCrumb::Func)?.repr(), "func");
        assert_eq!(get(PrefixCrumb::Arg)?.repr(),  "arg");

        assert_eq!(set(PrefixCrumb::Func, foo.clone())?.repr(), "foo arg");
        assert_eq!(set(PrefixCrumb::Arg,  x.clone())?.repr(), "func x");

        Ok(())
    }

    #[test]
    fn iterate_prefix() -> FallibleResult<()> {
        let prefix = Ast::prefix(Ast::var("func"), Ast::var("arg"));

        let (func,arg) = prefix.iter_subcrumbs().expect_tuple();

        assert_eq!(func, Crumb::Prefix(PrefixCrumb::Func));
        assert_eq!(arg, Crumb::Prefix(PrefixCrumb::Arg));

        Ok(())
    }


    // === SectionLeft ===

    #[test]
    fn section_left_crumb() -> FallibleResult<()> {
        let app = Ast::section_left(Ast::var("foo"), "bar");
        let get   = |app_crumb| {
            let crumb = Crumb::SectionLeft(app_crumb);
            app.get(&crumb)
        };
        let set   = |app_crumb, ast| {
            let crumb = Crumb::SectionLeft(app_crumb);
            app.set(&crumb,ast)
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
    fn iterate_section_left() -> FallibleResult<()> {
        let app = Ast::section_left(Ast::var("foo"), "bar");

        let (arg,opr) = app.iter_subcrumbs().expect_tuple();
        assert_eq!(arg, Crumb::SectionLeft(SectionLeftCrumb::Arg));
        assert_eq!(opr, Crumb::SectionLeft(SectionLeftCrumb::Opr));

        Ok(())
    }


    // === SectionRight ===

    #[test]
    fn section_right_crumb() -> FallibleResult<()> {
        let app = Ast::section_right("foo", Ast::var("bar"));
        let get   = |app_crumb| {
            let crumb = Crumb::SectionRight(app_crumb);
            app.get(&crumb)
        };
        let set   = |app_crumb, ast| {
            let crumb = Crumb::SectionRight(app_crumb);
            app.set(&crumb,ast)
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
    fn iterate_section_right() -> FallibleResult<()> {
        let app = Ast::section_right("foo", Ast::var("bar"));

        let (opr,arg) = app.iter_subcrumbs().expect_tuple();
        assert_eq!(arg, Crumb::SectionRight(SectionRightCrumb::Arg));
        assert_eq!(opr, Crumb::SectionRight(SectionRightCrumb::Opr));

        Ok(())
    }


    // === SectionSides ===

    #[test]
    fn section_sides_crumb() -> FallibleResult<()> {
        let app = Ast::section_sides("foo");
        let get   = |app_crumb| {
            let crumb = Crumb::SectionSides(app_crumb);
            app.get(&crumb)
        };
        let set   = |app_crumb, ast| {
            let crumb = Crumb::SectionSides(app_crumb);
            app.set(&crumb,ast)
        };
        let opr = Ast::var("opr");

        assert_eq!(app.repr(), "foo");

        assert_eq!(get(SectionSidesCrumb)?.repr(), "foo");
        assert_eq!(set(SectionSidesCrumb, opr.clone())?.repr(), "opr");

        Ok(())
    }

    #[test]
    fn iterate_section_sides() -> FallibleResult<()> {
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
        let lines = [
            Some(var.clone_ref()),
            None,
            Some(var.clone_ref()),
        ];
        let module = crate::Module::from_lines(&lines);
        assert_eq!(module.repr(), "foo\n\nfoo");

        let (line0,line2) = module.iter_subcrumbs().expect_tuple();
        assert_eq!(line0.line_index,0);
        assert_eq!(line2.line_index,2);
    }


    // === Block ===

    #[test]
    fn iterate_block() {
        let first_line = crate::Ast::var("foo");
        let lines      = [
            Some(crate::Ast::var("bar")),
            None,
            Some(crate::Ast::var("baz")),
        ];
        let block               = crate::Block::from_lines(&first_line,&lines);
        let (line0,line1,line3) = block.iter_subcrumbs().expect_tuple();
        assert_eq!(line0, BlockCrumb::HeadLine);
        assert_eq!(line1, BlockCrumb::TailLine {tail_index:0});
        assert_eq!(line3, BlockCrumb::TailLine {tail_index:2});
    }

    #[test]
    fn mismatched_crumb() {
        let sum        = Ast::infix_var("foo", "+", "bar");
        let crumb      = Crumb::Module(ModuleCrumb {line_index:0});
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
        assert_eq!(crumb0,&Crumb::Infix(InfixCrumb::Operator));

        let item2 = item.clone().map(|item| item.len() );
        assert_eq!(item2.item,3);
        assert_eq!(item.crumbs,item2.crumbs);
    }
}
