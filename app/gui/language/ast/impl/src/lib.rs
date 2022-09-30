// === Features ===
#![feature(associated_type_bounds)]
#![feature(bool_to_option)]
#![feature(generators, generator_trait)]
#![feature(trivial_bounds)]
#![feature(type_alias_impl_trait)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]


// ==============
// === Export ===
// ==============

#[warn(missing_docs)]
pub mod assoc;
#[warn(missing_docs)]
pub mod crumbs;
pub mod id_map;
#[warn(missing_docs)]
pub mod identifier;
#[warn(missing_docs)]
pub mod internal;
#[warn(missing_docs)]
pub mod known;
#[warn(missing_docs)]
pub mod macros;
#[warn(missing_docs)]
pub mod opr;
#[warn(missing_docs)]
pub mod prefix;
#[warn(missing_docs)]
pub mod repr;
#[warn(missing_docs)]
pub mod test_utils;
#[warn(missing_docs)]
pub mod traits;



pub mod prelude {
    pub use enso_prelude::*;

    pub use crate::traits::*;
    pub use crate::Ast;
}

/// The module containing constants related to the language AST describes.
// TODO[ao] not all constants are related to AST itself. Consider creating another "language" crate.
pub mod constants {

    /// A name of language this IDE supports
    pub const LANGUAGE_NAME: &str = "Enso";

    /// A file extension of modules of language this IDE supports without leading dot.
    pub const LANGUAGE_FILE_EXTENSION: &str = "enso";

    /// The directory in the project that contains all the source files.
    pub const SOURCE_DIRECTORY: &str = "src";

    /// The name of the main module in each project. The main module is explicitly imported when the
    /// import statement has the project name only.
    pub const PROJECTS_MAIN_MODULE: &str = "Main";

    /// A module with language-specific constants.
    pub mod keywords {

        /// The "void" atom returned by function meant to not return any argument.
        pub const NOTHING: &str = "Nothing";
    }
}

use crate::prelude::*;

pub use crumbs::Crumb;
pub use crumbs::Crumbs;
pub use id_map::IdMap;

use ast_macros::*;
use enso_shapely::*;
use enso_text::traits::*;
use enso_text::unit::*;
use serde::de::Deserializer;
use serde::de::Visitor;
use serde::ser::SerializeStruct;
use serde::ser::Serializer;
use serde::Deserialize;
use serde::Serialize;
use uuid::Uuid;

/// A sequence of AST nodes, typically the "token soup".
pub type Stream<T> = Vec<T>;



// ==============
// === Errors ===
// ==============

/// Exception raised by macro-generated TryFrom methods that try to "downcast"
/// enum type to its variant subtype if different constructor was used.
#[derive(Display, Debug, Fail)]
pub struct WrongEnum {
    pub expected_con: String,
}

#[allow(missing_docs)]
#[derive(Clone, Fail, Debug)]
#[fail(display = "No such child found under given AST node.")]
pub struct NoSuchChild;



// ============
// === Tree ===
// ============

/// A tree structure where each node may store value of `V` and has arbitrary
/// number of children nodes, each marked with a single `K`.
///
/// It is used to describe ambiguous macro match.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Tree<K, V> {
    pub value:    Option<V>,
    pub branches: Vec<(K, Tree<K, V>)>,
}



// ===============
// === Shifted ===
// ===============

/// A value of type `T` annotated with offset value `off`.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, Shrinkwrap, Iterator)]
#[shrinkwrap(mutable)]
pub struct Shifted<T> {
    #[shrinkwrap(main_field)]
    pub wrapped: T,
    pub off:     usize,
}

/// A non-empty sequence of `T`s interspersed by offsets.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, Iterator)]
pub struct ShiftedVec1<T> {
    pub head: T,
    pub tail: Vec<Shifted<T>>,
}

impl<T> Shifted<T> {
    pub fn new(off: usize, wrapped: T) -> Self {
        Shifted { wrapped, off }
    }
}


// =============
// === Layer ===
// =============

// === Trait ===

/// Types that can wrap a value of given `T`.
///
/// Same API as `From`, however not reflexive.
pub trait Layer<T> {
    fn layered(t: T) -> Self;
}

impl<T> From<T> for Layered<T> {
    fn from(t: T) -> Self {
        Layered::layered(t)
    }
}


// === Layered ===

/// A trivial `Layer` type that is just a strongly typed wrapper over `T`.
#[derive(Debug)]
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Layered<T>(pub T);

impl<T> Layer<T> for Layered<T> {
    fn layered(t: T) -> Self {
        Layered(t)
    }
}



// ============
// === Unit ===
// ============

/// A unit type defined as an empty struct.
///
/// Because it is defined using {} syntax, serde_json will serialize it to
/// an empty object rather than null node. This is to workaround issue with
/// using units in `Option`, reported here:
/// https://github.com/serde-rs/serde/issues/1690
#[ast_node]
pub struct Unit {}



// ===========
// === AST ===
// ===========

/// The primary class for Enso Abstract Syntax Tree.
///
/// This implementation is paired with AST implementation for Scala. Any changes
/// to either of the implementation need to be applied to the other one as well.
///
/// Each AST node is annotated with span and an optional ID.
#[derive(CloneRef, Eq, PartialEq, Debug, Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Ast {
    pub wrapped: Rc<WithID<WithLength<Shape<Ast>>>>,
}

impl Clone for Ast {
    fn clone(&self) -> Self {
        Ast { wrapped: self.wrapped.clone() }
    }
}

/// `IntoIterator` for `&Ast` that just delegates to `&Shape`'s `IntoIterator`.
impl<'t> IntoIterator for &'t Ast {
    type Item = <&'t Shape<Ast> as IntoIterator>::Item;
    type IntoIter = <&'t Shape<Ast> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.shape().into_iter()
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr())
    }
}

impl From<Ast> for String {
    fn from(ast: Ast) -> Self {
        ast.to_string()
    }
}

impl From<&Ast> for String {
    fn from(ast: &Ast) -> Self {
        ast.to_string()
    }
}

impl Ast {
    pub fn shape(&self) -> &Shape<Ast> {
        self
    }

    /// Wraps given shape with ID into Ast with random ID if id=None.
    /// Length will ba automatically calculated based on Shape.
    /// This constructor shouldn't be used for AST that can't have ID because of scala AST design.
    /// For more info see `Ast::new_no_id`
    pub fn new<S: Into<Shape<Ast>>>(shape: S, id: Option<Id>) -> Ast {
        let shape = shape.into();
        let id = id.unwrap_or_else(Id::new_v4);
        let length = shape.char_count();
        Ast::from_ast_id_len(shape, Some(id), length)
    }

    /// Wraps given shape without ID into Ast.
    /// Length will ba automatically calculated based on Shape.
    /// Should be only used on nodes that can't have ID because of scala AST design.
    /// Example: Module, Section.opr, MacroMatchSegment.head.
    /// Tracking issue: https://github.com/enso-org/ide/issues/434
    pub fn new_no_id<S: Into<Shape<Ast>>>(shape: S) -> Ast {
        let shape = shape.into();
        let length = shape.char_count();
        Ast::from_ast_id_len(shape, None, length)
    }

    /// Just wraps shape, id and len into Ast node.
    fn from_ast_id_len(shape: Shape<Ast>, id: Option<Id>, char_count: Chars) -> Ast {
        let with_length = WithLength { wrapped: shape, length: char_count };
        let with_id = WithID { wrapped: with_length, id };
        Ast { wrapped: Rc::new(with_id) }
    }

    /// Iterates over all transitive child nodes (including self).
    pub fn iter_recursive(&self) -> impl Iterator<Item = &Ast> {
        internal::iterate_subtree(self)
    }

    /// Returns this AST node with ID set to given value.
    pub fn with_id(&self, id: Id) -> Ast {
        Ast::new(self.shape().clone(), Some(id))
    }

    /// Returns this AST node with a newly generated unique ID.
    pub fn with_new_id(&self) -> Ast {
        self.with_id(Id::new_v4())
    }

    /// Returns this AST node with shape set to given value.
    pub fn with_shape<S: Into<Shape<Ast>>>(&self, shape: S) -> Ast {
        Ast::new(shape.into(), self.id)
    }

    /// Find a node in the AST with id equal to the given argument.
    pub fn find_by_id(&self, id: Id) -> Option<&Ast> {
        self.iter_recursive().find(|ast| ast.id == Some(id))
    }

    /// Find a node in the AST with text representation equal to given string.
    ///
    /// If multiple nodes match, it is unspecified which one of them will be returned.
    pub fn find_by_repr(&self, repr: &str) -> Option<&Ast> {
        // TODO: [mwu]
        //   We could do much better with HasTokens and iterative matching.
        //   Still, no need at this point.
        self.iter_recursive().find(|ast| ast.repr() == repr)
    }

    /// Get the offset relative to self for a given child node.
    ///
    /// Returned index is the position of the first character of child's text representation within
    /// the text representation of this AST node.
    pub fn child_offset(&self, child: &Ast) -> FallibleResult<Bytes> {
        let searched_token = Token::Ast(child);
        let mut found_child = false;
        let mut position = 0.bytes();
        self.shape().feed_to(&mut |token: Token| {
            if searched_token == token {
                found_child = true
            } else if !found_child {
                position += token.len()
            }
        });
        if found_child {
            Ok(position)
        } else {
            Err(NoSuchChild.into())
        }
    }

    /// Get the span (relative to self) for a child node identified by given crumb.
    pub fn span_of_child_at(&self, crumb: &Crumb) -> FallibleResult<enso_text::Range<Bytes>> {
        let child = self.get(crumb)?;
        let offset = self.child_offset(child)?;
        Ok(enso_text::Range::new(offset, offset + child.len()))
    }
}

/// Fills `id` with `None` by default.
impl<T: Into<Shape<Ast>>> From<T> for Ast {
    fn from(t: T) -> Self {
        let id = None;
        Ast::new(t, id)
    }
}


// === Serialization & Deserialization === //

/// Literals used in `Ast` serialization and deserialization.
pub mod ast_schema {
    pub const STRUCT_NAME: &str = "Ast";
    pub const SHAPE: &str = "shape";
    pub const ID: &str = "id";
    pub const LENGTH: &str = "span"; // scala parser is still using `span`
    pub const FIELDS: [&str; 3] = [SHAPE, ID, LENGTH];
    pub const COUNT: usize = FIELDS.len();
}

impl Serialize for Ast {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        use ast_schema::*;
        let mut state = serializer.serialize_struct(STRUCT_NAME, COUNT)?;
        state.serialize_field(SHAPE, &self.shape())?;
        if self.id.is_some() {
            state.serialize_field(ID, &self.id)?;
        }
        state.serialize_field(LENGTH, &self.length.as_usize())?;
        state.end()
    }
}

/// Type to provide serde::de::Visitor to deserialize data into `Ast`.
struct AstDeserializationVisitor;

impl<'de> Visitor<'de> for AstDeserializationVisitor {
    type Value = Ast;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ast_schema::*;
        write!(formatter, "an object with `{}` and `{}` fields", SHAPE, LENGTH)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where A: serde::de::MapAccess<'de> {
        use ast_schema::*;

        let mut shape: Option<Shape<Ast>> = None;
        let mut id: Option<Option<Id>> = None;
        let mut len: Option<usize> = None;

        while let Some(key) = map.next_key()? {
            match key {
                SHAPE => shape = Some(map.next_value()?),
                ID => id = Some(map.next_value()?),
                LENGTH => len = Some(map.next_value()?),
                _ => {}
            }
        }

        let shape = shape.ok_or_else(|| serde::de::Error::missing_field(SHAPE))?;
        let id = id.unwrap_or(None); // allow missing `id` field
        let len = len.ok_or_else(|| serde::de::Error::missing_field(LENGTH))?;
        Ok(Ast::from_ast_id_len(shape, id, len.into()))
    }
}

impl<'de> Deserialize<'de> for Ast {
    fn deserialize<D>(deserializer: D) -> Result<Ast, D::Error>
    where D: Deserializer<'de> {
        use ast_schema::FIELDS;
        let visitor = AstDeserializationVisitor;
        deserializer.deserialize_struct("AstOf", &FIELDS, visitor)
    }
}



// =============
// === Shape ===
// =============

/// Defines shape of the subtree. Parametrized by the child node type `T`.
///
/// Shape describes names of children and spacing between them.
#[ast(flat)]
#[derive(HasTokens)]
pub enum Shape<T> {
    Unrecognized {
        str: String,
    },
    Unexpected {
        msg:    String,
        stream: Vec<Shifted<T>>,
    },
    InvalidQuote {
        quote: Builder,
    },
    InlineBlock {
        quote: Builder,
    },

    // === Identifiers ===
    Blank {},
    Var {
        name: String,
    },
    Cons {
        name: String,
    },
    Opr {
        name: String,
    },
    Annotation {
        name: String,
    },
    Mod {
        name: String,
    },
    InvalidSuffix {
        elem:   T,
        suffix: String,
    },

    // === Number ===
    Number {
        base: Option<String>,
        int:  String,
    },
    DanglingBase {
        base: String,
    },

    // === Text ===
    TextLineRaw {
        text: Vec<SegmentRaw>,
    },
    TextLineFmt {
        text: Vec<SegmentFmt<T>>,
    },
    TextBlockRaw {
        text:   Vec<TextBlockLine<SegmentRaw>>,
        spaces: usize,
        offset: usize,
    },
    TextBlockFmt {
        text:   Vec<TextBlockLine<SegmentFmt<T>>>,
        spaces: usize,
        offset: usize,
    },
    TextUnclosed {
        line: TextLine<T>,
    },

    // === Applications ===
    Prefix {
        func: T,
        off:  usize,
        arg:  T,
    },
    Infix {
        larg: T,
        loff: usize,
        opr:  T,
        roff: usize,
        rarg: T,
    },
    SectionLeft {
        arg: T,
        off: usize,
        opr: T,
    },
    SectionRight {
        opr: T,
        off: usize,
        arg: T,
    },
    SectionSides {
        opr: T,
    },

    // === Module ===
    /// Module represent the file's root block: sequence of possibly empty lines with no leading
    /// indentation.
    Module {
        lines: Vec<BlockLine<Option<T>>>,
    },
    /// Block is the sequence of equally indented lines. Lines may contain some child `T` or be
    /// empty. Block is used for all code blocks except for the root one, which uses `Module`.
    Block {
        /// Type of Block, depending on whether it is introduced by an operator.
        /// Note [mwu] Doesn't really do anything right now, likely to be removed.
        ty:          BlockType,
        /// Absolute's block indent, counting from the module's root.
        indent:      usize,
        /// Leading empty lines. Each line is represented by absolute count of spaces
        /// it contains, counting from the root.
        empty_lines: Vec<usize>,
        /// First line with non-empty item.
        first_line:  BlockLine<T>,
        /// Rest of lines, each of them optionally having contents.
        lines:       Vec<BlockLine<Option<T>>>,
        /// If false, the Block will start with a leading newline.
        is_orphan:   bool,
    },

    // === Macros ===
    Match {
        pfx:      Option<MacroPatternMatch<Shifted<T>>>,
        segs:     ShiftedVec1<MacroMatchSegment<T>>,
        resolved: Option<Ast>,
    },
    Ambiguous {
        segs:  ShiftedVec1<MacroAmbiguousSegment<T>>,
        paths: Tree<Ast, Unit>,
    },

    // === Spaceless AST ===
    Comment(Comment),
    Documented(Documented<T>),
    Import(Import<T>),
    Export(Export<T>),
    JavaImport(JavaImport<T>),
    Mixfix(Mixfix<T>),
    Group(Group<T>),
    SequenceLiteral(SequenceLiteral<T>),
    TypesetLiteral(TypesetLiteral<T>),
    Def(Def<T>),
    Foreign(Foreign),
    Modified(Modified<T>),
}

/// Macrot that calls its argument (possibly other macro
#[macro_export]
macro_rules! with_shape_variants {
    ($f:ident) => {
        $f! { [Unrecognized] [Unexpected Ast] [InvalidQuote] [InlineBlock]
          [Blank] [Var] [Cons] [Opr] [Annotation] [Mod] [InvalidSuffix Ast]
          [Number] [DanglingBase]
          [TextLineRaw] [TextLineFmt Ast] [TextBlockRaw] [TextBlockFmt Ast] [TextUnclosed Ast]
          [Prefix Ast] [Infix Ast] [SectionLeft Ast] [SectionRight Ast] [SectionSides Ast]
          [Module Ast] [Block Ast]
          [Match Ast] [Ambiguous Ast]
          // Note: Spaceless AST is intentionally omitted here.
        }
    };
}



// ===============
// === Builder ===
// ===============

#[ast(flat)]
#[derive(HasTokens)]
pub enum Builder {
    Empty,
    Letter { char: char },
    Space { span: usize },
    Text { str: String },
    Seq { first: Rc<Builder>, second: Rc<Builder> },
}



// ============
// === Text ===
// ============

// === Text Block Lines ===

#[ast]
pub struct TextBlockLine<T> {
    pub empty_lines: Vec<usize>,
    pub text:        Vec<T>,
}

#[ast(flat)]
#[derive(HasTokens)]
pub enum TextLine<T> {
    TextLineRaw(TextLineRaw),
    TextLineFmt(TextLineFmt<T>),
}


// === Text Segments ===
#[ast(flat)]
#[derive(HasTokens)]
pub enum SegmentRaw {
    SegmentPlain(SegmentPlain),
    SegmentRawEscape(SegmentRawEscape),
}

#[ast(flat)]
#[derive(HasTokens)]
pub enum SegmentFmt<T> {
    SegmentPlain(SegmentPlain),
    SegmentRawEscape(SegmentRawEscape),
    SegmentExpr(SegmentExpr<T>),
    SegmentEscape(SegmentEscape),
}

#[ast_node]
pub struct SegmentPlain {
    pub value: String,
}
#[ast_node]
pub struct SegmentRawEscape {
    pub code: RawEscape,
}
#[ast_node]
pub struct SegmentExpr<T> {
    pub value: Option<T>,
}
#[ast_node]
pub struct SegmentEscape {
    pub code: Escape,
}


// === Text Segment Escapes ===

#[ast(flat)]
#[derive(HasTokens)]
pub enum RawEscape {
    Unfinished {},
    Invalid { str: char },
    Slash {},
    Quote {},
    RawQuote {},
}

#[ast]
#[derive(HasTokens)]
pub enum Escape {
    Character { c: char },
    Control { name: String, code: u8 },
    Number { digits: String },
    Unicode16 { digits: String },
    Unicode21 { digits: String },
    Unicode32 { digits: String },
}



// =============
// === Block ===
// =============

#[ast_node]
pub enum BlockType {
    Continuous {},
    Discontinuous {},
}

/// Holder for line in `Block` or `Module`. Lines store value of `T` and trailing whitespace info.
#[ast]
pub struct BlockLine<T> {
    /// The AST stored in the line.
    pub elem: T,
    /// The trailing whitespace in the line after the `elem`.
    pub off:  usize,
}


// =============
// === Macro ===
// =============

#[ast]
pub struct MacroMatchSegment<T> {
    pub head: T,
    pub body: MacroPatternMatch<Shifted<T>>,
}

#[ast]
pub struct MacroAmbiguousSegment<T> {
    pub head: T,
    pub body: Option<Shifted<T>>,
}

pub type MacroPattern = Rc<MacroPatternRaw>;
#[ast]
pub enum MacroPatternRaw {
    // === Boundary Patterns ===
    Begin {},
    End {},

    // === Structural Patterns ===
    Nothing {},
    Seq { pat1: MacroPattern, pat2: MacroPattern },
    Or { pat1: MacroPattern, pat2: MacroPattern },
    Many { pat: MacroPattern },
    Except { not: MacroPattern, pat: MacroPattern },

    // === Meta Patterns ===
    Build { pat: MacroPattern },
    Err { msg: String, pat: MacroPattern },
    Tag { tag: String, pat: MacroPattern },
    Cls { cls: PatternClass, pat: MacroPattern },

    // === Token Patterns ===
    Tok { spaced: Spaced, ast: Ast },
    Blank { spaced: Spaced },
    Var { spaced: Spaced },
    Cons { spaced: Spaced },
    Opr { spaced: Spaced, max_prec: Option<usize> },
    Annotation { spaced: Spaced },
    Mod { spaced: Spaced },
    Num { spaced: Spaced },
    Text { spaced: Spaced },
    Block { spaced: Spaced },
    Macro { spaced: Spaced },
    Invalid { spaced: Spaced },
    FailedMatch { spaced: Spaced },
}

#[ast]
pub enum PatternClass {
    Normal,
    Pattern,
}
pub type Spaced = Option<bool>;

// Note: Switch Implementation
#[ast(flat)]
pub enum Switch<T> {
    Left { value: T },
    Right { value: T },
}

// Note: Switch Implementation
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Switch is not defined as Either<T,T> because an iterator generated for such
// type would only iterate over right element, while we require both.
//
// Switch however does not need to be #[ast], when derive(Iterator) supports
// enum with struct variants, this attribute should be possible to remove.

impl<T> Deref for Switch<T> {
    type Target = T;

    fn deref(&self) -> &T {
        match self {
            Switch::Left(elem) => &elem.value,
            Switch::Right(elem) => &elem.value,
        }
    }
}

impl<T> DerefMut for Switch<T> {
    fn deref_mut(&mut self) -> &mut T {
        match self {
            Switch::Left(elem) => &mut elem.value,
            Switch::Right(elem) => &mut elem.value,
        }
    }
}

pub type MacroPatternMatch<T> = Rc<MacroPatternMatchRaw<T>>;

#[ast]
#[derive(HasTokens)]
pub enum MacroPatternMatchRaw<T> {
    // === Boundary Matches ===
    Begin { pat: MacroPatternRawBegin },
    End { pat: MacroPatternRawEnd },

    // === Structural Matches ===
    Nothing { pat: MacroPatternRawNothing },
    Seq { pat: MacroPatternRawSeq, elem: (MacroPatternMatch<T>, MacroPatternMatch<T>) },
    Or { pat: MacroPatternRawOr, elem: Switch<MacroPatternMatch<T>> },
    Many { pat: MacroPatternRawMany, elem: Vec<MacroPatternMatch<T>> },
    Except { pat: MacroPatternRawExcept, elem: MacroPatternMatch<T> },

    // === Meta Matches ===
    Build { pat: MacroPatternRawBuild, elem: T },
    Err { pat: MacroPatternRawErr, elem: T },
    Tag { pat: MacroPatternRawTag, elem: MacroPatternMatch<T> },
    Cls { pat: MacroPatternRawCls, elem: MacroPatternMatch<T> },

    // === Token Matches ===
    Tok { pat: MacroPatternRawTok, elem: T },
    Blank { pat: MacroPatternRawBlank, elem: T },
    Var { pat: MacroPatternRawVar, elem: T },
    Cons { pat: MacroPatternRawCons, elem: T },
    Opr { pat: MacroPatternRawOpr, elem: T },
    Annotation { pat: MacroPatternRawAnnotation, elem: T },
    Mod { pat: MacroPatternRawMod, elem: T },
    Num { pat: MacroPatternRawNum, elem: T },
    Text { pat: MacroPatternRawText, elem: T },
    Block { pat: MacroPatternRawBlock, elem: T },
    Macro { pat: MacroPatternRawMacro, elem: T },
    Invalid { pat: MacroPatternRawInvalid, elem: T },
    FailedMatch { pat: MacroPatternRawFailedMatch },
}

// =============================================================================
// === Spaceless AST ===========================================================
// =============================================================================

#[ast]
pub struct Comment {
    pub lines: Vec<String>,
}

#[ast]
pub struct Documented<T> {
    pub doc: T,
    pub emp: i32,
    pub ast: T,
}

#[allow(non_snake_case)]
#[ast]
pub struct Import<T> {
    pub path:        Vec<T>,
    pub rename:      Option<T>,
    pub isAll:       bool,
    pub onlyNames:   Option<Vec<T>>,
    pub hidingNames: Option<Vec<T>>,
}

#[allow(non_snake_case)]
#[ast]
pub struct Export<T> {
    pub path:        Vec<T>,
    pub rename:      Option<T>,
    pub isAll:       bool,
    pub onlyNames:   Option<Vec<T>>,
    pub hidingNames: Option<Vec<T>>,
}

#[ast]
pub struct JavaImport<T> {
    pub path:   Vec<T>,
    pub rename: Option<T>,
}

#[ast]
pub struct Mixfix<T> {
    pub name: Vec<T>,
    pub args: Vec<T>,
}

#[ast]
pub struct Group<T> {
    pub body: Option<T>,
}

#[ast]
pub struct SequenceLiteral<T> {
    pub items: Vec<T>,
}

#[ast]
pub struct TypesetLiteral<T> {
    pub expression: Option<T>,
}

#[ast]
pub struct Def<T> {
    pub name: T, // being with Cons
    pub args: Vec<T>,
    pub body: Option<T>,
}

#[ast]
pub struct Foreign {
    pub indent: usize,
    pub lang:   String,
    pub code:   Vec<String>,
}

#[ast]
pub struct Modified<T> {
    pub modifier:   String,
    pub definition: T,
}



// ===========
// === AST ===
// ===========


// === Tokenizer ===

/// An enum of valid Ast tokens.
#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Off(usize),
    Chr(char),
    Str(&'a str),
    Ast(&'a Ast),
}

/// Things that can be turned into stream of tokens.
pub trait HasTokens {
    /// Feeds TokenBuilder with stream of tokens obtained from `self`.
    fn feed_to(&self, consumer: &mut impl TokenConsumer);
}

/// Helper trait for Tokenizer, which consumes the token stream.
pub trait TokenConsumer {
    /// consumes one token
    fn feed(&mut self, val: Token);
}

impl<F: FnMut(Token)> TokenConsumer for F {
    fn feed(&mut self, val: Token) {
        self(val)
    }
}

impl<'a> HasTokens for Token<'a> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        consumer.feed(self.clone())
    }
}

impl HasTokens for &str {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        consumer.feed(Token::Str(self));
    }
}

impl HasTokens for String {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        consumer.feed(Token::Str(self.as_str()));
    }
}

impl HasTokens for usize {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        consumer.feed(Token::Off(*self));
    }
}

impl HasTokens for char {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        consumer.feed(Token::Chr(*self));
    }
}

impl HasTokens for Ast {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        consumer.feed(Token::Ast(self));
    }
}

impl<T: HasTokens> HasTokens for Option<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        for t in self {
            t.feed_to(consumer);
        }
    }
}

impl<T: HasTokens> HasTokens for Vec<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        for t in self {
            t.feed_to(consumer);
        }
    }
}

impl<T: HasTokens> HasTokens for Rc<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        self.content().feed_to(consumer);
    }
}

impl<T: HasTokens> HasTokens for &T {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        self.deref().feed_to(consumer);
    }
}

impl<T: HasTokens, U: HasTokens> HasTokens for (T, U) {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        self.0.feed_to(consumer);
        self.1.feed_to(consumer);
    }
}
impl<T: HasTokens, U: HasTokens, V: HasTokens> HasTokens for (T, U, V) {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        self.0.feed_to(consumer);
        self.1.feed_to(consumer);
        self.2.feed_to(consumer);
    }
}


// === HasIdMap ===

/// Things that have IdMap.
pub trait HasIdMap {
    /// Extracts IdMap from `self`.
    fn id_map(&self) -> IdMap;
}

#[derive(Debug, Clone, Default)]
struct IdMapBuilder {
    id_map: IdMap,
    offset: Bytes,
}

impl TokenConsumer for IdMapBuilder {
    fn feed(&mut self, token: Token) {
        match token {
            Token::Off(val) => self.offset += Bytes::from(' '.len_utf8() * val),
            Token::Chr(_) => self.offset += 1.bytes(),
            Token::Str(val) => self.offset += Bytes::from(val.len()),
            Token::Ast(val) => {
                let begin = self.offset;
                val.shape().feed_to(self);
                let end = self.offset;
                if let Some(id) = val.id {
                    self.id_map.insert(begin..end, id);
                }
            }
        }
    }
}

impl<T: HasTokens> HasIdMap for T {
    fn id_map(&self) -> IdMap {
        let mut consumer = IdMapBuilder::default();
        self.feed_to(&mut consumer);
        consumer.id_map
    }
}


// === HasRepr ===

/// Things that can be asked about their textual representation.
pub trait HasRepr {
    /// Obtain the text representation for the This type.
    fn repr(&self) -> String;

    /// Get the representation length in bytes.
    ///
    /// May be implemented in a quicker way than building string. Must meet the constraint
    /// `x.len() == x.repr().len()` for any `x: impl HasRepr`.
    fn len(&self) -> Bytes {
        self.repr().len().into()
    }

    /// Check if the representation is empty.
    fn is_empty(&self) -> bool {
        self.len() >= 0.bytes()
    }

    /// Get the representation length in chars.
    ///
    /// May be implemented in a quicker way than building string. Must meet the constraint
    /// `x.char_count() == x.repr().chars().count()` for any `x: impl HasRepr`.
    fn char_count(&self) -> Chars {
        self.repr().chars().count().into()
    }
}

#[derive(Debug, Clone, Default)]
struct ReprBuilder {
    repr: String,
}

impl TokenConsumer for ReprBuilder {
    fn feed(&mut self, token: Token) {
        match token {
            Token::Off(val) => self.repr.push_str(&" ".repeat(val)),
            Token::Chr(val) => self.repr.push(val),
            Token::Str(val) => self.repr.push_str(val),
            Token::Ast(val) => val.shape().feed_to(self),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct LengthBuilder {
    length: Bytes,
}

impl TokenConsumer for LengthBuilder {
    fn feed(&mut self, token: Token) {
        match token {
            Token::Off(val) => self.length += Bytes::from(' '.len_utf8() * val),
            Token::Chr(chr) => self.length += Bytes::from(chr.len_utf8()),
            Token::Str(val) => self.length += Bytes::from(val.len()),
            Token::Ast(val) => val.shape().feed_to(self),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct CharCountBuilder {
    char_count: Chars,
}


impl TokenConsumer for CharCountBuilder {
    fn feed(&mut self, token: Token) {
        match token {
            Token::Off(val) => self.char_count += Chars::from(val),
            Token::Chr(_) => self.char_count += 1.chars(),
            Token::Str(val) => self.char_count += Chars::from(val.chars().count()),
            Token::Ast(val) => val.shape().feed_to(self),
        }
    }
}

impl<T: HasTokens> HasRepr for T {
    fn repr(&self) -> String {
        let mut consumer = ReprBuilder::default();
        self.feed_to(&mut consumer);
        consumer.repr
    }

    fn len(&self) -> Bytes {
        let mut consumer = LengthBuilder::default();
        self.feed_to(&mut consumer);
        consumer.length
    }

    fn char_count(&self) -> Chars {
        let mut consumer = CharCountBuilder::default();
        self.feed_to(&mut consumer);
        consumer.char_count
    }
}


// === WithID ===

pub type Id = Uuid;

pub trait HasID {
    fn id(&self) -> Option<Id>;
}

#[derive(Eq, PartialEq, Debug, Shrinkwrap, Serialize, Deserialize)]
#[shrinkwrap(mutable)]
pub struct WithID<T> {
    #[shrinkwrap(main_field)]
    #[serde(flatten)]
    pub wrapped: T,
    pub id:      Option<Id>,
}

impl<T> HasID for WithID<T>
where T: HasID
{
    fn id(&self) -> Option<Id> {
        self.id
    }
}

impl<T, S: Layer<T>> Layer<T> for WithID<S> {
    fn layered(t: T) -> Self {
        WithID { wrapped: Layer::layered(t), id: None }
    }
}

impl<T> HasRepr for WithID<T>
where T: HasRepr
{
    fn repr(&self) -> String {
        self.deref().repr()
    }

    fn len(&self) -> Bytes {
        self.deref().len()
    }

    fn char_count(&self) -> Chars {
        self.deref().char_count()
    }
}



#[derive(Debug, Clone)]
struct TraverserWithOffset<F> {
    offset:   Chars,
    callback: F,
}

impl<F> TraverserWithOffset<F> {
    pub fn new(callback: F) -> TraverserWithOffset<F> {
        let offset = 0.chars();
        TraverserWithOffset { offset, callback }
    }
}

impl<F> TokenConsumer for TraverserWithOffset<F>
where F: FnMut(Chars, &Ast)
{
    fn feed(&mut self, token: Token) {
        if let Token::Ast(val) = token {
            (self.callback)(self.offset, val);
            val.shape().feed_to(self);
        } else {
            self.offset += token.char_count();
        }
    }
}

/// Visits each Ast node, while keeping track of its index.
pub fn traverse_with_offset(ast: &impl HasTokens, f: impl FnMut(Chars, &Ast)) {
    let mut traverser = TraverserWithOffset::new(f);
    ast.feed_to(&mut traverser);
}

/// Visits each Ast node, while keeping track of its span.
pub fn traverse_with_span(ast: &impl HasTokens, mut f: impl FnMut(enso_text::Range<Chars>, &Ast)) {
    traverse_with_offset(ast, move |offset, ast| {
        f(enso_text::Range::new(offset, offset + ast.char_count()), ast)
    })
}

// === WithLength ===

/// Stores a value of type `T` and information about its length.
///
/// Even if `T` is `Spanned`, keeping `length` variable is desired for performance
/// purposes.
#[derive(Eq, PartialEq, Debug, Shrinkwrap, Serialize, Deserialize)]
#[shrinkwrap(mutable)]
pub struct WithLength<T> {
    #[shrinkwrap(main_field)]
    #[serde(flatten)]
    pub wrapped: T,
    pub length:  Chars,
}

impl<T> HasRepr for WithLength<T>
where T: HasRepr
{
    fn repr(&self) -> String {
        self.deref().repr()
    }

    fn len(&self) -> Bytes {
        self.deref().len()
    }

    fn char_count(&self) -> Chars {
        self.length
    }
}

impl<T, S> Layer<T> for WithLength<S>
where T: HasRepr + Into<S>
{
    fn layered(t: T) -> Self {
        let char_count = t.char_count();
        WithLength { wrapped: t.into(), length: char_count }
    }
}

impl<T> HasID for WithLength<T>
where T: HasID
{
    fn id(&self) -> Option<Id> {
        self.deref().id()
    }
}


// =============================================================================
// === TO BE GENERATED =========================================================
// =============================================================================
// TODO: the definitions below should be removed and instead generated using
//  macros, as part of https://github.com/enso-org/enso/issues/338

// === Shape ===

impl<T> Module<T> {
    /// Convert into a [`Block`] with the given indentation.
    ///
    /// Returns None if does not contain any lines.
    pub fn as_block(&self, indent: usize) -> Option<Block<T>>
    where T: Clone {
        let is_empty = |line: &&BlockLine<Option<T>>| line.elem.is_none();
        let empty_lines = self.lines.iter().take_while(is_empty);
        let empty_lines = empty_lines.map(|line| line.off).collect_vec();
        let ty = BlockType::Discontinuous {};
        let first_line = self.lines.iter().find_map(|line| {
            Some(BlockLine { off: line.off, elem: line.elem.as_ref()?.clone() })
        })?;
        let lines = self.lines.iter().skip_while(is_empty).skip(1).cloned().collect();
        // We virtually never want a block to be an orphan (i.e. start with a first line attached
        // to whatever AST was there before). While it may seem tempting to have this for single
        // line blocks, it does not make sense. If we want expression inline, it shouldn't be a
        // block at all. Also, having inline expression can make invalid AST when the only line is
        // an assignment and we want to use block as a definition's body.
        let is_orphan = false;
        Some(Block { ty, indent, empty_lines, first_line, lines, is_orphan })
    }
}

impl<T> BlockLine<T> {
    /// Creates a new BlockLine wrapping given item and having 0 offset.
    pub fn new(elem: T) -> BlockLine<T> {
        BlockLine { elem, off: 0 }
    }

    /// Convert `&BlockLine<T>` into `BlockLine<&T>`.
    pub fn as_ref(&self) -> BlockLine<&T> {
        BlockLine { elem: &self.elem, off: self.off }
    }

    /// Maps `BlockLine<T>` into `BlockLine<U>` using the provided function.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> BlockLine<U> {
        BlockLine { elem: f(self.elem), off: self.off }
    }
}

impl<T> BlockLine<Option<T>> {
    /// Transpose a `BlockLine<Option<T>>` into `Option<BlockLine<T>>`.
    pub fn transpose(self) -> Option<BlockLine<T>> {
        let off = self.off;
        self.elem.map(|elem| BlockLine { elem, off })
    }

    /// Transpose a `&BlockLine<Option<T>>` into `Option<BlockLine<&T>>`.
    pub fn transpose_ref(&self) -> Option<BlockLine<&T>> {
        self.as_ref().map(Option::as_ref).transpose()
    }

    /// Map the inner contents of the line's stored element.
    pub fn map_opt<U>(self, f: impl FnOnce(T) -> U) -> BlockLine<Option<U>> {
        self.map(|elem| elem.map(f))
    }
}

/// Iterate over non-empty lines, while maintaining their indices.
pub fn enumerate_non_empty_lines<'a, T: 'a>(
    iter: impl IntoIterator<Item = &'a BlockLine<Option<T>>> + 'a,
) -> impl Iterator<Item = (usize, BlockLine<&'a T>)> + 'a {
    iter.into_iter().enumerate().filter_map(|(index, line): (usize, &BlockLine<Option<T>>)| {
        let non_empty_line = line.transpose_ref()?;
        Some((index, non_empty_line))
    })
}

impl<T> Block<T> {
    /// Iterates over all lines in the block, including leading empty lines.
    pub fn iter_all_lines(&self) -> impl Iterator<Item = BlockLine<Option<&T>>> + '_ {
        let indent = self.indent;
        let leading_empty_lines = self.empty_lines.iter().map(move |off| {
            let elem = None;
            // TODO [mwu]
            //  Empty lines use absolute indent, while BlockLines are relative to Block.
            //  We might lose some data here, as empty lines shorter than block will get filled
            //  with spaces. This is something that should be improved in the future but also
            //  requires changes in the AST.
            let off = off.saturating_sub(indent);
            BlockLine { elem, off }
        });

        let first_line = std::iter::once(self.first_line.as_ref().map(Some));
        let lines = self.lines.iter().map(|line| line.as_ref().map(|elem| elem.as_ref()));
        leading_empty_lines.chain(first_line).chain(lines)
    }

    /// Calculate absolute indentation of lines in this block.
    pub fn indent(&self, parent_indent: usize) -> usize {
        parent_indent + self.indent
    }

    /// Iterate over non-empty lines, while keeping their absolute indices.
    pub fn enumerate_non_empty_lines(&self) -> impl Iterator<Item = (usize, BlockLine<&T>)> + '_ {
        self.iter_all_lines().enumerate().filter_map(
            |(index, line): (usize, BlockLine<Option<&T>>)| {
                let non_empty_line = line.transpose()?;
                Some((index, non_empty_line))
            },
        )
    }
}

impl Block<Ast> {
    /// Create a block from given line ASTs.
    ///
    /// If there are no tail lines, the first line will be "inline" and the whole block.
    /// If there are tail lines, block will be leaded with a newline.
    pub fn from_lines(first_line: &Ast, tail_lines: &[Option<Ast>]) -> Block<Ast> {
        let ty = BlockType::Discontinuous {};
        let indent = 0;
        let empty_lines = Vec::new();
        let first_line = BlockLine::new(first_line.clone_ref());
        let lines = tail_lines.iter().cloned().map(BlockLine::new).collect();
        // We virtually never want a block to be an orphan (i.e. start with a first line attached
        // to whatever AST was there before). While it may seem tempting to have this for single
        // line blocks, it does not make sense. If we want expression inline, it shouldn't be a
        // block at all. Also, having inline expression can make invalid AST when the only line is
        // an assignment and we want to use block as a definition's body.
        let is_orphan = false;
        Block { ty, indent, empty_lines, first_line, lines, is_orphan }
    }
}

impl Infix<Ast> {
    /// Creates an `Infix` Shape, where both its operands are Vars and spacing is 1.
    pub fn from_vars(larg: impl Str, opr: impl Str, rarg: impl Str) -> Infix<Ast> {
        let larg = Ast::var(larg);
        let loff = 1;
        let opr = Ast::opr(opr);
        let roff = 1;
        let rarg = Ast::var(rarg);
        Infix { larg, loff, opr, roff, rarg }
    }
}

impl Module<Ast> {
    /// Creates a `Module` Shape with lines storing given Asts and having 0 offset.
    pub fn from_lines(line_asts: &[Option<Ast>]) -> Module<Ast> {
        let lines = line_asts.iter().cloned().map(|elem| BlockLine { elem, off: 0 }).collect();
        Module { lines }
    }

    pub fn from_line(line_ast: impl Into<Ast>) -> Self {
        Self::from_lines(&[Some(line_ast.into())])
    }
}

// === AST ===

impl Ast {
    // TODO smart constructors for other cases
    //  as part of https://github.com/enso-org/enso/issues/338

    /// Creates Blank ast node (underscore).
    pub fn blank() -> Ast {
        Ast::from(Blank {})
    }

    /// Creates an Ast node with Number inside.
    pub fn number(number: i64) -> Ast {
        let number = Number { base: None, int: number.to_string() };
        Ast::from(number)
    }

    /// Creates an Ast node with Cons inside.
    pub fn cons(name: impl Str) -> Ast {
        let cons = Cons { name: name.into() };
        Ast::from(cons)
    }

    /// Creates an Ast node with Var inside and given ID.
    pub fn var_with_id(name: impl Str, id: Id) -> Ast {
        let name = name.into();
        let var = Var { name };
        Ast::new(var, Some(id))
    }

    /// Creates an AST node with `Var` shape.
    pub fn var(name: impl Str) -> Ast {
        let var = Var { name: name.into() };
        Ast::from(var)
    }

    /// Creates an AST node with `Opr` shape.
    pub fn opr(name: impl Str) -> Ast {
        let opr = Opr { name: name.into() };
        Ast::from(opr)
    }

    /// Creates an AST node with `SectionLeft` shape.
    pub fn section_left<Arg: Into<Ast>>(arg: Arg, opr: impl Str) -> Ast {
        let off = 1;
        let opr = Ast::opr(opr);
        let section_left = SectionLeft { arg: arg.into(), off, opr };
        Ast::from(section_left)
    }

    /// Creates an AST node with `SectionRight` shape.
    pub fn section_right<Arg: Into<Ast>>(opr: impl Str, arg: Arg) -> Ast {
        let off = 1;
        let opr = Ast::opr(opr);
        let section_right = SectionRight { arg: arg.into(), off, opr };
        Ast::from(section_right)
    }

    /// Creates an AST node with `SectionSides` shape.
    pub fn section_sides(opr: impl Str) -> Ast {
        let opr = Ast::opr(opr);
        let section_sides = SectionSides { opr };
        Ast::from(section_sides)
    }

    /// Creates an AST node with `Prefix` shape.
    pub fn prefix<Func: Into<Ast>, Arg: Into<Ast>>(func: Func, arg: Arg) -> Ast {
        let off = 1;
        let opr = Prefix { func: func.into(), off, arg: arg.into() };
        Ast::from(opr)
    }

    /// Creates an AST node with `InvalidSuffix` shape.
    pub fn invalid_suffix(elem: impl Into<Ast>, suffix: impl Str) -> Ast {
        let elem = elem.into();
        let suffix = suffix.into();
        let invalid_suffix = InvalidSuffix { elem, suffix };
        Ast::from(invalid_suffix)
    }

    /// Creates an AST node with `Infix` shape.
    pub fn infix(larg: impl Into<Ast>, opr: impl Str, rarg: impl Into<Ast>) -> Ast {
        let larg = larg.into();
        let loff = 1;
        let opr = Ast::opr(opr);
        let roff = 1;
        let rarg = rarg.into();
        let infix = Infix { larg, loff, opr, roff, rarg };
        Ast::from(infix)
    }

    /// Creates AST node with `Module` shape with one line.
    pub fn one_line_module(line_ast: impl Into<Ast>) -> Ast {
        Module::from_line(line_ast).into()
    }

    /// Creates an AST node with `TextLineFmt` shape.
    pub fn text_line_fmt(text: Vec<SegmentFmt<Ast>>) -> Ast {
        let text_line_fmt = TextLineFmt { text };
        Ast::from(text_line_fmt)
    }

    /// Creates an AST node with `TextUnclosed` shape.
    pub fn text_unclosed(line: TextLine<Ast>) -> Ast {
        let text_unclosed = TextUnclosed { line };
        Ast::from(text_unclosed)
    }

    /// Creates an AST node with `TextBlockFmt` shape.
    pub fn text_block_fmt(text: Vec<TextBlockLine<SegmentFmt<Ast>>>, offset: usize) -> Ast {
        let spaces = 0;
        let text_block_fmt = TextBlockFmt { text, spaces, offset };
        Ast::from(text_block_fmt)
    }

    /// Creates an AST node with `Infix` shape, where both its operands are Vars.
    pub fn infix_var(larg: impl Str, opr: impl Str, rarg: impl Str) -> Ast {
        let infix = Infix::from_vars(larg, opr, rarg);
        Ast::from(infix)
    }
}


// === Text Conversion Boilerplate ===

// support for transitive conversions, like:
// RawEscapeSth -> RawEscape -> SegmentRawEscape -> SegmentRaw

impl From<Unfinished> for SegmentRaw {
    fn from(value: Unfinished) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}
impl From<Invalid> for SegmentRaw {
    fn from(value: Invalid) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}
impl From<Slash> for SegmentRaw {
    fn from(value: Slash) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}
impl From<Quote> for SegmentRaw {
    fn from(value: Quote) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}
impl From<RawQuote> for SegmentRaw {
    fn from(value: RawQuote) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}


// === RawEscapeSth -> RawEscape -> SegmentRawEscape -> SegmentFmt ===

impl<T> From<Unfinished> for SegmentFmt<T> {
    fn from(value: Unfinished) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}
impl<T> From<Invalid> for SegmentFmt<T> {
    fn from(value: Invalid) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}
impl<T> From<Slash> for SegmentFmt<T> {
    fn from(value: Slash) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}
impl<T> From<Quote> for SegmentFmt<T> {
    fn from(value: Quote) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}
impl<T> From<RawQuote> for SegmentFmt<T> {
    fn from(value: RawQuote) -> Self {
        SegmentRawEscape { code: value.into() }.into()
    }
}

impl<T> From<Escape> for SegmentFmt<T> {
    fn from(value: Escape) -> Self {
        SegmentEscape { code: value }.into()
    }
}


// === EscapeSth -> Escape -> SegmentEscape -> SegmentFmt ===

impl<T> From<EscapeCharacter> for SegmentFmt<T> {
    fn from(value: EscapeCharacter) -> Self {
        SegmentEscape { code: value.into() }.into()
    }
}

impl<T> From<EscapeControl> for SegmentFmt<T> {
    fn from(value: EscapeControl) -> Self {
        SegmentEscape { code: value.into() }.into()
    }
}

impl<T> From<EscapeNumber> for SegmentFmt<T> {
    fn from(value: EscapeNumber) -> Self {
        SegmentEscape { code: value.into() }.into()
    }
}

impl<T> From<EscapeUnicode16> for SegmentFmt<T> {
    fn from(value: EscapeUnicode16) -> Self {
        SegmentEscape { code: value.into() }.into()
    }
}

impl<T> From<EscapeUnicode21> for SegmentFmt<T> {
    fn from(value: EscapeUnicode21) -> Self {
        SegmentEscape { code: value.into() }.into()
    }
}

impl<T> From<EscapeUnicode32> for SegmentFmt<T> {
    fn from(value: EscapeUnicode32) -> Self {
        SegmentEscape { code: value.into() }.into()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use serde::de::DeserializeOwned;

    /// Assert that given value round trips JSON serialization.
    fn round_trips<T>(input_val: &T)
    where T: Serialize + DeserializeOwned + PartialEq + Debug {
        let json_str = serde_json::to_string(&input_val).unwrap();
        let deserialized_val: T = serde_json::from_str(&json_str).unwrap();
        assert_eq!(*input_val, deserialized_val);
    }

    #[test]
    fn ast_updating_id() {
        let var = Var { name: "foo".into() };
        let ast = Ast::new(var, None);
        assert!(ast.id.is_some());

        let id = Id::default();
        let ast = ast.with_id(id);
        assert_eq!(ast.id, Some(id));
    }

    #[test]
    fn var_smart_constructor() {
        let name = "foo".to_string();
        let v = Ast::var(name.clone());
        match v.shape() {
            Shape::Var(var) if *var.name == name => (),
            _ => panic!("expected Var with name `{}`", name),
        }
    }

    #[test]
    fn ast_length() {
        let ast = Ast::prefix(Ast::var("XĄ"), Ast::var("YY"));
        assert_eq!(ast.len(), 6.bytes());
        assert_eq!(ast.char_count(), 5.chars());
    }

    #[test]
    fn ast_repr() {
        let ast = Ast::prefix(Ast::var("XĄ"), Ast::var("YY"));
        assert_eq!(ast.repr().as_str(), "XĄ YY")
    }

    #[test]
    fn ast_id_map() {
        let span = |ix: usize, length: usize| {
            enso_text::Range::<Bytes>::new(ix.into(), (ix + length).into())
        };
        let uid = default();
        let ids = vec![(span(0, 2), uid), (span(3, 2), uid), (span(0, 5), uid)];
        let func = Ast::new(Var { name: "XX".into() }, Some(uid));
        let arg = Ast::new(Var { name: "YY".into() }, Some(uid));
        let ast = Ast::new(Prefix { func, off: 1, arg }, Some(uid));
        assert_eq!(ast.id_map(), IdMap::new(ids));
    }

    #[test]
    fn ast_wrapping() {
        // We can convert `Var` into AST without worrying about length nor id.
        let ident = "foo".to_string();
        let v = Var { name: ident.clone() };
        let ast = Ast::from(v);
        assert!(ast.wrapped.id.is_some());
        assert_eq!(ast.wrapped.wrapped.length, ident.chars().count().into());
    }

    #[test]
    fn serialization_round_trip() {
        let make_var = || Var { name: "foo".into() };
        round_trips(&make_var());

        let ast_without_id = Ast::new(make_var(), None);
        round_trips(&ast_without_id);

        let id = Id::parse_str("15").ok();
        let ast_with_id = Ast::new(make_var(), id);
        round_trips(&ast_with_id);
    }

    #[test]
    fn deserialize_var() {
        let var_name = "foo";
        let uuid_str = "51e74fb9-75a4-499d-9ea3-a90a2663b4a1";

        let sample_json = serde_json::json!({
            "shape": { "Var":{"name": var_name}},
            "id": uuid_str,
            "span": var_name.len()
        });
        let sample_json_text = sample_json.to_string();
        let ast: Ast = serde_json::from_str(&sample_json_text).unwrap();

        let expected_uuid = Id::parse_str(uuid_str).ok();
        assert_eq!(ast.id, expected_uuid);

        let expected_length = 3.chars();
        assert_eq!(ast.length, expected_length);

        let expected_var = Var { name: var_name.into() };
        let expected_shape = Shape::from(expected_var);
        assert_eq!(*ast.shape(), expected_shape);
    }

    #[test]
    /// Check if Ast can be iterated.
    fn iterating() {
        // TODO [mwu] When Repr is implemented, the below lambda sohuld be
        //            removed in favor of it.
        let to_string = |ast: &Ast| match ast.shape() {
            Shape::Var(var) => var.name.clone(),
            Shape::Opr(opr) => opr.name.clone(),
            _ => "«invalid»".to_string(),
        };

        let infix = Ast::infix_var("foo", "+", "bar");
        let strings = infix.iter().map(to_string);
        let strings = strings.collect::<Vec<_>>();

        let assert_contains = |searched: &str| assert!(strings.iter().any(|elem| elem == searched));
        assert_contains("foo");
        assert_contains("bar");
        assert_contains("+");
        assert_eq!(strings.len(), 3);
    }

    #[test]
    fn iterate_nested() {
        let a = Ast::var("a");
        let b = Ast::var("b");
        let c = Ast::var("c");
        let ab = Ast::prefix(a, b);
        let abc = Ast::prefix(ab, c); // repr is `a b c`

        assert_eq!((&abc).iter().count(), 2); // for App's two children
        assert_eq!(abc.iter_recursive().count(), 5); // for 2 Apps and 3 Vars
    }

    #[test]
    fn all_lines_of_block() {
        let ty = BlockType::Discontinuous {};
        let indent = 4;
        let empty_lines = vec![5];
        let first_line = BlockLine { elem: Ast::var("head"), off: 3 };
        let lines = vec![
            BlockLine { elem: Some(Ast::var("tail0")), off: 2 },
            BlockLine { elem: None, off: 1 },
            BlockLine { elem: Some(Ast::var("tail2")), off: 3 },
        ];
        let is_orphan = false;
        let block = Block { ty, indent, empty_lines, first_line, lines, is_orphan };
        let expected_repr = "\n     \n    head   \n    tail0  \n \n    tail2   ";
        assert_eq!(block.repr(), expected_repr);

        let all_lines = block.iter_all_lines().collect_vec();
        let (empty_line, head_line, tail0, tail1, tail2) = all_lines.iter().expect_tuple();
        assert!(empty_line.elem.is_none());
        assert_eq!(empty_line.off, 1); // other 4 indents are provided by Block
        assert_eq!(head_line.elem.as_ref().unwrap().repr(), "head");
        assert_eq!(head_line.off, 3);
        assert_eq!(tail0.elem.as_ref().unwrap().repr(), "tail0");
        assert_eq!(tail0.off, 2);
        assert!(tail1.elem.is_none());
        assert_eq!(tail1.off, 1);
        assert_eq!(tail2.elem.as_ref().unwrap().repr(), "tail2");
        assert_eq!(tail2.off, 3);
    }

    #[test]
    fn utf8_lengths() {
        let var = Ast::var("価");
        assert_eq!(var.char_count(), 1.chars());
        assert_eq!(var.len(), 3.bytes());

        let idmap = var.id_map();
        assert_eq!(idmap.vec[0].0, enso_text::Range::new(0.bytes(), 3.bytes()));
        assert_eq!(idmap.vec[0].1, var.id.unwrap());

        let builder_with_char = Token::Chr('壱');
        assert_eq!(builder_with_char.char_count(), 1.chars());
        assert_eq!(builder_with_char.len(), 3.bytes());
    }
}
