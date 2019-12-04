#![feature(type_alias_impl_trait)]
#![feature(generators, generator_trait)]

use prelude::*;

use serde::{Serialize, Deserialize};
use serde::ser::{Serializer, SerializeStruct};
use serde::de::{Deserializer, Visitor};
use uuid::Uuid;
use ast_macros::*;
use shapely::*;

pub type Stream<T> = Vec<T>;

// ==============
// === Errors ===
// ==============

/// Exception raised by macro-generated TryFrom methods that try to "downcast"
/// enum type to its variant subtype if different constructor was used.
#[derive(Display, Debug, Fail)]
pub struct WrongEnum { pub expected_con: String }

// ============
// === Tree ===
// ============

/// A tree structure where each node may store value of `K` and has arbitrary
/// number of children nodes, each marked with a single `K`.
///
/// It is used to describe ambiguous macro match.
#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Tree<K,V> {
    pub value    : Option<V>,
    pub branches : Vec<(K, Tree<K,V>)>,
}

// ===============
// === Shifted ===
// ===============

/// A value of type `T` annotated with offset value `off`.
#[derive(Eq, PartialEq, Debug, Serialize, Deserialize, Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Shifted<T> {
    #[shrinkwrap(main_field)]
    pub wrapped : T,
    pub off     : usize,
}

/// A non-empty sequence of `T`s interspersed by offsets.
#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct ShiftedVec1<T> {
    pub head: T,
    pub tail: Vec<Shifted<T>>
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
    fn from(t: T) -> Self {  Layered::layered(t) }
}

// === Layered ===

/// A trivial `Layer` type that is just a strongly typed wrapper over `T`.
#[derive(Debug)]
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Layered<T>(pub T);

impl<T> Layer<T> for Layered<T> {
    fn layered(t: T) -> Self { Layered(t) }
}

// ===========
// === AST ===
// ===========

/// The primary class for Enso Abstract Syntax Tree.
///
/// This implementation is paired with AST implementation for Scala. Any changes
/// to either of the implementation need to be applied to the other one as well.
///
/// Each AST node is annotated with span and an optional ID.
#[derive(Eq, PartialEq, Debug, Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Ast {
    pub wrapped: Rc<WithID<WithSpan<Shape<Ast>>>>
}

impl Clone for Ast {
    fn clone(&self) -> Self {
        Ast { wrapped: self.wrapped.clone() }
    }
}

impl Ast {
    pub fn iter(&self) -> Rc<dyn Iterator<Item = &'_ Shape<Ast>> + '_> {
        // TODO https://github.com/luna/enso/issues/338
        unimplemented!();
    }

    pub fn shape(&self) -> &Shape<Ast> {
        self
    }

    /// Wraps given shape with an optional ID into Ast. Span will ba
    /// automatically calculated based on Shape.
    pub fn new<S: Into<Shape<Ast>>>(shape: S, id: Option<ID>) -> Ast {
        let shape: Shape<Ast> = shape.into();
        let span = shape.span();
        Ast::new_with_span(shape, id, span)
    }

    pub fn new_with_span<S: Into<Shape<Ast>>>
    (shape: S, id: Option<ID>, span: usize) -> Ast {
        let shape     = shape.into();
        let with_span = WithSpan { wrapped: shape,     span };
        let with_id   = WithID   { wrapped: with_span, id   };
        Ast { wrapped: Rc::new(with_id) }
    }
}

impl HasSpan for Ast {
    fn span(&self) -> usize {
        self.wrapped.span()
    }
}

/// Fills `id` with `None` by default.
impl<T: Into<Shape<Ast>>>
From<T> for Ast {
    fn from(t: T) -> Self {
        let id = None;
        Ast::new(t, id)
    }
}

// Serialization & Deserialization //

/// Literals used in `Ast` serialization and deserialization.
pub mod ast_schema {
    pub const STRUCT_NAME: &str      = "Ast";
    pub const SHAPE:       &str      = "shape";
    pub const ID:          &str      = "id";
    pub const SPAN:        &str      = "span";
    pub const FIELDS:      [&str; 3] = [SHAPE, ID, SPAN];
    pub const COUNT:       usize     = FIELDS.len();
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
        state.serialize_field(SPAN,  &self.span)?;
        state.end()
    }
}

/// Type to provide serde::de::Visitor to deserialize data into `Ast`.
struct AstDeserializationVisitor;

impl<'de> Visitor<'de> for AstDeserializationVisitor {
    type Value = Ast;

    fn expecting
    (&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ast_schema::*;
        write!(formatter, "an object with `{}` and `{}` fields", SHAPE, SPAN)
    }

    fn visit_map<A>
    (self, mut map: A) -> Result<Self::Value, A::Error>
    where A: serde::de::MapAccess<'de>, {
        use ast_schema::*;

        let mut shape: Option<Shape<Ast>> = None;
        let mut id:    Option<Option<ID>> = None;
        let mut span:  Option<usize>      = None;

        while let Some(key) = map.next_key()? {
            match key {
                SHAPE => shape = Some(map.next_value()?),
                ID    => id    = Some(map.next_value()?),
                SPAN  => span  = Some(map.next_value()?),
                _     => {},
            }
        }

        let shape = shape.ok_or(serde::de::Error::missing_field(SHAPE))?;
        let id    = id.unwrap_or(None); // allow missing `id` field
        let span  = span.ok_or(serde::de::Error::missing_field(SPAN))?;
        Ok(Ast::new_with_span(shape, id, span))
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
#[ast(flat)] pub enum Shape<T> {

    // === Identifiers ===
    Blank     { },
    Var       { name : String },
    Cons      { name : String },
    Opr       { name : String },
    Mod       { name : String },

    // === Literals ===
    Number    { base: Option<String>, int: String },
    Text      (Text<T>),

    // === Expressions ===
    Prefix    { func : T   , off  : usize , arg: T                          },
    Infix     { larg : T   , loff : usize , opr: T , roff: usize , rarg: T  },
    SectLeft  { arg  : T   , off  : usize , opr: T                          },
    SectRight { opr  : T   , off  : usize , arg: T                          },
    SectSides { opr  : T                                                    },

    Invalid   (Invalid<T>),
    Block     (Block<T>),
    Module    (Module<T>),
    Macro     (Macro<T>),
    Comment   (Comment),
    Import    (Import<T>),
    Mixfix    (Mixfix<T>),
    Group     (Group<T>),
    Def       (Def<T>),
    Foreign   (Foreign),
}

// ===============
// === Invalid ===
// ===============

#[ast] pub enum Invalid<T> {
    Unrecognized  { input : String },
    Unexpected    { msg   : String, stream: Stream<T> },
    InvalidSuffex { elem  : T, suffix: String },
    // FIXME: missing constructors, https://github.com/luna/enso/issues/336
    // DanglingBase
}


// ============
// === Text ===
// ============

#[ast] pub enum Text<T> {
    Raw { body: TextBody<TextRawSegment> },
    Fmt { body: TextBody<TextFmtSegment<T>> }
}

#[ast] pub struct TextRawSegment {
    pub value: String
}

#[ast] pub enum TextFmtSegment<T> {
    Plain  { value  : String },
    Expr   { value  : Option<T> },
    Escape { escape : TextEscape },
}

pub type TextBlock<T> = Vec<TextLine<T>>;
#[ast] pub struct TextLine<T> { off: usize, elems: Vec<T> }
#[ast] pub struct TextBody<T> { quote: TextQuote, block: TextBlock<T> }
#[ast] pub enum   TextQuote   { Single, Triple }

// FIXME: missing TextEscape contents, https://github.com/luna/enso/issues/336
#[ast] pub struct TextEscape {}


// =============
// === Block ===
// =============

#[ast] pub struct Block<T> {
    pub ty          : BlockType,
    pub ident       : usize,
    pub empty_lines : usize,
    pub first_line  : BlockLine<T>,
    pub lines       : Vec<BlockLine<Option<T>>>,
    pub is_orphan   : bool,
}

#[ast] pub enum   BlockType     { Continuous, Discontinuous }
#[ast] pub struct BlockLine <T> { pub elem: T, pub off: usize }

// ==============
// === Module ===
// ==============

#[ast] pub struct Module<T> {  pub lines: Vec<BlockLine<Option<T>>> }

// =============
// === Macro ===
// =============

#[ast] pub enum Macro<T> {
    Match     (Match<T>),
    Ambiguous (Ambiguous),
}

#[ast] pub struct MacroMatch<T> {
    pub pfx      : Option<MacroPatternMatch<Ast>>,
    pub segs     : ShiftedVec1<MacroMatchSegment<T>>,
    pub resolved : Ast
}

#[ast] pub struct MacroAmbiguous {
    pub segs  : ShiftedVec1<MacroAmbiguousSegment>,
    pub paths : Tree<Ast, ()>,
}

#[ast] pub struct MacroMatchSegment<T> {
    pub head : Ast,
    pub body : MacroPatternMatch<Shifted<T>>
}

#[ast] pub struct MacroAmbiguousSegment {
    pub head: Ast,
    pub body: Option<Shifted<Ast>>
}

pub type MacroPattern = Rc<MacroPatternRaw>;
#[ast] pub enum MacroPatternRaw {

    // === Boundary Patterns ===
    Begin   ,
    End     ,

    // === Structural Patterns ===
    Nothing ,
    Seq     { pat1 : MacroPattern , pat2    : MacroPattern                    },
    Or      { pat1 : MacroPattern , pat2    : MacroPattern                    },
    Many    { pat  : MacroPattern                                             },
    Except  { not  : MacroPattern, pat      : MacroPattern                    },

    // === Meta Patterns ===
    Build   { pat  : MacroPattern                                             },
    Err     { msg  : String       , pat     : MacroPattern                    },
    Tag     { tag  : String       , pat     : MacroPattern                    },
    Cls     { cls  : PatternClass , pat     : MacroPattern                    },

    // === Token Patterns ===
    Tok     { spaced : Spaced     , ast     : Ast                             },
    Blank   { spaced : Spaced                                                 },
    Var     { spaced : Spaced                                                 },
    Cons    { spaced : Spaced                                                 },
    Opr     { spaced : Spaced     , max_prec : Option<usize>                  },
    Mod     { spaced : Spaced                                                 },
    Num     { spaced : Spaced                                                 },
    Text    { spaced : Spaced                                                 },
    Block   { spaced : Spaced                                                 },
    Macro   { spaced : Spaced                                                 },
    Invalid { spaced : Spaced                                                 },
}

#[ast] pub enum PatternClass { Normal, Pattern }
pub type Spaced = Option<bool>;

#[derive(Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub enum Either<L,R> { Left(L), Right(R) }
pub type Switch<T> = Either<T,T>;

pub type MacroPatternMatch<T> = Rc<FooRaw<T>>;
#[ast] pub enum FooRaw<T> {

    // === Boundary Matches ===
    Begin   { pat: MacroPatternRawBegin },
    End     { pat: MacroPatternRawEnd   },

    // === Structural Matches ===
    Nothing { pat: MacroPatternRawNothing                                     },
    Seq     { pat: MacroPatternRawSeq     , elem: (MacroPatternMatch<T>,
                                                   MacroPatternMatch<T>)      },
    Or      { pat: MacroPatternRawOr      , elem: Switch<MacroPatternMatch<T>>},
    Many    { pat: MacroPatternRawMany    , elem: Vec<MacroPatternMatch<T>>   },
    Except  { pat: MacroPatternRawExcept  , elem: MacroPatternMatch<T>        },

    // === Meta Matches ===
    Build   { pat: MacroPatternRawBuild   , elem: T                           },
    Err     { pat: MacroPatternRawErr     , elem: T                           },
    Tag     { pat: MacroPatternRawTag     , elem: MacroPatternMatch<T>        },
    Cls     { pat: MacroPatternRawCls     , elem: MacroPatternMatch<T>        },

    // === Token Matches ===
    Tok     { pat: MacroPatternRawTok     , elem: T                           },
    Blank   { pat: MacroPatternRawBlank   , elem: T                           },
    Var     { pat: MacroPatternRawVar     , elem: T                           },
    Cons    { pat: MacroPatternRawCons    , elem: T                           },
    Opr     { pat: MacroPatternRawOpr     , elem: T                           },
    Mod     { pat: MacroPatternRawMod     , elem: T                           },
    Num     { pat: MacroPatternRawNum     , elem: T                           },
    Text    { pat: MacroPatternRawText    , elem: T                           },
    Block   { pat: MacroPatternRawBlock   , elem: T                           },
    Macro   { pat: MacroPatternRawMacro   , elem: T                           },
    Invalid { pat: MacroPatternRawInvalid , elem: T                           },

}


// =============================================================================
// === Spaceless AST ===========================================================
// =============================================================================

#[ast] pub struct Comment {
    pub lines: Vec<String>
}

#[ast] pub struct Import<T> {
    pub lines: Vec<T>
}

#[ast] pub struct Mixfix<T> {
    pub name: Vec<T>,
    pub args: Vec<T>,
}

#[ast] pub struct Group<T> {
    pub body: Option<T>,
}

#[ast] pub struct Def<T> {
    pub name: Cons,
    pub args: Vec<T>,
    pub body: Option<T>
}

#[ast] pub struct Foreign {
    pub indent : usize,
    pub lang   : String,
    pub code   : Vec<String>
}


// ===========
// === AST ===
// ===========

// === HasSpan ===

/// Things that can be asked about their span.
pub trait HasSpan {
    fn span(&self) -> usize;
}

/// Counts codepoints.
impl HasSpan for String {
    fn span(&self) -> usize {
        self.as_str().span()
    }
}

/// Counts codepoints.
impl HasSpan for &str {
    fn span(&self) -> usize {
        self.chars().count()
    }
}

// === WithID ===

pub type ID = Uuid;

pub trait HasID {
    fn id(&self) -> Option<ID>;
}

#[derive(Eq, PartialEq, Debug, Shrinkwrap, Serialize, Deserialize)]
#[shrinkwrap(mutable)]
pub struct WithID<T> {
    #[shrinkwrap(main_field)]
    #[serde(flatten)]
    pub wrapped: T,
    pub id: Option<ID>
}

impl<T> HasID for WithID<T>
    where T: HasID {
    fn id(&self) -> Option<ID> {
        self.id
    }
}

impl<T, S:Layer<T>>
Layer<T> for WithID<S> {
    fn layered(t: T) -> Self {
        WithID { wrapped: Layer::layered(t), id: None }
    }
}

impl<T> HasSpan for WithID<T>
where T: HasSpan {
    fn span(&self) -> usize {
        self.deref().span()
    }
}

// === WithSpan ===

/// Stores a value of type `T` and information about its span.
///
/// Even if `T` is `Spanned`, keeping `span` variable is desired for performance
/// purposes.
#[derive(Eq, PartialEq, Debug, Shrinkwrap, Serialize, Deserialize)]
#[shrinkwrap(mutable)]
pub struct WithSpan<T> {
    #[shrinkwrap(main_field)]
    #[serde(flatten)]
    pub wrapped: T,
    pub span: usize
}

impl<T> HasSpan for WithSpan<T> {
    fn span(&self) -> usize { self.span }
}

impl<T, S> Layer<T> for WithSpan<S>
where T: HasSpan + Into<S> {
    fn layered(t: T) -> Self {
        let span = t.span();
        WithSpan { wrapped: t.into(), span }
    }
}

impl<T> HasID for WithSpan<T>
    where T: HasID {
    fn id(&self) -> Option<ID> {
        self.deref().id()
    }
}


// =============================================================================
// === TO BE GENERATED =========================================================
// =============================================================================
// TODO: the definitions below should be removed and instead generated using
//  macros, as part of https://github.com/luna/enso/issues/338

// === AST ===

impl Ast {
    // TODO smart constructors for other cases
    //  as part of https://github.com/luna/enso/issues/338
    pub fn var(name: String) -> Ast {
        let var = Var{ name };
        Ast::from(var)
    }
}

// === Shape ===

impl<T> Shape<T> {
    pub fn iter(&self) -> Box<dyn Iterator<Item = &'_ T> + '_> {
        // TODO: use child's derived iterator
        //  as part of https://github.com/luna/enso/issues/338
        unimplemented!()
    }
}

impl<T> HasSpan for Shape<T> {
    // TODO: sum spans of all members
    //  as part of https://github.com/luna/enso/issues/338
    fn span(&self) -> usize {
        match self {
            Shape::Var(var) => var.span(),
            _               => 0,
        }
    }
}

// === Var ===

impl HasSpan for Var {
    fn span(&self) -> usize {
        self.name.span()
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
        let json_str            = serde_json::to_string(&input_val).unwrap();
        let deserialized_val: T = serde_json::from_str(&json_str).unwrap();
        assert_eq!(*input_val, deserialized_val);
    }

    #[test]
    fn var_smart_constructor() {
        let name = "foo".to_string();
        let v    = Ast::var(name.clone());
        match v.shape() {
            Shape::Var(var) if *var.name == name =>
                (),
            _ =>
                panic!("expected Var with name `{}`", name),
        }
    }

    #[test]
    fn ast_wrapping() {
        // We can convert `Var` into AST without worrying about span nor id.
        let sample_name = "foo".to_string();
        let v = Var{ name: sample_name.clone() };
        let ast = Ast::from(v);
        assert_eq!(ast.wrapped.id, None);
        assert_eq!(ast.wrapped.wrapped.span, sample_name.span());
    }

    #[test]
    fn serialization_round_trip() {
        let make_var = || Var { name: "foo".into() };
        round_trips(&make_var());

        let ast_without_id = Ast::new(make_var(), None);
        round_trips(&ast_without_id);

        let id        = Uuid::parse_str("15").ok();
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
        let ast: Ast         = serde_json::from_str(&sample_json_text).unwrap();

        let expected_uuid = Uuid::parse_str(uuid_str).ok();
        assert_eq!(ast.id, expected_uuid);

        let expected_span = 3;
        assert_eq!(ast.span, expected_span);

        let expected_var   = Var { name: var_name.into() };
        let expected_shape = Shape::from(expected_var);
        assert_eq!(*ast.shape(), expected_shape);
    }
}
