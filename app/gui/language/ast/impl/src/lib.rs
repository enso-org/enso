// === Features ===
#![feature(associated_type_bounds)]
#![feature(generators, generator_trait)]
#![feature(trivial_bounds)]
#![feature(type_alias_impl_trait)]
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use crate::prelude::*;
use ast_macros::*;
use enso_shapely::*;
use enso_text::index::*;
use enso_text::traits::*;
use enso_text::unit::*;

use uuid::Uuid;


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

pub use crumbs::Crumb;
pub use crumbs::Crumbs;
pub use id_map::IdMap;

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



// ===============
// === Shifted ===
// ===============

/// A value of type `T` annotated with offset value `off`.
#[derive(Clone, Eq, PartialEq, Debug, Deref, DerefMut, Iterator)]
pub struct Shifted<T> {
    #[deref]
    #[deref_mut]
    pub wrapped: T,
    pub off:     usize,
}

/// A non-empty sequence of `T`s interspersed by offsets.
#[derive(Clone, Eq, PartialEq, Debug, Iterator)]
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
#[derive(Debug, Deref, DerefMut)]
pub struct Layered<T>(pub T);

impl<T> Layer<T> for Layered<T> {
    fn layered(t: T) -> Self {
        Layered(t)
    }
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
#[derive(CloneRef, Eq, PartialEq, Deref)]
pub struct Ast {
    wrapped: Rc<WithID<WithLength<Shape<Ast>>>>,
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

/// Custom `Debug` implementation that flattens the `WithID` and `WithLength` wrappers.
impl Debug for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ast")
            .field("id", &self.id)
            .field("length", &self.length)
            .field("shape", &self.shape())
            .finish()
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
    pub fn from_ast_id_len(shape: Shape<Ast>, id: Option<Id>, char_count: usize) -> Ast {
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

    pub fn find_many_by_ids<IdLike>(
        &self,
        ids: impl IntoIterator<Item = IdLike>,
    ) -> impl Iterator<Item = &Ast>
    where
        IdLike: std::borrow::Borrow<Id> + Eq + Hash,
    {
        let ids_set: HashSet<IdLike> = ids.into_iter().collect();
        self.iter_recursive().filter(move |ast| ast.id.map_or(false, |id| ids_set.contains(&id)))
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
    pub fn child_offset(&self, child: &Ast) -> FallibleResult<Byte> {
        let searched_token = Token::Ast(child);
        let mut found_child = false;
        let mut position = 0.byte();
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
    pub fn span_of_child_at(&self, crumb: &Crumb) -> FallibleResult<enso_text::Range<Byte>> {
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



// =============
// === Shape ===
// =============

/// Defines shape of the subtree. Parametrized by the child node type `T`.
///
/// Shape describes names of children and spacing between them.
#[ast(flat)]
#[derive(HasTokens)]
pub enum Shape<T> {
    // === Identifiers ===
    Blank {},
    Var {
        name: String,
    },
    Cons {
        name: String,
    },
    Opr {
        name:        String,
        right_assoc: bool,
    },
    Annotation {
        name: String,
    },
    Mod {
        name: String,
    },

    // === Number ===
    Number {
        base: Option<String>,
        int:  String,
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
        /// Absolute's block indent, counting from the module's root.
        indent:      usize,
        /// Leading empty lines. Each line is represented by absolute count of spaces
        /// it contains, counting from the root.
        empty_lines: Vec<usize>,
        /// First line with non-empty item.
        first_line:  BlockLine<T>,
        /// Rest of lines, each of them optionally having contents.
        lines:       Vec<BlockLine<Option<T>>>,
    },
    Tree {
        /// The information needed to iterate child tokens and nodes.
        span_info:        Vec<SpanSeed<T>>,
        /// Semantic information about the node.
        type_info:        TreeType,
        /// Information needed to produce a representation, for nodes whose contents are not child
        /// nodes.
        leaf_info:        Option<String>,
        /// Suitable for naming variables referring to this node.
        descriptive_name: Option<&'static str>,
        /// Extra characters after the node (e.g. a comment).
        trailing_token:   Option<String>,
    },
}

/// Macro that calls its argument (possibly other macro
#[macro_export]
macro_rules! with_shape_variants {
    ($f:ident) => {
        $f! {
          [Blank] [Var] [Cons] [Opr] [Annotation] [Mod]
          [Number]
          [Prefix Ast] [Infix Ast] [SectionLeft Ast] [SectionRight Ast] [SectionSides Ast]
          [Module Ast] [Block Ast]
          [Tree Ast]
        }
    };
}

/// Define variant name getter for all shapes.
macro_rules! generate_variant_name {
    ( $([$name:ident $($rest:tt)* ])* ) => {
        impl<T> Shape<T> {
            pub fn variant_name(&self) -> &'static str {
                match self {
                    $(Shape::$name { .. } => stringify!($name),)*
                }
            }
        }
    };
}

with_shape_variants!(generate_variant_name);


// === [`Tree`] data ===

/// Low-level builders.
impl<T> Tree<T> {
    pub fn expression(span_info: Vec<SpanSeed<T>>) -> Self {
        Self {
            span_info,
            type_info: default(),
            leaf_info: default(),
            descriptive_name: default(),
            trailing_token: default(),
        }
    }

    pub fn leaf(leaf_info: String) -> Self {
        Self {
            span_info:        default(),
            type_info:        default(),
            leaf_info:        Some(leaf_info),
            descriptive_name: default(),
            trailing_token:   None,
        }
    }

    pub fn with_type_info(self, type_info: TreeType) -> Self {
        let Self { span_info, type_info: _, leaf_info, descriptive_name, trailing_token } = self;
        Self { span_info, type_info, leaf_info, descriptive_name, trailing_token }
    }

    pub fn with_descriptive_name(self, descriptive_name: &'static str) -> Self {
        let Self { span_info, type_info, leaf_info, descriptive_name: _, trailing_token } = self;
        Self {
            span_info,
            type_info,
            leaf_info,
            descriptive_name: Some(descriptive_name),
            trailing_token,
        }
    }

    pub fn with_trailing_token(self, trailing_token: String) -> Self {
        let Self { span_info, type_info, leaf_info, descriptive_name, trailing_token: _ } = self;
        Self {
            span_info,
            type_info,
            leaf_info,
            descriptive_name,
            trailing_token: Some(trailing_token),
        }
    }
}

/// High-level helper builders.
impl<T> Tree<T> {
    pub fn lambda(span_info: Vec<SpanSeed<T>>) -> Self {
        Tree::expression(span_info).with_type_info(TreeType::Lambda)
    }

    pub fn group(span_info: Vec<SpanSeed<T>>) -> Self {
        Tree::expression(span_info).with_type_info(TreeType::Group)
    }

    pub fn text(leaf_info: String) -> Self {
        Tree::leaf(leaf_info).with_descriptive_name("text")
    }

    pub fn expression_with_comment(expression: Option<T>, space: usize, comment: String) -> Self {
        let mut span_info = vec![];
        span_info.extend(expression.map(SpanSeed::child));
        span_info.extend(SpanSeed::space(space));
        Tree::expression(span_info)
            .with_type_info(TreeType::ExpressionWithComment)
            .with_trailing_token(comment)
    }
}

/// Helper getters.
impl<T> Tree<T> {
    /// Retrieves the string content of the Tree node, if any.
    pub fn as_text(&self) -> Option<&str> {
        if self.type_info == TreeType::Expression && self.descriptive_name == Some("text") {
            self.leaf_info.as_deref().and_then(|s| {
                if s.starts_with(repr::FMT_BLOCK_QUOTES) || s.starts_with(repr::RAW_BLOCK_QUOTES) {
                    s.get(3..s.len() - 3)
                } else if s.starts_with(repr::FMT_QUOTE) || s.starts_with(repr::RAW_QUOTE) {
                    s.get(1..s.len() - 1)
                } else {
                    None
                }
            })
        } else {
            None
        }
    }
}

/// The semantic information about a node.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum TreeType {
    /// A normal expression.
    #[default]
    Expression,
    /// A documentation-comment.
    Documentation { rendered: ImString },
    /// An import declaration.
    Import { module: Vec<ImString>, imported: ImportedNames },
    /// A lambda.
    Lambda,
    /// A parenthesized expression.
    Group,
    /// A comment at the end of a line, possibly following an expression.
    ExpressionWithComment,
}

/// Describes the names imported by an import declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportedNames {
    Module { alias: Option<String> },
    All { except: std::collections::BTreeSet<String> },
    List { names: std::collections::BTreeSet<String> },
}

/// Represents the syntax tree, and its correspondence to the source text; with context information
/// provided by an evaluator, this can be used to produce a complete [`SpanTree`].
#[ast]
pub enum SpanSeed<T> {
    Space { space: usize },
    Token { token: String },
    Child { node: T },
}

impl<T> SpanSeed<T> {
    pub fn space(space: usize) -> Option<Self> {
        match space {
            0 => None,
            space => Some(SpanSeed::Space(SpanSeedSpace { space })),
        }
    }

    pub fn child(node: T) -> Self {
        Self::Child(SpanSeedChild { node })
    }

    pub fn token(token: String) -> Self {
        Self::Token(SpanSeedToken { token })
    }

    pub fn is_child(&self) -> bool {
        matches!(self, SpanSeed::Child { .. })
    }
}



// =============
// === Block ===
// =============

/// Holder for line in `Block` or `Module`. Lines store value of `T` and trailing whitespace info.
#[ast]
pub struct BlockLine<T> {
    /// The AST stored in the line.
    pub elem: T,
    /// The trailing whitespace in the line after the `elem`.
    pub off:  usize,
}



// ===========
// === AST ===
// ===========


// === Tokenizer ===

/// An enum of valid Ast tokens.
#[derive(Clone, Debug, PartialEq, Eq)]
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
        self.item().feed_to(consumer);
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
    offset: Byte,
}

impl TokenConsumer for IdMapBuilder {
    fn feed(&mut self, token: Token) {
        match token {
            Token::Off(val) => self.offset += Byte::from(' '.len_utf8() * val),
            Token::Chr(_) => self.offset += 1.byte(),
            Token::Str(val) => self.offset += Byte::from(val.len()),
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
        self.repr().len().bytes()
    }

    /// Check if the representation is empty.
    fn is_empty(&self) -> bool {
        self.len() <= 0.bytes()
    }

    /// Get the representation length in chars.
    ///
    /// May be implemented in a quicker way than building string. Must meet the constraint
    /// `x.char_count() == x.repr().chars().count()` for any `x: impl HasRepr`.
    fn char_count(&self) -> usize {
        self.repr().chars().count()
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
            Token::Off(val) => self.length += (' '.len_utf8() * val).bytes(),
            Token::Chr(chr) => self.length += chr.len_utf8().bytes(),
            Token::Str(val) => self.length += val.len().bytes(),
            Token::Ast(val) => val.shape().feed_to(self),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct CharCountBuilder {
    char_count: usize,
}


impl TokenConsumer for CharCountBuilder {
    fn feed(&mut self, token: Token) {
        match token {
            Token::Off(val) => self.char_count += val,
            Token::Chr(_) => self.char_count += 1,
            Token::Str(val) => self.char_count += val.chars().count(),
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

    fn char_count(&self) -> usize {
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

#[derive(Eq, PartialEq, Debug, Deref, DerefMut)]
pub struct WithID<T> {
    #[deref]
    #[deref_mut]
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

    fn char_count(&self) -> usize {
        self.deref().char_count()
    }
}



#[derive(Debug, Clone)]
struct TraverserWithOffset<F> {
    offset:   usize,
    callback: F,
}

impl<F> TraverserWithOffset<F> {
    pub fn new(callback: F) -> TraverserWithOffset<F> {
        let offset = 0;
        TraverserWithOffset { offset, callback }
    }
}

impl<F> TokenConsumer for TraverserWithOffset<F>
where F: FnMut(usize, &Ast)
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
pub fn traverse_with_offset(ast: &impl HasTokens, f: impl FnMut(usize, &Ast)) {
    let mut traverser = TraverserWithOffset::new(f);
    ast.feed_to(&mut traverser);
}

/// Visits each Ast node, while keeping track of its span.
pub fn traverse_with_span(ast: &impl HasTokens, mut f: impl FnMut(enso_text::Range<usize>, &Ast)) {
    traverse_with_offset(ast, move |offset, ast| {
        f(enso_text::Range::new(offset, offset + ast.char_count()), ast)
    })
}

// === WithLength ===

/// Stores a value of type `T` and information about its length.
///
/// Even if `T` is `Spanned`, keeping `length` variable is desired for performance
/// purposes.
#[derive(Eq, PartialEq, Debug, Deref, DerefMut)]
pub struct WithLength<T> {
    #[deref]
    #[deref_mut]
    pub wrapped: T,
    pub length:  usize,
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

    fn char_count(&self) -> usize {
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
        let first_line = self.lines.iter().find_map(|line| {
            Some(BlockLine { off: line.off, elem: line.elem.as_ref()?.clone() })
        })?;
        let lines = self.lines.iter().skip_while(is_empty).skip(1).cloned().collect();
        Some(Block { indent, empty_lines, first_line, lines })
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
        let indent = 0;
        let empty_lines = Vec::new();
        let first_line = BlockLine::new(first_line.clone_ref());
        let lines = tail_lines.iter().cloned().map(BlockLine::new).collect();
        Block { indent, empty_lines, first_line, lines }
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
        let name = name.into();
        let right_assoc = crate::assoc::Assoc::of(&name) == crate::assoc::Assoc::Right;
        let opr = Opr { name, right_assoc };
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

    /// Creates an AST node with `Infix` shape, where both its operands are Vars.
    pub fn infix_var(larg: impl Str, opr: impl Str, rarg: impl Str) -> Ast {
        let infix = Infix::from_vars(larg, opr, rarg);
        Ast::from(infix)
    }

    /// Creates an AST node with `Infix` assignment to a `Var` with given name and no spacing.
    pub fn named_argument(name: impl Str, expression: impl Into<Ast>) -> Ast {
        let larg = Ast::var(name);
        let loff = 0;
        let opr = Ast::opr(opr::predefined::ASSIGNMENT);
        let roff = 0;
        let rarg = expression.into();
        let infix = Infix { larg, loff, opr, roff, rarg };
        Ast::from(infix)
    }

    /// Creates a raw text literal that evaluates to the given string.
    pub fn raw_text_literal(value: impl Str) -> Ast {
        let value: &str = value.as_ref();
        let mut escaped = String::with_capacity(value.len() + 2);
        escaped.push('\'');
        for char in value.chars() {
            match char {
                '\'' => escaped.push_str("\\'"),
                char => escaped.push(char),
            }
        }
        escaped.push('\'');
        Self::from(Tree::text(escaped))
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

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
            _ => panic!("expected Var with name `{name}`"),
        }
    }

    #[test]
    fn ast_length() {
        let ast = Ast::prefix(Ast::var("XĄ"), Ast::var("YY"));
        assert_eq!(ast.len(), 6.bytes());
        assert_eq!(ast.char_count(), 5);
    }

    #[test]
    fn ast_repr() {
        let ast = Ast::prefix(Ast::var("XĄ"), Ast::var("YY"));
        assert_eq!(ast.repr().as_str(), "XĄ YY")
    }

    #[test]
    fn ast_id_map() {
        let span = |ix: usize, length: usize| {
            enso_text::Range::<Byte>::new(ix.into(), (ix + length).into())
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
        assert_eq!(ast.wrapped.wrapped.length, ident.chars().count());
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

        assert_eq!((abc).iter().count(), 2); // for App's two children
        assert_eq!(abc.iter_recursive().count(), 5); // for 2 Apps and 3 Vars
    }

    #[test]
    fn all_lines_of_block() {
        let indent = 4;
        let empty_lines = vec![5];
        let first_line = BlockLine { elem: Ast::var("head"), off: 3 };
        let lines = vec![
            BlockLine { elem: Some(Ast::var("tail0")), off: 2 },
            BlockLine { elem: None, off: 1 },
            BlockLine { elem: Some(Ast::var("tail2")), off: 3 },
        ];
        let block = Block { indent, empty_lines, first_line, lines };
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
        assert_eq!(var.char_count(), 1);
        assert_eq!(var.len(), 3.bytes());

        let idmap = var.id_map();
        assert_eq!(idmap.vec[0].0, enso_text::Range::new(0.byte(), 3.byte()));
        assert_eq!(idmap.vec[0].1, var.id.unwrap());

        let builder_with_char = Token::Chr('壱');
        assert_eq!(builder_with_char.char_count(), 1);
        assert_eq!(builder_with_char.len(), 3.bytes());
    }
}
