//! Implementation of Syntax Tree, known as well as Abstract Syntax Tree, or AST.

use crate::prelude::*;
use crate::source::*;
use crate::syntax::*;

use crate::span_builder;

use enso_parser_syntax_tree_visitor::Visitor;
use enso_shapely_macros::tagged_enum;


// ==============
// === Export ===
// ==============

pub mod block;



// ============
// === Tree ===
// ============

/// The Abstract Syntax Tree of the language.
#[derive(Clone, Deref, DerefMut, Eq, PartialEq, Serialize, Reflect, Deserialize)]
#[allow(missing_docs)]
pub struct Tree<'s> {
    #[deref]
    #[deref_mut]
    #[reflect(subtype)]
    pub variant: Box<Variant<'s>>,
    #[reflect(flatten)]
    pub span:    Span<'s>,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Tree<'s>(span: Span<'s>, variant: impl Into<Variant<'s>>) -> Tree<'s> {
    let variant = Box::new(variant.into());
    Tree { variant, span }
}

impl<'s> Debug for Tree<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let max_code_len = 30;
        let ellipsis = "...";
        let mut code = self.code();
        if code.len() > max_code_len {
            code = format!("{}{}", &code[..max_code_len - ellipsis.len()], ellipsis);
        }
        write!(f, "[{}:{}:\"{}\"] ", self.span.left_offset.visible, self.span.code_length, code)?;
        Debug::fmt(&self.variant, f)
    }
}

impl<'s> AsRef<Span<'s>> for Tree<'s> {
    fn as_ref(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s> Default for Tree<'s> {
    fn default() -> Self {
        Self {
            variant: Box::new(Variant::Ident(Ident { token: Default::default() })),
            span:    Default::default(),
        }
    }
}

/// Macro providing [`Tree`] type definition. It is used to both define the ast [`Variant`], and to
/// define impls for every token type in other modules.
#[macro_export]
macro_rules! with_ast_definition { ($f:ident ($($args:tt)*)) => { $f! { $($args)*
    /// [`Tree`] variants definition. See its docs to learn more.
    #[tagged_enum]
    #[derive(Clone, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
    #[allow(clippy::large_enum_variant)] // Inefficient. Will be fixed in #182878443.
    #[tagged_enum(apply_attributes_to = "variants")]
    #[reflect(inline)]
    pub enum Variant<'s> {
        /// Invalid [`Tree`] fragment with an attached [`Error`].
        Invalid {
            pub error: Error,
            pub ast: Tree<'s>,
        },
        /// Indicates a subtree in which an unimplemented case was reached.
        Unsupported {
            pub error: String,
            pub ast: Tree<'s>,
        },
        /// A sequence of lines introduced by a line ending in an operator.
        BodyBlock {
            /// The lines of the block.
            pub statements: Vec<block::Line<'s>>,
        },
        /// A sequence of lines comprising the arguments of a function call.
        ArgumentBlockApplication {
            /// The expression for the value to which the arguments are to be applied.
            pub lhs: Option<Tree<'s>>,
            /// The lines of the block.
            pub arguments: Vec<block::Line<'s>>,
        },
        /// A sequence of lines comprising a tree of operator expressions.
        OperatorBlockApplication {
            /// The expression preceding the block; this will be the leftmost-leaf of the binary
            /// tree.
            pub lhs: Option<Tree<'s>>,
            /// The lines of the block.
            pub expressions: Vec<block::OperatorLine<'s>>,
            /// Lines that appear lexically within the block, but are not syntactically consistent
            /// with an operator block.
            pub excess: Vec<block::Line<'s>>,
        },
        /// A simple identifier, like `foo` or `bar`.
        Ident {
            pub token: token::Ident<'s>,
        },
        /// A numeric literal, like `10`.
        Number {
            pub token: token::Number<'s>,
        },
        TextLiteral {
            pub open_quote: Option<token::TextStart<'s>>,
            pub elements: Vec<TextElement<'s>>,
            /// Conditions when this is `None`:
            /// - Block literal: Always.
            /// - Inline literal: On error: EOL or EOF occurred without the string being closed.
            pub close_quote: Option<token::TextEnd<'s>>,
            pub trim: VisibleOffset,
        },
        /// A simple application, like `print "hello"`.
        App {
            pub func: Tree<'s>,
            pub arg: Tree<'s>,
        },
        /// Application of an operator, like `a + b`. The left or right operands might be missing,
        /// thus creating an operator section like `a +`, `+ b`, or simply `+`. See the
        /// [`OprSectionBoundary`] variant to learn more about operator section scope.
        OprApp {
            pub lhs: Option<Tree<'s>>,
            pub opr: OperatorOrError<'s>,
            pub rhs: Option<Tree<'s>>,
        },
        /// Application of a unary operator, like `-a` or `~handler`. It is a syntax error for `rhs`
        /// to be `None`.
        UnaryOprApp {
            pub opr: token::Operator<'s>,
            pub rhs: Option<Tree<'s>>,
        },
        /// Defines the point where operator sections should be expanded to lambdas. Let's consider
        /// the expression `map (.sum 1)`. It should be desugared to `map (x -> x.sum 1)`, not to
        /// `map ((x -> x.sum) 1)`. The expression `.sum` will be parsed as operator section
        /// ([`OprApp`] with left operand missing), and the [`OprSectionBoundary`] will be placed
        /// around the whole `.sum 1` expression.
        OprSectionBoundary {
            pub ast: Tree<'s>,
        },
        /// An application of a multi-segment function, such as `if ... then ... else ...`. Each
        /// segment starts with a token and contains an expression. Some multi-segment functions can
        /// have a prefix, an expression that is argument of the function, but is placed before the
        /// first token. Lambda is a good example for that. In an expression
        /// `Vector x y z -> x + y + z`, the `->` token is the beginning of the section, the
        /// `x + y + z` is the section body, and `Vector x y z` is the prefix of this function
        /// application.
        MultiSegmentApp {
            pub segments: NonEmptyVec<MultiSegmentAppSegment<'s>>,
        },
        /// A type definition; introduced by a line consisting of the keyword `type`, an identifier
        /// to be used as the name of the type, and zero or more specifications of type parameters.
        /// The following indented block contains two types of lines:
        /// - First zero or more type constructors, and their subordinate blocks.
        /// - Then a block of statements, which may define methods or type methods.
        TypeDef {
            pub keyword: Token<'s>,
            pub name: Tree<'s>,
            pub params: Vec<Tree<'s>>,
            pub constructors: Vec<TypeConstructorLine<'s>>,
            pub block: Vec<block::Line<'s>>,
        },
        /// A variable assignment, like `foo = bar 23`.
        Assignment {
            /// The pattern which should be unified with the expression.
            pub pattern: Tree<'s>,
            /// The `=` token.
            pub equals: token::Operator<'s>,
            /// The expression initializing the value(s) in the pattern.
            pub expr: Tree<'s>,
        },
        /// A function definition, like `add x y = x + y`.
        Function {
            /// The identifier to which the function should be bound.
            pub name: token::Ident<'s>,
            /// The argument patterns.
            pub args: Vec<Tree<'s>>,
            /// The `=` token.
            pub equals: token::Operator<'s>,
            /// The body, which will typically be an inline expression or a `BodyBlock` expression.
            /// It is an error for this to be empty.
            pub body: Option<Tree<'s>>,
        },
        /// An import statement.
        Import {
            pub polyglot:  Option<MultiSegmentAppSegment<'s>>,
            pub from:      Option<MultiSegmentAppSegment<'s>>,
            pub from_as:   Option<MultiSegmentAppSegment<'s>>,
            pub import:    MultiSegmentAppSegment<'s>,
            pub import_as: Option<MultiSegmentAppSegment<'s>>,
            pub hiding:    Option<MultiSegmentAppSegment<'s>>,
        },
        /// An export statement.
        Export {
            pub from:      Option<MultiSegmentAppSegment<'s>>,
            pub from_as:   Option<MultiSegmentAppSegment<'s>>,
            pub export:    MultiSegmentAppSegment<'s>,
            pub export_as: Option<MultiSegmentAppSegment<'s>>,
            pub hiding:    Option<MultiSegmentAppSegment<'s>>,
        },
        /// An expression grouped by matched parentheses.
        Group {
            pub open:  token::Symbol<'s>,
            pub body:  Option<Tree<'s>>,
            pub close: token::Symbol<'s>,
        },
        /// Statement declaring the type of a variable.
        TypeSignature {
            /// The variable whose type is being declared.
            pub variable: token::Ident<'s>,
            /// The `:` token.
            pub operator: token::Operator<'s>,
            /// The variable's type.
            #[reflect(rename = "type")]
            pub type_: Tree<'s>,
        },
        /// An expression with explicit type information attached.
        TypeAnnotated {
            /// The expression whose type is being annotated.
            pub expression: Tree<'s>,
            /// The `:` token.
            pub operator: token::Operator<'s>,
            /// The expression's type.
            #[reflect(rename = "type")]
            pub type_: Tree<'s>,
        },
    }
}};}


macro_rules! generate_variant_constructors {
    (
        $(#$enum_meta:tt)*
        pub enum $enum:ident<'s> {
            $(
                $(#$variant_meta:tt)*
                $variant:ident $({$($(#$field_meta:tt)* pub $field:ident : $field_ty:ty),* $(,)? })?
            ),* $(,)?
        }
    ) => { paste! {
        impl<'s> Tree<'s> {
            $(
                /// Constructor.
                pub fn [<$variant:snake:lower>]($($(mut $field : $field_ty),*)?) -> Self {
                    let span = span_builder![$($($field),*)?];
                    Tree(span, $variant($($($field),*)?))
                }
            )*
        }
    }};
}

macro_rules! generate_ast_definition {
    ($($ts:tt)*) => {
        $($ts)*
        generate_variant_constructors!{$($ts)*}
    };
}

with_ast_definition!(generate_ast_definition());


// === Invalid ===

/// Error of parsing attached to an [`Tree`] node.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
#[allow(missing_docs)]
#[reflect(transparent)]
#[serde(from = "crate::serialization::Error")]
pub struct Error {
    #[serde(skip_deserializing)]
    pub message: Cow<'static, str>,
}

impl Error {
    /// Constructor.
    pub fn new(message: impl Into<Cow<'static, str>>) -> Self {
        let message = message.into();
        Self { message }
    }
}

impl<'s> Tree<'s> {
    /// Constructor.
    pub fn with_error(self, message: impl Into<Cow<'static, str>>) -> Self {
        Tree::invalid(Error::new(message), self)
    }

    /// Constructor.
    pub fn with_unsupported(self, message: String) -> Self {
        eprintln!("Unsupported syntax: {}", &message);
        Tree::unsupported(message, self)
    }
}

impl<'s> span::Builder<'s> for Error {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span
    }
}


// === Type Definitions ===

/// A line within a type definition, containing a type constructor definition.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct TypeConstructorLine<'s> {
    /// The token beginning the line.
    pub newline:    token::Newline<'s>,
    /// The type constructor definition, unless this is an empty line.
    pub expression: Option<TypeConstructorDef<'s>>,
}

impl<'s> span::Builder<'s> for TypeConstructorLine<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.newline).add(&mut self.expression)
    }
}

impl<'s> From<token::Newline<'s>> for TypeConstructorLine<'s> {
    fn from(newline: token::Newline<'s>) -> Self {
        Self { newline, expression: None }
    }
}

/// A type constructor definition within a type definition.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct TypeConstructorDef<'s> {
    /// The identifier naming the type constructor.
    pub constructor: token::Ident<'s>,
    /// The arguments the type constructor accepts, specified inline.
    pub arguments:   Vec<Tree<'s>>,
    /// The arguments the type constructor accepts, specified on their own lines.
    pub block:       Vec<block::Line<'s>>,
}

impl<'s> span::Builder<'s> for TypeConstructorDef<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.constructor).add(&mut self.arguments).add(&mut self.block)
    }
}


// === Text literals ===

/// A component of a text literal, within the quotation marks.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub enum TextElement<'s> {
    /// The text content of the literal. If it is multiline, the offset information may contain
    /// part of the content, after trimming appropriately.
    Section {
        /// The text content.
        text: token::TextSection<'s>,
    },
    /// A \ character.
    Escape {
        /// The \ character.
        backslash: token::TextEscape<'s>,
    },
}

impl<'s> span::Builder<'s> for TextElement<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        match self {
            TextElement::Section { text } => text.add_to_span(span),
            TextElement::Escape { backslash } => backslash.add_to_span(span),
        }
    }
}

impl<'s, 'a> TreeVisitable<'s, 'a> for VisibleOffset {}
impl<'s, 'a> TreeVisitableMut<'s, 'a> for VisibleOffset {}
impl<'a, 't, 's> SpanVisitable<'s, 'a> for VisibleOffset {}
impl<'a, 't, 's> SpanVisitableMut<'s, 'a> for VisibleOffset {}
impl<'a, 't, 's> ItemVisitable<'s, 'a> for VisibleOffset {}
impl<'s> span::Builder<'s> for VisibleOffset {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span
    }
}


// === OprApp ===

/// Operator or [`MultipleOperatorError`].
pub type OperatorOrError<'s> = Result<token::Operator<'s>, MultipleOperatorError<'s>>;

/// Error indicating multiple operators found next to each other, like `a + * b`.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
#[allow(missing_docs)]
pub struct MultipleOperatorError<'s> {
    pub operators: NonEmptyVec<token::Operator<'s>>,
}

impl<'s> span::Builder<'s> for MultipleOperatorError<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        self.operators.add_to_span(span)
    }
}

/// A sequence of one or more operators.
pub trait NonEmptyOperatorSequence<'s> {
    /// Return a reference to the first operator.
    fn first_operator(&self) -> &token::Operator<'s>;
    /// Return a mutable reference to the first operator.
    fn first_operator_mut(&mut self) -> &mut token::Operator<'s>;
}

impl<'s> NonEmptyOperatorSequence<'s> for OperatorOrError<'s> {
    fn first_operator(&self) -> &token::Operator<'s> {
        match self {
            Ok(opr) => opr,
            Err(oprs) => oprs.operators.first(),
        }
    }
    fn first_operator_mut(&mut self) -> &mut token::Operator<'s> {
        match self {
            Ok(opr) => opr,
            Err(oprs) => oprs.operators.first_mut(),
        }
    }
}


// === MultiSegmentApp ===

/// A segment of [`MultiSegmentApp`], like `if cond` in the `if cond then ok else fail` expression.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
#[allow(missing_docs)]
pub struct MultiSegmentAppSegment<'s> {
    pub header: Token<'s>,
    pub body:   Option<Tree<'s>>,
}

impl<'s> span::Builder<'s> for MultiSegmentAppSegment<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.header).add(&mut self.body)
    }
}



// ====================================
// === Tree-construction operations ===
// ====================================

/// Join two nodes with a new node appropriate for their types.
///
/// For most input types, this simply constructs an `App`; however, for some operand types
/// application has special semantics.
pub fn apply<'s>(mut func: Tree<'s>, mut arg: Tree<'s>) -> Tree<'s> {
    match &mut *func.variant {
        Variant::OprApp(app)
        if let Ok(opr) = &app.opr && !opr.can_form_section && app.rhs.is_none() => {
            app.rhs = Some(arg);
            return func;
        }
        Variant::TextLiteral(lhs) if let Variant::TextLiteral(rhs) = &mut *arg.variant
                && lhs.close_quote.is_none() && rhs.open_quote.is_none() => {
            match rhs.elements.first_mut() {
                Some(TextElement::Section { text }) => text.left_offset = arg.span.left_offset,
                Some(TextElement::Escape { backslash }) =>
                    backslash.left_offset = arg.span.left_offset,
                None => (),
            }
            lhs.elements.append(&mut rhs.elements);
            lhs.close_quote = rhs.close_quote.take();
            if rhs.trim != VisibleOffset(0) && (lhs.trim == VisibleOffset(0) || rhs.trim < lhs.trim) {
                lhs.trim = rhs.trim;
            }
            return func;
        }
        Variant::TextLiteral(lhs) if let Variant::TextLiteral(rhs) = &mut *arg.variant => {
            panic!("lhs: {:?}, rhs: {:?}", lhs, rhs)
        }
        _ => (),
    }
    match &mut *arg.variant {
        Variant::ArgumentBlockApplication(block) if block.lhs.is_none() => {
            block.lhs = Some(func);
            arg
        }
        Variant::OperatorBlockApplication(block) if block.lhs.is_none() => {
            block.lhs = Some(func);
            arg
        }
        _ => Tree::app(func, arg),
    }
}

/// Join two nodes with an operator, in a way appropriate for their types.
///
/// For most operands this will simply construct an `OprApp`; however, a non-operator block (i.e. an
/// `ArgumentBlock`) is reinterpreted as a `BodyBlock` when it appears in the RHS of an operator
/// expression.
pub fn apply_operator<'s>(
    lhs: Option<Tree<'s>>,
    opr: Vec<token::Operator<'s>>,
    mut rhs: Option<Tree<'s>>,
) -> Tree<'s> {
    let opr = match opr.len() {
        0 => return apply(lhs.unwrap(), rhs.unwrap()),
        1 => Ok(opr.into_iter().next().unwrap()),
        _ => Err(MultipleOperatorError { operators: NonEmptyVec::try_from(opr).unwrap() }),
    };
    if let Ok(opr_) = &opr && opr_.is_type_annotation {
        let (lhs, rhs) = match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => (lhs, rhs),
            (lhs, rhs) => {
                let invalid = Tree::opr_app(lhs, opr, rhs);
                let err = Error::new("`:` operator must be applied to two operands.");
                return Tree::invalid(err, invalid);
            }
        };
        return Tree::type_annotated(lhs, opr.unwrap(), rhs);
    }
    if let Some(rhs_) = rhs.as_mut() {
        if let Variant::ArgumentBlockApplication(block) = &mut *rhs_.variant {
            if block.lhs.is_none() {
                let ArgumentBlockApplication { lhs: _, arguments } = block;
                let arguments = mem::take(arguments);
                let rhs_ = block::body_from_lines(arguments);
                rhs = Some(rhs_);
            }
        }
    }
    Tree::opr_app(lhs, opr, rhs)
}



// ================
// === Visitors ===
// ================

/// The visitor pattern for [`AST`].
///
/// # Visitor traits
/// There are several visitor traits defined allowing for traversal of specific AST elements, such
/// as AST nodes ([`TreeVisitor`]), span information ([`SpanVisitor`]), and AST nodes or tokens
/// altogether ([`ItemVisitor`]). A visitor is a struct that is modified when traversing the target
/// elements. Visitors are also capable of tracking when they entered or exited a nested
/// [`Tree`] structure, and they can control how deep the traversal should be performed. To learn
/// more, see the [`RefCollectorVisitor`] implementation, which traverses [`Tree`] and collects
/// references to all [`Tree`] nodes in a vector.
///
/// # Visitable traits
/// This macro also defines visitable traits, such as [`TreeVisitable`] or [`SpanVisitable`], which
/// provide [`Tree`] elements with such functions as [`visit`], [`visit_mut`], [`visit_span`], or
/// [`visit_span_mut`]. These functions let you run visitors. However, as defining a visitor is
/// relatively complex, a set of traversal functions are provided, such as [`map`], [`map_mut`],
/// [`map_span`], or [`map_span_mut`].
///
/// # Generalization of the implementation
/// The current implementation bases on a few non-generic traits. One might define a way better
/// implementation (causing way less boilerplate), such as:
/// ```text
/// pub trait Visitor<T> {
///     fn visit(&mut self, elem: &T);
/// }
/// ```
/// Such definition could be implemented for every [`Tree`] node (the [`T`] parameter).
/// Unfortunately, due to Rust compiler errors, Rust is not able to compile such a definition. We
/// could move to it as soon as this error gets resolved:
/// https://github.com/rust-lang/rust/issues/96634.
#[allow(missing_docs)]
pub trait Visitor {
    fn before_visiting_children(&mut self) {}
    fn after_visiting_children(&mut self) {}
}

/// The visitor trait allowing for [`Tree`] nodes traversal.
#[allow(missing_docs)]
pub trait TreeVisitor<'s, 'a>: Visitor {
    fn visit(&mut self, ast: &'a Tree<'s>) -> bool;
}

/// The visitor trait allowing for [`Tree`] nodes mutable traversal.
#[allow(missing_docs)]
pub trait TreeVisitorMut<'s>: Visitor {
    fn visit_mut(&mut self, ast: &mut Tree<'s>) -> bool;
}

/// The visitor trait allowing for [`Span`] traversal.
#[allow(missing_docs)]
pub trait SpanVisitor<'s, 'a>: Visitor {
    fn visit(&mut self, ast: span::Ref<'s, 'a>) -> bool;
}

/// The visitor trait allowing for [`Span`] mutable traversal.
#[allow(missing_docs)]
pub trait SpanVisitorMut<'s>: Visitor {
    fn visit_mut(&mut self, ast: span::RefMut<'s, '_>) -> bool;
}

/// The visitor trait allowing for [`Item`] traversal.
#[allow(missing_docs)]
pub trait ItemVisitor<'s, 'a>: Visitor {
    fn visit_item(&mut self, ast: item::Ref<'s, 'a>) -> bool;
}

macro_rules! define_visitor {
    ($name:ident, $visit:ident) => {
        define_visitor_no_mut! {$name, $visit}
        define_visitor_mut! {$name, $visit}
    };
}

macro_rules! define_visitor_no_mut {
    ($name:ident, $visit:ident) => {
        paste! {
            define_visitor_internal! {
                $name,
                $visit,
                [[<$name Visitor>]<'s, 'a>],
                [<$name Visitable>],
            }
        }
    };
}

macro_rules! define_visitor_mut {
    ($name:ident, $visit:ident) => {
        paste! {
            define_visitor_internal! {
                [_mut mut]
                $name,
                [<$visit _mut>],
                [[<$name VisitorMut>]<'s>],
                [<$name VisitableMut>],
            }
        }
    };
}

macro_rules! define_visitor_internal {
    (
        $([$pfx_mod:ident $mod:ident])?
        $name:ident,
        $visit:ident,
        [$($visitor:tt)*],
        $visitable:ident,
    ) => { paste! {
        /// The visitable trait. See documentation of [`define_visitor`] to learn more.
        #[allow(missing_docs)]
        pub trait $visitable<'s, 'a> {
            fn $visit<V: $($visitor)*>(&'a $($mod)? self, _visitor: &mut V) {}
        }

        impl<'s, 'a, T: $visitable<'s, 'a>> $visitable<'s, 'a> for Box<T> {
            fn $visit<V: $($visitor)*>(&'a $($mod)? self, visitor: &mut V) {
                $visitable::$visit(& $($mod)? **self, visitor)
            }
        }

        impl<'s, 'a, T: $visitable<'s, 'a>> $visitable<'s, 'a> for Option<T> {
            fn $visit<V: $($visitor)*>(&'a $($mod)? self, visitor: &mut V) {
                if let Some(elem) = self {
                    $visitable::$visit(elem, visitor)
                }
            }
        }

        impl<'s, 'a, T: $visitable<'s, 'a>, E: $visitable<'s, 'a>> $visitable<'s, 'a>
            for Result<T, E>
        {
            fn $visit<V: $($visitor)*>(&'a $($mod)? self, visitor: &mut V) {
                match self {
                    Ok(elem) => $visitable::$visit(elem, visitor),
                    Err(elem) => $visitable::$visit(elem, visitor),
                }
            }
        }

        impl<'s, 'a, T: $visitable<'s, 'a>> $visitable<'s, 'a> for Vec<T> {
            fn $visit<V: $($visitor)*>(&'a $($mod)? self, visitor: &mut V) {
                self.[<iter $($pfx_mod)?>]().map(|t| $visitable::$visit(t, visitor)).for_each(drop);
            }
        }

        impl<'s, 'a, T: $visitable<'s, 'a>> $visitable<'s, 'a> for NonEmptyVec<T> {
            fn $visit<V: $($visitor)*>(&'a $($mod)? self, visitor: &mut V) {
                self.[<iter $($pfx_mod)?>]().map(|t| $visitable::$visit(t, visitor)).for_each(drop);
            }
        }

        impl<'s, 'a> $visitable<'s, 'a> for &str {}
        impl<'s, 'a> $visitable<'s, 'a> for str {}
    }};
}

macro_rules! define_visitor_for_tokens {
    (
        $(#$kind_meta:tt)*
        pub enum $kind:ident {
            $(
              $(#$variant_meta:tt)*
              $variant:ident $({$($args:tt)*})?
            ),* $(,)?
        }
    ) => {
        impl<'s, 'a> TreeVisitable<'s, 'a> for token::$kind {}
        impl<'s, 'a> TreeVisitableMut<'s, 'a> for token::$kind {}
    };
}

define_visitor!(Tree, visit);
define_visitor!(Span, visit_span);
define_visitor_no_mut!(Item, visit_item);

crate::with_token_definition!(define_visitor_for_tokens());


// === TreeVisitable special cases ===

impl<'s, 'a> TreeVisitable<'s, 'a> for Tree<'s> {
    fn visit<V: TreeVisitor<'s, 'a>>(&'a self, visitor: &mut V) {
        if visitor.visit(self) {
            self.variant.visit(visitor)
        }
    }
}

impl<'s, 'a> TreeVisitableMut<'s, 'a> for Tree<'s> {
    fn visit_mut<V: TreeVisitorMut<'s>>(&'a mut self, visitor: &mut V) {
        if visitor.visit_mut(self) {
            self.variant.visit_mut(visitor)
        }
    }
}

impl<'s, 'a, T> TreeVisitable<'s, 'a> for Token<'s, T> {}
impl<'s, 'a, T> TreeVisitableMut<'s, 'a> for Token<'s, T> {}


// === SpanVisitable special cases ===

impl<'s, 'a> SpanVisitable<'s, 'a> for Tree<'s> {
    fn visit_span<V: SpanVisitor<'s, 'a>>(&'a self, visitor: &mut V) {
        if visitor.visit(span::Ref {
            left_offset: &self.span.left_offset,
            code_length: self.span.code_length,
        }) {
            self.variant.visit_span(visitor)
        }
    }
}

impl<'s, 'a> SpanVisitableMut<'s, 'a> for Tree<'s> {
    fn visit_span_mut<V: SpanVisitorMut<'s>>(&'a mut self, visitor: &mut V) {
        if visitor.visit_mut(span::RefMut {
            left_offset: &mut self.span.left_offset,
            code_length: self.span.code_length,
        }) {
            self.variant.visit_span_mut(visitor)
        }
    }
}

impl<'a, 't, 's, T> SpanVisitable<'s, 'a> for Token<'s, T> {
    fn visit_span<V: SpanVisitor<'s, 'a>>(&'a self, visitor: &mut V) {
        let code_length = self.code.len();
        visitor.visit(span::Ref { left_offset: &self.left_offset, code_length });
    }
}

impl<'a, 't, 's, T> SpanVisitableMut<'s, 'a> for Token<'s, T> {
    fn visit_span_mut<V: SpanVisitorMut<'s>>(&'a mut self, visitor: &mut V) {
        let code_length = self.code.len();
        visitor.visit_mut(span::RefMut { left_offset: &mut self.left_offset, code_length });
    }
}


// === ItemVisitable special cases ===

impl<'s, 'a> ItemVisitable<'s, 'a> for Tree<'s> {
    fn visit_item<V: ItemVisitor<'s, 'a>>(&'a self, visitor: &mut V) {
        if visitor.visit_item(item::Ref::Tree(self)) {
            self.variant.visit_item(visitor)
        }
    }
}

impl<'s: 'a, 'a, T: 'a> ItemVisitable<'s, 'a> for Token<'s, T>
where &'a Token<'s, T>: Into<token::Ref<'s, 'a>>
{
    fn visit_item<V: ItemVisitor<'s, 'a>>(&'a self, visitor: &mut V) {
        visitor.visit_item(item::Ref::Token(self.into()));
    }
}


// === String ===

impl<'s, 'a> TreeVisitable<'s, 'a> for String {}
impl<'s, 'a> TreeVisitableMut<'s, 'a> for String {}
impl<'a, 't, 's> SpanVisitable<'s, 'a> for String {}
impl<'a, 't, 's> SpanVisitableMut<'s, 'a> for String {}
impl<'a, 't, 's> ItemVisitable<'s, 'a> for String {}
impl<'s> span::Builder<'s> for String {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span
    }
}

impl<'s, 'a> TreeVisitable<'s, 'a> for Cow<'static, str> {}
impl<'s, 'a> TreeVisitableMut<'s, 'a> for Cow<'static, str> {}
impl<'a, 't, 's> SpanVisitable<'s, 'a> for Cow<'static, str> {}
impl<'a, 't, 's> SpanVisitableMut<'s, 'a> for Cow<'static, str> {}
impl<'a, 't, 's> ItemVisitable<'s, 'a> for Cow<'static, str> {}
impl<'s> span::Builder<'s> for Cow<'static, str> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span
    }
}



// ==========================
// === CodePrinterVisitor ===
// ==========================

/// A visitor collecting code representation of AST nodes.
#[derive(Debug, Default)]
#[allow(missing_docs)]
struct CodePrinterVisitor {
    pub code: String,
}

impl Visitor for CodePrinterVisitor {}
impl<'s, 'a> ItemVisitor<'s, 'a> for CodePrinterVisitor {
    fn visit_item(&mut self, item: item::Ref<'s, 'a>) -> bool {
        match item {
            item::Ref::Tree(tree) => self.code.push_str(&tree.span.left_offset.code),
            item::Ref::Token(token) => {
                self.code.push_str(&token.left_offset.code);
                self.code.push_str(token.code);
            }
        }
        true
    }
}

impl<'s> Tree<'s> {
    /// Code generator of this AST.
    pub fn code(&self) -> String {
        let mut visitor = CodePrinterVisitor::default();
        self.visit_item(&mut visitor);
        visitor.code
    }
}



// ===========================
// === RefCollectorVisitor ===
// ===========================

/// A visitor collecting references to all [`Tree`] nodes.
#[derive(Debug, Default)]
#[allow(missing_docs)]
struct RefCollectorVisitor<'s, 'a> {
    pub vec: Vec<&'a Tree<'s>>,
}

impl<'s, 'a> Visitor for RefCollectorVisitor<'s, 'a> {}
impl<'s, 'a> TreeVisitor<'s, 'a> for RefCollectorVisitor<'s, 'a> {
    fn visit(&mut self, ast: &'a Tree<'s>) -> bool {
        self.vec.push(ast);
        true
    }
}

impl<'s> Tree<'s> {
    /// Collect references to all [`Tree`] nodes and return them in a vector.
    pub fn collect_vec_ref(&self) -> Vec<&Tree<'s>> {
        let mut visitor = RefCollectorVisitor::default();
        self.visit(&mut visitor);
        visitor.vec
    }
}



// =================
// === FnVisitor ===
// =================

/// A visitor allowing running a function on every [`Tree`] node.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct FnVisitor<F>(pub F);

impl<F> Visitor for FnVisitor<F> {}
impl<'s: 'a, 'a, T, F: Fn(&'a Tree<'s>) -> T> TreeVisitor<'s, 'a> for FnVisitor<F> {
    fn visit(&mut self, ast: &'a Tree<'s>) -> bool {
        (self.0)(ast);
        true
    }
}

impl<'s, T, F: Fn(&mut Tree<'s>) -> T> TreeVisitorMut<'s> for FnVisitor<F> {
    fn visit_mut(&mut self, ast: &mut Tree<'s>) -> bool {
        (self.0)(ast);
        true
    }
}

impl<'s> Tree<'s> {
    /// Map the provided function over each [`Tree`] node. The function results will be discarded.
    pub fn map<T>(&self, f: impl Fn(&Tree<'s>) -> T) {
        let mut visitor = FnVisitor(f);
        self.visit(&mut visitor);
    }

    /// Map the provided function over each [`Tree`] node. The function results will be discarded.
    pub fn map_mut<T>(&mut self, f: impl Fn(&mut Tree<'s>) -> T) {
        let mut visitor = FnVisitor(f);
        self.visit_mut(&mut visitor);
    }
}
