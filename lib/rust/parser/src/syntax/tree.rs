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
    #[reflect(flatten, hide)]
    pub span:    Span<'s>,
    #[deref]
    #[deref_mut]
    #[reflect(subtype)]
    pub variant: Box<Variant<'s>>,
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
            pub base:              Option<token::NumberBase<'s>>,
            pub integer:           Option<token::Digits<'s>>,
            pub fractional_digits: Option<FractionalDigits<'s>>,
        },
        /// The wildcard marker, `_`.
        Wildcard {
            pub token: token::Wildcard<'s>,
            #[serde(serialize_with = "crate::serialization::serialize_optional_int")]
            #[serde(deserialize_with = "crate::serialization::deserialize_optional_int")]
            #[reflect(as = "i32")]
            pub de_bruijn_index: Option<u32>,
        },
        /// The auto-scoping marker, `...`.
        AutoScope {
            pub token: token::AutoScope<'s>,
        },
        TextLiteral {
            pub open:     Option<token::TextStart<'s>>,
            pub elements: Vec<TextElement<'s>>,
            pub close:    Option<token::TextEnd<'s>>,
            #[serde(skip)]
            #[reflect(skip)]
            pub trim:     VisibleOffset,
        },
        /// A simple application, like `print "hello"`.
        App {
            pub func: Tree<'s>,
            pub arg: Tree<'s>,
        },
        /// An application using an argument name, like `summarize_transaction (price = 100)`.
        NamedApp {
            pub func:   Tree<'s>,
            pub open:   Option<token::OpenSymbol<'s>>,
            pub name:   token::Ident<'s>,
            pub equals: token::Operator<'s>,
            pub arg:    Tree<'s>,
            pub close:  Option<token::CloseSymbol<'s>>,
        },
        /// Application using the `default` keyword.
        DefaultApp {
            pub func:    Tree<'s>,
            pub default: token::Ident<'s>,
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
            pub arguments: u32,
            pub ast:       Tree<'s>,
        },
        TemplateFunction {
            pub arguments: u32,
            pub ast:       Tree<'s>,
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
            pub keyword:      token::Ident<'s>,
            pub name:         token::Ident<'s>,
            pub params:       Vec<ArgumentDefinition<'s>>,
            pub constructors: Vec<TypeConstructorLine<'s>>,
            pub block:        Vec<block::Line<'s>>,
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
            /// The (qualified) name to which the function should be bound.
            pub name: Tree<'s>,
            /// The argument patterns.
            pub args: Vec<ArgumentDefinition<'s>>,
            /// The `=` token.
            pub equals: token::Operator<'s>,
            /// The body, which will typically be an inline expression or a `BodyBlock` expression.
            /// It is an error for this to be empty.
            pub body: Option<Tree<'s>>,
        },
        /// An import statement.
        Import {
            pub polyglot: Option<MultiSegmentAppSegment<'s>>,
            pub from:     Option<MultiSegmentAppSegment<'s>>,
            pub import:   MultiSegmentAppSegment<'s>,
            pub all:      Option<token::Ident<'s>>,
            #[reflect(rename = "as")]
            pub as_:      Option<MultiSegmentAppSegment<'s>>,
            pub hiding:   Option<MultiSegmentAppSegment<'s>>,
        },
        /// An export statement.
        Export {
            pub from:   Option<MultiSegmentAppSegment<'s>>,
            pub export: MultiSegmentAppSegment<'s>,
            pub all:    Option<token::Ident<'s>>,
            #[reflect(rename = "as")]
            pub as_:    Option<MultiSegmentAppSegment<'s>>,
            pub hiding: Option<MultiSegmentAppSegment<'s>>,
        },
        /// An expression grouped by matched parentheses.
        Group {
            pub open:  Option<token::OpenSymbol<'s>>,
            pub body:  Option<Tree<'s>>,
            pub close: Option<token::CloseSymbol<'s>>,
        },
        /// Statement declaring the type of a variable.
        TypeSignature {
            /// (Qualified) name of the item whose type is being declared.
            pub variable: Tree<'s>,
            /// The `:` token.
            pub operator: token::Operator<'s>,
            /// The variable's type.
            #[reflect(rename = "type")]
            pub type_:    Tree<'s>,
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
        /// A `case _ of` pattern-matching expression.
        CaseOf {
            pub case:       token::Ident<'s>,
            pub expression: Option<Tree<'s>>,
            pub of:         token::Ident<'s>,
            pub cases:      Vec<CaseLine<'s>>,
        },
        /// A lambda expression.
        Lambda {
            pub operator: token::Operator<'s>,
            pub arrow: Option<Tree<'s>>,
        },
        /// An array literal.
        Array {
            pub left:  token::OpenSymbol<'s>,
            pub first: Option<Tree<'s>>,
            pub rest:  Vec<OperatorDelimitedTree<'s>>,
            pub right: token::CloseSymbol<'s>,
        },
        /// A tuple literal.
        Tuple {
            pub left:  token::OpenSymbol<'s>,
            pub first: Option<Tree<'s>>,
            pub rest:  Vec<OperatorDelimitedTree<'s>>,
            pub right: token::CloseSymbol<'s>,
        },
        /// An expression preceded by an annotation, e.g. `@Builtin_Method foo`.
        Annotated {
            pub token:      token::Operator<'s>,
            pub annotation: token::Ident<'s>,
            pub newlines:   Vec<token::Newline<'s>>,
            pub expression: Option<Tree<'s>>,
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
    pub arguments:   Vec<ArgumentDefinition<'s>>,
    /// The arguments the type constructor accepts, specified on their own lines.
    pub block:       Vec<ArgumentDefinitionLine<'s>>,
}

impl<'s> span::Builder<'s> for TypeConstructorDef<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.constructor).add(&mut self.arguments).add(&mut self.block)
    }
}

/// An argument specification on its own line.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct ArgumentDefinitionLine<'s> {
    /// The token beginning the line.
    pub newline:  token::Newline<'s>,
    /// The argument definition, unless this is an empty line.
    pub argument: Option<ArgumentDefinition<'s>>,
}

impl<'s> span::Builder<'s> for ArgumentDefinitionLine<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.newline).add(&mut self.argument)
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
    /// An escaped character.
    Escape {
        /// The escape sequence.
        token: token::TextEscape<'s>,
    },
    /// An interpolated section within a text literal.
    Splice {
        /// The opening ` character.
        open:       token::OpenSymbol<'s>,
        /// The interpolated expression.
        expression: Option<Tree<'s>>,
        /// The closing ` character.
        close:      token::CloseSymbol<'s>,
    },
}

impl<'s> span::Builder<'s> for TextElement<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        match self {
            TextElement::Section { text } => text.add_to_span(span),
            TextElement::Escape { token } => span.add(token),
            TextElement::Splice { open, expression, close } =>
                span.add(open).add(expression).add(close),
        }
    }
}

impl<'s, 'a> TreeVisitable<'s, 'a> for VisibleOffset {}
impl<'s, 'a> TreeVisitableMut<'s, 'a> for VisibleOffset {}
impl<'a, 's> SpanVisitable<'s, 'a> for VisibleOffset {}
impl<'a, 's> SpanVisitableMut<'s, 'a> for VisibleOffset {}
impl<'a, 's> ItemVisitable<'s, 'a> for VisibleOffset {}
impl<'s> span::Builder<'s> for VisibleOffset {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span
    }
}

// === Number literals ===

#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
#[allow(missing_docs)]
pub struct FractionalDigits<'s> {
    /// The dot operator.
    pub dot:    token::Operator<'s>,
    /// The decimal digits after the dot.
    pub digits: token::Digits<'s>,
}

impl<'s> span::Builder<'s> for FractionalDigits<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.dot).add(&mut self.digits)
    }
}


// === Functions ===

/// A function argument definition.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct ArgumentDefinition<'s> {
    /// Opening parenthesis (outer).
    pub open:       Option<token::OpenSymbol<'s>>,
    /// Opening parenthesis (inner).
    pub open2:      Option<token::OpenSymbol<'s>>,
    /// An optional execution-suspension unary operator (~).
    pub suspension: Option<token::Operator<'s>>,
    /// The pattern being bound to an argument.
    pub pattern:    Tree<'s>,
    /// An optional type ascribed to an argument.
    #[reflect(rename = "type")]
    pub type_:      Option<ArgumentType<'s>>,
    /// Closing parenthesis (inner).
    pub close2:     Option<token::CloseSymbol<'s>>,
    /// An optional default value for an argument.
    pub default:    Option<ArgumentDefault<'s>>,
    /// Closing parenthesis (outer).
    pub close:      Option<token::CloseSymbol<'s>>,
}

impl<'s> span::Builder<'s> for ArgumentDefinition<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.open)
            .add(&mut self.open2)
            .add(&mut self.suspension)
            .add(&mut self.pattern)
            .add(&mut self.type_)
            .add(&mut self.close2)
            .add(&mut self.default)
            .add(&mut self.close)
    }
}

/// A default value specification in a function argument definition.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct ArgumentDefault<'s> {
    /// The `=` token.
    pub equals:     token::Operator<'s>,
    /// The default value.
    pub expression: Tree<'s>,
}

impl<'s> span::Builder<'s> for ArgumentDefault<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.equals).add(&mut self.expression)
    }
}

/// A type ascribed to an argument definition.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct ArgumentType<'s> {
    /// The `:` token.
    pub operator: token::Operator<'s>,
    /// The type.
    #[reflect(rename = "type")]
    pub type_:    Tree<'s>,
}

impl<'s> span::Builder<'s> for ArgumentType<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.operator).add(&mut self.type_)
    }
}


// === CaseOf ===

/// A that may contain a case-expression in a case-of expression.
#[derive(Clone, Debug, Default, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct CaseLine<'s> {
    /// The token beginning the line. This will always be present, unless the first case-expression
    /// is on the same line as the initial case-of.
    pub newline: Option<token::Newline<'s>>,
    /// The case-expression, if present.
    pub case:    Option<Case<'s>>,
}

impl<'s> CaseLine<'s> {
    /// Return a mutable reference to the `left_offset` of this object (which will actually belong
    /// to one of the object's children, if it has any).
    pub fn left_offset_mut(&mut self) -> Option<&mut Offset<'s>> {
        self.newline
            .as_mut()
            .map(|t| &mut t.left_offset)
            .or_else(|| self.case.as_mut().and_then(Case::left_offset_mut))
    }
}

impl<'s> span::Builder<'s> for CaseLine<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.newline).add(&mut self.case)
    }
}

/// A case-expression in a case-of expression.
#[derive(Clone, Debug, Default, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct Case<'s> {
    /// The pattern being matched. It is an error for this to be absent.
    pub pattern:    Option<Tree<'s>>,
    /// Token.
    pub arrow:      Option<token::Operator<'s>>,
    /// The expression associated with the pattern. It is an error for this to be empty.
    pub expression: Option<Tree<'s>>,
}

impl<'s> Case<'s> {
    /// Return a mutable reference to the `left_offset` of this object (which will actually belong
    /// to one of the object's children, if it has any).
    pub fn left_offset_mut(&mut self) -> Option<&mut Offset<'s>> {
        self.pattern
            .as_mut()
            .map(|p| &mut p.span.left_offset)
            .or_else(|| self.arrow.as_mut().map(|t| &mut t.left_offset))
            .or_else(|| self.expression.as_mut().map(|e| &mut e.span.left_offset))
    }
}

impl<'s> span::Builder<'s> for Case<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.pattern).add(&mut self.arrow).add(&mut self.expression)
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


// === Array and Tuple ===

/// A node following an operator.
#[derive(Clone, Debug, Eq, PartialEq, Visitor, Serialize, Reflect, Deserialize)]
pub struct OperatorDelimitedTree<'s> {
    /// The delimiting operator.
    pub operator: token::Operator<'s>,
    /// The expression.
    pub body:     Tree<'s>,
}

impl<'s> span::Builder<'s> for OperatorDelimitedTree<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.operator).add(&mut self.body)
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
        Variant::TextLiteral(lhs) if lhs.close.is_none()
                && let Tree { variant: box Variant::TextLiteral(rhs), span } = arg => {
            join_text_literals(lhs, rhs, span);
            return func;
        }
        Variant::Number(func_ @ Number { base: _, integer: None, fractional_digits: None })
            if let box
            Variant::Number(Number { base: None, integer, fractional_digits }) = &mut arg.variant
        => {
            func_.integer = mem::take(integer);
            func_.fractional_digits = mem::take(fractional_digits);
            return func;
        }
        Variant::Annotated(func_ @ Annotated { expression: None, .. }) => {
            func_.expression = arg.into();
            return func;
        }
        Variant::Annotated(Annotated { expression: Some(expression), .. }) => {
            *expression = apply(mem::take(expression), arg);
            return func;
        }
        _ => (),
    }
    match &mut *arg.variant {
        Variant::ArgumentBlockApplication(block) if block.lhs.is_none() => {
            let func_left_offset = mem::take(&mut func.span.left_offset);
            let arg_left_offset = mem::replace(&mut arg.span.left_offset, func_left_offset);
            if let Some(first) = block.arguments.first_mut() {
                first.newline.left_offset += arg_left_offset;
            }
            block.lhs = Some(func);
            arg
        }
        Variant::OperatorBlockApplication(block) if block.lhs.is_none() => {
            let func_left_offset = mem::take(&mut func.span.left_offset);
            let arg_left_offset = mem::replace(&mut arg.span.left_offset, func_left_offset);
            if let Some(first) = block.expressions.first_mut() {
                first.newline.left_offset += arg_left_offset;
            }
            block.lhs = Some(func);
            arg
        }
        Variant::OprApp(OprApp { lhs: Some(lhs), opr: Ok(opr), rhs: Some(rhs) })
                if opr.properties.is_assignment() && let Variant::Ident(lhs) = &*lhs.variant => {
            let mut lhs = lhs.token.clone();
            lhs.left_offset += arg.span.left_offset.clone();
            Tree::named_app(func, None, lhs, opr.clone(), rhs.clone(), None)
        }
        Variant::Group(Group { open: Some(open), body: Some(body), close: Some(close) }) if let box
        Variant::OprApp(OprApp { lhs: Some(lhs), opr: Ok(opr), rhs: Some(rhs) }) = &body.variant
                && opr.properties.is_assignment() && let Variant::Ident(lhs) = &*lhs.variant => {
            let mut open = open.clone();
            open.left_offset += arg.span.left_offset.clone();
            let open = Some(open);
            let close = Some(close.clone());
            Tree::named_app(func, open, lhs.token.clone(), opr.clone(), rhs.clone(), close)
        }
        Variant::Ident(Ident { token }) if token.is_default => {
            let mut token = token.clone();
            token.left_offset += arg.span.left_offset.clone();
            Tree::default_app(func, token)
        }
        _ => Tree::app(func, arg)
    }
}

fn join_text_literals<'s>(
    lhs: &'_ mut TextLiteral<'s>,
    mut rhs: TextLiteral<'s>,
    rhs_span: Span<'s>,
) {
    if rhs.trim != VisibleOffset(0) && (lhs.trim == VisibleOffset(0) || rhs.trim < lhs.trim) {
        lhs.trim = rhs.trim;
    }
    match rhs.elements.first_mut() {
        Some(TextElement::Section { text }) => text.left_offset += rhs_span.left_offset,
        Some(TextElement::Escape { token }) => token.left_offset += rhs_span.left_offset,
        Some(TextElement::Splice { open, .. }) => open.left_offset += rhs_span.left_offset,
        None => (),
    }
    lhs.elements.append(&mut rhs.elements);
    lhs.close = rhs.close.take();
    if lhs.open.is_some() {
        let trim = lhs.trim;
        let mut remaining = lhs.elements.len();
        let mut carried_offset = Offset::default();
        lhs.elements.retain_mut(|e| {
            remaining -= 1;
            let (offset, code) = match e {
                TextElement::Section { text } => (&mut text.left_offset, &mut text.code),
                TextElement::Escape { token } => (&mut token.left_offset, &mut token.code),
                TextElement::Splice { open, .. } => (&mut open.left_offset, &mut open.code),
            };
            *offset += mem::take(&mut carried_offset);
            crate::lexer::untrim(trim, offset, code);
            if remaining != 0 && code.is_empty() {
                carried_offset = mem::take(offset);
                return false;
            }
            true
        });
    }
}

/// Join two nodes with an operator, in a way appropriate for their types.
///
/// For most operands this will simply construct an `OprApp`; however, a non-operator block (i.e. an
/// `ArgumentBlock`) is reinterpreted as a `BodyBlock` when it appears in the RHS of an operator
/// expression.
pub fn apply_operator<'s>(
    mut lhs: Option<Tree<'s>>,
    opr: Vec<token::Operator<'s>>,
    mut rhs: Option<Tree<'s>>,
    nospace: bool,
) -> Tree<'s> {
    let opr = match opr.len() {
        0 => return apply(lhs.unwrap(), rhs.unwrap()),
        1 => Ok(opr.into_iter().next().unwrap()),
        _ => Err(MultipleOperatorError { operators: NonEmptyVec::try_from(opr).unwrap() }),
    };
    if let Ok(opr_) = &opr && opr_.properties.is_type_annotation() {
        return match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => {
                let rhs = crate::expression_to_type(rhs);
                Tree::type_annotated(lhs, opr.unwrap(), rhs)
            },
            (lhs, rhs) => {
                let invalid = Tree::opr_app(lhs, opr, rhs);
                let err = Error::new("`:` operator must be applied to two operands.");
                Tree::invalid(err, invalid)
            }
        };
    }
    if nospace
        && let Ok(opr) = &opr && opr.properties.can_be_decimal_operator()
        && let Some(lhs) = lhs.as_mut()
        && let box Variant::Number(lhs_) = &mut lhs.variant
        && lhs_.fractional_digits.is_none()
        && let Some(rhs) = rhs.as_mut()
        && let box Variant::Number(Number { base: None, integer: Some(digits), fractional_digits: None }) = &mut rhs.variant
    {
        let dot = opr.clone();
        let digits = digits.clone();
        lhs_.fractional_digits = Some(FractionalDigits { dot, digits });
        return lhs.clone();
    }
    if let Some(rhs_) = rhs.as_mut() {
        if let Variant::ArgumentBlockApplication(block) = &mut *rhs_.variant {
            if block.lhs.is_none() {
                if let Some(first) = block.arguments.first_mut() {
                    first.newline.left_offset += mem::take(&mut rhs_.span.left_offset);
                }
                let ArgumentBlockApplication { lhs: _, arguments } = block;
                let arguments = mem::take(arguments);
                let rhs_ = block::body_from_lines(arguments);
                rhs = Some(rhs_);
            }
        }
    }
    Tree::opr_app(lhs, opr, rhs)
}

/// Apply a unary operator to an operand.
///
/// For most inputs this will simply construct a `UnaryOprApp`; however, some operators are special.
pub fn apply_unary_operator<'s>(opr: token::Operator<'s>, rhs: Option<Tree<'s>>) -> Tree<'s> {
    if opr.properties.is_annotation()
            && let Some(Tree { variant: box Variant::Ident(Ident { token }), .. }) = rhs {
        Tree::annotated(opr, token, vec![], None)
    } else {
        Tree::unary_opr_app(opr, rhs)
    }
}

impl<'s> From<Token<'s>> for Tree<'s> {
    fn from(token: Token<'s>) -> Self {
        match token.variant {
            token::Variant::Ident(ident) => Tree::ident(token.with_variant(ident)),
            token::Variant::Digits(number) =>
                Tree::number(None, Some(token.with_variant(number)), None),
            token::Variant::NumberBase(base) =>
                Tree::number(Some(token.with_variant(base)), None, None),
            token::Variant::TextStart(open) =>
                Tree::text_literal(Some(token.with_variant(open)), default(), default(), default()),
            token::Variant::TextSection(section) => {
                let trim = token.left_offset.visible;
                let section = TextElement::Section { text: token.with_variant(section) };
                Tree::text_literal(default(), vec![section], default(), trim)
            }
            token::Variant::TextEscape(escape) => {
                let trim = token.left_offset.visible;
                let token = token.with_variant(escape);
                let section = TextElement::Escape { token };
                Tree::text_literal(default(), vec![section], default(), trim)
            }
            token::Variant::TextEnd(close) =>
                Tree::text_literal(default(), default(), Some(token.with_variant(close)), default()),
            token::Variant::Wildcard(wildcard) => Tree::wildcard(token.with_variant(wildcard), default()),
            token::Variant::AutoScope(t) => Tree::auto_scope(token.with_variant(t)),
            token::Variant::OpenSymbol(s) =>
                Tree::group(Some(token.with_variant(s)), default(), default()).with_error("Unmatched delimiter"),
            token::Variant::CloseSymbol(s) =>
                Tree::group(default(), default(), Some(token.with_variant(s))).with_error("Unmatched delimiter"),
            // These should be unreachable: They are handled when assembling items into blocks,
            // before parsing proper.
            token::Variant::Newline(_)
            | token::Variant::BlockStart(_)
            | token::Variant::BlockEnd(_)
            // This should be unreachable: `resolve_operator_precedence` doesn't calls `to_ast` for
            // operators.
            | token::Variant::Operator(_)
            // Map an error case in the lexer to an error in the AST.
            | token::Variant::Invalid(_) => {
                let message = format!("Unexpected token: {token:?}");
                let ident = token::variant::Ident(false, 0, false, false);
                let value = Tree::ident(token.with_variant(ident));
                Tree::with_error(value, message)
            }
        }
    }
}



// =============================
// === Traversing operations ===
// =============================

/// Recurse into the lexical LHS of the tree, applying a function to each node reached, until the
/// function returns `false`.
pub fn recurse_left_mut_while<'s>(
    mut tree: &'_ mut Tree<'s>,
    mut f: impl FnMut(&'_ mut Tree<'s>) -> bool,
) {
    while f(tree) {
        tree = match &mut *tree.variant {
            // No LHS.
            Variant::Invalid(_)
            | Variant::BodyBlock(_)
            | Variant::Ident(_)
            | Variant::Number(_)
            | Variant::Wildcard(_)
            | Variant::AutoScope(_)
            | Variant::TextLiteral(_)
            | Variant::UnaryOprApp(_)
            | Variant::MultiSegmentApp(_)
            | Variant::TypeDef(_)
            | Variant::Import(_)
            | Variant::Export(_)
            | Variant::Group(_)
            | Variant::CaseOf(_)
            | Variant::Lambda(_)
            | Variant::Array(_)
            | Variant::Annotated(_)
            | Variant::Tuple(_) => break,
            // Optional LHS.
            Variant::ArgumentBlockApplication(ArgumentBlockApplication { lhs, .. })
            | Variant::OperatorBlockApplication(OperatorBlockApplication { lhs, .. })
            | Variant::OprApp(OprApp { lhs, .. }) =>
                if let Some(lhs) = lhs {
                    lhs
                } else {
                    break;
                },
            // Unconditional LHS.
            Variant::App(App { func: lhs, .. })
            | Variant::NamedApp(NamedApp { func: lhs, .. })
            | Variant::OprSectionBoundary(OprSectionBoundary { ast: lhs, .. })
            | Variant::TemplateFunction(TemplateFunction { ast: lhs, .. })
            | Variant::DefaultApp(DefaultApp { func: lhs, .. })
            | Variant::Assignment(Assignment { pattern: lhs, .. })
            | Variant::TypeSignature(TypeSignature { variable: lhs, .. })
            | Variant::Function(Function { name: lhs, .. })
            | Variant::TypeAnnotated(TypeAnnotated { expression: lhs, .. }) => lhs,
        }
    }
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


// === Primitives ===

impl<'s, 'a> TreeVisitable<'s, 'a> for u32 {}
impl<'s, 'a> TreeVisitableMut<'s, 'a> for u32 {}
impl<'a, 's> SpanVisitable<'s, 'a> for u32 {}
impl<'a, 's> SpanVisitableMut<'s, 'a> for u32 {}
impl<'a, 's> ItemVisitable<'s, 'a> for u32 {}
impl<'s> span::Builder<'s> for u32 {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span
    }
}


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

impl<'a, 's, T> SpanVisitable<'s, 'a> for Token<'s, T> {
    fn visit_span<V: SpanVisitor<'s, 'a>>(&'a self, visitor: &mut V) {
        let code_length = self.code.length();
        visitor.visit(span::Ref { left_offset: &self.left_offset, code_length });
    }
}

impl<'a, 's, T> SpanVisitableMut<'s, 'a> for Token<'s, T> {
    fn visit_span_mut<V: SpanVisitorMut<'s>>(&'a mut self, visitor: &mut V) {
        let code_length = self.code.length();
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
impl<'a, 's> SpanVisitable<'s, 'a> for String {}
impl<'a, 's> SpanVisitableMut<'s, 'a> for String {}
impl<'a, 's> ItemVisitable<'s, 'a> for String {}
impl<'s> span::Builder<'s> for String {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span
    }
}

impl<'s, 'a> TreeVisitable<'s, 'a> for Cow<'static, str> {}
impl<'s, 'a> TreeVisitableMut<'s, 'a> for Cow<'static, str> {}
impl<'a, 's> SpanVisitable<'s, 'a> for Cow<'static, str> {}
impl<'a, 's> SpanVisitableMut<'s, 'a> for Cow<'static, str> {}
impl<'a, 's> ItemVisitable<'s, 'a> for Cow<'static, str> {}
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
