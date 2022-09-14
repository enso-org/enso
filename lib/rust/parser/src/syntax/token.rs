//! its source code and can be printed back. It also contains information about the offset to the
//! previous token if any.
//!
//! The [`Token`] structure has a signature of [`Token<'s, T>`], where [`T`] is the variant type.
//!
//!
//! # Variants
//! Each token contains a variant, a structure defining the meaning of the token. All variants are
//! defined in the [`variant`] module. Every variant is associated with a constructor of the same
//! name (tuple-struct like). For example, the [`variant::Ident`] is defined as:
//!
//! ```text
//! pub mod variant {
//!     pub struct Ident {
//!         pub is_free: bool,
//!         pub lift_level: usize
//!     }
//!     pub fn Ident(is_free: bool, lift_level: usize) -> Ident { ... }
//!     // ... many more variants
//! }
//! ```
//!
//!
//! # Variants as tokens
//! The [`Token`] structure can be parametrized with a variant type to form a token variant. This
//! module defines type aliases for every such a combination. For example, the [`Ident`] token
//! variant is defined as:
//!
//! ```text
//! pub type Ident<'s> = Token<'s, variant::Ident>;
//! ```
//!
//! There is a [`From`] conversion defined between any [`Token<'s, T>`] and [`Token<'s>`] for [`T`]
//! being one of variant structs. Moreover, every such type is accompanied by two constructor utils,
//! one creating a token variant and one creating a generic token instance. For example, the
//! [`Ident`] token variant constructors are defined as:
//!
//! ```text
//! pub fn ident  <'s> (is_free: bool, lift_level: usize) -> Ident<'s> { ... }
//! pub fn ident_ <'s> (is_free: bool, lift_level: usize) -> Token<'s> { ... }
//! ```
//!
//!
//! # The [`Variant`] type.
//! There are many variants of tokens, however, some places in the code need to distinguish them,
//! while some need to store several variants in the same collection. The [`Variant`] enum
//! generalizes the variant types:
//!
//! ```text
//! pub enum Variant {
//!     Newline (variant::Newline),
//!     Symbol (variant::Symbol),
//!     Wildcard (variant::Wildcard),
//!     Ident (variant::Ident),
//!     // ... many more
//! }
//! ```
//!
//! There is a [`From`] conversion defined between each variant and the [`Variant`] struct.
//! Moreover, the [`Variant`] struct defines a constructor function for each of its variants. For
//! example, the identifier variant constructor is defined as:
//!
//! ```text
//! impl Variant {
//!     pub fn ident(is_free: bool, lift_level: usize) -> Self {
//!         Self::Ident(variant::Ident(is_free, lift_level))
//!     }
//! }
//! ```
//!
//! # Generic token type
//! The [`Token`] structure has a default parametrization of [`Token<'s, Variant>`] which basically
//! is a token containing any of the defined variants.
//!
//!
//!
//! # Variant markers
//! There is also a special enum [`VariantMarker`] defined which can be used to mark which token
//! variant is used without keeping any of the variant data. It is defined as:
//!
//! ```text
//! pub enum VariantMarker {
//!     Newline,
//!     Symbol,
//!     Wildcard,
//!     Ident,
//!     // ... many more
//! }
//! ```
//!
//! See the definitions and macros below to learn more.

use crate::prelude::*;
use crate::source::*;

use enso_shapely_macros::tagged_enum;



// =============
// === Token ===
// =============

/// The lexical token definition. See the module docs to learn more about its usage scenarios.
#[derive(Clone, Default, Deref, DerefMut, Eq, PartialEq, Serialize, Reflect, Deserialize)]
#[allow(missing_docs)]
pub struct Token<'s, T = Variant> {
    #[reflect(flatten, hide)]
    pub left_offset: Offset<'s>,
    #[reflect(flatten)]
    pub code:        Code<'s>,
    #[deref]
    #[deref_mut]
    #[reflect(subtype)]
    pub variant:     T,
}

/// Constructor.
#[inline(always)]
#[allow(non_snake_case)]
pub fn Token<'s, T>(
    left_offset: impl Into<Offset<'s>>,
    code: impl Into<Code<'s>>,
    variant: T,
) -> Token<'s, T> {
    let left_offset = left_offset.into();
    let code = code.into();
    Token { variant, left_offset, code }
}

impl<'s, T> Token<'s, T> {
    /// Split the token at the provided byte offset. The offset is counted from the [`code`] start
    /// position, which does not include the [`left_offset`]. It means that `split_at(Bytes(0))`
    /// will split the token into left offset only and a left-trimmed token.
    #[inline(always)]
    pub fn split_at(self, offset: Bytes) -> (Token<'s, ()>, Token<'s, ()>, T) {
        let left_lexeme_offset = self.left_offset;
        let right_lexeme_offset = Offset::default();
        let left = Token(left_lexeme_offset, self.code.slice(Bytes(0)..offset), ());
        let right = Token(right_lexeme_offset, self.code.slice(offset..), ());
        (left, right, self.variant)
    }

    /// A version of [`split_at`] that discards the associated variant.
    #[inline(always)]
    pub fn split_at_(self, offset: Bytes) -> (Token<'s, ()>, Token<'s, ()>) {
        let (left, right, _) = self.split_at(offset);
        (left, right)
    }

    /// Modify the associated variant of this token with the provided function.
    #[inline(always)]
    pub fn map_variant<S>(self, f: impl FnOnce(T) -> S) -> Token<'s, S> {
        Token(self.left_offset, self.code, f(self.variant))
    }

    /// Replace the associated variant in this token.
    #[inline(always)]
    pub fn with_variant<S>(self, data: S) -> Token<'s, S> {
        self.map_variant(|_| data)
    }

    /// Span of this token.
    pub fn span<'a>(&'a self) -> span::Ref<'s, 'a> {
        let code_length = self.code.length();
        span::Ref { left_offset: &self.left_offset, code_length }
    }
}

impl<'s, T: Debug> Debug for Token<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}:{:?}] ", self.left_offset.visible, self.code)?;
        Debug::fmt(&self.variant, f)
    }
}

impl<'s, T: PartialEq> PartialEq<Token<'s, T>> for &Token<'s, T> {
    fn eq(&self, other: &Token<'s, T>) -> bool {
        <Token<'s, T> as PartialEq<Token<'s, T>>>::eq(*self, other)
    }
}

impl<'s, T> FirstChildTrim<'s> for Token<'s, T> {
    #[inline(always)]
    fn trim_as_first_child(&mut self) -> Span<'s> {
        let left_offset = mem::take(&mut self.left_offset);
        let code_length = self.code.length();
        Span { left_offset, code_length }
    }
}



// ===========
// === Ref ===
// ===========

/// A reference of a [`Token`]. It is used mostly by AST visitors.
///
/// There is an important question involved – why we don't just use [`&Token<'s, T>`] instead. The
/// reason for that is that sometimes AST nodes contain [`Token<'s, T>`] for a specific [`T`] and
/// we want to traverse them for any possible variant, thus converting [`T`] to [`token::Variant`]
/// first. However, we do not want to clone the code during such an operation. This struct allows
/// viewing any [`Token<'s, T>`] as [`Ref<'s, token::Variant>`].
#[derive(Clone, Copy, Deref, DerefMut, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Ref<'s, 'a, T = Variant> {
    #[deref]
    #[deref_mut]
    pub data:        T,
    pub left_offset: &'a Offset<'s>,
    pub code:        &'a Code<'s>,
}

impl<'s, 'a, T, S> From<&'a Token<'s, T>> for Ref<'s, 'a, S>
where T: Copy + Into<S>
{
    fn from(token: &'a Token<'s, T>) -> Self {
        Ref {
            data:        token.variant.into(),
            left_offset: &token.left_offset,
            code:        &token.code,
        }
    }
}

impl<'s, 'a, T: Debug> Debug for Ref<'s, 'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[off: {}, repr: \"{}\"] ", self.left_offset.visible, self.code)?;
        Debug::fmt(&self.data, f)
    }
}



// ===============
// === Variant ===
// ===============

/// Macro providing [`Token`] type definition. It is used to both define the token [`Variant`], and
/// to define impls for every token type in other modules.
#[macro_export]
macro_rules! with_token_definition { ($f:ident ($($args:tt)*)) => { $f! { $($args)*
    /// Elements that can be found in the source code.
    #[tagged_enum]
    #[derive(Clone, Copy, PartialEq, Eq, Serialize, Reflect, Deserialize)]
    #[allow(missing_docs)]
    #[tagged_enum(apply_attributes_to = "variants")]
    #[reflect(inline)]
    #[tagged_enum(apply_attributes_to = "variant-types")]
    #[derive(Default)]
    pub enum Variant {
        Newline,
        Symbol,
        BlockStart,
        BlockEnd,
        Wildcard {
            pub lift_level: usize
        },
        AutoScope,
        Ident {
            pub is_free:    bool,
            pub lift_level: usize,
            pub is_type:    bool,
            pub is_default: bool,
        },
        Operator {
            pub properties: OperatorProperties,
        },
        Modifier,
        DocComment,
        Digits {
            pub base: Option<Base>
        },
        NumberBase,
        TextStart,
        TextEnd,
        TextSection,
        TextEscapeSymbol,
        TextEscapeChar,
        TextEscapeLeader,
        TextEscapeHexDigits,
        TextEscapeSequenceStart,
        TextEscapeSequenceEnd,
    }
}}}

impl Variant {
    /// Return whether this token can introduce a macro invocation.
    pub fn can_start_macro(&self) -> bool {
        // Prevent macro interpretation of symbols that have been lexically contextualized as text
        // escape control characters.
        !matches!(self, Variant::TextEscapeSymbol(_) | Variant::TextEscapeSequenceStart(_))
    }
}

impl Default for Variant {
    fn default() -> Self {
        Self::Newline(variant::Newline {})
    }
}


// === Operator properties ===

/// Properties of an operator that are identified when lexing.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Serialize,
    Reflect,
    Deserialize,
    PartialOrd,
    Ord,
    Default
)]
pub struct OperatorProperties {
    // Precedence
    #[serde(skip)]
    #[reflect(skip)]
    binary_infix_precedence:   Option<Precedence>,
    #[serde(skip)]
    #[reflect(skip)]
    unary_prefix_precedence:   Option<Precedence>,
    // Special properties
    #[serde(skip)]
    #[reflect(skip)]
    is_compile_time_operation: bool,
    #[serde(skip)]
    #[reflect(skip)]
    is_right_associative:      bool,
    #[serde(skip)]
    #[reflect(skip)]
    can_be_decimal_operator:   bool,
    // Unique operators
    #[serde(skip)]
    #[reflect(skip)]
    is_type_annotation:        bool,
    #[serde(skip)]
    #[reflect(skip)]
    is_assignment:             bool,
    #[serde(skip)]
    #[reflect(skip)]
    is_arrow:                  bool,
    #[serde(skip)]
    #[reflect(skip)]
    is_sequence:               bool,
}

impl OperatorProperties {
    /// Construct an operator with default properties.
    pub fn new() -> Self {
        default()
    }

    /// Return a copy of this operator, with the given binary infix precedence.
    pub fn with_binary_infix_precedence(self, value: usize) -> Self {
        Self { binary_infix_precedence: Some(Precedence { value }), ..self }
    }

    /// Return a copy of this operator, with unary prefix parsing allowed.
    pub fn with_unary_prefix_mode(self, precedence: Precedence) -> Self {
        Self { unary_prefix_precedence: Some(precedence), ..self }
    }

    /// Return a copy of this operator, modified to be flagged as a compile time operation.
    pub fn as_compile_time_operation(self) -> Self {
        Self { is_compile_time_operation: true, ..self }
    }

    /// Return a copy of this operator, modified to be flagged as right associative.
    pub fn as_right_associative(self) -> Self {
        Self { is_right_associative: true, ..self }
    }

    /// Return a copy of this operator, modified to be flagged as a type annotation operator.
    pub fn as_type_annotation(self) -> Self {
        Self { is_type_annotation: true, ..self }
    }

    /// Return a copy of this operator, modified to be flagged as an assignment operator.
    pub fn as_assignment(self) -> Self {
        Self { is_assignment: true, ..self }
    }

    /// Return a copy of this operator, modified to be flagged as an arrow operator.
    pub fn as_arrow(self) -> Self {
        Self { is_arrow: true, ..self }
    }

    /// Return a copy of this operator, modified to be flagged as the sequence operator.
    pub fn as_sequence(self) -> Self {
        Self { is_sequence: true, ..self }
    }

    /// Return a copy of this operator, modified to allow an interpretion as a decmial point.
    pub fn with_decimal_interpretation(self) -> Self {
        Self { can_be_decimal_operator: true, ..self }
    }

    /// Return this operator's binary infix precedence, if it has one.
    pub fn binary_infix_precedence(&self) -> Option<Precedence> {
        self.binary_infix_precedence
    }

    /// Return this operator's unary prefix precedence, if it has one.
    pub fn unary_prefix_precedence(&self) -> Option<Precedence> {
        self.unary_prefix_precedence
    }

    /// Return whether this operator can form operator sections.
    pub fn can_form_section(&self) -> bool {
        !self.is_compile_time_operation
    }

    /// Return whether this operator is the type annotation operator.
    pub fn is_type_annotation(&self) -> bool {
        self.is_type_annotation
    }

    /// Return whether this operator is the assignment operator.
    pub fn is_assignment(&self) -> bool {
        self.is_assignment
    }

    /// Return whether this operator is the arrow operator.
    pub fn is_arrow(&self) -> bool {
        self.is_arrow
    }

    /// Return whether this operator is the sequence operator.
    pub fn is_sequence(&self) -> bool {
        self.is_sequence
    }

    /// Return this operator's associativity.
    pub fn associativity(&self) -> Associativity {
        match self.is_right_associative {
            false => Associativity::Left,
            true => Associativity::Right,
        }
    }

    /// Return whether this operator can be interpreted as a decimal point.
    pub fn can_be_decimal_operator(&self) -> bool {
        self.can_be_decimal_operator
    }
}

/// Value that can be compared to determine which operator will bind more tightly within an
/// expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Reflect, Deserialize, PartialOrd, Ord)]
pub struct Precedence {
    /// A numeric value determining precedence order.
    pub value: usize,
}

impl Precedence {
    /// Return a precedence that is not higher than any other precedence.
    pub fn min() -> Self {
        Precedence { value: 0 }
    }

    /// Return a precedence that is not lower than any other precedence.
    pub fn max() -> Self {
        Precedence { value: 100 }
    }
}

/// Associativity (left or right).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    /// Left-associative.
    Left,
    /// Right-associative.
    Right,
}


// === Numbers ===

/// Alternate numeric bases (decimal is the default).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Reflect, Deserialize)]
pub enum Base {
    /// Base 2.
    Binary,
    /// Base 8.
    Octal,
    /// Base 16.
    Hexadecimal,
}


// === Macro-based implementation ===

macro_rules! generate_token_aliases {
    (
        $(#$enum_meta:tt)*
        pub enum $enum:ident {
            $(
                $(#$variant_meta:tt)*
                $variant:ident $({ $(pub $field:ident : $field_ty:ty),* $(,)? })?
            ),* $(,)?
        }
    ) => { paste!{
        $(
            /// Token variant alias.
            pub type $variant<'s> = Token<'s, variant::$variant>;

            /// Constructor.
            pub fn [<$variant:snake:lower>]<'s> (
                left_offset: impl Into<Offset<'s>>,
                code: impl Into<Code<'s>>,
                $($($field : $field_ty),*)?
            ) -> $variant<'s> {
                Token(left_offset, code, variant::$variant($($($field),*)?))
            }

            /// Constructor.
            pub fn [<$variant:snake:lower _>]<'s> (
                left_offset: impl Into<Offset<'s>>,
                code: impl Into<Code<'s>>,
                $($($field : $field_ty),*)?
            ) -> Token<'s> {
                Token(left_offset, code, variant::$variant($($($field),*)?)).into()
            }

            impl<'s> From<Token<'s, variant::$variant>> for Token<'s, Variant> {
                fn from(token: Token<'s, variant::$variant>) -> Self {
                    token.map_variant(|t| t.into())
                }
            }
        )*
    }};
}

macro_rules! define_token_type {
    ($($ts:tt)*) => {
        /// All token variants.
        pub mod variant {
            use super::*;
            $($ts)*
        }
        generate_token_aliases! { $($ts)* }
    };
}

with_token_definition!(define_token_type());
pub use variant::Variant;
