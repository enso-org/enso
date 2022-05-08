use crate::prelude::*;

use crate::location;
use crate::source;
use crate::token;
use crate::token::Token;
use enso_shapely_macros::tagged_enum;
use location::Span;



// ===========
// === Ast ===
// ===========

/// The Abstract Syntax Tree of the language.
///
/// # Connection to the source file
/// Please note, that the AST does NOT contain sources, it keeps track of the char offsets only. If
/// you want to pretty print it, you should attach sources to it. The easiest way to do it is by
/// using the [`sources::With`] data, for example as:
/// ```text
/// println!("{:#?}", source::With::new(str, &ast));
/// ```
pub type Ast = location::With<Type>;

/// Macro providing [`Ast`] type definition. It is used to both define the ast [`Type`], and to
/// define impls for every token type in other modules.
#[macro_export]
macro_rules! with_ast_definition { ($f:ident ($($args:tt)*)) => { $f! { $($args)*
    #[tagged_enum(boxed)]
    #[derive(Clone, Eq, PartialEq, Visitor)]
    pub enum Type {
        /// Invalid [`Ast`] fragment with an attached [`Error`].
        Invalid {
            pub error: Error,
            pub ast: Ast,
        },
        /// A simple identifier, like `foo` or `bar`.
        Ident {
            pub token: token::Ident,
        },
        /// A simple application, like `print "hello"`.
        App {
            pub func: Ast,
            pub arg: Ast,
        },
        /// Application of an operator, like `a + b`. The left or right operands might be missing,
        /// thus creating an operator section like `a +`, `+ b`, or simply `+`. See the
        /// [`OprSectionBoundary`] variant to learn more about operator section scope.
        OprApp {
            pub lhs: Option<Ast>,
            pub opr: OperatorOrError,
            pub rhs: Option<Ast>,
        },
        /// Defines the point where operator sections should be expanded to lambdas. Let's consider
        /// the expression `map (.sum 1)`. It should be desugared to `map (x -> x.sum 1)`, not to
        /// `map ((x -> x.sum) 1)`. The expression `.sum` will be parsed as operator section
        /// ([`OprApp`] with left operand missing), and the [`OprSectionBoundary`] will be placed
        /// around the whole `.sum 1` expression.
        OprSectionBoundary {
            pub ast: Ast,
        },
        /// An application of a multi-segment function, such as `if ... then ... else ...`. Each
        /// segment starts with a token and contains an expression. Some multi-segment functions can
        /// have a prefix, an expression that is argument of the function, but is placed before the
        /// first token. Lambda is a good example for that. In an expression
        /// `Vector x y z -> x + y + z`, the `->` token is the beginning of the section, the
        /// `x + y + z` is the section body, and `Vector x y z` is the prefix of this function
        /// application.
        MultiSegmentApp {
            pub prefix: Option<Ast>,
            pub segments: NonEmptyVec<MultiSegmentAppSegment>,
        }
    }
}};}

macro_rules! define_ast_type {
    ($($ts:tt)*) => {
        $($ts)*
        define_debug_impls!{$($ts)*}
    };
}

macro_rules! define_debug_impls {
    (
        $(#$enum_meta:tt)*
        pub enum Type {
            $(
                $(#$meta:tt)*
                $variant:ident {
                    $(
                        pub $field:ident: $field_ty:ty
                    ),* $(,)?
               }
            ),* $(,)?
        }
    ) => {
        impl<'s> Debug for source::With<'s, &Type> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.data {
                    $(Type::$variant(t) => Debug::fmt(&self.with_data(t), f)),*
                }
            }
        }

        $(
            impl<'s> Debug for source::With<'s, &$variant> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.debug_struct(stringify!($variant))
                        $( .field(stringify!($field), &self.with_data(&self.$field)) )*
                        .finish()
                }
            }
        )*
    };
}
with_ast_definition!(define_ast_type());


// === Invalid ===

/// Error of parsing attached to an [`Ast`] node.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Visitor)]
#[allow(missing_docs)]
pub struct Error {
    pub message: &'static str,
}

impl Error {
    /// Constructor.
    pub fn new(message: &'static str) -> Self {
        Self { message }
    }
}

impl<'s> Debug for source::With<'s, &Error> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.data, f)
    }
}

impl Ast {
    /// Constructor.
    pub fn invalid(error: Error, ast: Ast) -> Self {
        let location = ast.span;
        let data = Type::from(Invalid(error, ast));
        location.with(data)
    }

    /// Constructor.
    pub fn with_error(self, message: &'static str) -> Self {
        Ast::invalid(Error::new(message), self)
    }
}


// === App ===

impl Ast {
    /// Constructor.
    pub fn app(func: Ast, arg: Ast) -> Ast {
        let (left_offset_span, func) = func.split_at_start();
        let total = left_offset_span.extended_to(&arg);
        let ast_data = Type::from(App(func, arg));
        total.with(ast_data)
    }
}


// === OprApp ===

/// Operator or [`MultipleOperatorError`].
pub type OperatorOrError = Result<location::With<token::Operator>, MultipleOperatorError>;

/// Error indicating multiple operators found next to each other, like `a + * b`.
#[derive(Clone, Debug, Eq, PartialEq, Visitor)]
#[allow(missing_docs)]
pub struct MultipleOperatorError {
    pub operators: NonEmptyVec<location::With<token::Operator>>,
}

impl MultipleOperatorError {
    /// Constructor.
    pub fn new(operators: NonEmptyVec<location::With<token::Operator>>) -> Self {
        Self { operators }
    }
}

impl Ast {
    /// Constructor.
    pub fn opr_app(mut lhs: Option<Ast>, mut opr: OperatorOrError, rhs: Option<Ast>) -> Ast {
        let left_offset_token = if let Some(lhs_val) = lhs {
            let (left_offset_token, new_lhs) = lhs_val.split_at_start();
            lhs = Some(new_lhs);
            left_offset_token
        } else {
            match &mut opr {
                Ok(opr) => {
                    let (left_offset_token, new_opr) = opr.split_at_start();
                    *opr = new_opr;
                    left_offset_token
                }
                Err(err) => {
                    let first = err.operators.first_mut();
                    let (left_offset_token, new_opr) = first.split_at_start();
                    *first = new_opr;
                    left_offset_token
                }
            }
        };
        let total = if let Some(ref rhs) = rhs {
            left_offset_token.extended_to(rhs)
        } else {
            match &opr {
                Ok(opr) => left_offset_token.extended_to(opr),
                Err(e) => left_offset_token.extended_to(e.operators.last()),
            }
        };
        let ast_data = Type::from(OprApp(lhs, opr, rhs));
        total.with(ast_data)
    }
}

impl<'s> Debug for source::With<'s, &MultipleOperatorError> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MultipleOperatorError")
            .field("operators", &self.with_data(&self.operators))
            .finish()
    }
}

impl<'s> Debug for source::With<'s, &MultiSegmentAppSegment> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MultiSegmentAppSegment")
            .field("header", &self.with_data(&self.header))
            .field("body", &self.with_data(&self.body))
            .finish()
    }
}


// === OprSectionBoundary ===

impl Ast {
    /// Constructor.
    pub fn opr_section_boundary(section: Ast) -> Ast {
        let (left_offset_span, section) = section.split_at_start();
        let total = left_offset_span.extended_to(&section);
        let ast_data = Type::from(OprSectionBoundary(section));
        total.with(ast_data)
    }
}


// === MultiSegmentApp ===

/// A segment of [`MultiSegmentApp`], like `if cond` in the `if cond then ok else fail` expression.
#[derive(Clone, Debug, Eq, PartialEq, Visitor)]
#[allow(missing_docs)]
pub struct MultiSegmentAppSegment {
    pub header: Token,
    pub body:   Option<Ast>,
}

impl Ast {
    /// Constructor.
    pub fn multi_segment_app(
        mut prefix: Option<Ast>,
        mut segments: NonEmptyVec<MultiSegmentAppSegment>,
    ) -> Self {
        let left_span = if let Some(prefix) = prefix.as_mut() {
            prefix.trim_left()
        } else {
            let first = segments.first_mut();
            let (left_span, left_trimmed_token) = first.header.split_at_start();
            first.header = left_trimmed_token;
            left_span
        };
        let last_segment = segments.last();
        let total = if let Some(last_segment_body) = &last_segment.body {
            left_span.extended_to(last_segment_body)
        } else {
            left_span.extended_to(&last_segment.header)
        };
        let data = Type::from(MultiSegmentApp { prefix, segments });
        total.with(data)
    }
}



// ====================
// === Ast Visitors ===
// ====================

/// The visitor pattern for [`AST`].
///
/// # Visitor traits
/// This macro defines visitor traits, such as [`AstVisitor`] or [`SpanVisitor`], which provide
/// abstraction for building a visitor for [`Ast`] and [`Span`] elements respectively. A visitor is
/// a struct that is modified when traversing all elements of [`Ast`]. Visitors are also capable of
/// tracking when they entered or exited a nested [`Ast`] structure, and they can control how deep
/// the traversal should be performed. To learn more, see the [`RefCollectorVisitor`]
/// implementation, which traverses [`Ast`] and collects references to all [`Ast`] nodes in a
/// vector.
///
/// # Visitable traits
/// This macro also defines visitable traits, such as [`AstVisitable`] or [`SpanVisitable`], which
/// provide [`Ast`] elements with such functions as [`visit`], [`visit_mut`], [`visit_span`], or
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
/// Such definition could be implemented for every [`Ast`] node (the [`T`] parameter).
/// Unfortunately, due to Rust compiler errors, Rust is not able to compile such a definition. We
/// could move to it as soon as this error gets resolved:
/// https://github.com/rust-lang/rust/issues/96634.
macro_rules! define_visitor {
    ($name:ident, $visit:ident, $visit_mut:ident) => {
        paste! {
            define_visitor_internal! {
                $name,
                $visit,
                $visit_mut,
                [<$name Visitor>],
                [<$name VisitorMut>],
                [<$name Visitable>],
                [<$name VisitableMut>]
            }
        }
    };
}

/// Internal helper for [`define_visitor`].
macro_rules! define_visitor_internal {
    (
        $name:ident,
        $visit:ident,
        $visit_mut:ident,
        $visitor:ident,
        $visitor_mut:ident,
        $visitable:ident,
        $visitable_mut:ident
    ) => {
        /// The visitor trait. See documentation of [`define_visitor`] to learn more.
        #[allow(missing_docs)]
        pub trait $visitor<'a> {
            fn before_visiting_children(&mut self) {}
            fn after_visiting_children(&mut self) {}
            /// Visit the given [`ast`] node. If it returns [`true`], children of the node will be
            /// traversed as well.
            fn visit(&mut self, ast: &'a $name) -> bool;
        }

        /// The visitor trait. See documentation of [`define_visitor`] to learn more.
        #[allow(missing_docs)]
        pub trait $visitor_mut {
            fn before_visiting_children(&mut self) {}
            fn after_visiting_children(&mut self) {}
            /// Visit the given [`ast`] node. If it returns [`true`], children of the node will be
            /// traversed as well.
            fn visit_mut(&mut self, ast: &mut $name) -> bool;
        }

        /// The visitable trait. See documentation of [`define_visitor`] to learn more.
        #[allow(missing_docs)]
        pub trait $visitable<'a> {
            fn $visit<V: $visitor<'a>>(&'a self, _visitor: &mut V) {}
        }

        /// The visitable trait. See documentation of [`define_visitor`] to learn more.
        #[allow(missing_docs)]
        pub trait $visitable_mut<'a> {
            fn $visit_mut<V: $visitor_mut>(&'a mut self, _visitor: &mut V) {}
        }

        impl<'a, T: $visitable<'a>> $visitable<'a> for Box<T> {
            fn $visit<V: $visitor<'a>>(&'a self, visitor: &mut V) {
                $visitable::$visit(&**self, visitor)
            }
        }

        impl<'a, T: $visitable_mut<'a>> $visitable_mut<'a> for Box<T> {
            fn $visit_mut<V: $visitor_mut>(&'a mut self, visitor: &mut V) {
                $visitable_mut::$visit_mut(&mut **self, visitor)
            }
        }

        impl<'a, T: $visitable<'a>> $visitable<'a> for Option<T> {
            fn $visit<V: $visitor<'a>>(&'a self, visitor: &mut V) {
                if let Some(elem) = self {
                    $visitable::$visit(elem, visitor)
                }
            }
        }

        impl<'a, T: $visitable_mut<'a>> $visitable_mut<'a> for Option<T> {
            fn $visit_mut<V: $visitor_mut>(&'a mut self, visitor: &mut V) {
                if let Some(elem) = self {
                    $visitable_mut::$visit_mut(elem, visitor)
                }
            }
        }

        impl<'a, T: $visitable<'a>, E: $visitable<'a>> $visitable<'a> for Result<T, E> {
            fn $visit<V: $visitor<'a>>(&'a self, visitor: &mut V) {
                match self {
                    Ok(elem) => $visitable::$visit(elem, visitor),
                    Err(elem) => $visitable::$visit(elem, visitor),
                }
            }
        }

        impl<'a, T: $visitable_mut<'a>, E: $visitable_mut<'a>> $visitable_mut<'a> for Result<T, E> {
            fn $visit_mut<V: $visitor_mut>(&'a mut self, visitor: &mut V) {
                match self {
                    Ok(elem) => $visitable_mut::$visit_mut(elem, visitor),
                    Err(elem) => $visitable_mut::$visit_mut(elem, visitor),
                }
            }
        }

        impl<'a, T: $visitable<'a>> $visitable<'a> for Vec<T> {
            fn $visit<V: $visitor<'a>>(&'a self, visitor: &mut V) {
                self.iter().map(|t| $visitable::$visit(t, visitor)).for_each(drop);
            }
        }

        impl<'a, T: $visitable_mut<'a>> $visitable_mut<'a> for Vec<T> {
            fn $visit_mut<V: $visitor_mut>(&'a mut self, visitor: &mut V) {
                self.iter_mut().map(|t| $visitable_mut::$visit_mut(t, visitor)).for_each(drop);
            }
        }

        impl<'a, T: $visitable<'a>> $visitable<'a> for NonEmptyVec<T> {
            fn $visit<V: $visitor<'a>>(&'a self, visitor: &mut V) {
                self.iter().map(|t| $visitable::$visit(t, visitor)).for_each(drop);
            }
        }

        impl<'a, T: $visitable_mut<'a>> $visitable_mut<'a> for NonEmptyVec<T> {
            fn $visit_mut<V: $visitor_mut>(&'a mut self, visitor: &mut V) {
                self.iter_mut().map(|t| $visitable_mut::$visit_mut(t, visitor)).for_each(drop);
            }
        }

        impl<'a> $visitable<'a> for &str {}
        impl<'a> $visitable<'a> for str {}

        impl<'a> $visitable_mut<'a> for &str {}
        impl<'a> $visitable_mut<'a> for str {}
    };
}

macro_rules! define_visitor_for_tokens {
    (
        $(#$kind_meta:tt)*
        pub enum $kind:ident {
            $( $variant:ident $({$($args:tt)*})? ),* $(,)?
        }
    ) => {
        impl<'a> AstVisitable<'a> for token::$kind {}
        impl<'a> AstVisitableMut<'a> for token::$kind {}
        impl<'a> SpanVisitable<'a> for token::$kind {}
        impl<'a> SpanVisitableMut<'a> for token::$kind {}
        $(
            impl<'a> AstVisitable<'a> for token::$variant {}
            impl<'a> AstVisitableMut<'a> for token::$variant {}
            impl<'a> SpanVisitable<'a> for token::$variant {}
            impl<'a> SpanVisitableMut<'a> for token::$variant {}
        )*
    };
}

define_visitor!(Ast, visit, visit_mut);
define_visitor!(Span, visit_span, visit_span_mut);
crate::with_token_definition!(define_visitor_for_tokens());


// === Special cases ===

impl<'a> AstVisitable<'a> for Ast {
    fn visit<V: AstVisitor<'a>>(&'a self, visitor: &mut V) {
        if visitor.visit(self) {
            self.elem.visit(visitor)
        }
    }
}

impl<'a> AstVisitableMut<'a> for Ast {
    fn visit_mut<V: AstVisitorMut>(&'a mut self, visitor: &mut V) {
        if visitor.visit_mut(self) {
            self.elem.visit_mut(visitor)
        }
    }
}

impl<'a, T: AstVisitable<'a>> AstVisitable<'a> for location::With<T> {
    default fn visit<V: AstVisitor<'a>>(&'a self, visitor: &mut V) {
        self.elem.visit(visitor)
    }
}

impl<'a, T: AstVisitableMut<'a>> AstVisitableMut<'a> for location::With<T> {
    default fn visit_mut<V: AstVisitorMut>(&'a mut self, visitor: &mut V) {
        self.elem.visit_mut(visitor)
    }
}

impl<'a, T: SpanVisitable<'a>> SpanVisitable<'a> for location::With<T> {
    fn visit_span<V: SpanVisitor<'a>>(&'a self, visitor: &mut V) {
        if visitor.visit(&self.span) {
            self.elem.visit_span(visitor)
        }
    }
}

impl<'a, T: SpanVisitableMut<'a>> SpanVisitableMut<'a> for location::With<T> {
    fn visit_span_mut<V: SpanVisitorMut>(&'a mut self, visitor: &mut V) {
        if visitor.visit_mut(&mut self.span) {
            self.elem.visit_span_mut(visitor)
        }
    }
}



// ===========================
// === RefCollectorVisitor ===
// ===========================

/// A visitor collecting references to all [`Ast`] nodes.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct RefCollectorVisitor<'a> {
    pub vec: Vec<&'a Ast>,
}

impl<'a> AstVisitor<'a> for RefCollectorVisitor<'a> {
    fn visit(&mut self, ast: &'a Ast) -> bool {
        self.vec.push(ast);
        true
    }
}

impl Ast {
    /// Collect references to all [`Ast`] nodes and return them in a vector.
    pub fn collect_vec_ref(&self) -> Vec<&Ast> {
        let mut visitor = RefCollectorVisitor::default();
        self.visit(&mut visitor);
        visitor.vec
    }
}



// =================
// === FnVisitor ===
// =================

/// A visitor allowing running a function on every [`Ast`] node.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct FnVisitor<F>(pub F);

impl<'a, T, F: Fn(&'a Ast) -> T> AstVisitor<'a> for FnVisitor<F> {
    fn visit(&mut self, ast: &'a Ast) -> bool {
        (self.0)(ast);
        true
    }
}

impl<T, F: Fn(&mut Ast) -> T> AstVisitorMut for FnVisitor<F> {
    fn visit_mut(&mut self, ast: &mut Ast) -> bool {
        (self.0)(ast);
        true
    }
}

impl<'a, T, F: Fn(&'a Span) -> T> SpanVisitor<'a> for FnVisitor<F> {
    fn visit(&mut self, ast: &'a Span) -> bool {
        (self.0)(ast);
        true
    }
}

impl<T, F: Fn(&mut Span) -> T> SpanVisitorMut for FnVisitor<F> {
    fn visit_mut(&mut self, ast: &mut Span) -> bool {
        (self.0)(ast);
        true
    }
}


impl Ast {
    /// Map the provided function over each [`Ast`] node. The function results will be discarded.
    pub fn map<T>(&self, f: impl Fn(&Ast) -> T) {
        let mut visitor = FnVisitor(f);
        self.visit(&mut visitor);
    }

    /// Map the provided function over each [`Ast`] node. The function results will be discarded.
    pub fn map_mut<T>(&mut self, f: impl Fn(&mut Ast) -> T) {
        let mut visitor = FnVisitor(f);
        self.visit_mut(&mut visitor);
    }

    /// Map the provided function over each [`Span`] element. The function results will be
    /// discarded.
    pub fn map_span<T>(&self, f: impl Fn(&Span) -> T) {
        let mut visitor = FnVisitor(f);
        self.visit_span(&mut visitor);
    }

    /// Map the provided function over each [`Span`] element. The function results will be
    /// discarded.
    pub fn map_span_mut<T>(&mut self, f: impl Fn(&mut Span) -> T) {
        let mut visitor = FnVisitor(f);
        self.visit_span_mut(&mut visitor);
    }

    /// Remove all span information. This is mainly used for tests to compare AST structure.
    pub fn remove_span_info(&mut self) {
        self.map_span_mut(mem::take)
    }

    /// Remove all span information. This is mainly used for tests to compare AST structure.
    pub fn with_removed_span_info(mut self) -> Self {
        self.remove_span_info();
        self
    }
}
