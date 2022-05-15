//! Implementation of Syntax Tree, known as well as Abstract Syntax Tree, or AST.
use crate::prelude::*;

use crate::source::span;
use crate::source::span::Span;
use crate::source::span::SpanRefMut;
use crate::source::Offset;
use crate::syntax::token;
use crate::syntax::ItemRef;
// use crate::syntax::token::Token;

use crate::span_builder;
use enso_parser_syntax_tree_visitor::Visitor;
use enso_shapely_macros::tagged_enum;


// ============
// === Tree ===
// ============

/// The Abstract Syntax Tree of the language.
///
/// # Connection to the source file
/// Please note, that the AST does NOT contain sources, it keeps track of the char offsets only. If
/// you want to pretty print it, you should attach sources to it. The easiest way to do it is by
/// using the [`sources::With`] data, for example as:
/// ```text
/// println!("{:#?}", source::With::new(str, &ast));
/// ```
// pub type Tree = span::With<Type>;

#[derive(Clone, Deref, DerefMut, Eq, PartialEq)]
pub struct Tree<'s> {
    #[deref]
    #[deref_mut]
    pub variant: Box<Type<'s>>,
    pub span:    Span<'s>,
}

pub fn Tree<'s>(span: Span<'s>, variant: impl Into<Box<Type<'s>>>) -> Tree<'s> {
    let variant = variant.into();
    Tree { variant, span }
}

impl<'s> Debug for Tree<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[off: {}] ", self.span.left_offset.visible)?;
        Debug::fmt(&self.variant, f)
    }
}

impl<'s> AsRef<Span<'s>> for Tree<'s> {
    fn as_ref(&self) -> &Span<'s> {
        &self.span
    }
}

/// Macro providing [`Tree`] type definition. It is used to both define the ast [`Type`], and to
/// define impls for every token type in other modules.
#[macro_export]
macro_rules! with_ast_definition { ($f:ident ($($args:tt)*)) => { $f! { $($args)*
    /// [`Tree`] variants definition. See its docs to learn more.
    #[tagged_enum]
    #[derive(Clone, Eq, PartialEq, Visitor)]
    pub enum Type<'s> {
        /// Invalid [`Tree`] fragment with an attached [`Error`].
        Invalid {
            pub error: Error,
            pub ast: Tree<'s>,
        },
        /// A simple identifier, like `foo` or `bar`.
        Ident {
            pub token: token::Ident<'s>,
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
            pub prefix: Option<Tree<'s>>,
            pub segments: NonEmptyVec<MultiSegmentAppSegment<'s>>,
        }
    }
}};}

macro_rules! identity {
    ($($ts:tt)*) => {
        $($ts)*
    };
}

with_ast_definition!(identity());



// === Invalid ===

/// Error of parsing attached to an [`Tree`] node.
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


impl<'s> Tree<'s> {
    /// Constructor.
    pub fn invalid(error: Error, mut ast: Tree<'s>) -> Self {
        let span = ast.span.trim_as_first_child();
        Tree(span, Type::from(Invalid(error, ast)))
    }

    /// Constructor.
    pub fn with_error(self, message: &'static str) -> Self {
        Tree::invalid(Error::new(message), self)
    }
}



// === Ident ===

impl<'s> Tree<'s> {
    /// Constructor.
    pub fn ident(mut token: token::Ident<'s>) -> Tree<'s> {
        let span = span_builder![token];
        Tree(span, Type::from(Ident(token)))
    }
}


// === App ===

impl<'s> Tree<'s> {
    /// Constructor.
    pub fn app(mut func: Tree<'s>, mut arg: Tree<'s>) -> Tree<'s> {
        let span = span_builder![func, arg];
        Tree(span, Type::from(App(func, arg)))
    }
}


// === OprApp ===

/// Operator or [`MultipleOperatorError`].
pub type OperatorOrError<'s> = Result<token::Operator<'s>, MultipleOperatorError<'s>>;

/// Error indicating multiple operators found next to each other, like `a + * b`.
#[derive(Clone, Debug, Eq, PartialEq, Visitor)]
#[allow(missing_docs)]
pub struct MultipleOperatorError<'s> {
    pub operators: NonEmptyVec<token::Operator<'s>>,
}

impl<'s> MultipleOperatorError<'s> {
    /// Constructor.
    pub fn new(operators: NonEmptyVec<token::Operator<'s>>) -> Self {
        Self { operators }
    }
}


impl<'s, S> span::Build<S> for MultipleOperatorError<'s>
where NonEmptyVec<token::Operator<'s>>: span::Build<S>
{
    type Output = <NonEmptyVec<token::Operator<'s>> as span::Build<S>>::Output;
    fn build(&mut self, builder: span::Builder<S>) -> Self::Output {
        self.operators.build(builder)
    }
}

impl<'s> Tree<'s> {
    /// Constructor.
    pub fn opr_app(
        mut lhs: Option<Tree<'s>>,
        mut opr: OperatorOrError<'s>,
        mut rhs: Option<Tree<'s>>,
    ) -> Tree<'s> {
        let span = span_builder![lhs, opr, rhs];
        Tree(span, Type::from(OprApp(lhs, opr, rhs)))
    }
}



// === OprSectionBoundary ===

impl<'s> Tree<'s> {
    /// Constructor.
    pub fn opr_section_boundary(mut section: Tree<'s>) -> Tree<'s> {
        let span = span_builder![section];
        Tree(span, Type::from(OprSectionBoundary(section)))
    }
}


// === MultiSegmentApp ===

/// A segment of [`MultiSegmentApp`], like `if cond` in the `if cond then ok else fail` expression.
#[derive(Clone, Debug, Eq, PartialEq, Visitor)]
#[allow(missing_docs)]
pub struct MultiSegmentAppSegment<'s> {
    pub header: token::Token<'s>,
    pub body:   Option<Tree<'s>>,
}

impl<'s, S> span::Build<S> for MultiSegmentAppSegment<'s>
where token::Token<'s>: span::Build<S, Output = Span<'s>>
{
    type Output = Span<'s>;
    fn build(&mut self, builder: span::Builder<S>) -> Self::Output {
        builder.add(&mut self.header).add(&mut self.body).span
    }
}

impl<'s> Tree<'s> {
    /// Constructor.
    pub fn multi_segment_app(
        mut prefix: Option<Tree<'s>>,
        mut segments: NonEmptyVec<MultiSegmentAppSegment<'s>>,
    ) -> Self {
        let span = span_builder![prefix, segments];
        Tree(span, Type::from(MultiSegmentApp(prefix, segments)))
    }
}



// =====================
// === Tree Visitors ===
// =====================

/// The visitor pattern for [`AST`].
///
/// # Visitor traits
/// This macro defines visitor traits, such as [`TreeVisitor`] or [`SpanVisitor`], which provide
/// abstraction for building a visitor for [`Tree`] and [`Span`] elements respectively. A visitor is
/// a struct that is modified when traversing all elements of [`Tree`]. Visitors are also capable of
/// tracking when they entered or exited a nested [`Tree`] structure, and they can control how deep
/// the traversal should be performed. To learn more, see the [`RefCollectorVisitor`]
/// implementation, which traverses [`Tree`] and collects references to all [`Tree`] nodes in a
/// vector.
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
        pub trait $visitor<'a, 's> {
            fn before_visiting_children(&mut self) {}
            fn after_visiting_children(&mut self) {}
            /// Visit the given [`ast`] node. If it returns [`true`], children of the node will be
            /// traversed as well.
            fn visit(&mut self, ast: &'a $name<'s>) -> bool;
        }

        /// The visitor trait. See documentation of [`define_visitor`] to learn more.
        #[allow(missing_docs)]
        pub trait $visitor_mut<'s> {
            fn before_visiting_children(&mut self) {}
            fn after_visiting_children(&mut self) {}
            /// Visit the given [`ast`] node. If it returns [`true`], children of the node will be
            /// traversed as well.
            fn visit_mut(&mut self, ast: &mut $name<'s>) -> bool;
        }

        /// The visitable trait. See documentation of [`define_visitor`] to learn more.
        #[allow(missing_docs)]
        pub trait $visitable<'a, 's> {
            fn $visit<V: $visitor<'a, 's>>(&'a self, _visitor: &mut V) {}
        }

        /// The visitable trait. See documentation of [`define_visitor`] to learn more.
        #[allow(missing_docs)]
        pub trait $visitable_mut<'a, 's> {
            fn $visit_mut<V: $visitor_mut<'s>>(&'a mut self, _visitor: &mut V) {}
        }

        impl<'a, 's, T: $visitable<'a, 's>> $visitable<'a, 's> for Box<T> {
            fn $visit<V: $visitor<'a, 's>>(&'a self, visitor: &mut V) {
                $visitable::$visit(&**self, visitor)
            }
        }

        impl<'a, 's, T: $visitable_mut<'a, 's>> $visitable_mut<'a, 's> for Box<T> {
            fn $visit_mut<V: $visitor_mut<'s>>(&'a mut self, visitor: &mut V) {
                $visitable_mut::$visit_mut(&mut **self, visitor)
            }
        }

        impl<'a, 's, T: $visitable<'a, 's>> $visitable<'a, 's> for Option<T> {
            fn $visit<V: $visitor<'a, 's>>(&'a self, visitor: &mut V) {
                if let Some(elem) = self {
                    $visitable::$visit(elem, visitor)
                }
            }
        }

        impl<'a, 's, T: $visitable_mut<'a, 's>> $visitable_mut<'a, 's> for Option<T> {
            fn $visit_mut<V: $visitor_mut<'s>>(&'a mut self, visitor: &mut V) {
                if let Some(elem) = self {
                    $visitable_mut::$visit_mut(elem, visitor)
                }
            }
        }

        impl<'a, 's, T: $visitable<'a, 's>, E: $visitable<'a, 's>> $visitable<'a, 's>
            for Result<T, E>
        {
            fn $visit<V: $visitor<'a, 's>>(&'a self, visitor: &mut V) {
                match self {
                    Ok(elem) => $visitable::$visit(elem, visitor),
                    Err(elem) => $visitable::$visit(elem, visitor),
                }
            }
        }

        impl<'a, 's, T: $visitable_mut<'a, 's>, E: $visitable_mut<'a, 's>> $visitable_mut<'a, 's>
            for Result<T, E>
        {
            fn $visit_mut<V: $visitor_mut<'s>>(&'a mut self, visitor: &mut V) {
                match self {
                    Ok(elem) => $visitable_mut::$visit_mut(elem, visitor),
                    Err(elem) => $visitable_mut::$visit_mut(elem, visitor),
                }
            }
        }

        impl<'a, 's, T: $visitable<'a, 's>> $visitable<'a, 's> for Vec<T> {
            fn $visit<V: $visitor<'a, 's>>(&'a self, visitor: &mut V) {
                self.iter().map(|t| $visitable::$visit(t, visitor)).for_each(drop);
            }
        }

        impl<'a, 's, T: $visitable_mut<'a, 's>> $visitable_mut<'a, 's> for Vec<T> {
            fn $visit_mut<V: $visitor_mut<'s>>(&'a mut self, visitor: &mut V) {
                self.iter_mut().map(|t| $visitable_mut::$visit_mut(t, visitor)).for_each(drop);
            }
        }

        impl<'a, 's, T: $visitable<'a, 's>> $visitable<'a, 's> for NonEmptyVec<T> {
            fn $visit<V: $visitor<'a, 's>>(&'a self, visitor: &mut V) {
                self.iter().map(|t| $visitable::$visit(t, visitor)).for_each(drop);
            }
        }

        impl<'a, 's, T: $visitable_mut<'a, 's>> $visitable_mut<'a, 's> for NonEmptyVec<T> {
            fn $visit_mut<V: $visitor_mut<'s>>(&'a mut self, visitor: &mut V) {
                self.iter_mut().map(|t| $visitable_mut::$visit_mut(t, visitor)).for_each(drop);
            }
        }

        impl<'a, 's> $visitable<'a, 's> for &str {}
        impl<'a, 's> $visitable<'a, 's> for str {}

        impl<'a, 's> $visitable_mut<'a, 's> for &str {}
        impl<'a, 's> $visitable_mut<'a, 's> for str {}
    };
}

macro_rules! define_visitor_for_tokens {
    (
        $(#$kind_meta:tt)*
        pub enum $kind:ident {
            $( $variant:ident $({$($args:tt)*})? ),* $(,)?
        }
    ) => {
        impl<'a, 's> TreeVisitable<'a, 's> for token::$kind {}
        impl<'a, 's> TreeVisitableMut<'a, 's> for token::$kind {}
        // impl<'a, 's> SpanVisitable<'a> for token::$kind {}
        // impl<'a, 's> SpanVisitableMut<'a> for token::$kind {}
        // $(
        //     impl<'a, 's> TreeVisitable<'a, 's> for token::$variant<'s> {}
        //     impl<'a, 's> TreeVisitableMut<'a, 's> for token::$variant<'s> {}
        //     // impl<'a, 's> SpanVisitable<'a> for token::$variant {}
        //     // impl<'a, 's> SpanVisitableMut<'a> for token::$variant {}
        // )*
    };
}

define_visitor!(Tree, visit, visit_mut);
// define_visitor!(Span, visit_span, visit_span_mut);
crate::with_token_definition!(define_visitor_for_tokens());

impl<'a, 's, T> TreeVisitable<'a, 's> for token::Token<'s, T> {}
impl<'a, 's, T> TreeVisitableMut<'a, 's> for token::Token<'s, T> {}


// === Special cases ===

impl<'a, 's> TreeVisitable<'a, 's> for Tree<'s> {
    fn visit<V: TreeVisitor<'a, 's>>(&'a self, visitor: &mut V) {
        if visitor.visit(self) {
            self.variant.visit(visitor)
        }
    }
}

impl<'a, 's> TreeVisitableMut<'a, 's> for Tree<'s> {
    fn visit_mut<V: TreeVisitorMut<'s>>(&'a mut self, visitor: &mut V) {
        if visitor.visit_mut(self) {
            self.variant.visit_mut(visitor)
        }
    }
}

// impl<'a, 's> SpanVisitable<'a, 's> for Tree<'s> {
//     fn visit_span<V: SpanVisitor<'a, 's>>(&'a self, visitor: &mut V) {
//         if visitor.visit(&self.span) {
//             self.variant.visit_span(visitor)
//         }
//     }
// }
//
// impl<'a, 's, T> SpanVisitable<'a, 's> for token::Token<'s, T> {
//     fn visit_span<V: SpanVisitor<'a, 's>>(&'a self, visitor: &mut V) {
//         // visitor.visit(&self.span());
//         // FIXME
//     }
// }

impl<'a, 's> SpanVisitableMut<'a, 's> for Tree<'s> {
    fn visit_span_mut<V: SpanVisitorMut<'a, 's>>(&'a mut self, visitor: &mut V) {
        if visitor.visit_mut(SpanRefMut {
            left_offset: &mut self.span.left_offset,
            length:      self.span.length,
        }) {
            self.variant.visit_span_mut(visitor)
        }
    }
}

impl<'a, 't, 's, T> SpanVisitableMut<'a, 's> for token::Token<'s, T> {
    fn visit_span_mut<V: SpanVisitorMut<'a, 's>>(&'a mut self, visitor: &mut V) {
        let length = self.len();
        visitor.visit_mut(SpanRefMut { left_offset: &mut self.left_offset, length });
    }
}


/// The visitor trait. See documentation of [`define_visitor`] to learn more.
#[allow(missing_docs)]
pub trait SpanVisitorMut<'a, 's> {
    fn before_visiting_children(&mut self) {}
    fn after_visiting_children(&mut self) {}
    /// Visit the given [`ast`] node. If it returns [`true`], children of the node will be
    /// traversed as well.
    fn visit_mut(&mut self, ast: SpanRefMut<'a, 's>) -> bool;
}

/// The visitable trait. See documentation of [`define_visitor`] to learn more.
#[allow(missing_docs)]
pub trait SpanVisitableMut<'a, 's> {
    fn visit_span_mut<V: SpanVisitorMut<'a, 's>>(&'a mut self, _visitor: &mut V) {}
}
impl<'a, 't, 's, T: SpanVisitableMut<'a, 's>> SpanVisitableMut<'a, 's> for Box<T> {
    fn visit_span_mut<V: SpanVisitorMut<'a, 's>>(&'a mut self, visitor: &mut V) {
        SpanVisitableMut::visit_span_mut(&mut **self, visitor)
    }
}
impl<'a, 't, 's, T: SpanVisitableMut<'a, 's>> SpanVisitableMut<'a, 's> for Option<T> {
    fn visit_span_mut<V: SpanVisitorMut<'a, 's>>(&'a mut self, visitor: &mut V) {
        if let Some(elem) = self {
            SpanVisitableMut::visit_span_mut(elem, visitor)
        }
    }
}
impl<'a, 't, 's, T: SpanVisitableMut<'a, 's>, E: SpanVisitableMut<'a, 's>> SpanVisitableMut<'a, 's>
    for Result<T, E>
{
    fn visit_span_mut<V: SpanVisitorMut<'a, 's>>(&'a mut self, visitor: &mut V) {
        match self {
            Ok(elem) => SpanVisitableMut::visit_span_mut(elem, visitor),
            Err(elem) => SpanVisitableMut::visit_span_mut(elem, visitor),
        }
    }
}
impl<'a, 't, 's, T: SpanVisitableMut<'a, 's>> SpanVisitableMut<'a, 's> for Vec<T> {
    fn visit_span_mut<V: SpanVisitorMut<'a, 's>>(&'a mut self, visitor: &mut V) {
        self.iter_mut().map(|t| SpanVisitableMut::visit_span_mut(t, visitor)).for_each(drop);
    }
}
impl<'a, 't, 's, T: SpanVisitableMut<'a, 's>> SpanVisitableMut<'a, 's> for NonEmptyVec<T> {
    fn visit_span_mut<V: SpanVisitorMut<'a, 's>>(&'a mut self, visitor: &mut V) {
        self.iter_mut().map(|t| SpanVisitableMut::visit_span_mut(t, visitor)).for_each(drop);
    }
}
impl<'a, 's> SpanVisitableMut<'a, 's> for &str {}
impl<'a, 's> SpanVisitableMut<'a, 's> for str {}



/// The visitor trait. See documentation of [`define_visitor`] to learn more.
#[allow(missing_docs)]
pub trait ItemVisitor<'s, 'a> {
    fn before_visiting_children(&mut self) {}
    fn after_visiting_children(&mut self) {}
    /// Visit the given [`ast`] node. If it returns [`true`], children of the node will be
    /// traversed as well.
    fn visit_item(&mut self, ast: ItemRef<'s, 'a>) -> bool;
}

/// The visitable trait. See documentation of [`define_visitor`] to learn more.
#[allow(missing_docs)]
pub trait ItemVisitable<'s, 'a> {
    fn visit_item<V: ItemVisitor<'s, 'a>>(&'a mut self, _visitor: &mut V) {}
}
impl<'a, 't, 's, T: ItemVisitable<'s, 'a>> ItemVisitable<'s, 'a> for Box<T> {
    fn visit_item<V: ItemVisitor<'s, 'a>>(&'a mut self, visitor: &mut V) {
        ItemVisitable::visit_item(&mut **self, visitor)
    }
}
impl<'a, 't, 's, T: ItemVisitable<'s, 'a>> ItemVisitable<'s, 'a> for Option<T> {
    fn visit_item<V: ItemVisitor<'s, 'a>>(&'a mut self, visitor: &mut V) {
        if let Some(elem) = self {
            ItemVisitable::visit_item(elem, visitor)
        }
    }
}
impl<'a, 't, 's, T: ItemVisitable<'s, 'a>, E: ItemVisitable<'s, 'a>> ItemVisitable<'s, 'a>
    for Result<T, E>
{
    fn visit_item<V: ItemVisitor<'s, 'a>>(&'a mut self, visitor: &mut V) {
        match self {
            Ok(elem) => ItemVisitable::visit_item(elem, visitor),
            Err(elem) => ItemVisitable::visit_item(elem, visitor),
        }
    }
}
impl<'a, 't, 's, T: ItemVisitable<'s, 'a>> ItemVisitable<'s, 'a> for Vec<T> {
    fn visit_item<V: ItemVisitor<'s, 'a>>(&'a mut self, visitor: &mut V) {
        self.iter_mut().map(|t| ItemVisitable::visit_item(t, visitor)).for_each(drop);
    }
}
impl<'a, 't, 's, T: ItemVisitable<'s, 'a>> ItemVisitable<'s, 'a> for NonEmptyVec<T> {
    fn visit_item<V: ItemVisitor<'s, 'a>>(&'a mut self, visitor: &mut V) {
        self.iter_mut().map(|t| ItemVisitable::visit_item(t, visitor)).for_each(drop);
    }
}
impl<'a, 's> ItemVisitable<'s, 'a> for &str {}
impl<'a, 's> ItemVisitable<'s, 'a> for str {}

// impl<'a, T: TreeVisitable<'a>> TreeVisitable<'a> for span::With<T> {
//     default fn visit<V: TreeVisitor<'a>>(&'a self, visitor: &mut V) {
//         self.elem.visit(visitor)
//     }
// }
//
// impl<'a, T: TreeVisitableMut<'a>> TreeVisitableMut<'a> for span::With<T> {
//     default fn visit_mut<V: TreeVisitorMut>(&'a mut self, visitor: &mut V) {
//         self.elem.visit_mut(visitor)
//     }
// }
//
// impl<'a, T: SpanVisitable<'a>> SpanVisitable<'a> for span::With<T> {
//     fn visit_span<V: SpanVisitor<'a>>(&'a self, visitor: &mut V) {
//         if visitor.visit(&self.span) {
//             self.elem.visit_span(visitor)
//         }
//     }
// }
//
// impl<'a, T: SpanVisitableMut<'a>> SpanVisitableMut<'a> for span::With<T> {
//     fn visit_span_mut<V: SpanVisitorMut>(&'a mut self, visitor: &mut V) {
//         if visitor.visit_mut(&mut self.span) {
//             self.elem.visit_span_mut(visitor)
//         }
//     }
// }


// // ===========================
// // === RefCollectorVisitor ===
// // ===========================
//
// /// A visitor collecting references to all [`Tree`] nodes.
// #[derive(Debug, Default)]
// #[allow(missing_docs)]
// pub struct RefCollectorVisitor<'a> {
//     pub vec: Vec<&'a Tree>,
// }
//
// impl<'a> TreeVisitor<'a> for RefCollectorVisitor<'a> {
//     fn visit(&mut self, ast: &'a Tree) -> bool {
//         self.vec.push(ast);
//         true
//     }
// }
//
// impl Tree {
//     /// Collect references to all [`Tree`] nodes and return them in a vector.
//     pub fn collect_vec_ref(&self) -> Vec<&Tree> {
//         let mut visitor = RefCollectorVisitor::default();
//         self.visit(&mut visitor);
//         visitor.vec
//     }
// }
//
//
//
// // =================
// // === FnVisitor ===
// // =================
//
// /// A visitor allowing running a function on every [`Tree`] node.
// #[derive(Debug, Default)]
// #[allow(missing_docs)]
// pub struct FnVisitor<F>(pub F);
//
// impl<'a, T, F: Fn(&'a Tree) -> T> TreeVisitor<'a> for FnVisitor<F> {
//     fn visit(&mut self, ast: &'a Tree) -> bool {
//         (self.0)(ast);
//         true
//     }
// }
//
// impl<T, F: Fn(&mut Tree) -> T> TreeVisitorMut for FnVisitor<F> {
//     fn visit_mut(&mut self, ast: &mut Tree) -> bool {
//         (self.0)(ast);
//         true
//     }
// }
//
// impl<'a, T, F: Fn(&'a Span) -> T> SpanVisitor<'a> for FnVisitor<F> {
//     fn visit(&mut self, ast: &'a Span) -> bool {
//         (self.0)(ast);
//         true
//     }
// }
//
// impl<T, F: Fn(&mut Span) -> T> SpanVisitorMut for FnVisitor<F> {
//     fn visit_mut(&mut self, ast: &mut Span) -> bool {
//         (self.0)(ast);
//         true
//     }
// }
//
//
// impl Tree {
//     /// Map the provided function over each [`Tree`] node. The function results will be
// discarded.     pub fn map<T>(&self, f: impl Fn(&Tree) -> T) {
//         let mut visitor = FnVisitor(f);
//         self.visit(&mut visitor);
//     }
//
//     /// Map the provided function over each [`Tree`] node. The function results will be
// discarded.     pub fn map_mut<T>(&mut self, f: impl Fn(&mut Tree) -> T) {
//         let mut visitor = FnVisitor(f);
//         self.visit_mut(&mut visitor);
//     }
//
//     /// Map the provided function over each [`Span`] element. The function results will be
//     /// discarded.
//     pub fn map_span<T>(&self, f: impl Fn(&Span) -> T) {
//         let mut visitor = FnVisitor(f);
//         self.visit_span(&mut visitor);
//     }
//
//     /// Map the provided function over each [`Span`] element. The function results will be
//     /// discarded.
//     pub fn map_span_mut<T>(&mut self, f: impl Fn(&mut Span) -> T) {
//         let mut visitor = FnVisitor(f);
//         self.visit_span_mut(&mut visitor);
//     }
//
//     /// Remove all span information. This is mainly used for tests to compare AST structure.
//     pub fn remove_span_info(&mut self) {
//         self.map_span_mut(mem::take)
//     }
//
//     /// Remove all span information. This is mainly used for tests to compare AST structure.
//     pub fn with_removed_span_info(mut self) -> Self {
//         self.remove_span_info();
//         self
//     }
// }
