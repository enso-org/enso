//! Implementation of Syntax Tree, known as well as Abstract Syntax Tree, or AST.
use crate::prelude::*;

// use crate::source;
use crate::source::Offset;
use crate::syntax::token;
// use crate::syntax::token::Token;

use enso_parser_syntax_tree_visitor::Visitor;
use enso_shapely_macros::tagged_enum;
// use span::Span;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Span<'s> {
    pub left_offset: Offset<'s>,
    pub length:      Bytes,
}

impl<'s> Span<'s> {
    pub fn trim_as_first_child(&mut self) -> Span<'s> {
        let left_offset = mem::take(&mut self.left_offset);
        let length = self.length;
        Span { left_offset, length }
    }

    pub fn extend_raw(&mut self, other: &Span<'s>) {
        self.length += other.left_offset.len() + other.length;
    }

    pub fn extend<T: SpanExtend<'s>>(&mut self, elem: &T) {
        elem.extend_span(self);
    }

    pub fn extended<T: SpanExtend<'s>>(mut self, elem: &T) -> Self {
        elem.extend_span(&mut self);
        self
    }
}

impl<'s> AsRef<Span<'s>> for Span<'s> {
    fn as_ref(&self) -> &Span<'s> {
        self
    }
}


pub trait SpanExtend<'s> {
    fn extend_span(&self, span: &mut Span<'s>);
}

impl<'s, T> SpanExtend<'s> for token::Token<'s, T> {
    fn extend_span(&self, span: &mut Span<'s>) {
        span.extend(&self.span())
    }
}

impl<'s> SpanExtend<'s> for Span<'s> {
    fn extend_span(&self, span: &mut Span<'s>) {
        span.extend_raw(&self)
    }
}

impl<'s> SpanExtend<'s> for Tree<'s> {
    fn extend_span(&self, span: &mut Span<'s>) {
        span.extend_raw(self.as_ref())
    }
}

impl<'s, T: SpanExtend<'s>> SpanExtend<'s> for Vec<T> {
    fn extend_span(&self, span: &mut Span<'s>) {
        for elem in self {
            elem.extend_span(span);
        }
    }
}

impl<'s, T: SpanExtend<'s>> SpanExtend<'s> for NonEmptyVec<T> {
    fn extend_span(&self, span: &mut Span<'s>) {
        for elem in self {
            elem.extend_span(span);
        }
    }
}

impl<'s, T: SpanExtend<'s>> SpanExtend<'s> for Option<T> {
    fn extend_span(&self, span: &mut Span<'s>) {
        if let Some(elem) = self {
            elem.extend_span(span);
        }
    }
}

impl<'s, T: SpanExtend<'s>, E: SpanExtend<'s>> SpanExtend<'s> for Result<T, E> {
    fn extend_span(&self, span: &mut Span<'s>) {
        match self {
            Ok(t) => t.extend_span(span),
            Err(t) => t.extend_span(span),
        }
    }
}


//
// impl<'s, 't, T> SpanExtend<&'t Vec<T>> for Span<'s>
// where Span<'s>: SpanExtend<&'t T>
// {
//     fn extend(&mut self, other: &'t Vec<T>) {
//         for item in other {
//             // <Span<'s> as SpanExtend<&'t T>>::extend(&mut self, item);
//             // self.extend(item);
//         }
//     }
// }



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

#[derive(Clone, Debug, Deref, DerefMut, Eq, PartialEq)]
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
        // OprSectionBoundary {
        //     pub ast: Tree,
        // },
        /// An application of a multi-segment function, such as `if ... then ... else ...`. Each
        /// segment starts with a token and contains an expression. Some multi-segment functions can
        /// have a prefix, an expression that is argument of the function, but is placed before the
        /// first token. Lambda is a good example for that. In an expression
        /// `Vector x y z -> x + y + z`, the `->` token is the beginning of the section, the
        /// `x + y + z` is the section body, and `Vector x y z` is the prefix of this function
        /// application.
        // MultiSegmentApp {
        //     pub prefix: Option<Tree>,
        //     pub segments: NonEmptyVec<MultiSegmentAppSegment>,
        // }
        Test {
            pub token: token::Ident<'s>,
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

pub struct Builder<T = ()> {
    pub span: T,
}

// pub trait PartialBuilder {
//     fn build(self);
// }
//
// impl<S, T> PartialBuilder for (Builder<Option<S>>, T) {
//     default fn build(self) {
//         todo!()
//     }
// }
//
// impl<S, T> PartialBuilder for (Builder<Option<S>>, Option<T>) {
//     fn build(self) {
//         todo!()
//     }
// }

// pub trait PartialParentSpanBuilder2<T> {
//     type Output;
//     fn extend_parent_span2(&mut self, builder: ParentSpanBuilder<T>) -> Self::Output;
// }
//
// impl<'s, T> PartialParentSpanBuilder2<Option<Span<'s>>> for T
// where
//     T: PartialParentSpanBuilder2<Span<'s>, Output = Span<'s>>,
//     T: PartialParentSpanBuilder2<(), Output = Span<'s>>,
// {
//     type Output = Span<'s>;
//     default fn extend_parent_span2(
//         &mut self,
//         builder: ParentSpanBuilder<Option<Span<'s>>>,
//     ) -> Self::Output {
//         match builder.span {
//             Some(span) => <T as PartialParentSpanBuilder2<Span<'s>>>::extend_parent_span2(
//                 self,
//                 ParentSpanBuilder::new(span),
//             ),
//             None => <T as PartialParentSpanBuilder2<()>>::extend_parent_span2(
//                 self,
//                 ParentSpanBuilder::new(()),
//             ),
//         }
//     }
// }
//
// impl<'s, T> PartialParentSpanBuilder2<Option<Span<'s>>> for Option<T>
// where
//     T: PartialParentSpanBuilder2<Span<'s>, Output = Span<'s>>,
//     T: PartialParentSpanBuilder2<(), Output = Span<'s>>,
// {
//     type Output = Option<Span<'s>>;
//     fn extend_parent_span2(
//         &mut self,
//         builder: ParentSpanBuilder<Option<Span<'s>>>,
//     ) -> Self::Output {
//         match self {
//             Some(elem) => match builder.span {
//                 Some(span) => Some(elem.extend_parent_span2(ParentSpanBuilder::new(span))),
//                 None => Some(elem.extend_parent_span2(ParentSpanBuilder::new(()))),
//             },
//             None => builder.span,
//         }
//     }
// }
//
// impl<T> PartialParentSpanBuilder2<()> for Option<T>
// where T: PartialParentSpanBuilder2<()>
// {
//     type Output = Option<<T as PartialParentSpanBuilder2<()>>::Output>;
//     fn extend_parent_span2(&mut self, builder: ParentSpanBuilder<()>) -> Self::Output {
//         self.as_mut().map(|t| t.extend_parent_span2(builder))
//     }
// }
//
// impl<'s> PartialParentSpanBuilder2<()> for Tree<'s> {
//     type Output = Span<'s>;
//     fn extend_parent_span2(&mut self, builder: ParentSpanBuilder<()>) -> Self::Output {
//         self.span.trim_as_first_child()
//     }
// }
//
// impl<'s> PartialParentSpanBuilder2<Span<'s>> for Tree<'s> {
//     type Output = Span<'s>;
//     fn extend_parent_span2(&mut self, builder: ParentSpanBuilder<Span<'s>>) -> Self::Output {
//         builder.span.extended(&self.span)
//     }
// }
//
// impl<T, E> PartialParentSpanBuilder2<()> for Result<T, E>
// where
//     T: PartialParentSpanBuilder2<()>,
//     E: PartialParentSpanBuilder2<(), Output = <T as PartialParentSpanBuilder2<()>>::Output>,
// {
//     type Output = <T as PartialParentSpanBuilder2<()>>::Output;
//     fn extend_parent_span2(&mut self, builder: ParentSpanBuilder<()>) -> Self::Output {
//         match self {
//             Ok(t) => t.extend_parent_span2(builder),
//             Err(t) => t.extend_parent_span2(builder),
//         }
//     }
// }
//
// impl<'s, T, E> PartialParentSpanBuilder2<Span<'s>> for Result<T, E>
// where
//     T: PartialParentSpanBuilder2<Span<'s>>,
//     E: PartialParentSpanBuilder2<
//         Span<'s>,
//         Output = <T as PartialParentSpanBuilder2<Span<'s>>>::Output,
//     >,
// {
//     type Output = <T as PartialParentSpanBuilder2<Span<'s>>>::Output;
//     fn extend_parent_span2(&mut self, builder: ParentSpanBuilder<Span<'s>>) -> Self::Output {
//         match self {
//             Ok(t) => t.extend_parent_span2(builder),
//             Err(t) => t.extend_parent_span2(builder),
//         }
//     }
// }
//
//
// #[derive(Default)]
// pub struct ParentSpanBuilder<T> {
//     pub span: T,
// }
//
// impl<T> ParentSpanBuilder<T> {
//     pub fn new(span: T) -> Self {
//         Self { span }
//     }
//
//     pub fn extend<S: PartialParentSpanBuilder<T>>(
//         self,
//         elem: &mut S,
//     ) -> ParentSpanBuilder<S::Output> {
//         ParentSpanBuilder::new(elem.extend_parent_span(self))
//     }
//
//     pub fn extend2<S: PartialParentSpanBuilder2<T>>(
//         self,
//         elem: &mut S,
//     ) -> ParentSpanBuilder<S::Output> {
//         ParentSpanBuilder::new(elem.extend_parent_span2(self))
//     }
// }
//
//
//
// pub trait PartialParentSpanBuilder<T> {
//     type Output;
//     fn extend_parent_span(&mut self, builder: ParentSpanBuilder<T>) -> Self::Output;
// }
//
// impl<'s> PartialParentSpanBuilder<()> for Tree<'s> {
//     type Output = Span<'s>;
//     fn extend_parent_span(&mut self, builder: ParentSpanBuilder<()>) -> Self::Output {
//         self.span.trim_as_first_child()
//     }
// }
//
//
//
// impl<T> PartialParentSpanBuilder<()> for Option<T>
// where T: PartialParentSpanBuilder<()>
// {
//     type Output = Option<<T as PartialParentSpanBuilder<()>>::Output>;
//     fn extend_parent_span(&mut self, builder: ParentSpanBuilder<()>) -> Self::Output {
//         self.as_mut().map(|t| t.extend_parent_span(builder))
//     }
// }
//
// impl<T, E> PartialParentSpanBuilder<()> for Result<T, E>
// where
//     T: PartialParentSpanBuilder<()>,
//     E: PartialParentSpanBuilder<(), Output = <T as PartialParentSpanBuilder<()>>::Output>,
// {
//     type Output = <T as PartialParentSpanBuilder<()>>::Output;
//     fn extend_parent_span(&mut self, builder: ParentSpanBuilder<()>) -> Self::Output {
//         match self {
//             Ok(t) => t.extend_parent_span(builder),
//             Err(t) => t.extend_parent_span(builder),
//         }
//     }
// }
//
// impl<'s> PartialParentSpanBuilder<Span<'s>> for Tree<'s> {
//     type Output = Span<'s>;
//     fn extend_parent_span(&mut self, builder: ParentSpanBuilder<Span<'s>>) -> Self::Output {
//         builder.span.extended(&self.span)
//     }
// }

// impl<T> PartialParentSpanBuilder<Option<T>> for ParentSpanBuilder {
//     type Output = Span<'s>;
//     fn extend(self, elem: &mut Tree<'s>) -> ParentSpanBuilder<Self::Output> {
//         ParentSpanBuilder::new(elem.span.trim_as_first_child())
//     }
// }
//
// impl<'s> PartialParentSpanBuilder<Tree<'s>> for ParentSpanBuilder<Span<'s>> {
//     type Output = Span<'s>;
//     fn extend(self, elem: &mut Tree<'s>) -> ParentSpanBuilder<Self::Output> {
//         ParentSpanBuilder::new(self.span.extended(elem))
//     }
// }



// === App ===

impl<'s> Tree<'s> {
    /// Constructor.
    pub fn app(mut func: Tree<'s>, mut arg: Tree<'s>) -> Tree<'s> {
        let mut span = func.span.trim_as_first_child();
        span.extend(&arg);
        // let span = ParentSpanBuilder::<()>::default().extend(&mut func).extend(&mut arg).span;
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

impl<'s> SpanExtend<'s> for MultipleOperatorError<'s> {
    fn extend_span(&self, span: &mut Span<'s>) {
        span.extend(&self.operators)
    }
}

// impl<'s> PartialParentSpanBuilder2<Span<'s>> for MultipleOperatorError<'s> {
//     type Output = Span<'s>;
//     fn extend_parent_span2(&mut self, builder: ParentSpanBuilder<Span<'s>>) -> Self::Output {
//         builder.span.extended(&self.span)
//     }
// }

impl<'s> Tree<'s> {
    /// Constructor.
    pub fn opr_app(
        mut lhs: Option<Tree<'s>>,
        mut opr: OperatorOrError<'s>,
        rhs: Option<Tree<'s>>,
    ) -> Tree<'s> {
        // let span = ParentSpanBuilder::default().extend(&mut lhs).extend(&mut opr);
        // let span = ParentSpanBuilder::<()>::default().extend2(&mut lhs);
        let mut span = match &mut lhs {
            Some(lhs) => lhs.span.trim_as_first_child().extended(&opr),
            None => match &mut opr {
                Ok(opr) => opr.trim_as_first_child(),
                Err(err) => err.operators.first_mut().trim_as_first_child(),
            },
        };
        span.extend(&rhs);
        Tree(span, Type::from(OprApp(lhs, opr, rhs)))
    }
}

//
//
// // === OprSectionBoundary ===
//
// impl Tree {
//     /// Constructor.
//     pub fn opr_section_boundary(section: Tree) -> Tree {
//         let (left_offset_span, section) = section.split_at_start();
//         let total = left_offset_span.extended_to(&section);
//         let ast_data = Type::from(OprSectionBoundary(section));
//         total.with(ast_data)
//     }
// }
//
//
// // === MultiSegmentApp ===
//
// /// A segment of [`MultiSegmentApp`], like `if cond` in the `if cond then ok else fail`
// expression. #[derive(Clone, Debug, Eq, PartialEq, Visitor)]
// #[allow(missing_docs)]
// pub struct MultiSegmentAppSegment {
//     pub header: Token,
//     pub body:   Option<Tree>,
// }
//
// impl Tree {
//     /// Constructor.
//     pub fn multi_segment_app(
//         mut prefix: Option<Tree>,
//         mut segments: NonEmptyVec<MultiSegmentAppSegment>,
//     ) -> Self {
//         let left_span = if let Some(prefix) = prefix.as_mut() {
//             prefix.trim_left()
//         } else {
//             let first = segments.first_mut();
//             let (left_span, left_trimmed_token) = first.header.split_at_start();
//             first.header = left_trimmed_token;
//             left_span
//         };
//         let last_segment = segments.last();
//         let total = if let Some(last_segment_body) = &last_segment.body {
//             left_span.extended_to(last_segment_body)
//         } else {
//             left_span.extended_to(&last_segment.header)
//         };
//         let data = Type::from(MultiSegmentApp { prefix, segments });
//         total.with(data)
//     }
// }
//
//
//



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
        $(
            impl<'a, 's> TreeVisitable<'a, 's> for token::$variant<'s> {}
            impl<'a, 's> TreeVisitableMut<'a, 's> for token::$variant<'s> {}
            // impl<'a, 's> SpanVisitable<'a> for token::$variant {}
            // impl<'a, 's> SpanVisitableMut<'a> for token::$variant {}
        )*
    };
}

define_visitor!(Tree, visit, visit_mut);
// define_visitor!(Span, visit_span, visit_span_mut);
crate::with_token_definition!(define_visitor_for_tokens());


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
