//! This module provides KnownAst<T> wrapper over Ast that allows expressing that we already
//! know what `Shape` variant is being stored within this `Ast` node.

use crate::prelude::*;

use crate::with_shape_variants;
use crate::Ast;
use crate::HasTokens;
use crate::Shape;
use crate::TokenConsumer;

use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;



// =================
// === Known AST ===
// =================

/// Wrapper for an AST node of known shape type that we can access.
/// Use `TryFrom<&Ast>` to obtain values.
///
/// Provides `Deref` implementation that allows accessing underlying shape `T` value.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derive(Debug, PartialEq, Eq)]
pub struct KnownAst<T> {
    ast:     Ast,
    phantom: PhantomData<T>,
}

impl<T> KnownAst<T> {
    /// Creates a new `KnownAst<T>` from ast node containing shape of variant `T`.
    ///
    /// Note that this API requires caller to ensure that Ast stores proper shape. Violating this
    /// rule will lead to panics later.
    fn new_unchecked(ast: Ast) -> KnownAst<T> {
        KnownAst { ast, phantom: default() }
    }

    /// Gets AST id.
    pub fn id(&self) -> Option<crate::Id> {
        self.ast.id
    }

    /// Returns a reference to the stored `Ast` with `Shape` of `T`.
    pub fn ast(&self) -> &Ast {
        &self.ast
    }
}

impl<T, E> KnownAst<T>
where for<'t> &'t Shape<Ast>: TryInto<&'t T, Error = E>
{
    /// Checks if the shape of given Ast node is compatible with `T`.
    /// If yes, returns Ok with Ast node wrapped as KnownAst.
    /// Otherwise, returns an error.
    pub fn try_new(ast: Ast) -> Result<KnownAst<T>, E> {
        if let Some(error_matching) = ast.shape().try_into().err() {
            Err(error_matching)
        } else {
            Ok(KnownAst { ast, phantom: default() })
        }
    }

    /// Returns the AST's shape.
    pub fn shape(&self) -> &T
    where E: Debug {
        self.deref()
    }

    /// Updated self in place by applying given function on the stored Shape.
    pub fn update_shape<R>(&mut self, f: impl FnOnce(&mut T) -> R) -> R
    where
        T: Clone + Into<Shape<Ast>>,
        E: Debug, {
        let mut shape = self.shape().clone();
        let ret = f(&mut shape);
        self.ast = self.ast.with_shape(shape);
        ret
    }

    /// Create new instance of KnownAst with mapped shape.
    pub fn with_shape<S, E1>(&self, f: impl FnOnce(T) -> S) -> KnownAst<S>
    where
        for<'t> &'t Shape<Ast>: TryInto<&'t S, Error = E1>,
        T: Clone + Into<Shape<Ast>>,
        S: Clone + Into<Shape<Ast>>,
        E: Debug,
        E1: Debug, {
        let shape = self.shape().clone();
        let new_shape = f(shape);
        KnownAst::new_unchecked(self.ast.with_shape(new_shape))
    }
}

impl<T: Into<Shape<Ast>>> KnownAst<T> {
    /// Creates a new `KnownAst<T>` from `shape` with random ID if id=None.
    pub fn new(shape: T, id: Option<crate::Id>) -> KnownAst<T> {
        let ast = Ast::new(shape, id);
        Self::new_unchecked(ast)
    }

    /// Creates a new `KnownAst<T>` from `shape` with no ID.
    /// Should be only used on nodes that can't have ID because of scala AST design.
    /// Example: Module, Section.opr, MacroMatchSegment.head
    /// Tracking issue: https://github.com/enso-org/ide/issues/434
    pub fn new_no_id(shape: T) -> KnownAst<T> {
        let ast = Ast::new_no_id(shape);
        Self::new_unchecked(ast)
    }
}

impl<T, E> Deref for KnownAst<T>
where
    for<'t> &'t Shape<Ast>: TryInto<&'t T, Error = E>,
    E: Debug,
{
    type Target = T;
    fn deref(&self) -> &Self::Target {
        let result = self.ast.shape().try_into();
        // Below must never happen, as the only function for constructing values does check
        // if the shape type matches the `T`.
        result.expect("Internal Error: wrong shape in KnownAst.")
    }
}

impl<T> AsRef<Ast> for KnownAst<T> {
    fn as_ref(&self) -> &Ast {
        &self.ast
    }
}

impl<T, E> TryFrom<&Ast> for KnownAst<T>
where for<'t> &'t Shape<Ast>: TryInto<&'t T, Error = E>
{
    type Error = E;
    fn try_from(ast: &Ast) -> Result<KnownAst<T>, Self::Error> {
        KnownAst::try_new(ast.clone())
    }
}

impl<T, E> TryFrom<Ast> for KnownAst<T>
where for<'t> &'t Shape<Ast>: TryInto<&'t T, Error = E>
{
    type Error = E;
    fn try_from(ast: Ast) -> Result<KnownAst<T>, Self::Error> {
        KnownAst::try_new(ast)
    }
}

/// One can always throw away the knowledge.
impl<T> From<KnownAst<T>> for Ast {
    fn from(known_ast: KnownAst<T>) -> Ast {
        known_ast.ast
    }
}

impl<'a, T> From<&'a KnownAst<T>> for &'a Ast {
    fn from(known_ast: &'a KnownAst<T>) -> &'a Ast {
        &known_ast.ast
    }
}

impl<T> Serialize for KnownAst<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        self.ast.serialize(serializer)
    }
}

impl<'de, T, E> Deserialize<'de> for KnownAst<T>
where
    for<'t> &'t Shape<Ast>: TryInto<&'t T, Error = E>,
    E: fmt::Display,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer<'de> {
        let ast = Ast::deserialize(deserializer)?;
        Self::try_new(ast).map_err(serde::de::Error::custom)
    }
}

impl<T> HasTokens for KnownAst<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        self.ast.feed_to(consumer)
    }
}

impl<T> Display for KnownAst<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.ast, f)
    }
}



// ===============
// === Aliases ===
// ===============

/// For input like `[Unrecognized] [Prefix Ast]` generates aliases like:
/// ```compile_fail
/// pub type Unrecognized = KnownAst<crate::Unrecognized>;
/// pub type Prefix = KnownAst<crate::Prefix<Ast>>;
/// // etc ...
/// ```
macro_rules! generate_alias {
    ( $([$name:ident $($tp:ty)? ])* ) => {$(
        #[allow(missing_docs)]
        pub type $name = KnownAst<crate::$name $(<$tp>)? >;
    )*};
}

// Generates aliases for each Shape variant.
with_shape_variants!(generate_alias);



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_known_ast() {
        let ast_var = crate::Ast::var("foo");
        // This is truly var, so we can unwrap and directly access it's fields.
        let known_var = Var::try_from(&ast_var).unwrap();
        assert_eq!(known_var.name, "foo");

        let known_var: Var = ast_var.clone().try_into().unwrap();
        assert_eq!(known_var.name, "foo");


        // This is not an Infix, so we won't get KnownAst object.
        let known_infix_opt = Infix::try_from(&ast_var);
        assert!(known_infix_opt.is_err());
    }
}
