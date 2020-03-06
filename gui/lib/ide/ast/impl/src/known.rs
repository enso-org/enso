//! This module provides KnownAst<T> wrapper over Ast that allows expressing that we already
//! know what `Shape` variant is being stored within this `Ast` node.

use crate::prelude::*;

use crate::Ast;
use crate::Shape;
use crate::with_shape_variants;



// =================
// === Known AST ===
// =================

/// Wrapper for an AST node of known shape type that we can access.
/// Use `TryFrom<&Ast>` to obtain values.
///
/// Provides `Deref` implementation that allows accessing underlying shape `T` value.
#[derive(Derivative)]
#[derivative(Clone(bound=""))]
#[derive(Debug)]
pub struct KnownAst<T> {
    ast     : Ast,
    phantom : PhantomData<T>,
}

impl<T> KnownAst<T> {
    /// Checks if the shape of given Ast node is compatible with `T`.
    /// If yes, returns Ok with Ast node wrapped as KnownAst.
    /// Otherwise, returns an error.
    pub fn try_new<E>(ast:Ast) -> Result<KnownAst<T>,E>
    where for<'t> &'t Shape<Ast>: TryInto<&'t T, Error=E> {
        if let Some(error_matching) = ast.shape().try_into().err() {
            Err(error_matching)
        } else {
            Ok(KnownAst {ast,phantom:default()})
        }
    }
}

impl<T,E> Deref for KnownAst<T>
where for<'t> &'t Shape<Ast> : TryInto<&'t T,Error=E>,
      E                      : Debug, {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        let result = self.ast.shape().try_into();
        // Below must never happen, as the only function for constructing values does check
        // if the shape type matches the `T`.
        result.expect("Internal Error: wrong shape in KnownAst.")
    }
}

impl<T,E> TryFrom<&Ast> for KnownAst<T>
where for<'t> &'t Shape<Ast>:TryInto<&'t T,Error=E> {
    type Error = E;
    fn try_from(ast:&Ast) -> Result<KnownAst<T>,Self::Error> {
        KnownAst::try_new(ast.clone())
    }
}

impl<T,E> TryFrom<Ast> for KnownAst<T>
where for<'t> &'t Shape<Ast>:TryInto<&'t T,Error=E> {
    type Error = E;
    fn try_from(ast:Ast) -> Result<KnownAst<T>,Self::Error> {
        KnownAst::try_new(ast)
    }
}

/// One can always throw away the knowledge.
impl<T> From<KnownAst<T>> for Ast {
    fn from(known_ast:KnownAst<T>) -> Ast {
        known_ast.ast
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
