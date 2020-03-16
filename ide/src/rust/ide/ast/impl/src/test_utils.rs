//! Utility code for writing tests dealing with AST types.

use crate::prelude::*;

use crate::Ast;
use crate::Shape;
use crate::Module;

use utils::test::ExpectTuple;

/// "Downcasts" given AST's Shape to `T`. Panics if the shape doesn't match.
pub fn expect_shape<'t,T>(ast:&'t Ast) -> &'t T
    where &'t Shape<Ast>: TryInto<&'t T> {
    match ast.shape().try_into() {
        Ok(shape) => shape,
        _         => {
            let expected_typename = std::any::type_name::<T>();
            panic!("failed converting shape into {}",expected_typename)
        },
    }
}

/// Takes Ast being a module with a single line and returns that line's AST.
/// Panics, if this is not a module or if it does not have exactly one line.
pub fn expect_single_line(ast:&Ast) -> &Ast {
    let module:&Module<Ast> = expect_shape(ast);
    let (line,)             = (&module.lines).expect_tuple();
    line.elem.as_ref().unwrap()
}
