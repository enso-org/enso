//! Utility code for writing tests dealing with AST types.

use crate::prelude::*;

use crate::Ast;
use crate::HasRepr;
use crate::Module;
use crate::Shape;



/// "Downcasts" given AST's Shape to `T`. Panics if the shape doesn't match.
pub fn expect_shape<'t, T>(ast: &'t Ast) -> &'t T
where &'t Shape<Ast>: TryInto<&'t T> {
    match ast.shape().try_into() {
        Ok(shape) => shape,
        _ => {
            let expected_typename = std::any::type_name::<T>();
            panic!("failed converting shape into {expected_typename}, got {ast:?}")
        }
    }
}

/// Takes Ast being a module with a single non-empty line and returns that line's AST.
/// Panics, if this is not a module or if it does not have exactly one line.
pub fn expect_single_line(ast: &Ast) -> &Ast {
    let module: &Module<Ast> = expect_shape(ast);
    let (line,) = (module.iter()).expect_tuple();
    line
}

/// Checks if all nodes in subtree have declared spans equal to
/// spans we calculate.
pub fn validate_spans(ast: &Ast) {
    for node in ast.iter_recursive() {
        let calculated = node.shape().char_count();
        let declared = node.wrapped.wrapped.length;
        assert_eq!(calculated, declared, "`{}` part of `{}`", node.repr(), ast.repr());
    }
}

/// Panics if in the given AST duplicated IDs are present.
pub fn assert_unique_ids(ast: &Ast) {
    let mut ids = HashMap::new();
    for node in ast.iter_recursive() {
        if let Some(id) = node.id {
            if let Some(id2) = ids.insert(id, node) {
                panic!(
                    "Collision for id {id} between `{id2}` and `{node}`.\
                    \n\nWhole program is:\n{ast}"
                )
            }
        }
    }
}
