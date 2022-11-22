//! Functions related to identifiers family of nodes.

use crate::prelude::*;

use crate::Shape;



/// Checks if given Ast node can be used to represent identifier being part of definition name.
pub fn is_identifier(ast: &Ast) -> bool {
    name(ast).is_some()
}

/// Retrieves the identifier's name, if the Ast node is an identifier. Otherwise, returns None.
pub fn name(ast: &Ast) -> Option<&str> {
    match ast.shape() {
        Shape::Var(val) => Some(&val.name),
        Shape::Cons(val) => Some(&val.name),
        Shape::SectionSides(val) => name(&val.opr),
        Shape::Opr(val) => Some(&val.name),
        _ => None,
    }
}

/// If the node is a variable (lower-cased) identifier, return the name.
pub fn as_var(ast: &Ast) -> Option<&str> {
    match ast.shape() {
        Shape::Var(val) => Some(&val.name),
        _ => None,
    }
}
