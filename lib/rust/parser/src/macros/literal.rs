//! This file contains the literal matchers that are used to head up macro sections.

use crate::prelude::*;

use crate::prelude::lexer::token;


// ===============
// === Literal ===
// ===============

/// The kinds of literal that can be the head of a macro section.
///
/// For more detailed descriptions of the various literal types, please see the documentation of the
/// tokens in the Lexer.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Literal {
    Referent(String),
    Variable(String),
    External(String),
    Blank,
    Operator(String),
    Annotation(String),
}

impl Literal {
    /// Construct a referent identifier literal.
    pub fn referent(lit: impl Str) -> Literal {
        Literal::Referent(lit.into())
    }

    /// Construct a variable identifier literal.
    pub fn variable(lit: impl Str) -> Literal {
        Literal::Variable(lit.into())
    }

    /// Construct an external identifier literal.
    pub fn external(lit: impl Str) -> Literal {
        Literal::External(lit.into())
    }

    /// Construct a blank identifier literal.
    pub fn blank() -> Literal {
        Literal::Blank
    }

    /// Construct an operator identifier literal.
    pub fn operator(lit: impl Str) -> Literal {
        Literal::Operator(lit.into())
    }

    /// Construct an annodation identifier literal.
    pub fn annotation(lit: impl Str) -> Literal {
        Literal::Annotation(lit.into())
    }
}


// === Trait Impls ===

impl From<&Literal> for Literal {
    fn from(lit: &Literal) -> Self {
        lit.clone()
    }
}

impl From<Literal> for token::Shape {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Referent(str) => token::Shape::Referent(str),
            Literal::Variable(str) => token::Shape::Variable(str),
            Literal::External(str) => token::Shape::External(str),
            Literal::Blank => token::Shape::Blank,
            Literal::Operator(str) => token::Shape::Operator(str),
            Literal::Annotation(str) => token::Shape::Annotation(str),
        }
    }
}

impl TryFrom<token::Shape> for Literal {
    type Error = token::Shape;

    fn try_from(shape: token::Shape) -> Result<Self, Self::Error> {
        match shape {
            token::Shape::Referent(name) => Ok(Literal::Referent(name)),
            token::Shape::Variable(name) => Ok(Literal::Variable(name)),
            token::Shape::External(name) => Ok(Literal::External(name)),
            token::Shape::Blank => Ok(Literal::Blank),
            token::Shape::Operator(name) => Ok(Literal::Operator(name)),
            token::Shape::Annotation(name) => Ok(Literal::Annotation(name)),
            _ => Err(shape),
        }
    }
}
