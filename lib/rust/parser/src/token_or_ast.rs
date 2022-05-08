use crate::prelude::*;

use crate::ast::Ast;
use crate::lexer;
use crate::location;
use crate::token;
use crate::token::Token;



// ==================
// === TokenOrAst ===
// ==================

/// Abstraction for [`Token`] and [`Ast`]. Some functions, such as macro resolver need to
/// distinguish between two cases and need to handle both incoming tokens and already constructed
/// [`Ast`] nodes. This structure provides handy utilities to work with such cases.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum TokenOrAst {
    Token(Token),
    Ast(Ast),
}

impl TokenOrAst {
    /// Check whether the element is the provided token variant. Returns [`false`] if it was an
    /// [`Ast`] node.
    pub fn is_variant(&self, variant: token::TypeVariant) -> bool {
        match self {
            TokenOrAst::Token(token) => token.is(variant),
            _ => false,
        }
    }

    /// [`location::Span`] of the element.
    pub fn span(&self) -> location::Span {
        match self {
            Self::Token(t) => t.span,
            Self::Ast(t) => t.span,
        }
    }

    pub fn trim_left(&mut self) -> location::Span {
        match self {
            Self::Token(t) => t.trim_left(),
            Self::Ast(t) => t.trim_left(),
        }
    }
}

impl From<Token> for TokenOrAst {
    fn from(t: Token) -> Self {
        TokenOrAst::Token(t)
    }
}

impl From<Ast> for TokenOrAst {
    fn from(t: Ast) -> Self {
        TokenOrAst::Ast(t)
    }
}
