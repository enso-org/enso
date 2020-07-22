//! This module exports ast shapes that represent function application.

use crate::Ast;
use crate::AnyAst;
use crate::identifier::Opr;



// ===================
// === Application ===
// ===================

/// The ast node for application.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Prefix { pub func: Box<AnyAst>, pub arg: Box<AnyAst> }

/// The ast node for an infix operator application.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Infix { pub larg: Box<AnyAst>, pub opr: Box<Ast<Opr>>, pub rarg: Box<AnyAst> }
