//! This module exports ast shapes that represent definition of variable, function etc.

use crate::Ast;
use crate::AnyAst;
use crate::identifier::Opr;
use crate::identifier::Var;



// ==================
// === Definition ===
// ==================

/// The ast node for a method definition.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct FunDef { pub name: Box<Ast<Var>>, pub args: Box<AnyAst>, pub body: Box<AnyAst> }

/// The ast node for an operator definition.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct OprDef { pub name: Box<Ast<Opr>>, pub args: [Box<AnyAst>;2], pub body: Box<AnyAst> }

/// The ast node for a variable definition.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct VarDef { pub name: Box<Ast<Var>>, pub value: Box<AnyAst> }
