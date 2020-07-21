
// === Definition ===

/// The ast node for a method definition.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Method { pub name: Box<Ast<Var>>, pub args: Box<AnyAst>, pub body: Box<AnyAst> }

/// The ast node for an operator definition.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Operator { pub name: Box<Ast<Opr>>, pub args: [Box<AnyAst>;2], pub body: Box<AnyAst> }

/// The ast node for a variable definition.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Variable { pub name: Box<Ast<Var>>, pub value: Box<AnyAst> }
