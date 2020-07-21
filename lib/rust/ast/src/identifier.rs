// === Identifiers ===

/// The ast node for the underscore `_`.
#[allow(missing_docs)]
#[derive(Debug,Clone,Copy)]
pub struct Blank {}

/// The ast node for a variable.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Var { pub name: String }

/// The ast node for a constructor.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Cons { pub name: String }

/// The ast node for an operator.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Opr { pub name: String }
