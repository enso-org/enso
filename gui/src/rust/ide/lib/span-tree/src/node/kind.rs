//! SpanTree node kind implementation. Each node can be of a different kind (shape) and can contain
//! different information.

use crate::prelude::*;

use crate::ArgumentInfo;



// ============
// === Kind ===
// ============

/// An enum describing kind of node.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kind {
    /// A root of the expression tree.
    Root,
    /// A node chained with parent node. See crate's docs for more info about chaining.
    Chained,
    /// A node representing operation (operator or function) of parent Infix, Section or Prefix.
    Operation,
    /// A node being a target (or "self") parameter of parent Infix, Section or Prefix.
    This(This),
    /// A node being a normal (not target) parameter of parent Infix, Section or Prefix.
    Argument(Argument),
    /// A node being a unmodifiable token in macro.
    Token,
    /// A node being a placeholder for inserting new child to Prefix or Operator chain. It should
    /// not have children, but can be assigned with a span representing the number of spaces
    /// between AST tokens. For example, given expression `foo   bar`, the span assigned to the
    /// `InsertionPoint` between `foo` and `bar` should be set to 3.
    InsertionPoint(InsertionPoint),
}


// === Kind Constructors ===

#[allow(missing_docs)]
impl Kind {
    pub fn this() -> This {
        default()
    }
    pub fn argument() -> Argument {
        default()
    }
    pub fn insertion_point() -> InsertionPoint {
        default()
    }
}


// === Matchers ===

#[allow(missing_docs)]
impl Kind {
    pub fn is_root(&self) -> bool {
        matches!(self, Self::Root { .. })
    }
    pub fn is_chained(&self) -> bool {
        matches!(self, Self::Chained { .. })
    }
    pub fn is_operation(&self) -> bool {
        matches!(self, Self::Operation { .. })
    }
    pub fn is_this(&self) -> bool {
        matches!(self, Self::This { .. })
    }
    pub fn is_argument(&self) -> bool {
        matches!(self, Self::Argument { .. })
    }
    pub fn is_token(&self) -> bool {
        matches!(self, Self::Token { .. })
    }
    pub fn is_insertion_point(&self) -> bool {
        matches!(self, Self::InsertionPoint { .. })
    }

    /// Match the value with `Kind::InsertionPoint{..}` but not
    /// `Kind::InsertionPoint(ExpectedArgument(_))`.
    pub fn is_positional_insertion_point(&self) -> bool {
        self.is_insertion_point() && !self.is_expected_argument()
    }

    /// Match the value with `Kind::InsertionPoint(ExpectedArgument(_))`.
    pub fn is_expected_argument(&self) -> bool {
        match self {
            Self::InsertionPoint(t) => t.kind.is_expected_argument(),
            _ => false,
        }
    }
}


// === API ===

impl Kind {
    /// Name getter.
    pub fn name(&self) -> Option<&String> {
        match self {
            Self::Argument(t) => t.name.as_ref(),
            Self::InsertionPoint(t) => t.name.as_ref(),
            _ => None,
        }
    }

    /// Type getter.
    pub fn tp(&self) -> Option<&String> {
        match self {
            Self::Argument(t) => t.tp.as_ref(),
            Self::InsertionPoint(t) => t.tp.as_ref(),
            Self::This(t) => t.tp.as_ref(),
            _ => None,
        }
    }

    /// Removable flag getter.
    pub fn removable(&self) -> bool {
        match self {
            Self::This(t) => t.removable,
            Self::Argument(t) => t.removable,
            _ => false,
        }
    }

    /// `ArgumentInfo` getter. Returns `None` if the node could not be attached with the
    /// information.
    pub fn argument_info(&self) -> Option<ArgumentInfo> {
        match self {
            Self::This(t) => Some(ArgumentInfo::new(Some(t.name().into()), t.tp.clone())),
            Self::Argument(t) => Some(ArgumentInfo::new(t.name.clone(), t.tp.clone())),
            Self::InsertionPoint(t) => Some(ArgumentInfo::new(t.name.clone(), t.tp.clone())),
            _ => None,
        }
    }

    /// `ArgumentInfo` setter. Returns bool indicating whether the operation was possible
    /// or was skipped.
    pub fn set_argument_info(&mut self, argument_info: ArgumentInfo) -> bool {
        match self {
            Self::This(t) => {
                t.tp = argument_info.tp;
                true
            }
            Self::Argument(t) => {
                t.name = argument_info.name;
                t.tp = argument_info.tp;
                true
            }
            Self::InsertionPoint(t) => {
                t.name = argument_info.name;
                t.tp = argument_info.tp;
                true
            }
            _ => false,
        }
    }

    /// Short string representation. Skips the inner fields and returns only the variant name.
    pub fn variant_name(&self) -> &str {
        match self {
            Self::Root => "Root",
            Self::Chained => "Chained",
            Self::Operation => "Operation",
            Self::This(_) => "This",
            Self::Argument(_) => "Argument",
            Self::Token => "Token",
            Self::InsertionPoint(_) => "InsertionPoint",
        }
    }
}


// === Impls ===

impl Default for Kind {
    fn default() -> Self {
        Self::insertion_point().into()
    }
}



// ============
// === This ===
// ============

/// Kind representing "this" node. For example, in the following expressions, `foo` is considered
/// "this": `bar foo`, `foo.bar`, `foo + bar`, `foo.+ bar`.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct This {
    pub removable: bool,
    pub tp:        Option<String>,
}


// === Getters ===

impl This {
    /// Name of `This` argument.
    pub const NAME: &'static str = "this";

    /// Name getter. Please notice that the name of `This` argument is always "this".
    pub fn name(&self) -> &str {
        Self::NAME
    }
}


// === Setters ===

#[allow(missing_docs)]
impl This {
    pub fn removable(mut self) -> Self {
        self.removable = true;
        self
    }
    pub fn typed(mut self, tp: String) -> Self {
        self.tp = Some(tp);
        self
    }
    pub fn with_removable(mut self, removable: bool) -> Self {
        self.removable = removable;
        self
    }
    pub fn with_tp(mut self, tp: Option<String>) -> Self {
        self.tp = tp;
        self
    }
}

impl From<This> for Kind {
    fn from(t: This) -> Self {
        Self::This(t)
    }
}



// ================
// === Argument ===
// ================

/// Kind representing "argument" node. For example, in the following expressions, `a`, `b`, and `c`
/// are considered "arguments": `foo a b c`, `foo + a`.
#[derive(Clone, Default, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Argument {
    pub removable: bool,
    pub name:      Option<String>,
    pub tp:        Option<String>,
}


// === Setters ===

#[allow(missing_docs)]
impl Argument {
    pub fn removable(mut self) -> Self {
        self.removable = true;
        self
    }
    pub fn named(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }
    pub fn typed(mut self, tp: String) -> Self {
        self.tp = Some(tp);
        self
    }
    pub fn with_removable(mut self, rm: bool) -> Self {
        self.removable = rm;
        self
    }
    pub fn with_name(mut self, name: Option<String>) -> Self {
        self.name = name;
        self
    }
    pub fn with_tp(mut self, tp: Option<String>) -> Self {
        self.tp = tp;
        self
    }
}

impl From<Argument> for Kind {
    fn from(t: Argument) -> Self {
        Self::Argument(t)
    }
}



// ======================
// === InsertionPoint ===
// ======================

/// Kind representing "insertion point" node. For example, in the following expressions, all places
/// marked with `?` are considered "insertion points" (please note that `?` symbols are NOT part
/// of the source code): `foo ? a ? b ? c ? ? ?`. The final question marks indicate non-provided
/// arguments.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct InsertionPoint {
    pub kind: InsertionPointType,
    pub name: Option<String>,
    pub tp:   Option<String>,
}

// === Constructors ===

#[allow(missing_docs)]
impl InsertionPoint {
    pub fn before_target() -> Self {
        Self::default().with_kind(InsertionPointType::BeforeTarget)
    }
    pub fn after_target() -> Self {
        Self::default().with_kind(InsertionPointType::AfterTarget)
    }
    pub fn append() -> Self {
        Self::default().with_kind(InsertionPointType::Append)
    }
    pub fn expected_argument(ix: usize) -> Self {
        Self::default().with_kind(InsertionPointType::ExpectedArgument(ix))
    }
}


// === Setters ===

#[allow(missing_docs)]
impl InsertionPoint {
    pub fn named(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }
    pub fn typed(mut self, tp: String) -> Self {
        self.tp = Some(tp);
        self
    }
    pub fn with_kind(mut self, kind: InsertionPointType) -> Self {
        self.kind = kind;
        self
    }
    pub fn with_name(mut self, name: Option<String>) -> Self {
        self.name = name;
        self
    }
    pub fn with_tp(mut self, tp: Option<String>) -> Self {
        self.tp = tp;
        self
    }
}

impl From<InsertionPoint> for Kind {
    fn from(t: InsertionPoint) -> Self {
        Self::InsertionPoint(t)
    }
}



// ==========================
// === InsertionPointType ===
// ==========================

/// A helpful information about how the new AST should be inserted during Set action. See `action`
/// module.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum InsertionPointType {
    // FIXME: Why we need both before and after?
    BeforeTarget,
    AfterTarget,
    Append,
    // FIXME: When this insert type can be assigned to node without name?
    /// Ast should be inserted as an argument at given index into the chain.
    /// Note that this is just argument index in the application, it may be not the same as the
    /// index of the function parameter, as `this` argument might be passed using the `this.func`
    /// notation.
    ExpectedArgument(usize),
}

// === Matchers ===
#[allow(missing_docs)]
impl InsertionPointType {
    pub fn is_before_target(&self) -> bool {
        matches!(self, Self::BeforeTarget { .. })
    }
    pub fn is_after_target(&self) -> bool {
        matches!(self, Self::AfterTarget { .. })
    }
    pub fn is_append(&self) -> bool {
        matches!(self, Self::Append { .. })
    }
    pub fn is_expected_argument(&self) -> bool {
        matches!(self, Self::ExpectedArgument { .. })
    }
}

impl Default for InsertionPointType {
    fn default() -> Self {
        Self::Append
    }
}

impl From<InsertionPointType> for Kind {
    fn from(t: InsertionPointType) -> Self {
        InsertionPoint::from(t).into()
    }
}

impl From<InsertionPointType> for InsertionPoint {
    fn from(t: InsertionPointType) -> Self {
        InsertionPoint::default().with_kind(t)
    }
}
