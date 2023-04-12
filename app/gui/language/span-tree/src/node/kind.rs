//! SpanTree node kind implementation. Each node can be of a different kind (shape) and can contain
//! different information.

use crate::prelude::*;

use crate::ArgumentInfo;
use crate::TagValue;



// ============
// === Kind ===
// ============

/// An enum describing kind of node.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kind {
    /// A root of the expression tree.
    Root,
    /// A node chained with parent node. See crate's docs for more info about chaining.
    Chained(Chained),
    /// A node representing operation (operator or function) of parent Infix, Section or Prefix.
    Operation,
    /// A node being a normal (not target) parameter of parent Infix, Section or Prefix.
    Argument(Argument),
    /// A node representing a named argument. Always contains two tokens and an `Argument` node.
    NamedArgument,
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
    pub fn chained() -> Chained {
        default()
    }
    pub fn chained_this() -> Chained {
        Chained::this()
    }
    pub fn this() -> Argument {
        Self::argument().with_name(Some(Argument::THIS.into()))
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
        matches!(
            self,
            Self::Argument(Argument { name, .. }) | Self::Chained(Chained { name, .. })
            if name.as_deref() == Some(Argument::THIS)
        )
    }
    pub fn is_argument(&self) -> bool {
        matches!(self, Self::Argument { .. })
    }
    pub fn is_named_argument(&self) -> bool {
        matches!(self, Self::NamedArgument)
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
        matches!(self, Self::InsertionPoint(t) if t.kind.is_expected_argument())
    }

    /// Match any kind that can be a function parameter. This includes `This`, `Argument` and
    /// expected argument.
    pub fn is_function_parameter(&self) -> bool {
        self.is_this() || self.is_argument() || self.is_expected_argument()
    }
}


// === API ===

impl Kind {
    /// Name getter.
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Argument(t) => t.name.as_deref(),
            Self::InsertionPoint(t) => t.name.as_deref(),
            _ => None,
        }
    }

    /// Type getter.
    pub fn tp(&self) -> Option<&String> {
        match self {
            Self::Argument(t) => t.tp.as_ref(),
            Self::InsertionPoint(t) => t.tp.as_ref(),
            _ => None,
        }
    }

    /// Removable flag getter.
    pub fn removable(&self) -> bool {
        match self {
            Self::Argument(t) => t.removable,
            _ => false,
        }
    }

    /// `ArgumentInfo` getter. Returns `None` if the node could not be attached with the
    /// information.
    pub fn argument_info(&self) -> Option<ArgumentInfo> {
        Some(match self {
            Self::Argument(t) =>
                ArgumentInfo::new(t.name.clone(), t.tp.clone(), t.call_id, t.tag_values.clone()),
            Self::InsertionPoint(t) =>
                ArgumentInfo::new(t.name.clone(), t.tp.clone(), t.call_id, t.tag_values.clone()),
            _ => return None,
        })
    }

    /// Get a reference to the name of an argument represented by this node, if available. Returns
    /// `None` if the node could not be attached with the argument information.
    pub fn argument_name(&self) -> Option<&str> {
        match self {
            Self::Chained(t) => t.name.as_deref(),
            Self::Argument(t) => t.name.as_deref(),
            Self::InsertionPoint(t) => t.name.as_deref(),
            _ => None,
        }
    }

    /// Get the definition index of an argument represented by this node, if available. Returns
    /// `None` if the node could not be attached with the argument information.
    pub fn definition_index(&self) -> Option<usize> {
        match self {
            Self::Argument(t) => t.definition_index,
            _ => None,
        }
    }

    /// Get a reference to tag values of an argument represented by this node, if available. Returns
    /// `None` if the node could not be attached with the argument information.
    pub fn tag_values(&self) -> Option<&[TagValue]> {
        match self {
            Self::Argument(t) => Some(&t.tag_values),
            Self::InsertionPoint(t) => Some(&t.tag_values),
            _ => None,
        }
    }

    /// Get the function call AST ID associated with this argument.
    pub fn call_id(&self) -> Option<ast::Id> {
        match self {
            Self::Chained(t) => t.call_id,
            Self::Argument(t) => t.call_id,
            Self::InsertionPoint(t) => t.call_id,
            _ => None,
        }
    }

    /// `ArgumentInfo` setter. Returns bool indicating whether the operation was possible
    /// or was skipped.
    pub fn set_argument_info(&mut self, argument_info: ArgumentInfo) -> bool {
        match self {
            Self::Chained(t) => {
                t.name = argument_info.name;
                t.call_id = argument_info.call_id;
                true
            }
            Self::Argument(t) => {
                t.name = argument_info.name;
                t.tp = argument_info.tp;
                t.call_id = argument_info.call_id;
                t.tag_values = argument_info.tag_values;
                true
            }
            Self::InsertionPoint(t) => {
                t.name = argument_info.name;
                t.tp = argument_info.tp;
                t.call_id = argument_info.call_id;
                t.tag_values = argument_info.tag_values;
                true
            }
            _ => false,
        }
    }

    /// Argument definition index setter. Returns bool indicating whether the operation was
    /// possible.
    pub fn set_definition_index(&mut self, index: usize) -> bool {
        match self {
            Self::Argument(t) => {
                t.definition_index = Some(index);
                true
            }
            _ => false,
        }
    }

    /// Call ID setter. Returns bool indicating whether the operation was possible.
    pub fn set_call_id(&mut self, call_id: Option<ast::Id>) -> bool {
        match self {
            Self::Chained(t) => {
                t.call_id = call_id;
                true
            }
            Self::Argument(t) => {
                t.call_id = call_id;
                true
            }
            Self::InsertionPoint(t) => {
                t.call_id = call_id;
                true
            }
            _ => false,
        }
    }

    /// Short string representation. Skips the inner fields and returns only the variant name.
    pub fn variant_name(&self) -> &str {
        match self {
            Self::Root => "Root",
            Self::Chained(_) => "Chained",
            Self::Operation => "Operation",
            Self::Argument(_) => "Argument",
            Self::NamedArgument => "NamedArgument",
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


// ===============
// === Chained ===
// ===============

/// A node chained with parent node, potentially being a first argument of a nested infix call
/// expression.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Chained {
    pub name:    Option<String>,
    /// The AST ID of function application that this Chained is a target of. If this is a part of
    /// an infix operator chain (e.g. `1 + 2 + 3`) and this chained represents `1 + 2`
    /// subexpression, it is effectively a target (`self`) argument of the second `+` operator.
    /// In that case the `call_id` will point at its parent `1 + 2 + 3` expression.
    pub call_id: Option<ast::Id>,
}


// === Setters ===

impl Chained {
    /// Create chained in `self` position of parent expression.
    pub fn this() -> Self {
        Self { name: Some(Argument::THIS.into()), ..default() }
    }

    /// Set Chained `call_id` field. See [`Chained::call_id`] for more information.
    pub fn with_call_id(mut self, call_id: Option<ast::Id>) -> Self {
        self.call_id = call_id;
        self
    }
}

impl From<Chained> for Kind {
    fn from(t: Chained) -> Self {
        Self::Chained(t)
    }
}



// ================
// === Argument ===
// ================

/// Kind representing "argument" node. For example, in the following expressions, `a`, `b`, and `c`
/// are considered "arguments": `foo a b c`, `foo + a`.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Argument {
    pub removable:        bool,
    /// The index of the argument in the function definition.
    pub definition_index: Option<usize>,
    pub name:             Option<String>,
    pub tp:               Option<String>,
    pub call_id:          Option<ast::Id>,
    pub tag_values:       Vec<TagValue>,
}


// === Setters ===

#[allow(missing_docs)]
impl Argument {
    pub const THIS: &'static str = "self";

    pub fn typed(mut self, tp: String) -> Self {
        self.tp = Some(tp);
        self
    }
    pub fn removable(mut self) -> Self {
        self.removable = true;
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
    pub fn with_call_id(mut self, call_id: Option<ast::Id>) -> Self {
        self.call_id = call_id;
        self
    }
    pub fn indexed(mut self, index: usize) -> Self {
        self.definition_index = Some(index);
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
    pub kind:       InsertionPointType,
    pub name:       Option<String>,
    pub tp:         Option<String>,
    pub call_id:    Option<ast::Id>,
    pub tag_values: Vec<TagValue>,
}

// === Constructors ===

#[allow(missing_docs)]
impl InsertionPoint {
    pub fn expected_argument(index: usize) -> Self {
        Self::default().with_kind(InsertionPointType::ExpectedArgument { index, named: false })
    }
    pub fn expected_named_argument_erased(index: usize) -> Self {
        Self::default().with_kind(InsertionPointType::ExpectedArgument { index, named: true })
    }
    pub fn expected_named_argument(index: usize, name: impl Str) -> Self {
        let kind = InsertionPointType::ExpectedArgument { index, named: true };
        let name = Some(name.into());
        Self::default().with_kind(kind).with_name(name)
    }
}


// === Setters ===

#[allow(missing_docs)]
impl InsertionPoint {
    pub fn with_kind(mut self, kind: InsertionPointType) -> Self {
        self.kind = kind;
        self
    }

    pub fn with_name(mut self, name: Option<String>) -> Self {
        self.name = name;
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
    BeforeArgument(usize),
    Append,
    // FIXME: When this insert type can be assigned to node without name?
    /// AST should be inserted as an argument at given index into the chain.
    /// Note that this is just argument index in the application, it may be not the same as the
    /// index of the function parameter, as `this` argument might be passed using the `this.func`
    /// notation.
    ExpectedArgument {
        index: usize,
        named: bool,
    },
}

// === Matchers ===
#[allow(missing_docs)]
impl InsertionPointType {
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
