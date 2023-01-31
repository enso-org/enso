//! Module for types and utilities related to dealing with identifiers.

use crate::prelude::*;

use std::cmp::Ordering;



// ==================
// === Identifier ===
// ==================

// === Errors ===

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail)]
#[fail(display = "Identifier contains operator `{}`, so it cannot be made into var.", _0)]
pub struct OperatorCantBeMadeIntoVar(String);

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail)]
#[fail(display = "The `{}` is not a valid identifier.", _0)]
pub struct NotAnIdentifier(String);

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "Empty string is not a valid identifier.")]
pub struct IdentifierCannotBeEmpty;


// === Definition ===

/// Wrapper over an Ast that holds an atomic identifier of any kind.
///
/// Comparisons compare the underlying name strings.
///
/// Invariants: can get identifier name, the name is non-empty.
#[derive(Clone, Debug, Deref)]
pub struct Identifier(Ast);

impl Identifier {
    /// Wrap the `Ast` into `Identifier` if it actually is an identifier.
    pub fn new(ast: Ast) -> Option<Self> {
        let name = ast::identifier::name(&ast)?;
        (!name.is_empty()).as_some(Identifier(ast))
    }

    /// Convert given text into an identifier Ast and wrap.
    ///
    /// Can fail if a given string is not a valid identifier, however the exact scope of validation
    /// is currently unspecified.
    pub fn from_text(text: impl Into<String>) -> FallibleResult<Self> {
        // TODO? [mwu]
        //  We should be able to call parser or sth to verify that other requirements for the
        //  referent form identifiers are fulfilled.
        //  This is expected to become properly possible when the Rust rewrite of parser is done.
        //  See: https://github.com/enso-org/enso/issues/435
        //  On the other hand it is not clear how strict we want to be here, so as not to break
        //  processing invalid syntactically code.

        let text = text.into();
        let empty_string_error = failure::Error::from(IdentifierCannotBeEmpty);
        let first_char = text.chars().next().ok_or(empty_string_error)?;
        match first_char {
            c if c.is_lowercase() => Ok(Ast::var(text)),
            c if c.is_uppercase() => Ok(Ast::cons(text)),
            c if ast::opr::SYMBOLS.contains(&c) => Ok(Ast::opr(text)),
            _ => Err(NotAnIdentifier(text).into()),
        }
        .map(Identifier)
    }

    /// Get the identifier name.
    pub fn name(&self) -> &str {
        // Unwrap here is safe, as identifiers always allow obtaining an Identifier.
        ast::identifier::name(&self.0).unwrap()
    }

    /// Convert identifier to the variable form (i.e. non-referent). Fails if this is an operator.
    pub fn as_var(&self) -> Result<ast::Var, OperatorCantBeMadeIntoVar> {
        let name = self.name();
        // Unwrap below is safe, as identifier is always non-empty.
        let first_char = name.chars().next().unwrap();
        if first_char.is_alphabetic() {
            let name = name.to_lowercase();
            Ok(ast::Var { name })
        } else {
            Err(OperatorCantBeMadeIntoVar(name.to_owned()))
        }
    }

    /// Get the identifier's node with a newly assigned, unique id.
    ///
    /// This is needed if the identifier from AST is to be reused in a different part of the tree.
    /// Cloning it without generating a new ID would introduce two nodes with same id.
    pub fn with_new_id(&self) -> Self {
        Self(self.0.with_new_id())
    }
}


// === Implementations ===

impl PartialOrd for Identifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.name().partial_cmp(other.name())
    }
}

impl Ord for Identifier {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name().cmp(other.name())
    }
}

impl TryFrom<String> for Identifier {
    type Error = failure::Error;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Identifier::from_text(value)
    }
}
impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.name().into()
    }
}

impl TryFrom<&str> for Identifier {
    type Error = failure::Error;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Identifier::from_text(value)
    }
}

impl From<ast::known::Var> for Identifier {
    fn from(value: ast::known::Var) -> Self {
        Identifier(value.into())
    }
}

impl From<ast::known::Cons> for Identifier {
    fn from(value: ast::known::Cons) -> Self {
        Identifier(value.into())
    }
}

impl From<ast::known::Opr> for Identifier {
    fn from(value: ast::known::Opr) -> Self {
        Identifier(value.into())
    }
}

impl From<Identifier> for Ast {
    fn from(value: Identifier) -> Self {
        value.0
    }
}

impl From<&Identifier> for Ast {
    fn from(value: &Identifier) -> Self {
        value.0.clone()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.name(), f)
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name().eq(other.name())
    }
}

impl Eq for Identifier {}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name().hash(state)
    }
}



// =================
// === Utilities ===
// =================

/// Generate an identifier name that is not present in the given sequence.
///
/// The name is generated by taking `base` string and appending subsequent integers.
pub fn generate_name(
    base: impl AsRef<str>,
    unavailable: impl IntoIterator<Item = String>,
) -> FallibleResult<Identifier> {
    let base = base.as_ref();
    let is_relevant = |name: &String| name.starts_with(base);
    let unavailable = unavailable.into_iter().filter(is_relevant).collect::<HashSet<_>>();
    let name = (1..)
        .find_map(|i| {
            let candidate = format!("{base}{i}");
            let available = !unavailable.contains(&candidate);
            available.as_some(candidate)
        })
        .unwrap(); // It never yields `None`, as we iterate infinite sequence until we find match.
    Identifier::from_text(name)
}
