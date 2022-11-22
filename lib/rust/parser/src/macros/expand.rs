//! Macro expansion utilities. Allow expanding macro variables in the same as Rust macro rules do.

use crate::macros::pattern::*;
use crate::prelude::*;

use crate::syntax;



// ==============
// === VarMap ===
// ==============

/// A nested map of pattern variables (elements using the [`Pattern::Named`] variant). The validator
/// should be instantiated either with the [`EnabledValidator`] in case of user-defined
/// macros or with the [`DisabledValidator`] in case of built-in macros. The latter is
/// faster but does not provide nice error messages and allows for illegal code expansion, like
/// using two variables that have the same repetition depth, but have different parents (e.g. the
/// variables `$b` and `$e` from the example below).
///
/// To better understand how it works, let's consider the following pattern definition (using the
/// Rust macro rules syntax for simplicity):
///
/// ```text
/// $x:tt
/// $(
///     $a:tt
///     $(
///         $b:tt
///         $c:tt
///     )*
///
///     $d:tt
///     $(
///         $e:tt
///         $f:tt
///     )*
/// )*
/// ```
///
/// The following [`VarMap`] will be generated (some fields simplified for clarity):
///
/// ```text
/// VarMap {
///     map: [
///         ("x", VarMapEntry {
///             tokens: ["x"],
///             validator: EnabledValidator { scope: VarScope {
///                 locals: ["x"], parent: None
///             }}
///         }),
///     ],
///     nested: Some(VarMap {
///         map: [
///             ("a", VarMapEntry {
///                 tokens: ["a"],
///                 validator: EnabledValidator { scope: VarScope {
///                     locals: ["a","d"], parent: Some (VarScope {
///                         locals: ["x"], parent: None
///                     })
///                 }}
///             }),
///             ("e", VarMapEntry {
///                 tokens: ["e"],
///                 validator: EnabledValidator { scope: VarScope {
///                     locals: ["a","d"], parent: Some (VarScope {
///                         locals: ["x"], parent: None
///                     })
///                 }}
///             }),
///         ],
///         nested: Some(VarMap {
///             map: [
///                 ("b", VarMapEntry {
///                     tokens: ["b"],
///                     validator: EnabledValidator { scope: VarScope {
///                         locals: ["b","c"], parent: Some (VarScope {
///                             locals: ["a","d"], parent: Some (VarScope {
///                                 locals: ["x"], parent: None
///                             })
///                         })
///                     }}
///                 }),
///                 ("c", VarMapEntry {
///                     tokens: ["c"],
///                     validator: EnabledValidator { scope: VarScope {
///                         locals: ["b","c"], parent: Some (VarScope {
///                             locals: ["a","d"], parent: Some (VarScope {
///                                 locals: ["x"], parent: None
///                             })
///                         })
///                     }}
///                 }),
///                 ("e", VarMapEntry {
///                     tokens: ["e"],
///                     validator: EnabledValidator { scope: VarScope {
///                         locals: ["e","f"], parent: Some (VarScope {
///                             locals: ["a","d"], parent: Some (VarScope {
///                                 locals: ["x"], parent: None
///                             })
///                         })
///                     }}
///                 }),
///                 ("f", VarMapEntry {
///                     tokens: ["f"],
///                     validator: EnabledValidator { scope: VarScope {
///                         locals: ["e","f"], parent: Some (VarScope {
///                             locals: ["a","d"], parent: Some (VarScope {
///                                 locals: ["x"], parent: None
///                             })
///                         })
///                     }}
///                 }),
///             ],
///         })
///     })
/// }
/// ```
///
/// Validators can be queried during code expansion to check whether these variables belong to
/// the same repetition scope.
#[derive(Clone, Debug, Default)]
pub struct VarMap<'s, V> {
    nested: Option<Box<VarMap<'s, V>>>,
    map:    HashMap<String, VarMapEntry<'s, V>>,
}

/// Entry of the [`VarMap`] map.
#[derive(Clone, Debug, Default)]
struct VarMapEntry<'s, V> {
    pub tokens:    Vec<Vec<syntax::Item<'s>>>,
    pub validator: V,
}

impl<'s, V> VarMapEntry<'s, V> {
    /// Constructor.
    pub fn new(validator: V, tokens: Vec<Vec<syntax::Item<'s>>>) -> Self {
        Self { validator, tokens }
    }
}

impl<'s> Match<'s> {
    /// Convert the match into checked [`VarMap`].
    pub fn into_var_map(self) -> VarMap<'s, EnabledValidator> {
        let mut tree = VarMap::default();
        self.build_var_map(&mut tree, &default());
        tree
    }

    /// Convert the match into unchecked [`VarMap`]. The unchecked version has better performance,
    /// but does not provide nice user error messages and allows for illegal code expansion. Read
    /// the docs of [`VarMap`] to learn more.
    pub fn into_unchecked_var_map(self) -> VarMap<'s, DisabledValidator> {
        let mut tree = VarMap::default();
        self.build_var_map(&mut tree, &default());
        tree
    }

    fn build_var_map<V: Default + Validator>(self, tree: &mut VarMap<'s, V>, validator: &V) {
        match self {
            Self::Everything(_)
            | Self::Nothing
            | Self::Identifier(_)
            | Self::Expected(_, _)
            | Self::NotBlock(_) => {}
            Self::Or(box OrMatch::First(item) | box OrMatch::Second(item)) =>
                item.build_var_map(tree, validator),
            Self::Seq(first, second) => {
                first.build_var_map(tree, validator);
                second.build_var_map(tree, validator);
            }
            Self::Many(matches) => {
                let nested_validator = V::default();
                nested_validator.set_parent(validator);
                let nested = tree.nested.get_or_insert(default());
                for m in matches {
                    m.build_var_map(nested, &nested_validator);
                }
            }
            Self::Named(name, t) => {
                validator.insert_local_var(&name);
                tree.map
                    .entry(name)
                    .or_insert_with(|| VarMapEntry::new(validator.clone_ref(), default()))
                    .tokens
                    .push(t.tokens());
            }
        }
    }
}



// =================
// === Validator ===
// =================

/// Validator used to check if the macro generation correct. See the definition of [`VarMap`] to
/// learn more.
#[allow(missing_docs)]
pub trait Validator: PartialEq + Default + CloneRef {
    fn check(&self, name: &str) -> bool;
    fn parent(&self) -> Option<Self>;
    fn set_parent(&self, parent: &Self);
    fn insert_local_var(&self, var: &str);
}

/// Disabled validator. See the docs of [`VarMap`] to learn more.
#[derive(Copy, Clone, CloneRef, Debug, Default, PartialEq, Eq)]
pub struct DisabledValidator;

/// Enabled validator. See the docs of [`VarMap`] to learn more.
#[derive(Clone, CloneRef, Debug, Default)]
#[allow(missing_docs)]
pub struct EnabledValidator {
    scope: Rc<RefCell<VarScope>>,
}

#[derive(Clone, Debug, Default)]
struct VarScope {
    locals: HashSet<String>,
    parent: Option<EnabledValidator>,
}

impl PartialEq for EnabledValidator {
    fn eq(&self, other: &EnabledValidator) -> bool {
        Rc::ptr_eq(&self.scope, &other.scope)
    }
}

impl Validator for EnabledValidator {
    #[inline(always)]
    fn check(&self, name: &str) -> bool {
        self.scope.borrow().locals.contains(name)
    }

    #[inline(always)]
    fn parent(&self) -> Option<Self> {
        self.scope.borrow().parent.as_ref().map(|t| t.clone_ref())
    }

    #[inline(always)]
    fn set_parent(&self, parent: &Self) {
        self.scope.borrow_mut().parent = Some(parent.clone_ref());
    }

    #[inline(always)]
    fn insert_local_var(&self, var: &str) {
        self.scope.borrow_mut().locals.insert(var.to_string());
    }
}

impl Validator for DisabledValidator {
    #[inline(always)]
    fn check(&self, _name: &str) -> bool {
        true
    }

    #[inline(always)]
    fn parent(&self) -> Option<Self> {
        None
    }

    #[inline(always)]
    fn set_parent(&self, _parent: &Self) {}

    #[inline(always)]
    fn insert_local_var(&self, _var: &str) {}
}



// ==================
// === VarMapView ===
// ==================

/// A view for a [`VarMap`]. It allows focusing on a specific repetition scope and querying for
/// variables there. See the docs of [`VarMap`] to learn more.
#[derive(Clone, Debug, Default)]
pub struct VarMapView<'t, 's, V> {
    tree: Option<&'t VarMap<'s, V>>,
    resolved_validator: Option<V>,
    parent_validator_to_check: Option<V>,
}

impl<'t, 's, V> VarMapView<'t, 's, V> {
    /// Constructor.
    pub fn new(tree: &'t VarMap<'s, V>) -> Self {
        let resolved_validator = default();
        let parent_validator_to_check = default();
        Self { tree: Some(tree), resolved_validator, parent_validator_to_check }
    }
}

impl<'t, 's, V: Validator> VarMapView<'t, 's, V> {
    /// Get the view for the nested repetition scope.
    pub fn nested(&self) -> Self {
        let tree = self.tree.and_then(|t| t.nested.as_ref().map(|n| n.as_ref()));
        let resolved_validator = None;
        let parent_validator_to_check = self.resolved_validator.as_ref().map(|t| t.clone_ref());
        Self { tree, resolved_validator, parent_validator_to_check }
    }
}

impl<'t, 's, V: Validator> VarMapView<'t, 's, V> {
    /// Query for a variable.
    pub fn query(&mut self, name: &str) -> Option<&'t [Vec<syntax::Item<'s>>]> {
        self.tree.and_then(|t| {
            t.map.get(name).map(|entry| {
                match &self.resolved_validator {
                    Some(validator) =>
                        if !validator.check(name) {
                            todo!("Report nice error that the name does not belong to the scope.")
                        },
                    None => {
                        let resolved_validator = entry.validator.clone_ref();
                        if let Some(parent_validator_to_check) = &self.parent_validator_to_check {
                            let mut ok = false;
                            let mut validator = resolved_validator.clone();
                            loop {
                                if &validator == parent_validator_to_check {
                                    ok = true;
                                    break;
                                } else {
                                    match validator.parent() {
                                        Some(p) => validator = p,
                                        None => break,
                                    }
                                }
                            }
                            if !ok {
                                todo!("Report nice error that the name does not belong to the same scope as previous variables.")
                            }
                            self.parent_validator_to_check = None;
                        }
                        self.resolved_validator = Some(resolved_validator);
                    }
                }
                &entry.tokens[..]
            })
        })
    }
}

impl<'s, V> VarMap<'s, V> {
    /// Create a new view for this var map.
    pub fn view<'t>(&'t self) -> VarMapView<'t, 's, V> {
        VarMapView::new(self)
    }
}
