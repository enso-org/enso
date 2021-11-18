//! The macro registry that can be queried during the process of macro resolution.

use crate::prelude::*;
use enso_data::hash_map_tree::*;

use crate::macros::definition::Definition;
use crate::macros::literal::Literal;



// ================
// === Registry ===
// ================

/// The type of the tree that underlies the registry.
pub type Tree = HashMapTree<Literal, Option<Definition>>;

/// The registry is responsible for the registration of macro definitions, and the querying of said
/// definitions.
#[derive(Clone, Debug, Default, PartialEq)]
#[allow(missing_docs)]
pub struct Registry {
    tree: Tree,
}

impl Registry {
    /// Insert `definition` into the macro registry.
    pub fn insert(&mut self, definition: Definition) {
        self.tree.set(definition.path(), Some(definition));
    }

    /// Get a reference to the root of the registry.
    pub fn root(&self) -> &Tree {
        &self.tree
    }

    /// Query the registry for a tree.
    pub fn subtree<P>(&self, path: P) -> Option<&Tree>
    where
        P: IntoIterator,
        P::Item: Into<Literal>, {
        self.tree.get_node(path)
    }

    /// Query the registry for a tree, assuming such a tree is present.
    ///
    /// # Panics
    /// If no tree exists at `path`.
    pub fn unsafe_subtree<P>(&self, path: P) -> &Tree
    where
        P: IntoIterator,
        P::Item: Into<Literal>, {
        self.subtree(path).expect("A tree exists at the input path.")
    }

    /// Query the registry for a definition.
    pub fn definition<P>(&self, path: P) -> Option<&Definition>
    where
        P: IntoIterator,
        P::Item: Into<Literal>, {
        match self.tree.get(path) {
            Some(Some(def)) => Some(def),
            _ => None,
        }
    }

    /// Query the registry for a definition, assuming such a definition is present.
    ///
    /// # Panics
    /// If no definition exists at `path`.
    pub fn unsafe_definition<P>(&self, path: P) -> &Definition
    where
        P: IntoIterator,
        P::Item: Into<Literal>, {
        self.definition(path).expect("A definition exists at the input path.")
    }
}


// === Trait Impls ===

impl From<Vec<Definition>> for Registry {
    fn from(defs: Vec<Definition>) -> Self {
        let mut registry: Registry = default();
        defs.into_iter().for_each(|def| registry.insert(def));
        registry
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::macros::definition::Section;

    #[test]
    fn insert_query() {
        let mut registry = Registry::default();
        let definition = Definition::new("Test", vec![
            Section::new(Literal::variable("if")),
            Section::new(Literal::variable("then")),
            Section::new(Literal::variable("else")),
        ]);
        let path_1 =
            &[Literal::variable("if"), Literal::variable("then"), Literal::variable("else")];
        let path_2 = &[Literal::variable("if"), Literal::variable("then")];
        registry.insert(definition.clone());
        let result_1 = registry.definition(path_1);
        let result_2 = registry.definition(path_2);
        assert!(result_1.is_some());
        assert_eq!(result_1.unwrap(), &definition);
        assert_eq!(result_2, None);
    }

    #[test]
    fn from_defs() {
        let definitions = vec![
            Definition::new("if_then_else", vec![
                Section::new(Literal::variable("if")),
                Section::new(Literal::variable("then")),
                Section::new(Literal::variable("else")),
            ]),
            Definition::new("if_then", vec![
                Section::new(Literal::variable("if")),
                Section::new(Literal::variable("then")),
            ]),
            Definition::new("if_let", vec![
                Section::new(Literal::variable("if")),
                Section::new(Literal::variable("let")),
            ]),
        ];
        let registry = Registry::from(definitions.clone());
        let path_1 =
            &[Literal::variable("if"), Literal::variable("then"), Literal::variable("else")];
        let path_2 = &[Literal::variable("if"), Literal::variable("then")];
        let path_3 = &[Literal::variable("if"), Literal::variable("let")];
        let path_4 = &[Literal::variable("if")];
        let result_1 = registry.definition(path_1);
        let result_2 = registry.definition(path_2);
        let result_3 = registry.definition(path_3);
        let result_4 = registry.definition(path_4);
        assert!(result_1.is_some());
        assert!(result_2.is_some());
        assert!(result_3.is_some());
        assert!(result_4.is_none());
        assert_eq!(result_1, definitions.get(0));
        assert_eq!(result_2, definitions.get(1));
        assert_eq!(result_3, definitions.get(2));
    }
}
