use crate::prelude::*;



/// A trie data structure, where each node represents a single fs path component.
///
/// As such, a trie defines a set of fs paths (each being defined by a path within the trie).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Trie<'a> {
    pub children: HashMap<std::path::Component<'a>, Trie<'a>>,
    /// Number of paths that end in this node.
    pub count:    usize,
}

impl<'a> Trie<'a> {
    /// Insert a path into the trie.
    pub fn insert(&mut self, path: &'a Path) {
        let mut current = self;
        for component in path.components() {
            current = current.children.entry(component).or_default();
        }
        current.count += 1;
    }

    /// Is this node a leaf?
    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }
}

impl<'a> FromIterator<&'a Path> for Trie<'a> {
    fn from_iter<I: IntoIterator<Item = &'a Path>>(iter: I) -> Self {
        let mut trie = Trie::default();
        trie.extend(iter);
        trie
    }
}

impl<'a> FromIterator<&'a PathBuf> for Trie<'a> {
    fn from_iter<I: IntoIterator<Item = &'a PathBuf>>(iter: I) -> Self {
        let mut trie = Trie::default();
        trie.extend(iter);
        trie
    }
}

impl<'a> Extend<&'a Path> for Trie<'a> {
    fn extend<I: IntoIterator<Item = &'a Path>>(&mut self, iter: I) {
        for path in iter {
            self.insert(path);
        }
    }
}

impl<'a> Extend<&'a PathBuf> for Trie<'a> {
    fn extend<I: IntoIterator<Item = &'a PathBuf>>(&mut self, iter: I) {
        for path in iter {
            self.insert(path);
        }
    }
}
