//! Defines `Theme`, a smart style manager on top of style sheets.

use crate::prelude::*;

use crate::data::HashMapTree;
use crate::data::color;

use super::sheet::Path;
use super::sheet::Change;
use super::sheet::Value;
use super::sheet as style;



// =============
// === Theme ===
// =============

/// Smart style manager. Keeps a hierarchical style map. Styles can either be simple values or
/// expressions. Please note that expressions are not bound in themes and are being bound to
/// specific style sheet endpoints when theme is enabled in the themes `Manager`.
#[derive(Clone,Debug,Default)]
pub struct Theme {
    tree : HashMapTree<String,Option<Value>>
}

impl Theme {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Inserts a new style in the theme.
    pub fn insert<P,E>(&mut self, path:P, entry:E)
    where P:Into<Path>, E:Into<Value> {
        let path  = path.into();
        let entry = entry.into();
        self.tree.set(&path.rev_segments,Some(entry));
    }
}

impl Semigroup for Theme {
    fn concat_mut(&mut self, other:&Self) {
        self.tree.concat_mut(&other.tree);
    }
}




// ===============
// === Manager ===
// ===============

/// Theme manager. Allows registering themes by names, enabling, and disabling them.
#[derive(Debug,Default)]
pub struct Manager {
    all      : HashMap<String,Theme>,
    enabled  : Vec<String>,
    combined : Theme,
    sheets   : style::Sheet,
}

impl Manager {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Returns names of all enabled themes.
    pub fn enabled(&self) -> &Vec<String> {
        &self.enabled
    }

    /// Sets a new set of enabled themes.
    pub fn set_enabled<N>(&mut self, names:N)
    where N:IntoIterator, N::Item:ToString {
        let mut combined = Theme::new();
        self.enabled = names.into_iter().map(|name| name.to_string()).collect();
        for name in &self.enabled {
            if let Some(theme) = self.all.get(name) {
                combined.concat_mut(theme);
            }
        };

        let mut changes = Vec::<Change>::new();
        let diff        = self.combined.tree.zip_clone(&combined.tree);
        for (segments,values) in &diff {
            let path   = Path::from(segments);
            let first  = values.first().and_then(|t|t.as_ref());
            let second = values.second().and_then(|t|t.as_ref());
            if !values.same() {
                match (first,second) {
                    (None,None)     => {}
                    (Some(_),None)  => changes.push(Change::new(path,None)),
                    (_,Some(value)) => changes.push(Change::new(path,Some(value.clone()))),
                }
            }
        }
        self.combined = combined;
        self.sheets.apply_changes(changes);
    }

    /// Registers a new theme.
    pub fn register<T:Into<Theme>>(&mut self, name:impl Str, theme:T) {
        let name  = name.into();
        let theme = theme.into();
        self.all.insert(name,theme);
    }

    /// Removes the theme from the regitry.
    pub fn remove(&mut self, name:impl Str) {
        let name = name.as_ref();
        self.all.remove(name);
    }
}



// ============
// === Test ===
// ============

/// Test interactive usage. To be removed in the future.
pub fn test() {
    let mut registry = Manager::new();

    let mut theme1 = Theme::new();
    theme1.insert("application.background.color", color::Srgba::new(1.0,0.0,0.0,1.0));
    theme1.insert("animation.duration", 0.5);
    theme1.insert("graph.node.shadow.color", 5.0);
    theme1.insert("graph.node.shadow.size", 5.0);
    theme1.insert("mouse.pointer.color", color::Srgba::new(0.3,0.3,0.3,1.0));

    let mut theme2 = Theme::new();
    theme2.insert("application.background.color", color::Srgba::new(1.0,0.0,0.0,1.0));
    theme2.insert("animation.duration", 0.7);
    theme2.insert("graph.node.shadow.color", 5.0);
    theme2.insert("graph.node.shadow.size", 5.0);
    theme2.insert("mouse.pointer.color", color::Srgba::new(0.3,0.3,0.3,1.0));

    registry.register("theme1",theme1);
    registry.register("theme2",theme2);

    registry.set_enabled(&["theme1".to_string()]);
    println!("-------------------");
    registry.set_enabled(&["theme1","theme2"]);
}
