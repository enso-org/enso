//! Defines `Theme`, a smart style manager on top of style sheets.

use crate::control::callback::traits::*;
use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::control::callback;
use crate::data::color;
use crate::data::dirty;
use crate::data::HashMapTree;

use super::sheet as style;
use super::sheet::Change;
use super::sheet::Path;
use super::sheet::Value;



// =============
// === Theme ===
// =============

/// Smart style manager. Keeps a hierarchical style map. Styles can either be simple values or
/// expressions. Please note that expressions are not bound in themes and are being bound to
/// specific style sheet endpoints when theme is enabled in the themes `Manager`.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Theme {
    tree:   Rc<RefCell<HashMapTree<String, Option<Value>>>>,
    on_mut: callback::registry::NoArgs,
}

impl Theme {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// A deep clone of this theme.
    pub fn deep_clone(&self) -> Self {
        let tree = Rc::new(RefCell::new(self.tree.borrow().clone()));
        let on_mut = default();
        Self { tree, on_mut }
    }

    /// Insert or modify a style in the theme. Sets the style and returns [`true`] if `value` was
    /// successfully parsed to [`Data`], or returns [`false`] otherwise.
    pub fn parse_and_set(&self, path: impl Into<Path>, value: &str) -> bool {
        let parsed_value = style::Data::parse(value);
        if let Some(value) = parsed_value {
            self.set(path, value);
            true
        } else {
            false
        }
    }

    /// Insert or modify a style in the theme.
    pub fn set<P, E>(&self, path: P, value: E)
    where
        P: Into<Path>,
        E: Into<Value>, {
        let path = path.into();
        let value = value.into();
        self.tree.borrow_mut().set(path.rev_segments, Some(value));
        self.on_mut.run_all();
    }

    /// Add a new callback which will be triggered everytime this theme is modified.
    pub fn on_mut(&self, callback: impl callback::NoArgs) -> callback::Handle {
        self.on_mut.add(callback)
    }

    /// Get a copy of the value tree of this theme.
    pub fn value_tree(&self) -> HashMapTree<String, Option<Value>> {
        self.tree.borrow().clone()
    }

    /// Get a list of all paths and values of this theme.
    pub fn values(&self) -> Vec<(String, Value)> {
        self.tree
            .borrow()
            .iter()
            .filter_map(|(path, opt_val)| {
                opt_val.as_ref().map(|val| {
                    let path = path.into_iter().rev().cloned().collect_vec().join(".");
                    let val = val.clone();
                    (path, val)
                })
            })
            .collect_vec()
    }

    /// Compute changes between this and a target theme.
    pub fn diff(&self, tgt: &Theme) -> Vec<Change> {
        let mut changes = Vec::<Change>::new();
        let diff = self.tree.borrow().zip_clone(&tgt.tree.borrow());
        for (segments, values) in &diff {
            let path = Path::from_rev_segments(segments);
            let first = values.first().and_then(|t| t.as_ref());
            let second = values.second().and_then(|t| t.as_ref());
            if !values.same() {
                match (first, second) {
                    (None, None) => {}
                    (Some(_), None) => changes.push(Change::new(path, None)),
                    (_, Some(value)) => changes.push(Change::new(path, Some(value.clone()))),
                }
            }
        }
        changes
    }
}

impl PartialSemigroup<&Theme> for Theme {
    fn concat_mut(&mut self, other: &Self) {
        self.tree.borrow_mut().concat_mut(&*other.tree.borrow());
    }
}



// ===============
// === Manager ===
// ===============

/// Internal data used by the `Manager`.
#[derive(Debug, Default)]
pub struct ManagerData {
    all:         HashMap<String, Theme>,
    enabled:     Vec<String>,
    combined:    Theme,
    style_sheet: style::Sheet,
}

impl ManagerData {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Return names of all enabled themes.
    pub fn enabled(&self) -> &Vec<String> {
        &self.enabled
    }

    /// Return a reference to the theme of the given name.
    pub fn get(&self, name: &str) -> Option<&Theme> {
        self.all.get(name)
    }

    /// Return all registered theme names.
    pub fn keys(&self) -> Vec<String> {
        self.all.keys().cloned().collect_vec()
    }

    /// Sets a new set of enabled themes.
    pub fn set_enabled<N>(&mut self, names: N)
    where
        N: IntoIterator,
        N::Item: ToString, {
        self.enabled = names.into_iter().map(|name| name.to_string()).collect();
        let combined = self.combine(&self.enabled);
        let changes = self.combined.diff(&combined);
        self.combined = combined;
        self.style_sheet.apply_changes(changes);
    }

    /// Compute changes between the source and the target theme.
    pub fn diff(&self, src: &str, tgt: &str) -> Vec<Change> {
        match (self.get(src), self.get(tgt)) {
            (Some(src), Some(tgt)) => src.diff(tgt),
            _ => default(),
        }
    }

    /// Combine several themes into one.
    pub fn combine<N>(&self, names: N) -> Theme
    where
        N: IntoIterator,
        N::Item: AsRef<str>, {
        let mut combined = Theme::new();
        for name in names {
            if let Some(theme) = self.all.get(name.as_ref()) {
                combined.concat_mut(theme);
            }
        }
        combined
    }

    /// Reload the currently selected themes. This function is automatically called when an used
    /// theme changes. The refresh is done lazily, only on fields that actually changed. You should
    /// not need to call it manually.
    pub fn refresh(&mut self) {
        self.set_enabled(self.enabled.clone())
    }

    /// Registers a new theme.
    pub fn register<T: Into<Theme>>(&mut self, name: impl Str, theme: T) {
        let name = name.into();
        let theme = theme.into();
        self.all.insert(name, theme);
    }

    /// Removes the theme from the registry.
    pub fn remove(&mut self, name: impl Str) {
        let name = name.as_ref();
        self.all.remove(name);
    }
}

impl From<&style::Sheet> for ManagerData {
    fn from(style_sheet: &style::Sheet) -> Self {
        let style_sheet = style_sheet.clone_ref();
        Self { style_sheet, ..default() }
    }
}



// ===============
// === Manager ===
// ===============

/// Theme manager. Allows registering themes by names, enabling, and disabling them.
#[derive(Clone, CloneRef, Debug)]
pub struct Manager {
    data:          Rc<RefCell<ManagerData>>,
    handles:       Rc<RefCell<HashMap<String, callback::Handle>>>,
    current_dirty: dirty::SharedBool,
    enabled_dirty: dirty::SharedVector<String>,
    initialized:   Rc<Cell<bool>>,
}

impl Manager {
    /// Constructor.
    pub fn new() -> Self {
        let current_dirty = dirty::SharedBool::new(());
        let enabled_dirty = dirty::SharedVector::new(());
        let data = default();
        let handles = default();
        let initialized = default();
        Self { data, handles, current_dirty, enabled_dirty, initialized }
    }

    /// Return a theme of the given name.
    pub fn get(&self, name: &str) -> Option<Theme> {
        self.data.borrow().get(name).cloned()
    }

    /// Return all registered theme names.
    pub fn keys(&self) -> Vec<String> {
        self.data.borrow().keys()
    }

    /// Registers a new theme.
    pub fn register<T: Into<Theme>>(&self, name: impl Str, theme: T) {
        self.register_internal(name.into(), theme.into())
    }

    /// Compute changes between the source and the target theme.
    pub fn diff(&self, src: &str, tgt: &str) -> Vec<Change> {
        self.data.borrow().diff(src, tgt)
    }

    /// Make a snapshot of the current theme and save it with the provided name. It also sets the
    /// newly created theme as current theme.
    pub fn snapshot(&self, name: impl Str) {
        let name = name.into();
        let theme = self.data.borrow().combined.deep_clone();
        self.register_internal(name.clone(), theme);
        self.set_enabled([name]);
    }

    fn register_internal(&self, name: String, theme: Theme) {
        let dirty = self.current_dirty.clone_ref();
        let handle = theme.on_mut(move || dirty.set());
        self.data.borrow_mut().register(&name, theme);
        self.handles.borrow_mut().insert(name, handle);
    }

    /// Sets a new set of enabled themes.
    pub fn set_enabled<N>(&self, names: N)
    where
        N: IntoIterator,
        N::Item: ToString, {
        self.enabled_dirty.unset_all();
        for name in names {
            self.enabled_dirty.set(name.to_string())
        }
        // TODO[WD]: This impl should be uncommented, and the `self.update()` line removed,
        //   but now it causes project name to be red (to be investigated). This should be fixed
        //   after whole theme manager is finished: https://github.com/enso-org/ide/issues/795
        // // First theme set can skip lazy change, as this is normally done on app startup.
        // // It will also make the startup faster, as the theme will not be updated on the next
        // // frame, which would make all shaders re-compile.
        // if self.initialized.get() {
        //     self.initialized.set(true);
        //     self.update()
        // }
        self.update()
    }

    /// Update the theme manager. This should be done once per an animation frame.
    pub fn update(&self) {
        if self.enabled_dirty.check_all() {
            self.current_dirty.take();
            let names = self.enabled_dirty.take().vec;
            self.data.borrow_mut().set_enabled(names);
        } else if self.current_dirty.take().check() {
            self.data.borrow_mut().refresh()
        }
    }
}

impl Default for Manager {
    fn default() -> Self {
        Self::new()
    }
}

impl From<&style::Sheet> for Manager {
    fn from(style_sheet: &style::Sheet) -> Self {
        let data = Rc::new(RefCell::new(style_sheet.into()));
        Self { data, ..default() }
    }
}

impl AsRef<Manager> for Manager {
    fn as_ref(&self) -> &Manager {
        self
    }
}



// ============
// === Test ===
// ============

/// Test interactive usage. To be removed in the future.
pub fn test() {
    let theme_manager = Manager::new();

    let theme1 = Theme::new();
    theme1.set("application.background.color", color::Rgba::new(1.0, 0.0, 0.0, 1.0));
    theme1.set("animation.duration", 0.5);
    theme1.set("graph.node.shadow.color", 5.0);
    theme1.set("graph.node.shadow.size", 5.0);
    theme1.set("mouse.pointer.color", color::Rgba::new(0.3, 0.3, 0.3, 1.0));

    let theme2 = Theme::new();
    theme2.set("application.background.color", color::Rgba::new(1.0, 0.0, 0.0, 1.0));
    theme2.set("animation.duration", 0.7);
    theme2.set("graph.node.shadow.color", 5.0);
    theme2.set("graph.node.shadow.size", 5.0);
    theme2.set("mouse.pointer.color", color::Rgba::new(0.3, 0.3, 0.3, 1.0));

    theme_manager.register("theme1", theme1);
    theme_manager.register("theme2", theme2);

    theme_manager.set_enabled(["theme1".to_string()]);
    theme_manager.set_enabled(["theme1", "theme2"]);
}
