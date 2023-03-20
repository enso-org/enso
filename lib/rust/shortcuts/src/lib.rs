//! Keyboard shortcut manager implementation.

// === Features ===
#![feature(test)]
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]



extern crate test;

use enso_automata::*;
use enso_prelude::*;
use enso_web as web;



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    pub use super::Registry as TRAIT_Registry;
}



// ===============
// === Helpers ===
// ===============

/// Pretty prints the provided matrix.
pub fn print_matrix(matrix: &data::Matrix<dfa::State>) {
    println!("rows x cols = {} x {} ({})", matrix.rows, matrix.columns, matrix.matrix.len());
    for row in 0..matrix.rows {
        for column in 0..matrix.columns {
            let elem = matrix.safe_index(row, column).unwrap();
            let repr = if elem.is_invalid() { "-".into() } else { format!("{}", elem.id()) };
            print!("{repr} ");
        }
        println!();
    }
}

/// List of special keys. Special keys can be grouped together to distinguish action sequences like
/// `ctrl + a` and `a + ctrl`. Please note, that this is currently not happening.
const SIDE_KEYS: &[&str] = &["ctrl", "alt", "alt-graph", "meta", "cmd", "shift"];

lazy_static! {
    static ref SIDE_KEYS_SET: HashSet<&'static str> = SIDE_KEYS.iter().copied().collect();
}

/// The maximum time difference between presses/clicks where they are treated as single
/// `DoublePress`/`DoubleClick` event.
pub const DOUBLE_EVENT_TIME_MS: f32 = 300.0;



// ==================
// === ActionType ===
// ==================

/// The type of the action. Could be applied to keyboard, mouse, or any mix of input events.
/// As a clarification, the event `DoublePress` is emitted on second press of a button/key happening
/// in short time interval from the first one. `DoubleClick`, on the other hand, happens on release,
/// not on press.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(missing_docs)]
pub enum ActionType {
    Press,
    PressAndRepeat,
    Release,
    DoublePress,
    DoubleClick,
}
pub use ActionType::*;



// ================
// === Registry ===
// ================

/// Abstraction for shortcut registry implementation.
#[allow(missing_docs)]
pub trait Registry<T>: Default {
    /// Add a new action mapping. `The expr` needs to be a list of keys separated by space, like
    /// "ctrl shift a".
    fn add(&self, action_type: ActionType, expr: impl AsRef<str>, action: impl Into<T>);

    /// Get a list of items registered for the action that just happened. It might include items
    /// registered for `DoublePress` or `DoubleClick` if the actions were performed fast enough.
    fn on_press(&self, input: impl AsRef<str>) -> Vec<T>;

    /// Get a list of items registered for the action that just happened. It might include items
    /// registered for `DoublePress` or `DoubleClick` if the actions were performed fast enough.
    fn on_release(&self, input: impl AsRef<str>) -> Vec<T>;

    /// Some engines might implement a separate optimization stage. This is intended to force the
    /// optimization at a given point in time. Used mainly in benchmarks.
    fn optimize(&self) {}
}



// ============================
// === HashSetRegistryModel ===
// ============================

pub trait HashSetRegistryItem = Clone + Debug + Eq + Hash;

/// Internal model for `HashSetRegistry`.
#[derive(Debug)]
pub struct HashSetRegistryModel<T> {
    current_expr:  String,
    actions:       HashMap<ActionType, HashMap<String, Vec<T>>>,
    pressed:       HashSet<String>,
    press_times:   HashMap<String, f32>,
    release_times: HashMap<String, f32>,
    side_keys:     HashMap<String, Vec<String>>,
    key_aliases:   HashMap<String, String>,
}

impl<T> HashSetRegistryModel<T> {
    /// Constructor.
    pub fn new() -> Self {
        let current_expr = default();
        let actions = default();
        let pressed = default();
        let press_times = default();
        let release_times = default();
        let side_keys = default();
        let key_aliases = key_aliases();
        Self { current_expr, actions, pressed, press_times, release_times, side_keys, key_aliases }
            .init()
    }

    fn init(mut self) -> Self {
        for key in SIDE_KEYS {
            let alts = vec![format!("{key}-left"), format!("{key}-right"), (*key).to_string()];
            self.side_keys.insert((*key).to_string(), alts);
        }
        self
    }

    fn current_expr(&self) -> String {
        self.pressed.iter().sorted().join(" ")
    }
}

impl<T: HashSetRegistryItem> HashSetRegistryModel<T> {
    /// Add a new shortcut definition.
    pub fn add(&mut self, action_type: ActionType, input: impl AsRef<str>, action: impl Into<T>) {
        let input = input.as_ref();
        let action = action.into();
        let exprs = self.possible_exprs(input);
        let map = self.actions.entry(action_type).or_default();
        for expr in exprs {
            map.entry(expr).or_default().push(action.clone());
        }
    }

    #[allow(clippy::collapsible_else_if)]
    fn on_event(&mut self, input: impl AsRef<str>, press: bool) -> Vec<T> {
        let input = input.as_ref().to_lowercase();
        let exists = self.pressed.contains(&input);
        let repeat = if press { exists } else { !exists };
        if !repeat {
            let out = self.process_event(Release);
            if press {
                self.pressed.insert(input);
            } else {
                self.pressed.remove(&input);
            }
            self.current_expr = self.current_expr();
            out.extended(self.process_event(Press)).extended(self.process_event(PressAndRepeat))
        } else {
            if press {
                self.process_event(PressAndRepeat)
            } else {
                default()
            }
        }
    }

    fn process_event(&mut self, action: ActionType) -> Vec<T> {
        let expr = &self.current_expr;
        let mut out = self
            .actions
            .get(&action)
            .and_then(|t| t.get(expr))
            .into_iter()
            .flatten()
            .cloned()
            .collect_vec();
        if action != PressAndRepeat {
            let is_press = action == Press;
            let action2 = if is_press { DoublePress } else { DoubleClick };
            let time_map = if is_press { &mut self.press_times } else { &mut self.release_times };
            let time = web::time_from_start() as f32;
            let last_time = time_map.get(expr);
            let time_diff = last_time.map(|t| time - t);
            let is_double = time_diff.map(|t| t < DOUBLE_EVENT_TIME_MS) == Some(true);
            if is_double {
                out.extend(
                    self.actions
                        .get(&action2)
                        .and_then(|t| t.get(expr))
                        .into_iter()
                        .flatten()
                        .cloned(),
                );
                time_map.remove(expr);
            } else {
                *time_map.entry(expr.clone()).or_default() = time;
            }
        }
        out
    }

    /// Handle the key press.
    pub fn on_press(&mut self, input: impl AsRef<str>) -> Vec<T>
    where T: Debug {
        self.on_event(input, true)
    }

    /// Handle the key release.
    pub fn on_release(&mut self, input: impl AsRef<str>) -> Vec<T>
    where T: Debug {
        self.on_event(input, false)
    }

    /// Return all possible expressions with sorted keys for a given input expression. For example,
    /// for the input expression "cmd a", it will return ["a cmd", "a cmd-left", "a cmd-right"].
    fn possible_exprs(&self, expr: impl AsRef<str>) -> Vec<String> {
        let mut out = Vec::<String>::new();
        let expr = expr.as_ref();
        let chunks = expr.split(' ').map(|t| t.trim()).filter(|t| !t.is_empty());
        let keys = chunks.map(|t| self.key_aliases.get(t).map(|t| t.as_ref()).unwrap_or(t));
        for key in keys.sorted() {
            match self.side_keys.get(key) {
                Some(alts) =>
                    if out.is_empty() {
                        out.extend(alts.iter().cloned());
                    } else {
                        let local_out = mem::take(&mut out);
                        for alt in alts {
                            out.extend(local_out.iter().map(|expr| format!("{expr} {alt}")));
                        }
                    },
                None =>
                    if out.is_empty() {
                        out.push(key.into());
                    } else {
                        for el in out.iter_mut() {
                            *el = format!("{el} {key}");
                        }
                    },
            }
        }
        out
    }
}

impl<T> Default for HashSetRegistryModel<T> {
    fn default() -> Self {
        Self::new()
    }
}

fn key_aliases() -> HashMap<String, String> {
    let mut map = HashMap::<String, String>::new();
    let cmd_target = match web::platform::current() {
        Some(web::platform::MacOS) => "meta",
        _ => "ctrl",
    };
    #[allow(clippy::useless_format)]
    let insert_side_key = |map: &mut HashMap<String, String>, k: &str, v: &str| {
        map.insert(format!("{k}"), format!("{v}"));
        map.insert(format!("{k}-left"), format!("{v}-left"));
        map.insert(format!("{k}-right"), format!("{v}-right"));
    };
    let insert = |map: &mut HashMap<String, String>, k: &str, v: &str| {
        map.insert(k.into(), v.into());
    };
    insert_side_key(&mut map, "control", "ctrl");
    insert_side_key(&mut map, "option", "alt");
    insert_side_key(&mut map, "cmd", cmd_target);
    insert_side_key(&mut map, "command", cmd_target);
    insert(&mut map, "left", "arrow-left");
    insert(&mut map, "right", "arrow-right");
    insert(&mut map, "up", "arrow-up");
    insert(&mut map, "down", "arrow-down");
    insert(&mut map, "left-mouse-button", "mouse-button-0");
    insert(&mut map, "middle-mouse-button", "mouse-button-1");
    insert(&mut map, "right-mouse-button", "mouse-button-2");
    map
}



// =======================
// === HashSetRegistry ===
// =======================

/// Shortcut registry implementation based on hash sets.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
pub struct HashSetRegistry<T> {
    rc: Rc<RefCell<HashSetRegistryModel<T>>>,
}

impl<T> HashSetRegistry<T> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

impl<T: HashSetRegistryItem> Registry<T> for HashSetRegistry<T> {
    fn add(&self, action_type: ActionType, expr: impl AsRef<str>, action: impl Into<T>) {
        self.rc.borrow_mut().add(action_type, expr, action)
    }

    fn on_press(&self, input: impl AsRef<str>) -> Vec<T> {
        self.rc.borrow_mut().on_press(input)
    }

    fn on_release(&self, input: impl AsRef<str>) -> Vec<T> {
        self.rc.borrow_mut().on_release(input)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    // === Press ===

    #[test]
    fn hash_set_registry_press() {
        press::<HashSetRegistry<i32>>();
    }
    fn press<T: Registry<i32>>() -> T {
        let nothing = Vec::<i32>::new();
        let registry = <T>::default();
        registry.add(Press, "ctrl a", 0);
        assert_eq!(registry.on_press("ctrl-left"), nothing);
        for _ in 0..10 {
            assert_eq!(registry.on_press("a"), vec![0]);
            assert_eq!(registry.on_release("a"), nothing);
        }
        registry
    }


    // === Release ===

    #[test]
    fn hash_set_registry_release() {
        release::<HashSetRegistry<i32>>();
    }
    fn release<T: Registry<i32>>() -> T {
        let nothing = Vec::<i32>::new();
        let registry = <T>::default();
        registry.add(Release, "ctrl a", 0);
        assert_eq!(registry.on_press("ctrl-left"), nothing);
        for _ in 0..10 {
            assert_eq!(registry.on_press("a"), nothing);
            assert_eq!(registry.on_release("a"), vec![0]);
        }
        registry
    }


    // === DoublePress ===

    #[test]
    fn hash_set_registry_double_press() {
        double_press::<HashSetRegistry<i32>>();
    }
    fn double_press<T: Registry<i32>>() -> T {
        let nothing = Vec::<i32>::new();
        let registry = <T>::default();
        registry.add(DoublePress, "ctrl a", 0);
        assert_eq!(registry.on_press("ctrl-left"), nothing);
        for _ in 0..10 {
            assert_eq!(registry.on_press("a"), nothing);
            assert_eq!(registry.on_release("a"), nothing);
            web::simulate_sleep(100.0);
            assert_eq!(registry.on_press("a"), vec![0]);
            assert_eq!(registry.on_release("a"), nothing);
            web::simulate_sleep(100.0);
            assert_eq!(registry.on_press("a"), nothing);
            assert_eq!(registry.on_release("a"), nothing);
            web::simulate_sleep(1000.0);
        }
        registry
    }


    // === Overlapping Shortcuts ===

    #[test]
    fn hash_set_registry_overlapping() {
        overlapping::<HashSetRegistry<i32>>();
    }
    fn overlapping<T: Registry<i32>>() -> T {
        let nothing = Vec::<i32>::new();
        let registry = <T>::default();
        let items = (0..100)
            .map(|i| {
                registry.add(Press, "ctrl a", i);
                i
            })
            .collect_vec();
        assert_eq!(registry.on_press("ctrl-left"), nothing);
        assert_eq!(registry.on_press("a"), items);
        assert_eq!(registry.on_release("a"), nothing);
        registry
    }


    // === Side Keys ===

    #[test]
    fn hash_set_registry_side_keys() {
        side_keys::<HashSetRegistry<i32>>();
    }
    fn side_keys<T: Registry<i32>>() -> T {
        let nothing = Vec::<i32>::new();
        let registry: T = default();
        registry.add(Press, "ctrl meta a", 0);
        for ctrl in &["ctrl", "ctrl-left", "ctrl-right"] {
            for meta in &["meta", "meta-left", "meta-right"] {
                assert_eq!(registry.on_press(ctrl), nothing);
                assert_eq!(registry.on_press(meta), nothing);
                assert_eq!(registry.on_press("a"), vec![0]);
                assert_eq!(registry.on_release("a"), nothing);
                assert_eq!(registry.on_release(meta), nothing);
                assert_eq!(registry.on_release(ctrl), nothing);

                assert_eq!(registry.on_press(meta), nothing);
                assert_eq!(registry.on_press(ctrl), nothing);
                assert_eq!(registry.on_press("a"), vec![0]);
                assert_eq!(registry.on_release("a"), nothing);
                assert_eq!(registry.on_release(ctrl), nothing);
                assert_eq!(registry.on_release(meta), nothing);
            }
        }
        registry
    }


    // === Press / Release Sequence ===

    #[test]
    fn hash_set_registry_sequence() {
        sequence::<HashSetRegistry<&'static str>>();
    }
    fn sequence<T: Registry<&'static str>>() -> T {
        let nothing = Vec::<&'static str>::new();
        let registry: T = default();
        registry.add(Press, "ctrl a", "press ctrl a");
        registry.add(Release, "ctrl a", "release ctrl a");
        registry.add(Press, "a", "press a");
        registry.add(Release, "a", "release a");
        assert_eq!(registry.on_press("ctrl-left"), nothing);
        assert_eq!(registry.on_press("a"), vec!["press ctrl a"]);
        assert_eq!(registry.on_release("ctrl-left"), vec!["release ctrl a", "press a"]);
        assert_eq!(registry.on_release("a"), vec!["release a"]);
        registry
    }


    // === Disabled Key Repeat ===

    #[test]
    fn hash_set_registry_repeat() {
        repeat::<HashSetRegistry<&'static str>>();
    }
    fn repeat<T: Registry<&'static str>>() -> T {
        let nothing = Vec::<&'static str>::new();
        let registry: T = default();
        registry.add(Press, "a", "press a");
        registry.add(Release, "a", "release a");
        for _ in 0..10 {
            assert_eq!(registry.on_press("a"), vec!["press a"]);
            assert_eq!(registry.on_press("a"), nothing);
            assert_eq!(registry.on_press("a"), nothing);
            assert_eq!(registry.on_press("a"), nothing);
            assert_eq!(registry.on_release("a"), vec!["release a"]);
        }
        registry
    }


    // === Valid States ===

    #[test]
    fn hash_set_registry_valid_states() {
        valid_states::<HashSetRegistry<i32>>(false);
    }
    fn valid_states<T: Registry<i32>>(allow_broken_shortcut: bool) -> T {
        let nothing = Vec::<i32>::new();
        let registry: T = default();
        registry.add(Press, "ctrl meta a", 0);
        registry.add(Press, "ctrl meta b", 1);
        // First shortcut.
        assert_eq!(registry.on_press("meta-left"), nothing);
        assert_eq!(registry.on_press("ctrl-left"), nothing);
        assert_eq!(registry.on_press("a"), vec![0]);
        assert_eq!(registry.on_release("a"), nothing);
        assert_eq!(registry.on_press("a"), vec![0]);
        assert_eq!(registry.on_release("a"), nothing);
        // Second shortcut.
        assert_eq!(registry.on_press("b"), vec![1]);
        assert_eq!(registry.on_release("b"), nothing);
        // Incorrect sequence.
        assert_eq!(registry.on_press("meta-right"), nothing);
        assert_eq!(registry.on_release("meta-right"), nothing);
        if allow_broken_shortcut {
            // Broken shortcut after incorrect sequence.
            assert_eq!(registry.on_press("b"), nothing);
            assert_eq!(registry.on_release("b"), nothing);
        } else {
            assert_eq!(registry.on_press("b"), vec![1]);
            assert_eq!(registry.on_release("b"), nothing);
        }
        // Restoring shortcuts on release all keys.
        assert_eq!(registry.on_release("meta-left"), nothing);
        assert_eq!(registry.on_release("ctrl-left"), nothing);
        // Testing recovered first shortcut again.
        assert_eq!(registry.on_press("meta-left"), nothing);
        assert_eq!(registry.on_press("ctrl-left"), nothing);
        assert_eq!(registry.on_press("a"), vec![0]);
        registry
    }
}



// ==================
// === Benchmarks ===
// ==================

#[cfg(test)]
mod benchmarks {
    use super::*;
    use test::Bencher;

    const CONS_SIMPLE: &str = "ctrl";
    const CONS_COMPLEX: &str = "ctrl cmd alt shift";

    // === Construction ===

    #[bench]
    fn hashset_registry_construction_simple(bencher: &mut Bencher) {
        construction::<HashSetRegistry<i32>>(CONS_SIMPLE, true, bencher);
    }

    #[bench]
    fn hashset_registry_construction_complex(bencher: &mut Bencher) {
        construction::<HashSetRegistry<i32>>(CONS_COMPLEX, true, bencher);
    }

    fn construction<T: Registry<i32>>(input: &str, optimize: bool, bencher: &mut Bencher) -> T {
        bencher.iter(|| {
            let registry: T = default();
            let max_count = test::black_box(10);
            for i in 0..max_count {
                registry.add(Press, format!("{i} a{input}"), i);
            }
            if optimize {
                registry.optimize();
            }
        });
        default()
    }


    // === Lookup ===

    #[bench]
    fn hashset_registry_lookup(bencher: &mut Bencher) {
        lookup::<HashSetRegistry<i32>>(bencher);
    }

    fn lookup<T: Registry<i32>>(bencher: &mut Bencher) -> T {
        let registry: T = default();
        let nothing = Vec::<i32>::new();
        let max_count = test::black_box(100);
        for i in 0..max_count {
            registry.add(Press, format!("ctrl shift a{i}"), i);
        }
        registry.optimize();
        bencher.iter(|| {
            for i in 0..max_count {
                let key = format!("a{i}");
                assert_eq!(registry.on_press("ctrl-left"), nothing);
                assert_eq!(registry.on_press("shift-left"), nothing);
                assert_eq!(registry.on_press(&key), vec![i]);
                assert_eq!(registry.on_release(&key), nothing);
                assert_eq!(registry.on_release("shift-left"), nothing);
                assert_eq!(registry.on_release("ctrl-left"), nothing);
            }
        });
        registry
    }
}
