//! Keyboard and mouse shortcut management.

use crate::prelude::*;
use enso_shortcuts::traits::*;

use crate::frp;
use crate::frp::io::mouse::Mouse_DEPRECATED;

use super::command;
use enso_shortcuts as shortcuts;


// ==============
// === Export ===
// ==============

pub use shortcuts::ActionType;



// ============
// === Rule ===
// ============

/// Shortcut action rule, a combination of `ActionType`, like `Press` and a pattern, like
/// "ctrl shift s".
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[allow(missing_docs)]
pub struct Rule {
    pub tp:      ActionType,
    pub pattern: String,
}

impl Rule {
    /// Constructor.
    pub fn new(tp: impl Into<ActionType>, pattern: impl Into<String>) -> Self {
        let tp = tp.into();
        let pattern = pattern.into();
        Self { tp, pattern }
    }
}



// ===============
// === Command ===
// ===============

/// A command, textual label of action that should be evaluated in the target component.
#[derive(Clone, Debug, Eq, From, Hash, Into, PartialEq, Deref)]
pub struct Command {
    name: String,
}

impl From<&str> for Command {
    fn from(s: &str) -> Self {
        Self { name: s.into() }
    }
}



// =================
// === Condition ===
// =================

/// Condition expression.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[allow(missing_docs)]
pub enum Condition {
    Always,
    Never,
    When(String),
    Not(Box<Condition>),
    Or(Box<Condition>, Box<Condition>),
    And(Box<Condition>, Box<Condition>),
}

impl Condition {
    fn when(t: impl Into<String>) -> Self {
        Self::When(t.into())
    }

    fn not(a: Self) -> Self {
        Self::Not(Box::new(a))
    }

    fn and(a: Self, b: Self) -> Self {
        Self::And(Box::new(a), Box::new(b))
    }

    fn or(a: Self, b: Self) -> Self {
        Self::Or(Box::new(a), Box::new(b))
    }

    /// Split the input on the provided `separator`, process each chunk with `f`, and fold results
    /// using the `cons`.
    fn split_parse(
        input: &str,
        separator: char,
        cons: impl Fn(Self, Self) -> Self,
        f: impl Fn(&str) -> Self,
    ) -> Self {
        input.split(separator).map(|t| t.trim()).map(f).reduce(cons).unwrap_or(Self::Never)
    }

    /// Parses the provided input expression. The currently recognizable symbols are (sorted by
    /// precedence - high to low): negations(!), conjunctions (&), alternatives (|), and variables.
    /// For example, it parses the following expression: "a & b | !c". Parentheses are not supported
    /// yet.
    #[allow(clippy::redundant_closure)] // Rust TC does not agree.
    fn parse(s: impl AsRef<str>) -> Self {
        let s = s.as_ref().trim();
        if s.is_empty() {
            Self::Always
        } else {
            Self::split_parse(s, '|', Self::or, |s| {
                Self::split_parse(s, '&', Self::and, |s| {
                    if let Some(expr) = s.strip_prefix('!') {
                        Self::not(Self::when(expr.trim()))
                    } else {
                        Self::when(s)
                    }
                })
            })
        }
    }
}

impl From<&str> for Condition {
    fn from(s: &str) -> Self {
        Self::parse(s)
    }
}



// ==============
// === Action ===
// ==============

/// A shortcut action. Consist of target identifier (like "TextEditor"), a `Command` that will be
/// evaluated on the target, and a `Condition` which needs to be true in order for the command
/// to be executed.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Action {
    target:    String,
    command:   Command,
    condition: Condition,
}

impl Action {
    /// Constructor. Version without condition checker.
    pub fn new(target: impl Into<String>, command: impl Into<Command>) -> Self {
        Self::new_when(target, command, Condition::Always)
    }

    /// Constructor.
    pub fn new_when(
        target: impl Into<String>,
        command: impl Into<Command>,
        condition: impl Into<Condition>,
    ) -> Self {
        let target = target.into();
        let condition = condition.into();
        let command = command.into();
        Self { target, command, condition }
    }
}



// ================
// === Shortcut ===
// ================

/// A keyboard shortcut, an `Rule` associated with a `Action`.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Deref)]
pub struct Shortcut {
    #[deref]
    action: Action,
    rule:   Rule,
}

impl Shortcut {
    /// Constructor. Version without condition checker.
    pub fn new(
        rule: impl Into<Rule>,
        target: impl Into<String>,
        command: impl Into<Command>,
    ) -> Self {
        let action = Action::new(target, command);
        let rule = rule.into();
        Self { action, rule }
    }

    /// Constructor.
    pub fn new_when(
        rule: impl Into<Rule>,
        target: impl Into<String>,
        command: impl Into<Command>,
        condition: impl Into<Condition>,
    ) -> Self {
        let action = Action::new_when(target, command, condition);
        let rule = rule.into();
        Self { action, rule }
    }
}



// ================
// === Registry ===
// ================

/// Shortcut registry. See `Shortcut` to learn more.
///
/// You can add new shortcuts by using the `add` method and get a `Handle` back. When `Handle` is
/// dropped, the shortcut will be lazily removed. This is useful when defining shortcuts by GUI
/// components. When a component is unloaded, all its default shortcuts should be removed as well.
///
/// ## Implementation Notes
/// There should be a layer for user shortcuts which will remember handles permanently until a
/// shortcut is unregistered.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct Registry {
    #[deref]
    model:                 RegistryModel,
    network:               frp::Network,
    /// An FRP node that contains the name of the command currently being executed.
    /// `None` means that no command is being executed.
    pub currently_handled: frp::Source<Option<ImString>>,
}

/// Internal representation of `Registry`.
#[derive(Clone, CloneRef, Debug)]
pub struct RegistryModel {
    mouse:              Mouse_DEPRECATED,
    command_registry:   command::Registry,
    shortcuts_registry: shortcuts::HashSetRegistry<Shortcut>,
    currently_handled:  frp::Source<Option<ImString>>,
    /// If present, this is the receiver of commands.
    target:             Option<frp::NetworkId>,
}

impl Registry {
    /// Constructor.
    pub fn new(
        mouse: &Mouse_DEPRECATED,
        keyboard_target: &impl crate::display::Object,
        global_keyboard_target: &impl crate::display::Object,
        cmd_registry: &command::Registry,
    ) -> Self {
        frp::new_network! { network
            def currently_handled = source();
        }
        let model = RegistryModel::new(mouse, cmd_registry, currently_handled.clone_ref(), None);
        Self::extend_network(&network, &model, keyboard_target, global_keyboard_target);
        Self { model, network, currently_handled }
    }

    /// Create a shortcut registry inheriting global parameters, bound to the given instance, and
    /// owned by the given network.
    pub fn instance_bound_child_in_network(
        &self,
        instance: frp::NetworkId,
        keyboard_target: &impl crate::display::Object,
        global_keyboard_target: &impl crate::display::Object,
        network: &frp::Network,
    ) -> RegistryModel {
        let mouse = &self.mouse;
        let cmd_registry = &self.command_registry;
        let currently_handled = self.currently_handled.clone_ref();
        let model = RegistryModel::new(mouse, cmd_registry, currently_handled, Some(instance));
        Self::extend_network(network, &model, keyboard_target, global_keyboard_target);
        model
    }

    /// Connect the model to the given network and keyboard target.
    fn extend_network(
        network: &frp::Network,
        model: &RegistryModel,
        keyboard_target: &impl crate::display::Object,
        global_keyboard_target: &impl crate::display::Object,
    ) {
        let mouse = &model.mouse;
        let kb_down = keyboard_target.on_event::<crate::control::io::keyboard::KeyDown>();
        let kb_up =
            global_keyboard_target.on_event_capturing::<crate::control::io::keyboard::KeyUp>();
        let registry = &model.shortcuts_registry;
        frp::extend! { network
            kb_down <- kb_down.map(f!([registry](e)
                (e.propagation_stopper(), registry.on_press(e.key().simple_name()))));
            kb_up <- kb_up.map(f!([registry](e)
                (e.propagation_stopper(), registry.on_release(e.key().simple_name()))));
            mouse_down <- mouse.down.map(f!([registry](e)
                (default(), registry.on_press(e.simple_name()))));
            mouse_up <- mouse.up.map(f!([registry](e)
                (default(), registry.on_release(e.simple_name()))));
            event <- any(kb_down, kb_up, mouse_down, mouse_up);
            eval event (((event, rules)) model.process_rules(event, rules));
        }
    }
}

impl RegistryModel {
    /// Constructor.
    pub fn new(
        mouse: &Mouse_DEPRECATED,
        command_registry: &command::Registry,
        currently_handled: frp::Source<Option<ImString>>,
        target: Option<frp::NetworkId>,
    ) -> Self {
        let mouse = mouse.clone_ref();
        let command_registry = command_registry.clone_ref();
        let shortcuts_registry = default();
        Self { mouse, command_registry, shortcuts_registry, currently_handled, target }
    }

    fn process_rules(&self, stop_propagation: impl FnOnce<()>, rules: &[Shortcut]) {
        let mut targets = Vec::new();
        {
            let borrowed_command_map = self.command_registry.name_map.borrow();
            let bound_target =
                self.target.and_then(|id| self.command_registry.id_map.borrow().get(&id).cloned());
            for rule in rules {
                let instances = match bound_target.as_ref() {
                    Some(target) => slice::from_ref(target),
                    None => borrowed_command_map
                        .get(&rule.action.target)
                        .map(Vec::as_slice)
                        .unwrap_or_default(),
                };
                for instance in instances {
                    if Self::condition_checker(&rule.condition, &instance.status_map) {
                        let command_name = &rule.command.name;
                        match instance.command_map.borrow().get(command_name) {
                            Some(cmd) =>
                                if cmd.enabled {
                                    targets.push((cmd.frp.clone_ref(), command_name))
                                },
                            None => error!(
                                "Command {command_name} was not found on {}.",
                                match bound_target.as_ref() {
                                    Some(_) => "bound instance",
                                    None => &rule.action.target,
                                }
                            ),
                        }
                    }
                }
            }
        }
        if !targets.is_empty() {
            stop_propagation();
        }
        for (target, name) in targets {
            debug_span!("Emitting command {name} on {target:?}.").in_scope(|| {
                let name = Some(ImString::from(name));
                self.currently_handled.emit(name);
                target.emit(());
                self.currently_handled.emit(None);
            });
        }
    }

    fn condition_checker(
        condition: &Condition,
        status: &Rc<RefCell<HashMap<String, frp::Sampler<bool>>>>,
    ) -> bool {
        use Condition::*;
        match condition {
            Always => true,
            Never => false,
            When(name) => status.borrow().get(name).map(|t| t.value()).unwrap_or(false),
            Not(a) => !Self::condition_checker(a, status),
            Or(a, b) => Self::condition_checker(a, status) || Self::condition_checker(b, status),
            And(a, b) => Self::condition_checker(a, status) && Self::condition_checker(b, status),
        }
    }
}

impl Add<Shortcut> for &RegistryModel {
    type Output = ();
    fn add(self, shortcut: Shortcut) {
        self.shortcuts_registry.add(shortcut.rule.tp, &shortcut.rule.pattern, shortcut.clone());
    }
}
