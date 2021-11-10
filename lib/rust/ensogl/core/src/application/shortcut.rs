//! Keyboard and mouse shortcut management.

use crate::prelude::*;

use super::command;

use crate::frp;
use crate::frp::io::keyboard;
use crate::frp::io::mouse::Mouse;

use enso_shortcuts as shortcuts;
use enso_shortcuts::traits::*;

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
#[derive(Clone, Debug, Eq, From, Hash, Into, PartialEq, Shrinkwrap)]
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
        input.split(separator).map(|t| t.trim()).map(f).fold1(cons).unwrap_or(Self::Never)
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
#[derive(Clone, Debug, Eq, PartialEq, Hash, Shrinkwrap)]
pub struct Shortcut {
    #[shrinkwrap(main_field)]
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
#[derive(Clone, CloneRef, Debug)]
pub struct Registry {
    model:   RegistryModel,
    network: frp::Network,
}

/// Internal representation of `Registry`.
#[derive(Clone, CloneRef, Debug)]
pub struct RegistryModel {
    logger:             Logger,
    keyboard:           keyboard::Keyboard,
    mouse:              Mouse,
    command_registry:   command::Registry,
    shortcuts_registry: shortcuts::HashSetRegistry<Shortcut>,
}

impl Deref for Registry {
    type Target = RegistryModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl Registry {
    /// Constructor.
    pub fn new(
        logger: &Logger,
        mouse: &Mouse,
        keyboard: &keyboard::Keyboard,
        cmd_registry: &command::Registry,
    ) -> Self {
        let model = RegistryModel::new(logger, mouse, keyboard, cmd_registry);
        let mouse = &model.mouse;

        frp::new_network! { network
            kb_down    <- keyboard.down.map (f!((t) model.shortcuts_registry.on_press(t.simple_name())));
            kb_up      <- keyboard.up.map   (f!((t) model.shortcuts_registry.on_release(t.simple_name())));
            mouse_down <- mouse.down.map    (f!((t) model.shortcuts_registry.on_press(t.simple_name())));
            mouse_up   <- mouse.up.map      (f!((t) model.shortcuts_registry.on_release(t.simple_name())));
            event      <- any(kb_down,kb_up,mouse_down,mouse_up);
            eval event ((m) model.process_rules(m));
        }
        Self { model, network }
    }
}

impl RegistryModel {
    /// Constructor.
    pub fn new(
        logger: impl AnyLogger,
        mouse: &Mouse,
        keyboard: &keyboard::Keyboard,
        command_registry: &command::Registry,
    ) -> Self {
        let logger = Logger::new_sub(logger, "ShortcutRegistry");
        let keyboard = keyboard.clone_ref();
        let mouse = mouse.clone_ref();
        let command_registry = command_registry.clone_ref();
        let shortcuts_registry = default();
        Self { logger, keyboard, mouse, command_registry, shortcuts_registry }
    }

    fn process_rules(&self, rules: &[Shortcut]) {
        let mut targets = Vec::new();
        {
            let borrowed_command_map = self.command_registry.name_map.borrow();
            for rule in rules {
                let target = &rule.action.target;
                borrowed_command_map.get(target).for_each(|instances| {
                    for instance in instances {
                        if Self::condition_checker(&rule.condition, &instance.status_map) {
                            let command_name = &rule.command.name;
                            match instance.command_map.borrow().get(command_name) {
                                Some(cmd) =>
                                    if cmd.enabled {
                                        targets.push(cmd.frp.clone_ref())
                                    },
                                None => warning!(
                                    &self.logger,
                                    "Command {command_name} was not found on {target}."
                                ),
                            }
                        }
                    }
                })
            }
        }
        for target in targets {
            target.emit(())
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

impl Add<Shortcut> for &Registry {
    type Output = ();
    fn add(self, shortcut: Shortcut) {
        self.model.shortcuts_registry.add(
            shortcut.rule.tp,
            &shortcut.rule.pattern,
            shortcut.clone(),
        );
    }
}
